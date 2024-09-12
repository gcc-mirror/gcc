------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Accessibility;  use Accessibility;
with Atree;          use Atree;
with Aspects;        use Aspects;
with Checks;         use Checks;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Elists;         use Elists;
with Expander;       use Expander;
with Exp_Aggr;       use Exp_Aggr;
with Exp_Atag;       use Exp_Atag;
with Exp_Ch3;        use Exp_Ch3;
with Exp_Ch4;        use Exp_Ch4;
with Exp_Ch7;        use Exp_Ch7;
with Exp_Ch9;        use Exp_Ch9;
with Exp_Dbug;       use Exp_Dbug;
with Exp_Disp;       use Exp_Disp;
with Exp_Dist;       use Exp_Dist;
with Exp_Intr;       use Exp_Intr;
with Exp_Pakd;       use Exp_Pakd;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Freeze;         use Freeze;
with Inline;         use Inline;
with Itypes;         use Itypes;
with Lib;            use Lib;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Dim;        use Sem_Dim;
with Sem_Disp;       use Sem_Disp;
with Sem_Dist;       use Sem_Dist;
with Sem_Eval;       use Sem_Eval;
with Sem_Mech;       use Sem_Mech;
with Sem_Res;        use Sem_Res;
with Sem_SCIL;       use Sem_SCIL;
with Sem_Util;       use Sem_Util;
                     use Sem_Util.Storage_Model_Support;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Stand;          use Stand;
with Tbuild;         use Tbuild;
with Uintp;          use Uintp;
with Validsw;        use Validsw;

package body Exp_Ch6 is

   --------------------------------
   -- Function return mechanisms --
   --------------------------------

   --  This is a summary of the various function return mechanisms implemented
   --  in GNAT for Ada 2005 and later versions of the language. In the below
   --  table, the first column must be read as an if expression: if the result
   --  type of the function is limited, then the return mechanism is and ...;
   --  elsif the result type is indefinite or large definite, then ...; elsif
   --  ...; else ... The different mechanisms are implemented either in the
   --  front end, or in the back end, or partly in both ends, depending on the
   --  result type.

   --    Result type    |  Return mechanism    |    Front end    |   Back end
   --    --------------------------------------------------------------------

   --     Limited           Build In Place              All

   --     Indefinite/       Secondary Stack          Needs Fin.       Others
   --     Large definite

   --     Needs Fin.        Secondary Stack             All
   --     (BERS False)

   --     Needs Fin.        Invisible Parameter         All            All
   --     (BERS True)                                 (return)        (call)

   --     By Reference      Invisible Parameter                        All

   --     Others            Primary stack/                             All
   --                       Registers

   --    Needs Fin.: type needs finalization [RM 7.6(9.1/2-9.6/2)]
   --    BERS: Opt.Back_End_Return_Slot setting

   --  The table is valid for all calls except for those dispatching on result;
   --  the latter calls are considered as returning a class-wide type and thus
   --  always return on the secondary stack, with the help of a small wrapper
   --  function (thunk) if the original result type is not itself returned on
   --  the secondary stack as per the above table.

   --  Suffixes for Build-In-Place extra formals

   BIP_Alloc_Suffix            : constant String := "BIPalloc";
   BIP_Storage_Pool_Suffix     : constant String := "BIPstoragepool";
   BIP_Collection_Suffix       : constant String := "BIPcollection";
   BIP_Task_Master_Suffix      : constant String := "BIPtaskmaster";
   BIP_Activation_Chain_Suffix : constant String := "BIPactivationchain";
   BIP_Object_Access_Suffix    : constant String := "BIPaccess";

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
      Pool_Exp       : Node_Id             := Empty);
   --  Ada 2005 (AI-318-02): If the result type of a build-in-place call needs
   --  them, add the actuals parameters BIP_Alloc_Form and BIP_Storage_Pool.
   --  If Alloc_Form_Exp is present, then pass it for the first parameter,
   --  otherwise pass a literal corresponding to the Alloc_Form parameter
   --  (which must not be Unspecified in that case). If Pool_Exp is present,
   --  then use it for BIP_Storage_Pool, otherwise pass "null".

   procedure Add_Collection_Actual_To_Build_In_Place_Call
     (Function_Call  : Node_Id;
      Function_Id    : Entity_Id;
      Ptr_Typ        : Entity_Id := Empty;
      Collection_Exp : Node_Id   := Empty);
   --  Ada 2005 (AI-318-02): If the result type of a build-in-place call needs
   --  finalization actions, add an actual parameter which is a pointer to the
   --  collection of the access type used by the caller. If Collection_Exp is
   --  present, then that will be passed as the actual. Otherwise, if Ptr_Typ
   --  is Empty, this will result in an automatic "null" value for the actual.

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

   function Caller_Known_Size
     (Func_Call   : Node_Id;
      Result_Subt : Entity_Id) return Boolean;
   --  True if result subtype is definite or has a size that does not require
   --  secondary stack usage (i.e. no variant part or components whose type
   --  depends on discriminants). In particular, untagged types with only
   --  access discriminants do not require secondary stack use. Note we must
   --  always use the secondary stack for dispatching-on-result calls.

   function Check_BIP_Actuals
     (Subp_Call : Node_Id;
      Subp_Id   : Entity_Id) return Boolean;
   --  Given a subprogram call to the given subprogram return True if the
   --  names of BIP extra actual and formal parameters match, and the number
   --  of actuals (including extra actuals) matches the number of formals.

   function Check_Number_Of_Actuals
     (Subp_Call : Node_Id;
      Subp_Id   : Entity_Id) return Boolean;
   --  Given a subprogram call to the given subprogram return True if the
   --  number of actual parameters (including extra actuals) is correct.

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

   procedure Expand_Actuals
     (N         : Node_Id;
      Subp      : Entity_Id;
      Post_Call : out List_Id);
   --  Return a list of actions to take place after the call in Post_Call. The
   --  call will later be rewritten as an Expression_With_Actions, with the
   --  Post_Call actions inserted, and the call inside.
   --
   --  For each actual of an in-out or out parameter which is a numeric (view)
   --  conversion of the form T (A), where A denotes a variable, we insert the
   --  declaration:
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
   --  For nonscalar objects that are possibly unaligned, add call by copy code
   --  (copy in for IN and IN OUT, copy out for OUT and IN OUT).
   --
   --  For OUT and IN OUT parameters, add predicate checks after the call
   --  based on the predicates of the actual type.

   procedure Expand_Call_Helper (N : Node_Id; Post_Call : out List_Id);
   --  Does the main work of Expand_Call. Post_Call is as for Expand_Actuals.

   procedure Expand_Ctrl_Function_Call (N : Node_Id; Use_Sec_Stack : Boolean);
   --  N is a function call which returns a controlled object. Transform the
   --  call into a temporary which retrieves the returned object from the
   --  primary or secondary stack (Use_Sec_Stack says which) using 'reference.

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

   procedure Expand_Simple_Function_Return (N : Node_Id);
   --  Expand simple return from function. In the case where we are returning
   --  from a function body this is called by Expand_N_Simple_Return_Statement.

   procedure Insert_Post_Call_Actions (N : Node_Id; Post_Call : List_Id);
   --  Insert the Post_Call list previously produced by routine Expand_Actuals
   --  or Expand_Call_Helper into the tree.

   function Is_Function_Call_With_BIP_Formals (N : Node_Id) return Boolean;
   --  Ada 2005 (AI-318-02): Returns True if N denotes a call to a function
   --  that requires handling as a build-in-place call, that is, BIP function
   --  calls and calls to functions with inherited BIP formals. For example:
   --
   --    type Iface is limited interface;
   --    function Get_Object return Iface;
   --    --  This function has BIP extra formals
   --
   --    type Root1 is limited tagged record ...
   --    type T1 is new Root1 and Iface with ...
   --    function Get_Object return T1;
   --    --  This primitive requires the BIP formals, and the evaluation of
   --    --  Is_Build_In_Place_Function_Call returns True.
   --
   --    type Root2 is tagged record ...
   --    type T2 is new Root2 and Iface with ...
   --    function Get_Object return T2;
   --    --  This primitive inherits the BIP formals of the interface primitive
   --    --  but, given that T2 is not a limited type, it does not require such
   --    --  formals; therefore Is_Build_In_Place_Function_Call returns False.

   procedure Replace_Renaming_Declaration_Id
      (New_Decl  : Node_Id;
       Orig_Decl : Node_Id);
   --  Replace the internal identifier of the new renaming declaration New_Decl
   --  with the identifier of its original declaration Orig_Decl exchanging the
   --  entities containing their defining identifiers to ensure the correct
   --  replacement of the object declaration by the object renaming declaration
   --  to avoid homograph conflicts (since the object declaration's defining
   --  identifier was already entered in the current scope). The Next_Entity
   --  links of the two entities are also swapped since the entities are part
   --  of the return scope's entity list and the list structure would otherwise
   --  be corrupted. The homonym chain is preserved as well.

   procedure Set_Enclosing_Sec_Stack_Return (N : Node_Id);
   --  N is a return statement for a function that returns its result on the
   --  secondary stack. This sets the Sec_Stack_Needed_For_Return flag on the
   --  function and all blocks and loops that the return statement is jumping
   --  out of. This ensures that the secondary stack is not released; otherwise
   --  the function result would be reclaimed before returning to the caller.

   procedure Warn_BIP (Func_Call : Node_Id);
   --  Give a warning on a build-in-place function call if the -gnatd_B switch
   --  was given.

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

      if No (Return_Object) then
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
      Pool_Exp       : Node_Id             := Empty)
   is
      Loc : constant Source_Ptr := Sloc (Function_Call);

      Alloc_Form_Actual : Node_Id;
      Alloc_Form_Formal : Node_Id;

   begin
      --  Nothing to do when the size of the object is known, and the caller is
      --  in charge of allocating it, and the callee doesn't unconditionally
      --  require an allocation form (such as due to having a tagged result).

      if not Needs_BIP_Alloc_Form (Function_Id) then
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

      --  Pass the Storage_Pool parameter. This parameter is omitted on ZFP as
      --  those targets do not support pools.

      if RTE_Available (RE_Root_Storage_Pool_Ptr) then
         declare
            Pool_Actual : constant Node_Id :=
              (if Present (Pool_Exp) then Pool_Exp else Make_Null (Loc));
            Pool_Formal : constant Node_Id :=
              Build_In_Place_Formal (Function_Id, BIP_Storage_Pool);

         begin
            Analyze_And_Resolve (Pool_Actual, Etype (Pool_Formal));
            Add_Extra_Actual_To_Call (Function_Call, Pool_Formal, Pool_Actual);
         end;
      end if;
   end Add_Unconstrained_Actuals_To_Build_In_Place_Call;

   --------------------------------------------------
   -- Add_Collection_Actual_To_Build_In_Place_Call --
   --------------------------------------------------

   procedure Add_Collection_Actual_To_Build_In_Place_Call
     (Function_Call  : Node_Id;
      Function_Id    : Entity_Id;
      Ptr_Typ        : Entity_Id := Empty;
      Collection_Exp : Node_Id   := Empty)
   is
      Loc : constant Source_Ptr := Sloc (Function_Call);

      Actual    : Node_Id;
      Formal    : Node_Id;
      Desig_Typ : Entity_Id;

   begin
      if not Needs_BIP_Collection (Function_Id) then
         return;
      end if;

      Formal := Build_In_Place_Formal (Function_Id, BIP_Collection);

      --  If there is a finalization collection actual, such as the implicit
      --  finalization collection of an enclosing build-in-place function,
      --  then this must be added as an extra actual of the call.

      if Present (Collection_Exp) then
         Actual := Collection_Exp;

      --  Case where the context does not require an actual collection

      elsif No (Ptr_Typ) then
         Actual := Make_Null (Loc);

      else
         Desig_Typ := Directly_Designated_Type (Ptr_Typ);

         --  Check for a type that is subject to pragma No_Heap_Finalization.
         --  Such a type lacks a collection. Pass a null actual to callee to
         --  signal a missing collection.

         if No_Heap_Finalization (Ptr_Typ) then
            Actual := Make_Null (Loc);

         --  Types in need of finalization actions

         elsif Needs_Finalization (Desig_Typ) then

            --  The general mechanism of creating finalization collections
            --  for anonymous access types is disabled by default, otherwise
            --  finalization collections will pop all over the place. Instead
            --  such types use context-specific collections.

            if Ekind (Ptr_Typ) = E_Anonymous_Access_Type
              and then No (Finalization_Collection (Ptr_Typ))
            then
               Build_Anonymous_Collection (Ptr_Typ);
            end if;

            --  Access-to-controlled types should always have a collection

            pragma Assert (Present (Finalization_Collection (Ptr_Typ)));

            Actual :=
              Make_Attribute_Reference (Loc,
                Prefix =>
                  New_Occurrence_Of (Finalization_Collection (Ptr_Typ), Loc),
                Attribute_Name => Name_Unrestricted_Access);

         --  Tagged types

         else
            Actual := Make_Null (Loc);
         end if;
      end if;

      Analyze_And_Resolve (Actual, Etype (Formal));

      --  Build the parameter association for the new actual and add it to
      --  the end of the function's actuals.

      Add_Extra_Actual_To_Call (Function_Call, Formal, Actual);
   end Add_Collection_Actual_To_Build_In_Place_Call;

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
      Actual        : Node_Id;
      Chain_Actual  : Node_Id;
      Chain_Formal  : Node_Id;
      Master_Formal : Node_Id;

   begin
      pragma Assert (Ekind (Function_Id) in E_Function
                                          | E_Subprogram_Type);

      --  No such extra parameters are needed if there are no tasks

      if not Needs_BIP_Task_Actuals (Function_Id) then

         --  However we must add dummy extra actuals if the function is
         --  a dispatching operation that inherited these extra formals
         --  or an access-to-subprogram type that requires these extra
         --  actuals.

         if Has_BIP_Extra_Formal (Function_Id, BIP_Task_Master,
              Must_Be_Frozen => False)
         then
            Master_Formal :=
              Build_In_Place_Formal (Function_Id, BIP_Task_Master);
            Actual := Make_Integer_Literal (Loc, Uint_0);
            Analyze_And_Resolve (Actual, Etype (Master_Formal));
            Add_Extra_Actual_To_Call (Function_Call, Master_Formal, Actual);

            Chain_Formal :=
              Build_In_Place_Formal (Function_Id, BIP_Activation_Chain);
            Chain_Actual := Make_Null (Loc);
            Analyze_And_Resolve (Chain_Actual, Etype (Chain_Formal));
            Add_Extra_Actual_To_Call
              (Function_Call, Chain_Formal, Chain_Actual);
         end if;

         return;
      end if;

      Actual := Master_Actual;

      --  Use a dummy _master actual in case of No_Task_Hierarchy

      if Restriction_Active (No_Task_Hierarchy) then
         Actual := Make_Integer_Literal (Loc, Library_Task_Level);

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

      if Restriction_Active (No_Task_Hierarchy) then
         Chain_Actual := Make_Null (Loc);

      elsif No (Chain) then
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

   ----------------------------------
   -- Apply_CW_Accessibility_Check --
   ----------------------------------

   procedure Apply_CW_Accessibility_Check (Exp : Node_Id; Func : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (Exp);

   begin
       --  CodePeer does not do anything useful on Ada.Tags.Type_Specific_Data
       --  components.

      if Ada_Version >= Ada_2005
        and then not CodePeer_Mode
        and then Tagged_Type_Expansion
        and then not Scope_Suppress.Suppress (Accessibility_Check)
        and then
          (Is_Class_Wide_Type (Etype (Exp))
            or else Nkind (Exp) in
                      N_Type_Conversion | N_Unchecked_Type_Conversion
            or else (Is_Entity_Name (Exp)
                      and then Is_Formal (Entity (Exp)))
            or else Scope_Depth (Enclosing_Dynamic_Scope (Etype (Exp))) >
                      Scope_Depth (Enclosing_Dynamic_Scope (Func)))
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

            --  Suppress junk access chacks on RE_Tag_Ptr

            Insert_Action (Exp,
              Make_Raise_Program_Error (Loc,
                Condition =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Build_Get_Access_Level (Loc, Tag_Node),
                    Right_Opnd =>
                      Make_Integer_Literal (Loc,
                        Scope_Depth (Enclosing_Dynamic_Scope (Func)))),
                Reason    => PE_Accessibility_Check_Failed),
              Suppress => Access_Check);
         end;
      end if;
   end Apply_CW_Accessibility_Check;

   -----------------------
   -- BIP_Formal_Suffix --
   -----------------------

   function BIP_Formal_Suffix (Kind : BIP_Formal_Kind) return String is
   begin
      case Kind is
         when BIP_Alloc_Form =>
            return BIP_Alloc_Suffix;

         when BIP_Storage_Pool =>
            return BIP_Storage_Pool_Suffix;

         when BIP_Collection =>
            return BIP_Collection_Suffix;

         when BIP_Task_Master =>
            return BIP_Task_Master_Suffix;

         when BIP_Activation_Chain =>
            return BIP_Activation_Chain_Suffix;

         when BIP_Object_Access =>
            return BIP_Object_Access_Suffix;
      end case;
   end BIP_Formal_Suffix;

   ---------------------
   -- BIP_Suffix_Kind --
   ---------------------

   function BIP_Suffix_Kind (E : Entity_Id) return BIP_Formal_Kind is
      Nam : constant String := Get_Name_String (Chars (E));

      function Has_Suffix (Suffix : String) return Boolean;
      --  Return True if Nam has suffix Suffix

      function Has_Suffix (Suffix : String) return Boolean is
         Len : constant Natural := Suffix'Length;
      begin
         return Nam'Length > Len
           and then Nam (Nam'Last - Len + 1 .. Nam'Last) = Suffix;
      end Has_Suffix;

   --  Start of processing for BIP_Suffix_Kind

   begin
      if Has_Suffix (BIP_Alloc_Suffix) then
         return BIP_Alloc_Form;

      elsif Has_Suffix (BIP_Storage_Pool_Suffix) then
         return BIP_Storage_Pool;

      elsif Has_Suffix (BIP_Collection_Suffix) then
         return BIP_Collection;

      elsif Has_Suffix (BIP_Task_Master_Suffix) then
         return BIP_Task_Master;

      elsif Has_Suffix (BIP_Activation_Chain_Suffix) then
         return BIP_Activation_Chain;

      elsif Has_Suffix (BIP_Object_Access_Suffix) then
         return BIP_Object_Access;

      else
         raise Program_Error;
      end if;
   end BIP_Suffix_Kind;

   ---------------------------
   -- Build_In_Place_Formal --
   ---------------------------

   function Build_In_Place_Formal
     (Func : Entity_Id;
      Kind : BIP_Formal_Kind) return Entity_Id
   is
      Extra_Formal  : Entity_Id := Extra_Formals (Func);
      Formal_Suffix : constant String := BIP_Formal_Suffix (Kind);

   begin
      --  Maybe it would be better for each implicit formal of a build-in-place
      --  function to have a flag or a Uint attribute to identify it. ???

      --  The return type in the function declaration may have been a limited
      --  view, and the extra formals for the function were not generated at
      --  that point. At the point of call the full view must be available and
      --  the extra formals can be created and Returns_By_Ref computed.

      if No (Extra_Formal) then
         Create_Extra_Formals (Func);
         Extra_Formal := Extra_Formals (Func);
         Compute_Returns_By_Ref (Func);
      end if;

      --  We search for a formal with a matching suffix. We can't search
      --  for the full name, because of the code at the end of Sem_Ch6.-
      --  Create_Extra_Formals, which copies the Extra_Formals over to
      --  the Alias of an instance, which will cause the formals to have
      --  "incorrect" names.

      while Present (Extra_Formal) loop
         declare
            Name : constant String := Get_Name_String (Chars (Extra_Formal));
         begin
            exit when Name'Length >= Formal_Suffix'Length
              and then Formal_Suffix =
                Name (Name'Last - Formal_Suffix'Length + 1 .. Name'Last);
         end;

         Next_Formal_With_Extras (Extra_Formal);
      end loop;

      if No (Extra_Formal) then
         raise Program_Error;
      end if;

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

      Proc_Decl : constant Node_Id := Prev (Unit_Declaration_Node (Func_Id));
      --  It is assumed that the node before the declaration of the
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
               Replace_Returns (Param_Id,
                 Statements (Handled_Statement_Sequence (Stmt)));

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

   -----------------------
   -- Caller_Known_Size --
   -----------------------

   function Caller_Known_Size
     (Func_Call   : Node_Id;
      Result_Subt : Entity_Id) return Boolean
   is
      Utyp : constant Entity_Id := Underlying_Type (Result_Subt);

   begin
      return not Needs_Secondary_Stack (Utyp)
        and then not (Is_Tagged_Type (Utyp)
                       and then Present (Controlling_Argument (Func_Call)));
   end Caller_Known_Size;

   -----------------------
   -- Check_BIP_Actuals --
   -----------------------

   function Check_BIP_Actuals
     (Subp_Call : Node_Id;
      Subp_Id   : Entity_Id) return Boolean
   is
      Formal : Entity_Id;
      Actual : Node_Id;

   begin
      pragma Assert (Nkind (Subp_Call) in N_Entry_Call_Statement
                                        | N_Function_Call
                                        | N_Procedure_Call_Statement);

      --  In CodePeer_Mode, the tree for `'Elab_Spec` procedures will be
      --  malformed because GNAT does not perform the usual expansion that
      --  results in the importation of external elaboration procedure symbols.
      --  This is expected: the CodePeer backend has special handling for this
      --  malformed tree.
      --  Thus, we do not need to check the tree (and in fact can't, because
      --  it's malformed).

      if CodePeer_Mode
        and then Nkind (Name (Subp_Call)) = N_Attribute_Reference
        and then Attribute_Name (Name (Subp_Call)) in Name_Elab_Spec
                                                    | Name_Elab_Body
                                                    | Name_Elab_Subp_Body
      then
         return True;
      end if;

      Formal := First_Formal_With_Extras (Subp_Id);
      Actual := First_Actual (Subp_Call);

      while Present (Formal) and then Present (Actual) loop
         if Is_Build_In_Place_Entity (Formal)
           and then Nkind (Actual) = N_Identifier
           and then Is_Build_In_Place_Entity (Entity (Actual))
           and then BIP_Suffix_Kind (Formal)
                      /= BIP_Suffix_Kind (Entity (Actual))
         then
            return False;
         end if;

         Next_Formal_With_Extras (Formal);
         Next_Actual (Actual);
      end loop;

      return No (Formal) and then No (Actual);
   end Check_BIP_Actuals;

   -----------------------------
   -- Check_Number_Of_Actuals --
   -----------------------------

   function Check_Number_Of_Actuals
     (Subp_Call : Node_Id;
      Subp_Id   : Entity_Id) return Boolean
   is
      Formal : Entity_Id;
      Actual : Node_Id;

   begin
      pragma Assert (Nkind (Subp_Call) in N_Entry_Call_Statement
                                        | N_Function_Call
                                        | N_Procedure_Call_Statement);

      Formal := First_Formal_With_Extras (Subp_Id);
      Actual := First_Actual (Subp_Call);

      while Present (Formal) and then Present (Actual) loop
         Next_Formal_With_Extras (Formal);
         Next_Actual (Actual);
      end loop;

      return No (Formal) and then No (Actual);
   end Check_Number_Of_Actuals;

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

   procedure Expand_Actuals
     (N         : Node_Id;
      Subp      : Entity_Id;
      Post_Call : out List_Id)
   is
      Loc      : constant Source_Ptr := Sloc (N);
      Actual   : Node_Id;
      Formal   : Entity_Id;
      N_Node   : Node_Id;
      E_Actual : Entity_Id;
      E_Formal : Entity_Id;

      procedure Add_Call_By_Copy_Code;
      --  For cases where the parameter must be passed by copy, this routine
      --  generates a temporary variable into which the actual is copied and
      --  then passes this as the parameter. For an OUT or IN OUT parameter,
      --  an assignment is also generated to copy the result back. The call
      --  also takes care of any constraint checks required for the type
      --  conversion case (on both the way in and the way out).

      procedure Add_Simple_Call_By_Copy_Code (Force : Boolean);
      --  This is similar to the above, but is used in cases where we know
      --  that all that is needed is to simply create a temporary and copy
      --  the value in and out of the temporary. If Force is True, then the
      --  procedure may disregard legality considerations.

      --  ??? We need to do the copy for a bit-packed array because this is
      --  where the rewriting into a mask-and-shift sequence is done. But of
      --  course this may break the program if it expects bits to be really
      --  passed by reference. That's what we have done historically though.

      procedure Add_Validation_Call_By_Copy_Code (Act : Node_Id);
      --  Perform copy-back for actual parameter Act which denotes a validation
      --  variable.

      procedure Check_Fortran_Logical;
      --  A value of type Logical that is passed through a formal parameter
      --  must be normalized because .TRUE. usually does not have the same
      --  representation as True. We assume that .FALSE. = False = 0.
      --  What about functions that return a logical type ???

      function Is_Legal_Copy return Boolean;
      --  Check that an actual can be copied before generating the temporary
      --  to be used in the call. If the formal is of a by_reference type or
      --  is aliased, then the program is illegal (this can only happen in
      --  the presence of representation clauses that force a misalignment)
      --  If the formal is a by_reference parameter imposed by a DEC pragma,
      --  emit a warning that this might lead to unaligned arguments.

      function Make_Var (Actual : Node_Id) return Entity_Id;
      --  Returns an entity that refers to the given actual parameter, Actual
      --  (not including any type conversion). If Actual is an entity name,
      --  then this entity is returned unchanged, otherwise a renaming is
      --  created to provide an entity for the actual.

      procedure Reset_Packed_Prefix;
      --  The expansion of a packed array component reference is delayed in
      --  the context of a call. Now we need to complete the expansion, so we
      --  unmark the analyzed bits in all prefixes.

      function Requires_Atomic_Or_Volatile_Copy return Boolean;
      --  Returns whether a copy is required as per RM C.6(19) and gives a
      --  warning in this case. This also handles the special case of a base
      --  array type with full access semantics.

      ---------------------------
      -- Add_Call_By_Copy_Code --
      ---------------------------

      procedure Add_Call_By_Copy_Code is
         Crep  : Boolean;
         Expr  : Node_Id;
         F_Typ : Entity_Id := Etype (Formal);
         Indic : Node_Id;
         Init  : Node_Id;
         Temp  : Entity_Id;
         V_Typ : Entity_Id;
         Var   : Entity_Id;

      begin
         if not Is_Legal_Copy then
            return;
         end if;

         Temp := Make_Temporary (Loc, 'T', Actual);

         --  Handle formals whose type comes from the limited view

         if From_Limited_With (F_Typ)
           and then Has_Non_Limited_View (F_Typ)
         then
            F_Typ := Non_Limited_View (F_Typ);
         end if;

         --  Use formal type for temp, unless formal type is an unconstrained
         --  array, in which case we don't have to worry about bounds checks,
         --  and we use the actual type, since that has appropriate bounds.

         if Is_Array_Type (F_Typ) and then not Is_Constrained (F_Typ) then
            Indic := New_Occurrence_Of (Etype (Actual), Loc);
         else
            Indic := New_Occurrence_Of (F_Typ, Loc);
         end if;

         --  The new code will be properly analyzed below and the setting of
         --  the Do_Range_Check flag recomputed so remove the obsolete one.

         Set_Do_Range_Check (Actual, False);

         if Nkind (Actual) = N_Type_Conversion then
            Set_Do_Range_Check (Expression (Actual), False);

            V_Typ := Etype (Expression (Actual));

            --  If the formal is an (in-)out parameter, capture the name
            --  of the variable in order to build the post-call assignment.

            Var := Make_Var (Expression (Actual));

            Crep := not Has_Compatible_Representation
                          (Target_Typ  => F_Typ,
                           Operand_Typ => Etype (Expression (Actual)));

         else
            V_Typ := Etype (Actual);
            Var   := Make_Var (Actual);
            Crep  := False;
         end if;

         --  If the actual denotes a variable which captures the value of an
         --  object for validation purposes, we propagate the link with this
         --  object to the new variable made from the actual just above.

         if Ekind (Formal) /= E_In_Parameter
           and then Is_Validation_Variable_Reference (Actual)
         then
            declare
               Ref : constant Node_Id := Unqual_Conv (Actual);

            begin
               if Is_Entity_Name (Ref) then
                  Set_Validated_Object (Var, Validated_Object (Entity (Ref)));

               else
                  pragma Assert (False);
                  null;
               end if;
            end;
         end if;

         --  Setup initialization for case of in out parameter, or an out
         --  parameter where the formal is an unconstrained array (in the
         --  latter case, we have to pass in an object with bounds).

         --  If this is an out parameter, the initial copy is wasteful, so as
         --  an optimization for the one-dimensional case we extract the
         --  bounds of the actual and build an uninitialized temporary of the
         --  right size.

         --  If the formal is an out parameter with discriminants, the
         --  discriminants must be captured even if the rest of the object
         --  is in principle uninitialized, because the discriminants may
         --  be read by the called subprogram.

         if Ekind (Formal) = E_In_Out_Parameter
           or else (Is_Array_Type (F_Typ) and then not Is_Constrained (F_Typ))
           or else Has_Discriminants (F_Typ)
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

         --  Access types are passed in without checks, but if a copy-back is
         --  required for a null-excluding check on an in-out or out parameter,
         --  then the initial value is that of the actual.

         elsif Is_Access_Type (E_Formal)
           and then Can_Never_Be_Null (Etype (Actual))
           and then not Can_Never_Be_Null (E_Formal)
         then
            Init := New_Occurrence_Of (Var, Loc);

         --  View conversions when the formal type has the Default_Value aspect
         --  require passing in the value of the conversion's operand. The type
         --  of that operand also has Default_Value, as required by AI12-0074
         --  (RM 6.4.1(5.3/4)). The subtype denoted by the subtype_indication
         --  is changed to the base type of the formal subtype, to ensure that
         --  the actual's value can be assigned without a constraint check
         --  (note that no check is done on passing to an out parameter). Also
         --  note that the two types necessarily share the same ancestor type,
         --  as required by 6.4.1(5.2/4), so underlying base types will match.

         elsif Ekind (Formal) = E_Out_Parameter
           and then Is_Scalar_Type (Etype (F_Typ))
           and then Nkind (Actual) = N_Type_Conversion
           and then Present (Default_Aspect_Value (Etype (F_Typ)))
         then
            Indic := New_Occurrence_Of (Base_Type (F_Typ), Loc);
            Init  := Convert_To
                       (Base_Type (F_Typ), New_Occurrence_Of (Var, Loc));

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
            Set_Is_True_Constant (Temp, False);

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

            Rewrite (Actual, New_Occurrence_Of (Temp, Sloc (Actual)));
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
                 and then not No_Dynamic_Accessibility_Checks_Enabled (Lhs)
               then
                  --  Copyback target is an Ada 2012 stand-alone object of an
                  --  anonymous access type.

                  pragma Assert (Ada_Version >= Ada_2012);

                  Apply_Accessibility_Check (Lhs, E_Formal, N);

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
                  if Is_Access_Type (E_Formal)
                    and then Can_Never_Be_Null (Etype (Actual))
                    and then not Can_Never_Be_Null (E_Formal)
                  then
                     Append_To (Post_Call,
                       Make_Raise_Constraint_Error (Loc,
                         Condition =>
                           Make_Op_Eq (Loc,
                             Left_Opnd  => New_Occurrence_Of (Temp, Loc),
                             Right_Opnd => Make_Null (Loc)),
                         Reason => CE_Access_Check_Failed));
                  end if;

                  Append_To (Post_Call,
                    Make_Assignment_Statement (Loc,
                      Name       => Lhs,
                      Expression => Expr));
               end if;

               --  Add a copy-back to reflect any potential changes in value
               --  back into the original object, if any.

               if Is_Validation_Variable_Reference (Lhs) then
                  Add_Validation_Call_By_Copy_Code (Lhs);
               end if;
            end;
         end if;
      end Add_Call_By_Copy_Code;

      ----------------------------------
      -- Add_Simple_Call_By_Copy_Code --
      ----------------------------------

      procedure Add_Simple_Call_By_Copy_Code (Force : Boolean) is
         With_Storage_Model : constant Boolean :=
           Nkind (Actual) = N_Explicit_Dereference
             and then
               Has_Designated_Storage_Model_Aspect (Etype (Prefix (Actual)));

         Cpcod  : List_Id;
         Decl   : Node_Id;
         F_Typ  : Entity_Id;
         Incod  : Node_Id;
         Indic  : Node_Id;
         Lhs    : Node_Id;
         Outcod : Node_Id;
         Rhs    : Node_Id;
         Temp   : Entity_Id;

      begin
         --  Unless forced not to, check the legality of the copy operation

         if not Force and then not Is_Legal_Copy then
            return;
         end if;

         F_Typ := Etype (Formal);

         --  Handle formals whose type comes from the limited view

         if From_Limited_With (F_Typ)
           and then Has_Non_Limited_View (F_Typ)
         then
            F_Typ := Non_Limited_View (F_Typ);
         end if;

         --  Use formal type for temp, unless formal type is an unconstrained
         --  composite, in which case we don't have to worry about checks and
         --  we can use the actual type, since that has appropriate bounds.

         if Is_Composite_Type (F_Typ) and then not Is_Constrained (F_Typ) then
            Indic := New_Occurrence_Of (Get_Actual_Subtype (Actual), Loc);
         else
            Indic := New_Occurrence_Of (F_Typ, Loc);
         end if;

         --  Prepare to generate code

         Reset_Packed_Prefix;

         Incod  := Relocate_Node (Actual);
         Outcod := New_Copy_Tree (Incod);

         --  Generate declaration of temporary variable, initializing it
         --  with the input parameter unless we have an OUT formal or
         --  this is an initialization call.

         if Ekind (Formal) = E_Out_Parameter then
            Incod := Empty;

         elsif Inside_Init_Proc then

            --  Skip using the actual as the expression in Decl if we are in
            --  an init proc and it is not a component which depends on a
            --  discriminant, because, in this case, we need to use the actual
            --  type of the component instead.

            if Nkind (Actual) /= N_Selected_Component
              or else
                not Has_Discriminant_Dependent_Constraint
                  (Entity (Selector_Name (Actual)))
            then
               Incod := Empty;

            --  Otherwise, keep the component so we can generate the proper
            --  actual subtype - since the subtype depends on enclosing
            --  discriminants.

            else
               null;
            end if;
         end if;

         Cpcod := New_List;

         if With_Storage_Model then
            Temp :=
              Build_Temporary_On_Secondary_Stack (Loc, Entity (Indic), Cpcod);

            if Present (Incod) then
               Append_To (Cpcod,
                 Make_Assignment_Statement (Loc,
                   Name       =>
                     Make_Explicit_Dereference (Loc,
                       Prefix => New_Occurrence_Of (Temp, Loc)),
                   Expression => Incod));
               Set_Suppress_Assignment_Checks (Last (Cpcod));
            end if;

         else
            Temp := Make_Temporary (Loc, 'T', Actual);

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp,
                Object_Definition   => Indic,
                Expression          => Incod);

            --  If the call is to initialize a component of a composite type,
            --  and the component does not depend on discriminants, use the
            --  actual type of the component. This is required in case the
            --  component is constrained, because in general the formal of the
            --  initialization procedure will be unconstrained. Note that if
            --  the component being initialized is constrained by an enclosing
            --  discriminant, the presence of the initialization in the
            --  declaration will generate an expression for the actual subtype.

            if Inside_Init_Proc and then No (Incod) then
               Set_No_Initialization (Decl);
               Set_Object_Definition (Decl,
                 New_Occurrence_Of (Etype (Actual), Loc));
            end if;

            Append_To (Cpcod, Decl);
         end if;

         Insert_Actions (N, Cpcod);

         --  The actual is simply a reference to the temporary

         if With_Storage_Model then
            Rewrite (Actual,
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Temp, Loc)));
         else
            Rewrite (Actual, New_Occurrence_Of (Temp, Loc));
         end if;

         Analyze (Actual);

         --  Generate copy out if OUT or IN OUT parameter

         if Ekind (Formal) /= E_In_Parameter then
            Lhs := Outcod;

            if With_Storage_Model then
               Rhs :=
                 Make_Explicit_Dereference (Loc,
                   Prefix => New_Occurrence_Of (Temp, Loc));
            else
               Rhs := New_Occurrence_Of (Temp, Loc);
               Set_Is_True_Constant (Temp, False);
            end if;

            --  Deal with conversion

            if Nkind (Lhs) = N_Type_Conversion then
               Lhs := Expression (Lhs);
               Rhs := Convert_To (Etype (Actual), Rhs);
            end if;

            Append_To (Post_Call,
              Make_Assignment_Statement (Loc,
                Name       => Lhs,
                Expression => Rhs));
            Set_Suppress_Assignment_Checks (Last (Post_Call));
            Set_Assignment_OK (Name (Last (Post_Call)));
         end if;
      end Add_Simple_Call_By_Copy_Code;

      --------------------------------------
      -- Add_Validation_Call_By_Copy_Code --
      --------------------------------------

      procedure Add_Validation_Call_By_Copy_Code (Act : Node_Id) is
         Var : constant Node_Id := Unqual_Conv (Act);

         Expr    : Node_Id;
         Obj     : Node_Id;
         Obj_Typ : Entity_Id;
         Var_Id  : Entity_Id;

      begin
         --  Generate range check if required

         if Do_Range_Check (Actual) then
            Generate_Range_Check (Actual, E_Formal, CE_Range_Check_Failed);
         end if;

         --  If there is a type conversion in the actual, it will be reinstated
         --  below, the new instance will be properly analyzed and the setting
         --  of the Do_Range_Check flag recomputed so remove the obsolete one.

         if Nkind (Actual) = N_Type_Conversion then
            Set_Do_Range_Check (Expression (Actual), False);
         end if;

         --  Copy the value of the validation variable back into the object
         --  being validated.

         if Is_Entity_Name (Var) then
            Var_Id  := Entity (Var);
            Obj     := Validated_Object (Var_Id);
            Obj_Typ := Etype (Obj);

            Expr := New_Occurrence_Of (Var_Id, Loc);

            --  A type conversion is needed when the validation variable and
            --  the validated object carry different types. This case occurs
            --  when the actual is qualified in some fashion.

            --    Common:
            --      subtype Int is Integer range ...;
            --      procedure Call (Val : in out Integer);

            --    Original:
            --      Object : Int;
            --      Call (Integer (Object));

            --    Expanded:
            --      Object : Int;
            --      Var : Integer := Object;  --  conversion to base type
            --      if not Var'Valid then     --  validity check
            --      Call (Var);               --  modify Var
            --      Object := Int (Var);      --  conversion to subtype

            if Etype (Var_Id) /= Obj_Typ then
               Expr :=
                 Make_Type_Conversion (Loc,
                   Subtype_Mark => New_Occurrence_Of (Obj_Typ, Loc),
                   Expression   => Expr);
            end if;

            --  Generate:
            --    Object := Var;
            --      <or>
            --    Object := Object_Type (Var);

            Append_To (Post_Call,
              Make_Assignment_Statement (Loc,
                Name       => Obj,
                Expression => Expr));

         --  If the flow reaches this point, then this routine was invoked with
         --  an actual which does not denote a validation variable.

         else
            pragma Assert (False);
            null;
         end if;
      end Add_Validation_Call_By_Copy_Code;

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
         --  Calls to the initialization procedure of full access types may
         --  require a copy in order to implement the full access semantics.

         if Is_Init_Proc (Subp) and then Is_Full_Access (Etype (Formal)) then
            return True;

         --  In the other cases, a copy is not allowed for by-reference types
         --  or if the parameter is aliased or explicitly passed by reference.

         elsif Is_By_Reference_Type (Etype (Formal))
           or else Is_Aliased (Formal)
           or else (Mechanism (Formal) = By_Reference
                     and then not Has_Foreign_Convention (Subp))
         then
            --  An attempt to copy a value of such types can only occur if
            --  representation clauses give the actual a misaligned address.
            --  The actual may in fact be properly aligned but there is not
            --  enough front-end information to determine this. In that case
            --  gigi will emit an error or a warning if a copy is not legal,
            --  or generate the proper code.

            return False;

         --  For users of Starlet, we assume that the specification of by-
         --  reference mechanism is mandatory. This may lead to unaligned
         --  objects but at least for DEC legacy code it is known to work.
         --  The warning will alert users of this code that a problem may
         --  be lurking.

         elsif Mechanism (Formal) = By_Reference
           and then Ekind (Scope (Formal)) = E_Procedure
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
              Nkind (Pfx) not in N_Selected_Component | N_Indexed_Component;
            Pfx := Prefix (Pfx);
         end loop;
      end Reset_Packed_Prefix;

      ----------------------------------------
      --  Requires_Atomic_Or_Volatile_Copy  --
      ----------------------------------------

      function Requires_Atomic_Or_Volatile_Copy return Boolean is
      begin
         --  If the formal is already passed by copy, no need to do anything

         if Is_By_Copy_Type (E_Formal) then
            return False;
         end if;

         --  There is no requirement inside initialization procedures and this
         --  would generate copies for atomic or volatile composite components.

         if Inside_Init_Proc then
            return False;
         end if;

         --  Check for atomicity mismatch

         if Is_Atomic_Object (Actual) and then not Is_Atomic (E_Formal)
         then
            if Comes_From_Source (N) then
               Error_Msg_N
                 ("??atomic actual passed by copy (RM C.6(19))", Actual);
            end if;
            return True;
         end if;

         --  Check for volatility mismatch

         if Is_Volatile_Object_Ref (Actual) and then not Is_Volatile (E_Formal)
         then
            if Comes_From_Source (N) then
               Error_Msg_N
                 ("??volatile actual passed by copy (RM C.6(19))", Actual);
            end if;
            return True;
         end if;

         --  Special case for the base type of a full access array type: full
         --  access semantics cannot be enforced for the base type inside the
         --  called subprogram so we do it at the call site by means of a copy.

         if Ekind (E_Formal) = E_Array_Type and then Is_Full_Access (E_Formal)
         then
            return True;
         end if;

         return False;
      end Requires_Atomic_Or_Volatile_Copy;

   --  Start of processing for Expand_Actuals

   begin
      Post_Call := New_List;

      Formal := First_Formal (Subp);
      Actual := First_Actual (N);
      while Present (Formal) loop
         E_Formal := Etype (Formal);
         E_Actual := Etype (Actual);

         --  Handle formals whose type comes from the limited view

         if From_Limited_With (E_Formal)
           and then Has_Non_Limited_View (E_Formal)
         then
            E_Formal := Non_Limited_View (E_Formal);
         end if;

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
            --  to be created and access to it must be passed to the function
            --  (and ensure that we have an activation chain defined for tasks
            --  and a Master variable).

            --  Currently we limit such functions to those with inherently
            --  limited result subtypes, but eventually we plan to expand the
            --  functions that are treated as build-in-place to include other
            --  composite result types.

            --  But do not do it here for intrinsic subprograms since this will
            --  be done properly after the subprogram is expanded.

            if Is_Intrinsic_Subprogram (Subp) then
               null;

            elsif Is_Build_In_Place_Function_Call (Actual) then
               if Might_Have_Tasks (Etype (Actual)) then
                  Build_Activation_Chain_Entity (N);
                  Build_Master_Entity (Etype (Actual));
               end if;

               Make_Build_In_Place_Call_In_Anonymous_Context (Actual);

            --  Ada 2005 (AI-318-02): Specialization of the previous case for
            --  actuals containing build-in-place function calls whose returned
            --  object covers interface types.

            elsif Present (Unqual_BIP_Iface_Function_Call (Actual)) then
               Build_Activation_Chain_Entity (N);
               Build_Master_Entity (Etype (Actual));
               Make_Build_In_Place_Iface_Call_In_Anonymous_Context (Actual);
            end if;

            Apply_Constraint_Check (Actual, E_Formal);

         --  Out parameter case. No constraint checks on access type
         --  RM 6.4.1 (13), but on return a null-excluding check may be
         --  required (see below).

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

            --  If argument is a type conversion for a type that is passed by
            --  copy, then we must pass the parameter by copy.

            if Nkind (Actual) = N_Type_Conversion
              and then
                (Is_Elementary_Type (E_Formal)
                  or else Is_Bit_Packed_Array (Etype (Formal))
                  or else Is_Bit_Packed_Array (Etype (Expression (Actual)))

                  --  Also pass by copy if change of representation

                  or else not Has_Compatible_Representation
                                (Target_Typ  => Etype (Formal),
                                 Operand_Typ => Etype (Expression (Actual))))
            then
               Add_Call_By_Copy_Code;

            --  References to components of bit-packed arrays are expanded
            --  at this point, rather than at the point of analysis of the
            --  actuals, to handle the expansion of the assignment to
            --  [in] out parameters.

            elsif Is_Ref_To_Bit_Packed_Array (Actual) then
               Add_Simple_Call_By_Copy_Code (Force => True);

            --  If the actual has a nonnative storage model, we need a copy

            elsif Nkind (Actual) = N_Explicit_Dereference
              and then
                Has_Designated_Storage_Model_Aspect (Etype (Prefix (Actual)))
              and then
                (Present (Storage_Model_Copy_To
                            (Storage_Model_Object (Etype (Prefix (Actual)))))
                  or else
                    (Ekind (Formal) = E_In_Out_Parameter
                      and then
                        Present (Storage_Model_Copy_From
                          (Storage_Model_Object (Etype (Prefix (Actual)))))))
            then
               Add_Simple_Call_By_Copy_Code (Force => True);

            --  If a nonscalar actual is possibly bit-aligned, we need a copy
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
               Add_Simple_Call_By_Copy_Code (Force => False);

            --  References to slices of bit-packed arrays are expanded

            elsif Is_Ref_To_Bit_Packed_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  References to possibly unaligned slices of arrays are expanded

            elsif Is_Possibly_Unaligned_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  Deal with access types where the actual subtype and the
            --  formal subtype are not the same, requiring a check.

            --  It is necessary to exclude tagged types because of "downward
            --  conversion" errors, but null-excluding checks on return may be
            --  required.

            elsif Is_Access_Type (E_Formal)
              and then not Is_Tagged_Type (Designated_Type (E_Formal))
              and then (not Same_Type (E_Formal, E_Actual)
                or else (Can_Never_Be_Null (E_Actual)
                          and then not Can_Never_Be_Null (E_Formal)))
            then
               Add_Call_By_Copy_Code;

            --  We may need to force a copy because of atomicity or volatility
            --  considerations.

            elsif Requires_Atomic_Or_Volatile_Copy then
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
               Add_Call_By_Copy_Code;

            --  The actual denotes a variable which captures the value of an
            --  object for validation purposes. Add a copy-back to reflect any
            --  potential changes in value back into the original object.

            --    Var : ... := Object;
            --    if not Var'Valid then  --  validity check
            --    Call (Var);            --  modify var
            --    Object := Var;         --  update Object

            elsif Is_Validation_Variable_Reference (Actual) then
               Add_Validation_Call_By_Copy_Code (Actual);
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

            begin
               if No (Aund) then
                  Atyp := E_Actual;
               else
                  Atyp := Aund;
               end if;

               if Predicate_Enabled (Atyp)

                 --  Skip predicate checks for special cases

                 and then Predicate_Tests_On_Arguments (Subp)
               then
                  Append_To (Post_Call,
                    Make_Predicate_Check (Atyp, Actual));
               end if;
            end By_Ref_Predicate_Check;

         --  Processing for IN parameters

         else
            --  Generate range check if required

            if Do_Range_Check (Actual) then
               Generate_Range_Check (Actual, E_Formal, CE_Range_Check_Failed);
            end if;

            --  For IN parameters in the bit-packed array case, we expand an
            --  indexed component (the circuit in Exp_Ch4 deliberately left
            --  indexed components appearing as actuals untouched, so that
            --  the special processing above for the OUT and IN OUT cases
            --  could be performed. We could make the test in Exp_Ch4 more
            --  complex and have it detect the parameter mode, but it is
            --  easier simply to handle all cases here.)

            if Nkind (Actual) = N_Indexed_Component
              and then Is_Bit_Packed_Array (Etype (Prefix (Actual)))
            then
               Reset_Packed_Prefix;
               Expand_Packed_Element_Reference (Actual);

            --  If we have a reference to a bit-packed array, we copy it, since
            --  the actual must be byte aligned.

            --  Is this really necessary in all cases???

            elsif Is_Ref_To_Bit_Packed_Array (Actual) then
               Add_Simple_Call_By_Copy_Code (Force => True);

            --  If the actual has a nonnative storage model, we need a copy

            elsif Nkind (Actual) = N_Explicit_Dereference
              and then
                Has_Designated_Storage_Model_Aspect (Etype (Prefix (Actual)))
              and then
                Present (Storage_Model_Copy_From
                           (Storage_Model_Object (Etype (Prefix (Actual)))))
            then
               Add_Simple_Call_By_Copy_Code (Force => True);

            --  If we have a C++ constructor call, we need to create the object

            elsif Is_CPP_Constructor_Call (Actual) then
               Add_Simple_Call_By_Copy_Code (Force => True);

            --  If a nonscalar actual is possibly unaligned, we need a copy

            elsif Is_Possibly_Unaligned_Object (Actual)
              and then not Represented_As_Scalar (Etype (Formal))
            then
               Add_Simple_Call_By_Copy_Code (Force => False);

            --  Similarly, we have to expand slices of packed arrays here
            --  because the result must be byte aligned.

            elsif Is_Ref_To_Bit_Packed_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  Only processing remaining is to pass by copy if this is a
            --  reference to a possibly unaligned slice, since the caller
            --  expects an appropriately aligned argument.

            elsif Is_Possibly_Unaligned_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  We may need to force a copy because of atomicity or volatility
            --  considerations.

            elsif Requires_Atomic_Or_Volatile_Copy then
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

         --  Type-invariant checks for in-out and out parameters, as well as
         --  for in parameters of procedures (AI05-0289 and AI12-0044).

         if Ekind (Formal) /= E_In_Parameter
           or else Ekind (Subp) = E_Procedure
         then
            Caller_Side_Invariant_Checks : declare

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
                    Nkind (Parent (Subp)) in N_Private_Extension_Declaration
                                           | N_Full_Type_Declaration
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

            --  Start of processing for Caller_Side_Invariant_Checks

            begin
               --  We generate caller-side invariant checks in two cases:

               --  a) when calling an inherited operation, where there is an
               --  implicit view conversion of the actual to the parent type.

               --  b) When the conversion is explicit

               --  We treat these cases separately because the required
               --  conversion for a) is added later when expanding the call.

               if Has_Invariants (Etype (Actual))
                  and then
                    Nkind (Parent (Etype (Actual)))
                      = N_Private_Extension_Declaration
               then
                  if Comes_From_Source (N) and then Is_Public_Subp then
                     Append_To (Post_Call, Make_Invariant_Call (Actual));
                  end if;

               elsif Nkind (Actual) = N_Type_Conversion
                 and then Has_Invariants (Etype (Expression (Actual)))
               then
                  if Comes_From_Source (N) and then Is_Public_Subp then
                     Append_To
                       (Post_Call, Make_Invariant_Call (Expression (Actual)));
                  end if;
               end if;
            end Caller_Side_Invariant_Checks;
         end if;

         Next_Formal (Formal);
         Next_Actual (Actual);
      end loop;
   end Expand_Actuals;

   -----------------
   -- Expand_Call --
   -----------------

   procedure Expand_Call (N : Node_Id) is
      function Is_Unchecked_Union_Equality (N : Node_Id) return Boolean;
      --  Return True if N is a call to the predefined equality operator of an
      --  unchecked union type, or a renaming thereof.

      ---------------------------------
      -- Is_Unchecked_Union_Equality --
      ---------------------------------

      function Is_Unchecked_Union_Equality (N : Node_Id) return Boolean is
      begin
         if Is_Entity_Name (Name (N))
           and then Ekind (Entity (Name (N))) = E_Function
           and then Present (First_Formal (Entity (Name (N))))
           and then
             Is_Unchecked_Union (Etype (First_Formal (Entity (Name (N)))))
         then
            declare
               Func : constant Entity_Id := Entity (Name (N));
               Typ  : constant Entity_Id := Etype (First_Formal (Func));
               Decl : constant Node_Id   :=
                 Original_Node (Parent (Declaration_Node (Func)));

            begin
               return Func = TSS (Typ, TSS_Composite_Equality)
                 or else (Nkind (Decl) = N_Subprogram_Renaming_Declaration
                           and then Nkind (Name (Decl)) = N_Operator_Symbol
                           and then Chars (Name (Decl)) = Name_Op_Eq
                           and then Ekind (Entity (Name (Decl))) = E_Operator);
            end;

         else
            return False;
         end if;
      end Is_Unchecked_Union_Equality;

      --  If this is an indirect call through an Access_To_Subprogram
      --  with contract specifications, it is rewritten as a call to
      --  the corresponding Access_Subprogram_Wrapper with the same
      --  actuals, whose body contains a naked indirect call (which
      --  itself must not be rewritten, to prevent infinite recursion).

      Must_Rewrite_Indirect_Call : constant Boolean :=
        Ada_Version >= Ada_2022
          and then Nkind (Name (N)) = N_Explicit_Dereference
          and then Ekind (Etype (Name (N))) = E_Subprogram_Type
          and then Present
            (Access_Subprogram_Wrapper (Etype (Name (N))));

      Post_Call : List_Id;

   --  Start of processing for Expand_Call

   begin
      pragma Assert (Nkind (N) in N_Entry_Call_Statement
                                | N_Function_Call
                                | N_Procedure_Call_Statement);

      --  Check that this is not the call in the body of the access
      --  subprogram wrapper or the postconditions wrapper.

      if Must_Rewrite_Indirect_Call
        and then (not Is_Overloadable (Current_Scope)
             or else not (Is_Access_Subprogram_Wrapper (Current_Scope)
                           or else
                             (Chars (Current_Scope) = Name_uWrapped_Statements
                               and then Is_Access_Subprogram_Wrapper
                                          (Scope (Current_Scope)))))
      then
         declare
            Loc      : constant Source_Ptr := Sloc (N);
            Wrapper  : constant Entity_Id :=
              Access_Subprogram_Wrapper (Etype (Name (N)));
            Ptr      : constant Node_Id   := Prefix (Name (N));
            Ptr_Type : constant Entity_Id := Etype (Ptr);
            Typ      : constant Entity_Id := Etype (N);

            New_N    : Node_Id;
            Parms    : List_Id := Parameter_Associations (N);
            Ptr_Act  : Node_Id;

         begin
            --  The last actual in the call is the pointer itself.
            --  If the aspect is inherited, convert the pointer to the
            --  parent type that specifies the contract.
            --  If the original access_to_subprogram has defaults for
            --  in-mode parameters, the call may include named associations,
            --  so we create one for the pointer as well.

            if Is_Derived_Type (Ptr_Type)
              and then Ptr_Type /= Etype (Last_Formal (Wrapper))
            then
               Ptr_Act :=
                Make_Type_Conversion (Loc,
                  New_Occurrence_Of
                    (Etype (Last_Formal (Wrapper)), Loc), Ptr);

            else
               Ptr_Act := Ptr;
            end if;

            --  Handle parameterless subprogram.

            if No (Parms) then
               Parms := New_List;
            end if;

            Append
             (Make_Parameter_Association (Loc,
                Selector_Name => Make_Identifier (Loc,
                   Chars (Last_Formal (Wrapper))),
                 Explicit_Actual_Parameter => Ptr_Act),
              Parms);

            if Nkind (N) = N_Procedure_Call_Statement then
               New_N := Make_Procedure_Call_Statement (Loc,
                  Name => New_Occurrence_Of (Wrapper, Loc),
                  Parameter_Associations => Parms);
            else
               New_N := Make_Function_Call (Loc,
                  Name => New_Occurrence_Of (Wrapper, Loc),
                  Parameter_Associations => Parms);
            end if;

            Rewrite (N, New_N);
            Analyze_And_Resolve (N, Typ);
         end;

      --  Case of a call to the predefined equality operator of an unchecked
      --  union type, which requires specific processing.

      elsif Is_Unchecked_Union_Equality (N) then
         declare
            Eq : constant Entity_Id := Entity (Name (N));

         begin
            Expand_Unchecked_Union_Equality (N);

            --  If the call was not rewritten as a raise, expand the actuals

            if Nkind (N) = N_Function_Call then
               pragma Assert (Check_Number_Of_Actuals (N, Eq));
               Expand_Actuals (N, Eq, Post_Call);
               pragma Assert (Is_Empty_List (Post_Call));
            end if;
         end;

      --  Normal case

      else
         Expand_Call_Helper (N, Post_Call);
         Insert_Post_Call_Actions (N, Post_Call);
      end if;
   end Expand_Call;

   ------------------------
   -- Expand_Call_Helper --
   ------------------------

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

   procedure Expand_Call_Helper (N : Node_Id; Post_Call : out List_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      Call_Node     : Node_Id := N;
      Extra_Actuals : List_Id := No_List;
      Prev          : Node_Id := Empty;

      procedure Add_Actual_Parameter (Insert_Param : Node_Id);
      --  Adds one entry to the end of the actual parameter list. Used for
      --  default parameters and for extra actuals (for Extra_Formals). The
      --  argument is an N_Parameter_Association node.

      procedure Add_Cond_Expression_Extra_Actual (Formal : Entity_Id);
      --  Adds extra accessibility actuals in the case of a conditional
      --  expression corresponding to Formal.

      --  Note: Conditional expressions used as actuals for anonymous access
      --  formals complicate the process of propagating extra accessibility
      --  actuals and must be handled in a recursive fashion since they can
      --  be embedded within each other.

      procedure Add_Dummy_Build_In_Place_Actuals
        (Function_Id             : Entity_Id;
         Num_Added_Extra_Actuals : Nat := 0);
      --  Adds dummy actuals for the BIP extra formals of the called function.
      --  Num_Added_Extra_Actuals is the number of non-BIP extra actuals added
      --  to the actuals immediately before calling this subprogram.

      procedure Add_Extra_Actual (Expr : Node_Id; EF : Entity_Id);
      --  Adds an extra actual to the list of extra actuals. Expr is the
      --  expression for the value of the actual, EF is the entity for the
      --  extra formal.

      procedure Add_View_Conversion_Invariants
        (Formal : Entity_Id;
         Actual : Node_Id);
      --  Adds invariant checks for every intermediate type between the range
      --  of a view converted argument to its ancestor (from parent to child).

      function Can_Fold_Predicate_Call (P : Entity_Id) return Boolean;
      --  Try to constant-fold a predicate check, which often enough is a
      --  simple arithmetic expression that can be computed statically if
      --  its argument is static. This cleans up the output of CCG, even
      --  though useless predicate checks will be generally removed by
      --  back-end optimizations.

      procedure Check_Subprogram_Variant;
      --  Emit a call to the internally generated procedure with checks for
      --  aspect Subprogram_Variant, if present and enabled.

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

      function Is_Class_Wide_Interface_Type (E : Entity_Id) return Boolean;
      --  Return True when E is a class-wide interface type or an access to
      --  a class-wide interface type.

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

      --------------------------------------
      -- Add_Cond_Expression_Extra_Actual --
      --------------------------------------

      procedure Add_Cond_Expression_Extra_Actual
        (Formal : Entity_Id)
      is
         Decl : Node_Id;
         Lvl  : Entity_Id;

         procedure Insert_Level_Assign (Branch : Node_Id);
         --  Recursively add assignment of the level temporary on each branch
         --  while moving through nested conditional expressions.

         -------------------------
         -- Insert_Level_Assign --
         -------------------------

         procedure Insert_Level_Assign (Branch : Node_Id) is

            procedure Expand_Branch (Res_Assn : Node_Id);
            --  Perform expansion or iterate further within nested
            --  conditionals given the object declaration or assignment to
            --  result object created during expansion which represents a
            --  branch of the conditional expression.

            -------------------
            -- Expand_Branch --
            -------------------

            procedure Expand_Branch (Res_Assn : Node_Id) is
            begin
               pragma Assert (Nkind (Res_Assn) in
                               N_Assignment_Statement |
                               N_Object_Declaration);

               --  There are more nested conditional expressions so we must go
               --  deeper.

               if Nkind (Expression (Res_Assn)) = N_Expression_With_Actions
                 and then
                   Nkind (Original_Node (Expression (Res_Assn)))
                     in N_Case_Expression | N_If_Expression
               then
                  Insert_Level_Assign
                    (Expression (Res_Assn));

               --  Add the level assignment

               else
                  Insert_Before_And_Analyze (Res_Assn,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Lvl, Loc),
                      Expression =>
                        Accessibility_Level
                          (Expr            => Expression (Res_Assn),
                           Level           => Dynamic_Level,
                           Allow_Alt_Model => False)));
               end if;
            end Expand_Branch;

            Cond : Node_Id;
            Alt  : Node_Id;

         --  Start of processing for Insert_Level_Assign

         begin
            --  Examine further nested conditionals

            pragma Assert (Nkind (Branch) =
                            N_Expression_With_Actions);

            --  Find the relevant statement in the actions

            Cond := First (Actions (Branch));
            while Present (Cond) loop
               exit when Nkind (Cond) in N_Case_Statement | N_If_Statement;
               Next (Cond);
            end loop;

            --  The conditional expression may have been optimized away, so
            --  examine the actions in the branch.

            if No (Cond) then
               Expand_Branch (Last (Actions (Branch)));

            --  Iterate through if expression branches

            elsif Nkind (Cond) = N_If_Statement then
               Expand_Branch (Last (Then_Statements (Cond)));
               Expand_Branch (Last (Else_Statements (Cond)));

            --  Iterate through case alternatives

            elsif Nkind (Cond) = N_Case_Statement then

               Alt := First (Alternatives (Cond));
               while Present (Alt) loop
                  Expand_Branch (Last (Statements (Alt)));
                  Next (Alt);
               end loop;
            end if;
         end Insert_Level_Assign;

      --  Start of processing for cond expression case

      begin
         --  Create declaration of a temporary to store the accessibility
         --  level of each branch of the conditional expression.

         Lvl  := Make_Temporary (Loc, 'L');
         Decl := Make_Object_Declaration (Loc,
                   Defining_Identifier => Lvl,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Natural, Loc));

         --  Install the declaration and perform necessary expansion if we
         --  are dealing with a procedure call.

         if Nkind (Call_Node) = N_Procedure_Call_Statement then
            --  Generate:
            --    Lvl : Natural;
            --    Call (
            --     {do
            --        If_Exp_Res : Typ;
            --        if Cond then
            --           Lvl        := 0; --  Access level
            --           If_Exp_Res := Exp;
            --        ...
            --      in If_Exp_Res end;},
            --      Lvl,
            --      ...
            --    )

            Insert_Before_And_Analyze (Call_Node, Decl);

         --  Ditto for a function call. Note that we do not wrap the function
         --  call into an expression with action to avoid bad interactions with
         --  Exp_Ch4.Process_Transient_In_Expression.

         else
            --  Generate:
            --    Lvl : Natural;  --  placed above the function call
            --    ...
            --    Func_Call (
            --     {do
            --        If_Exp_Res : Typ
            --        if Cond then
            --           Lvl := 0; --  Access level
            --           If_Exp_Res := Exp;
            --      in If_Exp_Res end;},
            --      Lvl,
            --      ...
            --    )

            Insert_Action (Call_Node, Decl);
            Analyze (Call_Node);
         end if;

         --  Decorate the conditional expression with assignments to our level
         --  temporary.

         Insert_Level_Assign (Prev);

         --  Make our level temporary the passed actual

         Add_Extra_Actual
           (Expr => New_Occurrence_Of (Lvl, Loc),
            EF   => Extra_Accessibility (Formal));
      end Add_Cond_Expression_Extra_Actual;

      --------------------------------------
      -- Add_Dummy_Build_In_Place_Actuals --
      --------------------------------------

      procedure Add_Dummy_Build_In_Place_Actuals
        (Function_Id             : Entity_Id;
         Num_Added_Extra_Actuals : Nat := 0)
      is
         Loc        : constant Source_Ptr := Sloc (Call_Node);
         Formal     : Entity_Id           := Extra_Formals (Function_Id);
         Actual     : Node_Id;
         Skip_Extra : Nat;

      begin
         --  We never generate extra formals if expansion is not active because
         --  we don't need them unless we are generating code. No action needed
         --  for thunks since they propagate all their extra actuals.

         if not Expander_Active
           or else Is_Thunk (Current_Scope)
         then
            return;
         end if;

         --  Skip already-added non-BIP extra actuals

         Skip_Extra := Num_Added_Extra_Actuals;
         while Skip_Extra > 0 loop
            pragma Assert (not Is_Build_In_Place_Entity (Formal));
            Formal := Extra_Formal (Formal);
            Skip_Extra := Skip_Extra - 1;
         end loop;

         --  Append the dummy BIP extra actuals

         while Present (Formal) loop
            pragma Assert (Is_Build_In_Place_Entity (Formal));

            --  BIPalloc

            if Etype (Formal) = Standard_Natural then
               Actual := Make_Integer_Literal (Loc, Uint_0);
               Analyze_And_Resolve (Actual, Standard_Natural);
               Add_Extra_Actual_To_Call (N, Formal, Actual);

            --  BIPtaskmaster

            elsif Etype (Formal) = Standard_Integer then
               Actual := Make_Integer_Literal (Loc, Uint_0);
               Analyze_And_Resolve (Actual, Standard_Integer);
               Add_Extra_Actual_To_Call (N, Formal, Actual);

            --  BIPstoragepool, BIPcollection, BIPactivationchain,
            --  and BIPaccess.

            elsif Is_Access_Type (Etype (Formal)) then
               Actual := Make_Null (Loc);
               Analyze_And_Resolve (Actual, Etype (Formal));
               Add_Extra_Actual_To_Call (N, Formal, Actual);

            else
               pragma Assert (False);
               raise Program_Error;
            end if;

            Formal := Extra_Formal (Formal);
         end loop;

         --  Mark the call as processed build-in-place call; required
         --  to avoid adding the extra formals twice.

         Set_Is_Expanded_Build_In_Place_Call (Call_Node);

         pragma Assert (Check_Number_Of_Actuals (Call_Node, Function_Id));
         pragma Assert (Check_BIP_Actuals (Call_Node, Function_Id));
      end Add_Dummy_Build_In_Place_Actuals;

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

      ------------------------------------
      -- Add_View_Conversion_Invariants --
      ------------------------------------

      procedure Add_View_Conversion_Invariants
        (Formal : Entity_Id;
         Actual : Node_Id)
      is
         Arg        : Entity_Id;
         Curr_Typ   : Entity_Id;
         Inv_Checks : List_Id;
         Par_Typ    : Entity_Id;

      begin
         Inv_Checks := No_List;

         --  Extract the argument from a potentially nested set of view
         --  conversions.

         Arg := Actual;
         while Nkind (Arg) = N_Type_Conversion loop
            Arg := Expression (Arg);
         end loop;

         --  Move up the derivation chain starting with the type of the formal
         --  parameter down to the type of the actual object.

         Curr_Typ := Empty;
         Par_Typ  := Etype (Arg);
         while Par_Typ /= Etype (Formal) and Par_Typ /= Curr_Typ loop
            Curr_Typ := Par_Typ;

            if Has_Invariants (Curr_Typ)
              and then Present (Invariant_Procedure (Curr_Typ))
            then
               --  Verify the invariant of the current type. Generate:

               --    <Curr_Typ>Invariant (Curr_Typ (Arg));

               Prepend_New_To (Inv_Checks,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of
                       (Invariant_Procedure (Curr_Typ), Loc),
                   Parameter_Associations => New_List (
                     Make_Type_Conversion (Loc,
                       Subtype_Mark => New_Occurrence_Of (Curr_Typ, Loc),
                       Expression   => New_Copy_Tree (Arg)))));
            end if;

            Par_Typ := Base_Type (Etype (Curr_Typ));
         end loop;

         --  If the node is a function call the generated tests have been
         --  already handled in Insert_Post_Call_Actions.

         if not Is_Empty_List (Inv_Checks)
           and then Nkind (Call_Node) = N_Procedure_Call_Statement
         then
            Insert_Actions_After (Call_Node, Inv_Checks);
         end if;
      end Add_View_Conversion_Invariants;

      -----------------------------
      -- Can_Fold_Predicate_Call --
      -----------------------------

      function Can_Fold_Predicate_Call (P : Entity_Id) return Boolean is
         Actual : Node_Id;

         function Augments_Other_Dynamic_Predicate (DP_Aspect_Spec : Node_Id)
           return Boolean;
         --  Given a Dynamic_Predicate aspect aspecification for a
         --  discrete type, returns True iff another DP specification
         --  applies (indirectly, via a subtype type or a derived type)
         --  to the same entity that this aspect spec applies to.

         function May_Fold (N : Node_Id) return Traverse_Result;
         --  The predicate expression is foldable if it only contains operators
         --  and literals. During this check, we also replace occurrences of
         --  the formal of the constructed predicate function with the static
         --  value of the actual. This is done on a copy of the analyzed
         --  expression for the predicate.

         --------------------------------------
         -- Augments_Other_Dynamic_Predicate --
         --------------------------------------

         function Augments_Other_Dynamic_Predicate (DP_Aspect_Spec : Node_Id)
           return Boolean
         is
            Aspect_Bearer : Entity_Id := Entity (DP_Aspect_Spec);
         begin
            loop
               Aspect_Bearer := Nearest_Ancestor (Aspect_Bearer);

               if No (Aspect_Bearer) then
                  return False;
               end if;

               declare
                  Aspect_Spec : constant Node_Id :=
                    Find_Aspect (Aspect_Bearer, Aspect_Dynamic_Predicate);
               begin
                  if Present (Aspect_Spec)
                    and then Aspect_Spec /= DP_Aspect_Spec
                  then
                     --  Found another Dynamic_Predicate aspect spec
                     return True;
                  end if;
               end;
            end loop;
         end Augments_Other_Dynamic_Predicate;

         --------------
         -- May_Fold --
         --------------

         function May_Fold (N : Node_Id) return Traverse_Result is
         begin
            case Nkind (N) is
               when N_Op =>
                  return OK;

               when N_Expanded_Name
                  | N_Identifier
               =>
                  if Ekind (Entity (N)) = E_In_Parameter
                    and then Entity (N) = First_Entity (P)
                  then
                     Rewrite (N, New_Copy (Actual));
                     Set_Is_Static_Expression (N);
                     return OK;

                  elsif Ekind (Entity (N)) = E_Enumeration_Literal then
                     return OK;

                  else
                     return Abandon;
                  end if;

               when N_Case_Expression
                  | N_If_Expression
               =>
                  return OK;

               when N_Integer_Literal =>
                  return OK;

               when others =>
                  return Abandon;
            end case;
         end May_Fold;

         function Try_Fold is new Traverse_Func (May_Fold);

         --  Other Local variables

         Subt   : constant Entity_Id := Etype (First_Entity (P));
         Aspect : Node_Id;
         Pred   : Node_Id;

      --  Start of processing for Can_Fold_Predicate_Call

      begin
         --  Folding is only interesting if the actual is static and its type
         --  has a Dynamic_Predicate aspect. For CodePeer we preserve the
         --  function call.

         Actual := First (Parameter_Associations (Call_Node));
         Aspect := Find_Aspect (Subt, Aspect_Dynamic_Predicate);

         --  If actual is a declared constant, retrieve its value

         if Is_Entity_Name (Actual)
           and then Ekind (Entity (Actual)) = E_Constant
         then
            Actual := Constant_Value (Entity (Actual));
         end if;

         if No (Actual)
           or else Nkind (Actual) /= N_Integer_Literal
           or else not Has_Dynamic_Predicate_Aspect (Subt)
           or else No (Aspect)

           --  Do not fold if multiple applicable predicate aspects
           or else Has_Ghost_Predicate_Aspect (Subt)
           or else Has_Aspect (Subt, Aspect_Static_Predicate)
           or else Has_Aspect (Subt, Aspect_Predicate)
           or else Augments_Other_Dynamic_Predicate (Aspect)
           or else CodePeer_Mode
         then
            return False;
         end if;

         --  Retrieve the analyzed expression for the predicate

         Pred := New_Copy_Tree (Expression (Aspect));

         if Try_Fold (Pred) = OK then
            Rewrite (Call_Node, Pred);
            Analyze_And_Resolve (Call_Node, Standard_Boolean);
            return True;

         --  Otherwise continue the expansion of the function call

         else
            return False;
         end if;
      end Can_Fold_Predicate_Call;

      ------------------------------
      -- Check_Subprogram_Variant --
      ------------------------------

      procedure Check_Subprogram_Variant is

         function Duplicate_Params_Without_Extra_Actuals
           (Call_Node : Node_Id) return List_Id;
         --  Duplicate actual parameters of Call_Node into New_Call without
         --  extra actuals.

         --------------------------------------------
         -- Duplicate_Params_Without_Extra_Actuals --
         --------------------------------------------

         function Duplicate_Params_Without_Extra_Actuals
           (Call_Node : Node_Id) return List_Id
         is
            Proc_Id : constant Entity_Id := Entity (Name (Call_Node));
            Actuals : constant List_Id := Parameter_Associations (Call_Node);
            NL      : List_Id;
            Actual  : Node_Or_Entity_Id;
            Formal  : Entity_Id;

         begin
            if Actuals = No_List then
               return No_List;

            else
               NL     := New_List;
               Actual := First (Actuals);
               Formal := First_Formal (Proc_Id);

               while Present (Formal)
                 and then Formal /= Extra_Formals (Proc_Id)
               loop
                  Append (New_Copy (Actual), NL);
                  Next (Actual);

                  Next_Formal (Formal);
               end loop;

               return NL;
            end if;
         end Duplicate_Params_Without_Extra_Actuals;

         --  Local variables

         Variant_Prag : constant Node_Id :=
           Get_Pragma (Current_Scope, Pragma_Subprogram_Variant);

         New_Call     : Node_Id;
         Pragma_Arg1  : Node_Id;
         Variant_Proc : Entity_Id;

      begin
         if Present (Variant_Prag) and then Is_Checked (Variant_Prag) then

            Pragma_Arg1 :=
              Expression (First (Pragma_Argument_Associations (Variant_Prag)));

            --  If pragma parameter is still an aggregate, it comes from a
            --  structural variant, which is not expanded and ignored for
            --  run-time execution.

            if Nkind (Pragma_Arg1) = N_Aggregate then
               pragma Assert
                 (Chars
                    (First
                      (Choices
                         (First (Component_Associations (Pragma_Arg1))))) =
                  Name_Structural);
               return;
            end if;

            --  Otherwise, analysis of the pragma rewrites its argument with a
            --  reference to the internally generated procedure.

            Variant_Proc := Entity (Pragma_Arg1);

            New_Call :=
              Make_Procedure_Call_Statement (Loc,
                 Name                   =>
                   New_Occurrence_Of (Variant_Proc, Loc),
                 Parameter_Associations =>
                   Duplicate_Params_Without_Extra_Actuals (Call_Node));

            Insert_Action (Call_Node, New_Call);

            pragma Assert (Etype (New_Call) /= Any_Type
              or else Serious_Errors_Detected > 0);
         end if;
      end Check_Subprogram_Variant;

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

      ----------------------------------
      -- Is_Class_Wide_Interface_Type --
      ----------------------------------

      function Is_Class_Wide_Interface_Type (E : Entity_Id) return Boolean is
         DDT : Entity_Id;
         Typ : Entity_Id := E;

      begin
         if Has_Non_Limited_View (Typ) then
            Typ := Non_Limited_View (Typ);
         end if;

         if Ekind (Typ) = E_Anonymous_Access_Type then
            DDT := Directly_Designated_Type (Typ);

            if Has_Non_Limited_View (DDT) then
               DDT := Non_Limited_View (DDT);
            end if;

            return Is_Class_Wide_Type (DDT) and then Is_Interface (DDT);
         else
            return Is_Class_Wide_Type (Typ) and then Is_Interface (Typ);
         end if;
      end Is_Class_Wide_Interface_Type;

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
               Formal : Entity_Id;

            begin
               Actual := First_Actual (Call_Node);
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

                  Next_Actual (Actual);
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

      Remote        : constant Boolean := Is_Remote_Call (Call_Node);
      Actual        : Node_Id;
      Formal        : Entity_Id;
      Orig_Subp     : Entity_Id := Empty;
      Param_Count   : Positive;
      Parent_Formal : Entity_Id;
      Parent_Subp   : Entity_Id;
      Scop          : Entity_Id;
      Subp          : Entity_Id;

      CW_Interface_Formals_Present : Boolean := False;

   --  Start of processing for Expand_Call_Helper

   begin
      Post_Call := New_List;

      --  Expand the function or procedure call if the first actual has a
      --  declared dimension aspect, and the subprogram is declared in one
      --  of the dimension I/O packages.

      if Ada_Version >= Ada_2012
        and then Nkind (Call_Node) in N_Subprogram_Call
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

      --  Ensure that the called subprogram has all its formals

      if not Is_Frozen (Subp) then
         Create_Extra_Formals (Subp);
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

            if Present (Parent (Ren_Root))
              and then Present (Original_Node (Parent (Parent (Ren_Root))))
            then
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

      --  If this is a call to a predicate function, try to constant fold it

      if Nkind (Call_Node) = N_Function_Call
        and then Is_Entity_Name (Name (Call_Node))
        and then Is_Predicate_Function (Subp)
        and then Can_Fold_Predicate_Call (Subp)
      then
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
         pragma Assert (Extra_Formals_Match_OK (Current_Scope, Subp));

         declare
            Target_Formal : Entity_Id;
            Thunk_Formal  : Entity_Id;

         begin
            Target_Formal := Extra_Formals (Subp);
            Thunk_Formal  := Extra_Formals (Current_Scope);
            while Present (Target_Formal) loop
               Add_Extra_Actual
                 (Expr => New_Occurrence_Of (Thunk_Formal, Loc),
                  EF   => Thunk_Formal);

               Target_Formal := Extra_Formal (Target_Formal);
               Thunk_Formal  := Extra_Formal (Thunk_Formal);
            end loop;

            while Is_Non_Empty_List (Extra_Actuals) loop
               Add_Actual_Parameter (Remove_Head (Extra_Actuals));
            end loop;

            --  Mark the call as processed build-in-place call; required
            --  to avoid adding the extra formals twice.

            if Nkind (Call_Node) = N_Function_Call then
               Set_Is_Expanded_Build_In_Place_Call (Call_Node);
            end if;

            Expand_Actuals (Call_Node, Subp, Post_Call);
            pragma Assert (Is_Empty_List (Post_Call));
            pragma Assert (Check_Number_Of_Actuals (Call_Node, Subp));
            pragma Assert (Check_BIP_Actuals (Call_Node, Subp));
            return;
         end;
      end if;

      Formal := First_Formal (Subp);
      Actual := First_Actual (Call_Node);
      Param_Count := 1;
      while Present (Formal) loop
         --  Prepare to examine current entry

         Prev := Actual;

         --  Ada 2005 (AI-251): Check if any formal is a class-wide interface
         --  to expand it in a further round.

         CW_Interface_Formals_Present :=
           CW_Interface_Formals_Present
             or else Is_Class_Wide_Interface_Type (Etype (Formal));

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
            if Is_Mutably_Tagged_Type (Etype (Actual))
              or else (Is_Private_Type (Etype (Prev))
                        and then not Has_Discriminants
                                       (Base_Type (Etype (Prev))))
            then
               Add_Extra_Actual
                 (Expr => New_Occurrence_Of (Standard_False, Loc),
                  EF   => Extra_Constrained (Formal));

            elsif Is_Constrained (Etype (Formal))
              or else not Has_Discriminants (Etype (Prev))
            then
               Add_Extra_Actual
                 (Expr => New_Occurrence_Of (Standard_True, Loc),
                  EF   => Extra_Constrained (Formal));

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
                  while Nkind (Act_Prev) in N_Type_Conversion
                                          | N_Unchecked_Type_Conversion
                  loop
                     Act_Prev := Expression (Act_Prev);
                  end loop;

                  --  If the expression is a conversion of a dereference, this
                  --  is internally generated code that manipulates addresses,
                  --  e.g. when building interface tables. No check should
                  --  occur in this case, and the discriminated object is not
                  --  directly at hand.

                  if not Comes_From_Source (Actual)
                    and then Nkind (Actual) = N_Unchecked_Type_Conversion
                    and then Nkind (Act_Prev) = N_Explicit_Dereference
                  then
                     Add_Extra_Actual
                       (Expr => New_Occurrence_Of (Standard_False, Loc),
                        EF   => Extra_Constrained (Formal));

                  else
                     Add_Extra_Actual
                       (Expr =>
                          Make_Attribute_Reference (Sloc (Prev),
                            Prefix         =>
                              Duplicate_Subexpr_No_Checks
                                (Act_Prev, Name_Req => True),
                            Attribute_Name => Name_Constrained),
                        EF   => Extra_Constrained (Formal));
                  end if;
               end;
            end if;
         end if;

         --  Create possible extra actual for accessibility level

         if Present (Extra_Accessibility (Formal)) then
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
                    (Expr => Accessibility_Level
                               (Expr            => Parm_Ent,
                                Level           => Dynamic_Level,
                                Allow_Alt_Model => False),
                     EF   => Extra_Accessibility (Formal));
               end;

            --  Conditional expressions

            elsif Nkind (Prev) = N_Expression_With_Actions
              and then Nkind (Original_Node (Prev)) in
                         N_If_Expression | N_Case_Expression
            then
               Add_Cond_Expression_Extra_Actual (Formal);

            --  Internal constant generated to remove side effects (normally
            --  from the expansion of dispatching calls).

            --  First verify the actual is internal

            elsif not Comes_From_Source (Prev)
              and then not Is_Rewrite_Substitution (Prev)

              --  Next check that the actual is a constant

              and then Nkind (Prev) = N_Identifier
              and then Ekind (Entity (Prev)) = E_Constant
              and then Nkind (Parent (Entity (Prev))) = N_Object_Declaration
            then
               --  Generate the accessibility level based on the expression in
               --  the constant's declaration.

               declare
                  Ent : Entity_Id := Entity (Prev);

               begin
                  --  Handle deferred constants

                  if Present (Full_View (Ent)) then
                     Ent := Full_View (Ent);
                  end if;

                  Add_Extra_Actual
                    (Expr => Accessibility_Level
                               (Expr            => Expression (Parent (Ent)),
                                Level           => Dynamic_Level,
                                Allow_Alt_Model => False),
                     EF   => Extra_Accessibility (Formal));
               end;

            --  Normal case

            else
               Add_Extra_Actual
                 (Expr => Accessibility_Level
                            (Expr            => Prev,
                             Level           => Dynamic_Level,
                             Allow_Alt_Model => False),
                  EF   => Extra_Accessibility (Formal));
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

            elsif Nkind (Prev) in N_Allocator | N_Attribute_Reference then
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
                  while Nkind (Nod) in
                    N_Indexed_Component | N_Selected_Component
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

               if Is_Assignable (Ent) then
                  Sav := Last_Assignment (Ent);
                  Kill_Current_Values (Ent);
                  Set_Last_Assignment (Ent, Sav);
                  Set_Is_Known_Valid (Ent, False);
                  Set_Is_True_Constant (Ent, False);

               --  For all other cases, just kill the current values

               else
                  Kill_Current_Values (Ent);
               end if;
            end;
         end if;

         --  If the formal is class-wide and the actual is an aggregate, force
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

         --  Perform invariant checks for all intermediate types in a view
         --  conversion after successful return from a call that passes the
         --  view conversion as an IN OUT or OUT parameter (RM 7.3.2 (12/3,
         --  13/3, 14/3)). Consider only source conversion in order to avoid
         --  generating spurious checks on complex expansion such as object
         --  initialization through an extension aggregate.

         if Comes_From_Source (Call_Node)
           and then Ekind (Formal) /= E_In_Parameter
           and then Nkind (Actual) = N_Type_Conversion
         then
            Add_View_Conversion_Invariants (Formal, Actual);
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

      if Ekind (Subp) in E_Function | E_Operator | E_Subprogram_Type
        and then
          Present (Extra_Accessibility_Of_Result (Ultimate_Alias (Subp)))
      then
         declare
            Extra_Form : Node_Id := Empty;
            Level      : Node_Id := Empty;

         begin
            --  Detect cases where the function call has been internally
            --  generated by examining the original node and return library
            --  level - taking care to avoid ignoring function calls expanded
            --  in prefix notation.

            if Nkind (Original_Node (Call_Node)) not in N_Function_Call
                                                      | N_Selected_Component
                                                      | N_Indexed_Component
            then
               Level := Make_Integer_Literal
                          (Loc, Scope_Depth (Standard_Standard));

            --  Otherwise get the level normally based on the call node

            else
               Level := Accessibility_Level
                          (Expr            => Call_Node,
                           Level           => Dynamic_Level,
                           Allow_Alt_Model => False);
            end if;

            --  It may be possible that we are re-expanding an already
            --  expanded call when are are dealing with dispatching ???

            if No (Parameter_Associations (Call_Node))
              or else Nkind (Last (Parameter_Associations (Call_Node)))
                        /= N_Parameter_Association
              or else not Is_Accessibility_Actual
                              (Last (Parameter_Associations (Call_Node)))
            then
               Extra_Form := Extra_Accessibility_Of_Result
                               (Ultimate_Alias (Subp));

               Add_Extra_Actual
                 (Expr => Level,
                  EF   => Extra_Form);
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
               --  Move the error messages below to sem???

               if Is_Access_Type (Etype (Call_Node)) then
                  if Designated_Type (Etype (Call_Node)) /=
                    Root_Type (Etype (Name (Ass)))
                  then
                     Error_Msg_NE
                       ("tag-indeterminate expression must have designated "
                        & "type& (RM 5.2 (6))",
                         Call_Node, Root_Type (Etype (Name (Ass))));
                  else
                     Propagate_Tag (Name (Ass), Call_Node);
                  end if;

               elsif Etype (Call_Node) /= Root_Type (Etype (Name (Ass))) then
                  Error_Msg_NE
                    ("tag-indeterminate expression must have type & "
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
      --  it to point to the correct secondary virtual table.

      if Nkind (Call_Node) in N_Subprogram_Call
        and then CW_Interface_Formals_Present
      then
         Expand_Interface_Actuals (Call_Node);
      end if;

      --  Install class-wide preconditions runtime check when this is a
      --  dispatching primitive that has or inherits class-wide preconditions;
      --  otherwise no runtime check is installed.

      if Nkind (Call_Node) in N_Subprogram_Call
        and then Is_Dispatching_Operation (Subp)
      then
         Install_Class_Preconditions_Check (Call_Node);
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
         if Tagged_Type_Expansion then
            Expand_Dispatching_Call (Call_Node);

            --  Expand_Dispatching_Call takes care of all the needed processing

            return;
         end if;

         --  VM targets

         declare
            Call_Typ   : constant Entity_Id := Etype (Call_Node);
            Typ        : constant Entity_Id := Find_Dispatching_Type (Subp);
            Eq_Prim_Op : Entity_Id := Empty;
            New_Call   : Node_Id;
            Param      : Node_Id;
            Prev_Call  : Node_Id;

         begin
            Apply_Tag_Checks (Call_Node);

            if not Is_Limited_Type (Typ) then
               Eq_Prim_Op := Find_Prim_Op (Typ, Name_Op_Eq);
            end if;

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
         and then Ekind (Subp) in E_Procedure | E_Function
      then
         null;

      --  During that loop we gathered the extra actuals (the ones that
      --  correspond to Extra_Formals), so now they can be appended.

      elsif Is_Non_Empty_List (Extra_Actuals) then
         declare
            Num_Extra_Actuals : constant Nat := List_Length (Extra_Actuals);

         begin
            while Is_Non_Empty_List (Extra_Actuals) loop
               Add_Actual_Parameter (Remove_Head (Extra_Actuals));
            end loop;

            --  Add dummy extra BIP actuals if we are calling a function that
            --  inherited the BIP extra actuals but does not require them.

            if Nkind (Call_Node) = N_Function_Call
              and then Is_Function_Call_With_BIP_Formals (Call_Node)
              and then not Is_Build_In_Place_Function_Call (Call_Node)
            then
               Add_Dummy_Build_In_Place_Actuals (Subp,
                 Num_Added_Extra_Actuals => Num_Extra_Actuals);
            end if;
         end;

      --  Add dummy extra BIP actuals if we are calling a function that
      --  inherited the BIP extra actuals but does not require them.

      elsif Nkind (Call_Node) = N_Function_Call
        and then Is_Function_Call_With_BIP_Formals (Call_Node)
        and then not Is_Build_In_Place_Function_Call (Call_Node)
      then
         Add_Dummy_Build_In_Place_Actuals (Subp);
      end if;

      --  At this point we have all the actuals, so this is the point at which
      --  the various expansion activities for actuals is carried out.

      Expand_Actuals (Call_Node, Subp, Post_Call);

      --  If it is a recursive call then call the internal procedure that
      --  verifies Subprogram_Variant contract (if present and enabled).
      --  Detecting calls to subprogram aliases is necessary for recursive
      --  calls in instances of generic subprograms, where the renaming of
      --  the current subprogram is called.

      if Is_Subprogram (Subp)
        and then not Is_Ignored_Ghost_Entity (Subp)
        and then Same_Or_Aliased_Subprograms (Subp, Current_Scope)
      then
         Check_Subprogram_Variant;
      end if;

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
                  Rewrite (Act, OK_Convert_To (Typ, Act));
                  Analyze_And_Resolve (Act, Typ);
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

                  --  For an IN parameter of a scalar type, the derived formal
                  --  type and parent formal type differ, and the parent formal
                  --  type and actual type do not match statically.

                  if Is_Scalar_Type (Formal_Typ)
                    and then Ekind (Formal) = E_In_Parameter
                    and then Formal_Typ /= Parent_Typ
                    and then
                      not Subtypes_Statically_Match (Parent_Typ, Actual_Typ)
                    and then not Raises_Constraint_Error (Actual)
                  then
                     Convert (Actual, Parent_Typ);

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
                          Unchecked_Convert_To (Parent_Typ, Actual));
                        Analyze_And_Resolve (Actual, Parent_Typ);
                     end if;

                  --  If there is a change of representation, then generate a
                  --  warning, and do the change of representation.

                  elsif not Has_Compatible_Representation
                              (Target_Typ  => Formal_Typ,
                               Operand_Typ => Parent_Typ)
                  then
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
                    and then Known_Esize (Formal_Typ)
                    and then Known_Esize (Parent_Typ)
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

      if Ekind (Subp) in E_Function | E_Procedure then

         --  We perform a simple optimization on calls for To_Address by
         --  replacing them with an unchecked conversion. Not only is this
         --  efficient, but it also avoids order of elaboration problems when
         --  address clauses are inlined (address expression elaborated at the
         --  wrong point).

         --  We perform this optimization regardless of whether we are in the
         --  main unit or in a unit in the context of the main unit, to ensure
         --  that the generated tree is the same in both cases, for CodePeer
         --  use.

         if Is_RTE (Subp, RE_To_Address) then
            Rewrite (Call_Node,
              Unchecked_Convert_To
                (RTE (RE_Address), Relocate_Node (First_Actual (Call_Node))));
            return;

         --  A call to a null procedure is replaced by a null statement, but we
         --  are not allowed to ignore possible side effects of the call, so we
         --  make sure that actuals are evaluated.
         --  We also suppress this optimization for GNATcoverage.

         elsif Is_Null_Procedure (Subp)
           and then not Opt.Suppress_Control_Flow_Optimizations
         then
            Actual := First_Actual (Call_Node);
            while Present (Actual) loop
               Remove_Side_Effects (Actual);
               Next_Actual (Actual);
            end loop;

            Rewrite (Call_Node, Make_Null_Statement (Loc));
            return;
         end if;

         --  Handle inlining. No action needed if the subprogram is not inlined

         if not Is_Inlined (Subp) then
            null;

         --  Front-end inlining of expression functions (performed also when
         --  back-end inlining is enabled).

         elsif Is_Inlinable_Expression_Function (Subp) then
            Rewrite
              (Call_Node, New_Copy (Expression_Of_Expression_Function (Subp)));
            Analyze (Call_Node);
            return;

         --  Handle front-end inlining

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

                  elsif not In_Extended_Main_Code_Unit (Call_Node)
                    and then In_Package_Body
                  then
                     Must_Inline := not In_Extended_Main_Source_Unit (Subp);
                  end if;
               end if;

               if Must_Inline then
                  Expand_Inlined_Call (Call_Node, Subp, Orig_Subp);

               else
                  --  Let the back end handle it

                  Add_Inlined_Body (Subp, Call_Node);

                  if Front_End_Inlining
                    and then Nkind (Spec) = N_Subprogram_Declaration
                    and then In_Extended_Main_Code_Unit (Call_Node)
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

         --  Front-end expansion of simple functions returning unconstrained
         --  types (see Check_And_Split_Unconstrained_Function). Note that the
         --  case of a simple renaming (Body_To_Inline in N_Entity below, see
         --  also Build_Renamed_Body) cannot be expanded here because this may
         --  give rise to order-of-elaboration issues for the types of the
         --  parameters of the subprogram, if any.

         elsif Present (Unit_Declaration_Node (Subp))
           and then Nkind (Unit_Declaration_Node (Subp)) =
                                                       N_Subprogram_Declaration
           and then Present (Body_To_Inline (Unit_Declaration_Node (Subp)))
           and then
             Nkind (Body_To_Inline (Unit_Declaration_Node (Subp))) not in
                                                                       N_Entity
         then
            Expand_Inlined_Call (Call_Node, Subp, Orig_Subp);

         --  Back-end inlining either if optimization is enabled, we're
         --  generating C, or the call is required to be inlined.

         elsif Optimization_Level > 0
           or else CCG_Mode
           or else Has_Pragma_Inline_Always (Subp)
         then
            Add_Inlined_Body (Subp, Call_Node);
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
      --  function call is transformed into a reference to the result that has
      --  been built either on the primary or the secondary stack.

      if Nkind (Call_Node) = N_Function_Call
        and then Needs_Finalization (Etype (Call_Node))
      then
         if not Is_Build_In_Place_Function_Call (Call_Node)
           and then
             (No (First_Formal (Subp))
               or else
                 not Is_Concurrent_Record_Type (Etype (First_Formal (Subp))))
         then
            Expand_Ctrl_Function_Call
              (Call_Node, Needs_Secondary_Stack (Etype (Call_Node)));

         --  Build-in-place function calls which appear in anonymous contexts
         --  need a transient scope to ensure the proper finalization of the
         --  intermediate result after its use.

         elsif Is_Build_In_Place_Function_Call (Call_Node)
           and then Nkind (Parent (Unqual_Conv (Call_Node))) in
                      N_Attribute_Reference
                    | N_Function_Call
                    | N_Indexed_Component
                    | N_Object_Renaming_Declaration
                    | N_Procedure_Call_Statement
                    | N_Selected_Component
                    | N_Slice
           and then
             (Ekind (Current_Scope) /= E_Loop
               or else Nkind (Parent (Call_Node)) /= N_Function_Call
               or else not
                 Is_Build_In_Place_Function_Call (Parent (Call_Node)))
         then
            Establish_Transient_Scope
              (Call_Node, Needs_Secondary_Stack (Etype (Call_Node)));
         end if;
      end if;
   end Expand_Call_Helper;

   -------------------------------
   -- Expand_Ctrl_Function_Call --
   -------------------------------

   procedure Expand_Ctrl_Function_Call (N : Node_Id; Use_Sec_Stack : Boolean)
   is
      Par : constant Node_Id := Parent (N);

   begin
      --  Optimization: if the returned value is returned again, then no need
      --  to copy/readjust/finalize, we can just pass the value through (see
      --  Expand_N_Simple_Return_Statement), and thus no attachment is needed.
      --  Note that simple return statements are distributed into conditional
      --  expressions but we may be invoked before this distribution is done.

      if Nkind (Par) = N_Simple_Return_Statement
        or else (Nkind (Par) = N_If_Expression
                  and then Nkind (Parent (Par)) = N_Simple_Return_Statement)
        or else (Nkind (Par) = N_Case_Expression_Alternative
                  and then
                    Nkind (Parent (Parent (Par))) = N_Simple_Return_Statement)
      then
         return;
      end if;

      --  Another optimization: if the returned value is used to initialize an
      --  object, then no need to copy/readjust/finalize, we can initialize it
      --  in place. However, if the call returns on the secondary stack, then
      --  we need the expansion because we'll be renaming the temporary as the
      --  (permanent) object. We also apply it in the case of the expression of
      --  a delta aggregate, since it is used only to initialize a temporary.

      if Nkind (Par) in N_Object_Declaration | N_Delta_Aggregate
        and then Expression (Par) = N
        and then not Use_Sec_Stack
      then
         return;
      end if;

      --  Resolution is now finished, make sure we don't start analysis again
      --  because of the duplication.

      Set_Analyzed (N);

      --  Apply the transformation unless it was already applied earlier. This
      --  may happen because Remove_Side_Effects can be called during semantic
      --  analysis, for example from Build_Actual_Subtype_Of_Component. It is
      --  crucial to avoid creating a reference of reference here, because it
      --  would not be subsequently recognized by the Is_Finalizable_Transient
      --  and Requires_Cleanup_Actions predicates.

      if Nkind (Par) /= N_Reference then
         Remove_Side_Effects (N);
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
   --  (in which case default initial values might need to be set)).

   procedure Expand_N_Extended_Return_Statement (N : Node_Id) is
      Loc          : constant Source_Ptr := Sloc (N);
      Func_Id      : constant Entity_Id :=
                       Return_Applies_To (Return_Statement_Entity (N));
      Is_BIP_Func  : constant Boolean   :=
                       Is_Build_In_Place_Function (Func_Id);
      Ret_Obj_Id   : constant Entity_Id :=
                       First_Entity (Return_Statement_Entity (N));
      Ret_Obj_Decl : constant Node_Id   := Parent (Ret_Obj_Id);
      Ret_Typ      : constant Entity_Id := Etype (Func_Id);

      function Move_Activation_Chain (Func_Id : Entity_Id) return Node_Id;
      --  Construct a call to System.Tasking.Stages.Move_Activation_Chain
      --  with parameters:
      --    From         current activation chain
      --    To           activation chain passed in by the caller
      --    New_Master   master passed in by the caller
      --
      --  Func_Id is the entity of the function where the extended return
      --  statement appears.

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

      Exp         : Node_Id;
      HSS         : Node_Id;
      Result      : Node_Id;
      Stmts       : List_Id := No_List;

      Return_Stmt : Node_Id := Empty;
      --  Force initialization to facilitate static analysis

   --  Start of processing for Expand_N_Extended_Return_Statement

   begin
      --  Given that functionality of interface thunks is simple (just displace
      --  the pointer to the object) they are always handled by means of
      --  simple return statements.

      pragma Assert (not Is_Thunk (Current_Subprogram));

      if Nkind (Ret_Obj_Decl) = N_Object_Declaration then
         Exp := Expression (Ret_Obj_Decl);

         --  Assert that if F says "return R : T := G(...) do..."
         --  then F and G are both b-i-p, or neither b-i-p.

         if Present (Exp) and then Nkind (Exp) = N_Function_Call then
            pragma Assert (Ekind (Current_Subprogram) = E_Function);
            pragma Assert
              (Is_Build_In_Place_Function (Current_Subprogram) =
               Is_Build_In_Place_Function_Call (Exp));
            null;
         end if;

      else
         Exp := Empty;
      end if;

      HSS := Handled_Statement_Sequence (N);

      --  Build a simple_return_statement that returns the return object when
      --  there is a statement sequence, or no expression, or the analysis of
      --  the return object declaration generated extra actions, or the result
      --  will be built in place. Note however that we currently do this for
      --  all composite cases, even though they are not built in place.

      if Present (HSS)
        or else No (Exp)
        or else List_Length (Return_Object_Declarations (N)) > 1
        or else Is_Composite_Type (Ret_Typ)
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

         --  If the returned object needs finalization actions, the function
         --  must perform the appropriate cleanup should it fail to return.

         if Needs_Finalization (Etype (Ret_Obj_Id)) then
            Append_To
              (Stmts, Make_Suppress_Object_Finalize_Call (Loc, Ret_Obj_Id));
         end if;

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

         --  Build a simple_return_statement that returns the return object

         Return_Stmt :=
           Make_Simple_Return_Statement (Loc,
             Expression => New_Occurrence_Of (Ret_Obj_Id, Loc));
         Append_To (Stmts, Return_Stmt);

      --  Case where we do not need to build a block. But we're about to drop
      --  Return_Object_Declarations on the floor, so assert that it contains
      --  only the return object declaration.

      else pragma Assert (List_Length (Return_Object_Declarations (N)) = 1);

         --  Build simple_return_statement that returns the expression directly

         Return_Stmt := Make_Simple_Return_Statement (Loc, Expression => Exp);
         Result := Return_Stmt;
      end if;

      --  Set the flag to prevent infinite recursion

      Set_Comes_From_Extended_Return_Statement (Return_Stmt);
      Set_Return_Statement (Ret_Obj_Id, Return_Stmt);

      Rewrite (N, Result);

      --  AI12-043: The checks of 6.5(8.1/3) and 6.5(21/3) are made immediately
      --  before an object is returned. A predicate that applies to the return
      --  subtype is checked immediately before an object is returned.

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
   begin
      Expand_Call (N);
   end Expand_N_Procedure_Call_Statement;

   ------------------------------------
   -- Expand_N_Return_When_Statement --
   ------------------------------------

   procedure Expand_N_Return_When_Statement (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
   begin
      Rewrite (N,
        Make_If_Statement (Loc,
          Condition       => Condition (N),
          Then_Statements => New_List (
            Make_Simple_Return_Statement (Loc,
              Expression => Expression (N)))));

      Analyze (N);
   end Expand_N_Return_When_Statement;

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
         when E_Function
            | E_Generic_Function
         =>
            Expand_Simple_Function_Return (N);

         when E_Entry
            | E_Entry_Family
            | E_Generic_Procedure
            | E_Procedure
            | E_Return_Statement
         =>
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

   --  Add dummy push/pop label nodes at start and end to clear any local
   --  exception indications if local-exception-to-goto optimization is active.

   --  Add return statement if last statement in body is not a return statement
   --  (this makes things easier on Gigi which does not want to have to handle
   --  a missing return).

   --  Deal with possible detection of infinite recursion

   --  Eliminate body completely if convention stubbed

   --  Encode entity names within body, since we will not need to reference
   --  these entities any longer in the front end.

   --  Initialize scalar out parameters if Initialize/Normalize_Scalars

   --  Reset Pure indication if any parameter has root type System.Address
   --  or has any parameters of limited types, where limited means that the
   --  run-time view is limited (i.e. the full type is limited).

   --  Apply raise check

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

            --  Ada 2022 (AI12-0279): append the call to 'Yield unless this is
            --  a generic subprogram (since in such case it will be added to
            --  the instantiations).

            if Has_Yield_Aspect (Spec_Id)
              and then Ekind (Spec_Id) /= E_Generic_Procedure
              and then RTE_Available (RE_Yield)
            then
               Insert_Action (Stmt,
                 Make_Procedure_Call_Statement (Loc,
                   New_Occurrence_Of (RTE (RE_Yield), Loc)));
            end if;
         end if;
      end Add_Return;

      --  Local variables

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
      --  this if we have No_Exception_Handlers or expanding a entry barrier
      --  function, since they are useless and interfere with analysis (e.g. by
      --  CodePeer) and other optimizations. We also don't need these if we're
      --  unnesting subprograms because the only purpose of these nodes is to
      --  ensure we don't set a label in one subprogram and branch to it in
      --  another.

      if (Debug_Flag_Dot_G
           or else Restriction_Active (No_Exception_Propagation))
        and then not Restriction_Active (No_Exception_Handlers)
        and then not CodePeer_Mode
        and then not Is_Entry_Barrier_Function (N)
        and then not Unnest_Subprogram_Mode
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

      Compute_Returns_By_Ref (Spec_Id);

      --  For a procedure, we add a return for all possible syntactic ends of
      --  the subprogram.

      if Ekind (Spec_Id) in E_Procedure | E_Generic_Procedure then
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

      --  If the body belongs to a nonabstract library-level source primitive
      --  of a tagged type, install an elaboration check which ensures that a
      --  dispatching call targeting the primitive will not execute the body
      --  without it being previously elaborated.

      Install_Primitive_Elaboration_Check (N);

      --  If the subprogram is subject to pragma No_Raise, apply the check

      Apply_Raise_Check (N);
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

   begin
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
            Pop_Scope;
         end if;

      --  Ada 2005 (AI-348): Generate body for a null procedure. In most
      --  cases this is superfluous because calls to it will be automatically
      --  inlined, but we definitely need the body if preconditions for the
      --  procedure are present, or if performing coverage analysis.

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

      Loc       : constant Source_Ptr := Sloc (N);
      Scope_Id  : Entity_Id := Return_Applies_To (Return_Statement_Entity (N));
      Kind      : constant Entity_Kind := Ekind (Scope_Id);
      Call      : Node_Id;
      Acc_Stat  : Node_Id;
      Goto_Stat : Node_Id;
      Lab_Node  : Node_Id;

   begin
      --  Ada 2022 (AI12-0279)

      if Has_Yield_Aspect (Scope_Id)
        and then RTE_Available (RE_Yield)
      then
         Insert_Action (N,
           Make_Procedure_Call_Statement (Loc,
             New_Occurrence_Of (RTE (RE_Yield), Loc)));
      end if;

      --  If it is a return from a procedure do no extra steps

      if Kind = E_Procedure or else Kind = E_Generic_Procedure then
         return;

      --  If it is a nested return within an extended one, replace it with a
      --  return of the previously declared return object.

      elsif Kind = E_Return_Statement then
         declare
            Ret_Obj_Id : constant Entity_Id := First_Entity (Scope_Id);

         begin
            --  Apply the same processing as Expand_N_Extended_Return_Statement
            --  if the returned object needs finalization actions. Note that we
            --  are invoked before Expand_N_Extended_Return_Statement but there
            --  may be multiple nested returns within the extended one.

            if Needs_Finalization (Etype (Ret_Obj_Id)) then
               Insert_Action
                 (N, Make_Suppress_Object_Finalize_Call (Loc, Ret_Obj_Id));
            end if;

            Rewrite (N,
              Make_Simple_Return_Statement (Loc,
                Expression => New_Occurrence_Of (Ret_Obj_Id, Loc)));
            Set_Comes_From_Extended_Return_Statement (N);
            Set_Return_Statement_Entity (N, Scope_Id);
            Expand_Simple_Function_Return (N);
            return;
         end;
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
      while Present (Proc) and then Scope (Proc) /= Scop loop
         Proc := Scope (Proc);
         if Is_Subprogram (Proc)
           and then Present (Protected_Subprogram (Proc))
         then
            Proc := Protected_Subprogram (Proc);
         end if;
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
      Rec : Node_Id;

      procedure Expand_Internal_Init_Call;
      --  A call to an operation of the type may occur in the initialization
      --  of a private component. In that case the prefix of the call is an
      --  entity name and the call is treated as internal even though it
      --  appears in code outside of the protected type.

      procedure Freeze_Called_Function;
      --  If it is a function call it can appear in elaboration code and
      --  the called entity must be frozen before the call. This must be
      --  done before the call is expanded, as the expansion may rewrite it
      --  to something other than a call (e.g. a temporary initialized in a
      --  transient block).

      -------------------------------
      -- Expand_Internal_Init_Call --
      -------------------------------

      procedure Expand_Internal_Init_Call is
      begin
         --  If the context is a protected object (rather than a protected
         --  type) the call itself is bound to raise program_error because
         --  the protected body will not have been elaborated yet. This is
         --  diagnosed subsequently in Sem_Elab.

         Freeze_Called_Function;

         --  The target of the internal call is the first formal of the
         --  enclosing initialization procedure.

         Rec := New_Occurrence_Of (First_Formal (Current_Scope), Sloc (N));
         Build_Protected_Subprogram_Call (N,
           Name     => Name (N),
           Rec      => Rec,
           External => False);
         Analyze (N);
         Resolve (N, Etype (Subp));
      end Expand_Internal_Init_Call;

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
      --  The object may be a component of some other data structure, in which
      --  case this must be handled as an inter-object call.

      if not Scope_Within_Or_Same (Inner => Current_Scope, Outer => Scop)
        or else Is_Entry_Wrapper (Current_Scope)
        or else not Is_Entity_Name (Name (N))
      then
         if Nkind (Name (N)) = N_Selected_Component then
            Rec := Prefix (Name (N));

         elsif Nkind (Name (N)) = N_Indexed_Component then
            Rec := Prefix (Prefix (Name (N)));

         --  If this is a call within an entry wrapper, it appears within a
         --  precondition that calls another primitive of the synchronized
         --  type. The target object of the call is the first actual on the
         --  wrapper. Note that this is an external call, because the wrapper
         --  is called outside of the synchronized object. This means that
         --  an entry call to an entry with preconditions involves two
         --  synchronized operations.

         elsif Ekind (Current_Scope) = E_Procedure
           and then Is_Entry_Wrapper (Current_Scope)
         then
            Rec := New_Occurrence_Of (First_Entity (Current_Scope), Sloc (N));

         --  A default parameter of a protected operation may be a call to
         --  a protected function of the type. This appears as an internal
         --  call in the profile of the operation, but if the context is an
         --  external call we must convert the call into an external one,
         --  using the protected object that is the target, so that:

         --     Prot.P (F)
         --  is transformed into
         --     Prot.P (Prot.F)

         elsif Nkind (Parent (N)) = N_Procedure_Call_Statement
           and then Nkind (Name (Parent (N))) = N_Selected_Component
           and then Is_Protected_Type (Etype (Prefix (Name (Parent (N)))))
           and then Is_Entity_Name (Name (N))
           and then Scope (Entity (Name (N))) =
                      Etype (Prefix (Name (Parent (N))))
         then
            Rewrite (Name (N),
              Make_Selected_Component (Sloc (N),
                Prefix        => New_Copy_Tree (Prefix (Name (Parent (N)))),
                Selector_Name => Relocate_Node (Name (N))));

            Analyze_And_Resolve (N);
            return;

         else
            --  If the context is the initialization procedure for a protected
            --  type, the call is legal because the called entity must be a
            --  function of that enclosing type, and this is treated as an
            --  internal call.

            pragma Assert
              (Is_Entity_Name (Name (N)) and then Inside_Init_Proc);

            Expand_Internal_Init_Call;
            return;
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
      --  The underlying result type of the function

      Exp : Node_Id := Expression (N);
      pragma Assert (Present (Exp));

      Exp_Is_Function_Call : constant Boolean :=
        Nkind (Exp) = N_Function_Call
          or else
            (Is_Captured_Function_Call (Exp)
              and then Is_Related_To_Func_Return (Entity (Prefix (Exp))));
      --  If the expression is a captured function call, then we need to make
      --  sure that the object doing the capture is properly recognized by the
      --  Is_Related_To_Func_Return predicate; otherwise, if it is of a type
      --  that needs finalization, Requires_Cleanup_Actions would return true
      --  because of this and Build_Finalizer would finalize it prematurely.

      Exp_Typ : constant Entity_Id := Etype (Exp);
      --  The type of the expression (not necessarily the same as R_Type)

      Subtype_Ind : Node_Id;
      --  If the result type of the function is class-wide and the expression
      --  has a specific type, then we use the expression's type as the type of
      --  the return object. In cases where the expression is an aggregate that
      --  is built in place, this avoids the need for an expensive conversion
      --  of the return object to the specific type on assignments to the
      --  individual components.

   --  Start of processing for Expand_Simple_Function_Return

   begin
      if Is_Class_Wide_Type (R_Type)
        and then not Is_Class_Wide_Type (Exp_Typ)
        and then Nkind (Exp) /= N_Type_Conversion
      then
         Subtype_Ind := New_Occurrence_Of (Exp_Typ, Loc);
      else
         Subtype_Ind := New_Occurrence_Of (R_Type, Loc);

         --  If the result type is class-wide and the expression is a view
         --  conversion, the conversion plays no role in the expansion because
         --  it does not modify the tag of the object. Remove the conversion
         --  altogether to prevent tag overwriting.

         if Is_Class_Wide_Type (R_Type)
           and then not Is_Class_Wide_Type (Exp_Typ)
           and then Nkind (Exp) = N_Type_Conversion
         then
            Exp := Expression (Exp);
         end if;
      end if;

      --  For the case of a simple return that does not come from an
      --  extended return, in the case of build-in-place, we rewrite
      --  "return <expression>;" to be:

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

      pragma Assert
        (Comes_From_Extended_Return_Statement (N)
          or else not Is_Build_In_Place_Function_Call (Exp)
          or else Has_BIP_Formals (Scope_Id));

      if not Comes_From_Extended_Return_Statement (N)
        and then Is_Build_In_Place_Function (Scope_Id)

         --  The functionality of interface thunks is simple and it is always
         --  handled by means of simple return statements. This leaves their
         --  expansion simple and clean.

        and then not Is_Thunk (Scope_Id)
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

      --  Assert that if F says "return G(...);"
      --  then F and G are both b-i-p, or neither b-i-p.

      if Nkind (Exp) = N_Function_Call then
         pragma Assert (Ekind (Scope_Id) = E_Function);
         pragma Assert
           (Is_Build_In_Place_Function (Scope_Id) =
            Is_Build_In_Place_Function_Call (Exp));
         null;
      end if;

      --  Here we have a simple return statement that is part of the expansion
      --  of an extended return statement (either written by the user, or
      --  generated by the above code).

      --  Always normalize C/Fortran boolean result. This is not always needed,
      --  but it seems a good idea to minimize the passing around of non-
      --  normalized values, and in any case this handles the processing of
      --  barrier functions for protected types, which turn the condition into
      --  a return statement.

      if Is_Boolean_Type (Exp_Typ) and then Nonzero_Is_True (Exp_Typ) then
         Adjust_Condition (Exp);
         Adjust_Result_Type (Exp, Exp_Typ);

         --  The adjustment of the expression may have rewritten the return
         --  statement itself, e.g. when it is turned into an if expression.

         if Nkind (N) /= N_Simple_Return_Statement then
            return;
         end if;
      end if;

      --  Do validity check if enabled for returns

      if Validity_Checks_On and then Validity_Check_Returns then
         Ensure_Valid (Exp);
      end if;

      --  Check the result expression of a scalar function against the subtype
      --  of the function by inserting a conversion. This conversion must
      --  eventually be performed for other classes of types, but for now it's
      --  only done for scalars ???

      if Is_Scalar_Type (Exp_Typ) and then Exp_Typ /= R_Type then
         Rewrite (Exp, Convert_To (R_Type, Exp));

         --  The expression is resolved to ensure that the conversion gets
         --  expanded to generate a possible constraint check.

         Analyze_And_Resolve (Exp, R_Type);
      end if;

      --  Deal with returning variable length objects and controlled types

      --  Nothing to do if we are returning by reference

      if Is_Build_In_Place_Function (Scope_Id) then
         --  Prevent the reclamation of the secondary stack by all enclosing
         --  blocks and loops as well as the related function; otherwise the
         --  result would be reclaimed too early.

         if Needs_BIP_Alloc_Form (Scope_Id) then
            Set_Enclosing_Sec_Stack_Return (N);
         end if;

      elsif Is_Inherently_Limited_Type (R_Type) then
         null;

      --  No copy needed for thunks returning interface type objects since
      --  the object is returned by reference and the maximum functionality
      --  required is just to displace the pointer.

      elsif Is_Thunk (Scope_Id) and then Is_Interface (Exp_Typ) then
         null;

      --  If the call is within a thunk and the type is a limited view, the
      --  back end will eventually see the non-limited view of the type.

      elsif Is_Thunk (Scope_Id) and then Is_Incomplete_Type (Exp_Typ) then
         return;

      --  A return statement from an ignored Ghost function does not use the
      --  secondary stack (or any other one).

      elsif (not Needs_Secondary_Stack (R_Type)
              and then not Is_Secondary_Stack_Thunk (Scope_Id))
        or else Is_Ignored_Ghost_Entity (Scope_Id)
      then
         --  Mutable records with variable-length components are not returned
         --  on the sec-stack, so we need to make sure that the back end will
         --  only copy back the size of the actual value, and not the maximum
         --  size. We create an actual subtype for this purpose. However we
         --  need not do it if the expression is a function call since this
         --  will be done in the called function and doing it here too would
         --  cause a temporary with maximum size to be created. Likewise for
         --  a special return object, since there is no copy in this case.

         declare
            Ubt  : constant Entity_Id := Underlying_Type (Base_Type (Exp_Typ));
            Decl : Node_Id;
            Ent  : Entity_Id;

         begin
            if not Exp_Is_Function_Call
              and then not (Is_Entity_Name (Exp)
                             and then Is_Special_Return_Object (Entity (Exp)))
              and then Has_Defaulted_Discriminants (Ubt)
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

         --  For types which need finalization, do the allocation on the return
         --  stack manually in order to call Adjust at the right time:

         --    type Ann is access R_Type;
         --    for Ann'Storage_pool use rs_pool;
         --    Rnn : constant Ann := new Exp_Typ'(Exp);
         --    return Rnn.all;

         --  but optimize the case where the result is a function call that
         --  also needs finalization. In this case the result can directly be
         --  allocated on the return stack of the caller and no further
         --  processing is required. Likewise if this is a return object.

         if Comes_From_Extended_Return_Statement (N) then
            null;

         elsif Present (Utyp)
           and then Needs_Finalization (Utyp)
           and then not (Exp_Is_Function_Call
                          and then Needs_Finalization (Exp_Typ))
         then
            declare
               Acc_Typ : constant Entity_Id := Make_Temporary (Loc, 'A');

               Alloc_Node : Node_Id;
               Temp       : Entity_Id;

            begin
               Mutate_Ekind (Acc_Typ, E_Access_Type);

               Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_RS_Pool));

               --  This is an allocator for the return stack, and it's fine
               --  to have Comes_From_Source set False on it, as gigi knows not
               --  to flag it as a violation of No_Implicit_Heap_Allocations.

               Alloc_Node :=
                 Make_Allocator (Loc,
                   Expression =>
                     Make_Qualified_Expression (Loc,
                       Subtype_Mark => New_Occurrence_Of (Exp_Typ, Loc),
                       Expression   => Relocate_Node (Exp)));

               --  We do not want discriminant checks on the declaration,
               --  given that it gets its value from the allocator.

               Set_No_Initialization (Alloc_Node);

               Temp := Make_Temporary (Loc, 'R', Alloc_Node);

               Insert_Actions (Exp, New_List (
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Acc_Typ,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication => Subtype_Ind)),

                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Acc_Typ, Loc),
                   Expression          => Alloc_Node)));

               Rewrite (Exp,
                 Make_Explicit_Dereference (Loc,
                 Prefix => New_Occurrence_Of (Temp, Loc)));

               Analyze_And_Resolve (Exp, R_Type);
            end;
         end if;

      --  Here if secondary stack is used

      else
         --  Prevent the reclamation of the secondary stack by all enclosing
         --  blocks and loops as well as the related function; otherwise the
         --  result would be reclaimed too early.

         Set_Enclosing_Sec_Stack_Return (N);

         --  Nothing else to do for a return object

         if Comes_From_Extended_Return_Statement (N) then
            null;

         --  Optimize the case where the result is a function call that also
         --  returns on the secondary stack; in this case the result is already
         --  on the secondary stack and no further processing is required.

         elsif Exp_Is_Function_Call
           and then Needs_Secondary_Stack (Exp_Typ)
         then
            --  Remove side effects from the expression now so that other parts
            --  of the expander do not have to reanalyze this node without this
            --  optimization

            Rewrite (Exp, Duplicate_Subexpr_No_Checks (Exp));

            --  Ada 2005 (AI-251): If the type of the returned object is
            --  an interface then add an implicit type conversion to force
            --  displacement of the "this" pointer.

            if Is_Interface (R_Type) then
               Rewrite (Exp, Convert_To (R_Type, Relocate_Node (Exp)));
            end if;

            Analyze_And_Resolve (Exp, R_Type);

         --  For types which both need finalization and are returned on the
         --  secondary stack, do the allocation on secondary stack manually
         --  in order to call Adjust at the right time:

         --    type Ann is access R_Type;
         --    for Ann'Storage_pool use ss_pool;
         --    Rnn : constant Ann := new Exp_Typ'(Exp);
         --    return Rnn.all;

         --  And we do the same for class-wide types that are not potentially
         --  controlled (by the virtue of restriction No_Finalization) because
         --  gigi is not able to properly allocate class-wide types.

         --  But optimize the case where the result is a function call that
         --  also needs finalization; in this case the result can directly be
         --  allocated on the secondary stack and no further processing is
         --  required, unless the returned object is an interface.

         elsif CW_Or_Needs_Finalization (Utyp)
           and then (Is_Interface (R_Type)
                      or else not (Exp_Is_Function_Call
                                    and then Needs_Finalization (Exp_Typ)))
         then
            declare
               Acc_Typ : constant Entity_Id := Make_Temporary (Loc, 'A');

               Alloc_Node : Node_Id;
               Temp       : Entity_Id;

            begin
               Mutate_Ekind (Acc_Typ, E_Access_Type);
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

               Insert_Actions (Exp, New_List (
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Acc_Typ,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication => Subtype_Ind)),

                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
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

      --  We must also verify an underlying type exists for the return type in
      --  case it is incomplete - in which case is not necessary to generate a
      --  check anyway since an incomplete limited tagged return type would
      --  qualify as a premature usage.

      if Present (Utyp)
        and then Is_Tagged_Type (Utyp)
        and then not Is_Class_Wide_Type (Utyp)
        and then Is_Conversion_Or_Reference_To_Formal (Exp)
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

      --  Ada 2005 (AI95-344): If the result type is class-wide, then insert
      --  a check that the level of the return expression's underlying type
      --  is not deeper than the level of the master enclosing the function.

      --  AI12-043: The check is made immediately after the return object is
      --  created. This means that we do not apply it to the simple return
      --  generated by the expansion of an extended return statement.

      --  No runtime check needed in interface thunks since it is performed
      --  by the target primitive associated with the thunk.

      elsif Is_Class_Wide_Type (R_Type)
        and then not Comes_From_Extended_Return_Statement (N)
        and then not Is_Thunk (Scope_Id)
      then
         Apply_CW_Accessibility_Check (Exp, Scope_Id);

      --  Ada 2012 (AI05-0073): If the result subtype of the function is
      --  defined by an access_definition designating a specific tagged
      --  type T, a check is made that the result value is null or the tag
      --  of the object designated by the result value identifies T.

      --  The return expression is referenced twice in the code below, so it
      --  must be made free of side effects. Given that different compilers
      --  may evaluate these parameters in different order, both occurrences
      --  perform a copy.

      elsif Ekind (R_Type) = E_Anonymous_Access_Type
        and then Is_Tagged_Type (Designated_Type (R_Type))
        and then not Is_Class_Wide_Type (Designated_Type (R_Type))
        and then Nkind (Original_Node (Exp)) /= N_Null
        and then not Tag_Checks_Suppressed (Designated_Type (R_Type))
      then
         --  Generate:
         --    [Constraint_Error
         --       when Exp /= null
         --         and then Exp.all not in Designated_Type]

         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_And_Then (Loc,
                 Left_Opnd  =>
                   Make_Op_Ne (Loc,
                     Left_Opnd  => Duplicate_Subexpr (Exp),
                     Right_Opnd => Make_Null (Loc)),

                 Right_Opnd =>
                   Make_Not_In (Loc,
                     Left_Opnd  =>
                       Make_Explicit_Dereference (Loc,
                         Prefix => Duplicate_Subexpr (Exp)),
                     Right_Opnd =>
                       New_Occurrence_Of (Designated_Type (R_Type), Loc))),

             Reason    => CE_Tag_Check_Failed),
             Suppress  => All_Checks);
      end if;

      --  If the result is of an unconstrained array subtype with fixed lower
      --  bound, then sliding to that bound may be needed.

      if Is_Fixed_Lower_Bound_Array_Subtype (R_Type) then
         Expand_Sliding_Conversion (Exp, R_Type);
      end if;

      --  If we are returning a nonscalar object that is possibly unaligned,
      --  then copy the value into a temporary first. This copy may need to
      --  expand to a loop of component operations.

      if Is_Possibly_Unaligned_Slice (Exp)
        or else (Is_Possibly_Unaligned_Object (Exp)
                  and then not Represented_As_Scalar (Etype (Exp)))
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

      --  Ada 2005 (AI-251): If this return statement corresponds with an
      --  simple return statement associated with an extended return statement
      --  and the type of the returned object is an interface then generate an
      --  implicit conversion to force displacement of the "this" pointer.

      if Ada_Version >= Ada_2005
        and then Comes_From_Extended_Return_Statement (N)
        and then Nkind (Expression (N)) = N_Identifier
        and then Is_Interface (Utyp)
        and then Utyp /= Underlying_Type (Exp_Typ)
      then
         Rewrite (Exp, Convert_To (Utyp, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp);
      end if;

      --  Ada 2022 (AI12-0279)

      if Has_Yield_Aspect (Scope_Id)
        and then RTE_Available (RE_Yield)
      then
         Insert_Action (N,
           Make_Procedure_Call_Statement (Loc,
             New_Occurrence_Of (RTE (RE_Yield), Loc)));
      end if;
   end Expand_Simple_Function_Return;

   -----------------------
   -- Freeze_Subprogram --
   -----------------------

   procedure Freeze_Subprogram (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Subp : constant Entity_Id  := Entity (N);

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

            L : List_Id;

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
               and then Convention (Subp) in Convention_C_Family
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
                     L := Register_Predefined_Primitive (Loc, Subp);
                  else
                     L := New_List;
                  end if;

                  Append_List_To (L, Register_Primitive (Loc, Subp));

                  if Is_Empty_List (L) then
                     null;

                  elsif No (Actions (N)) then
                     Set_Actions (N, L);

                  else
                     Append_List (L, Actions (N));
                  end if;
               end if;
            end if;
         end;
      end if;

      --  Mark functions that return by reference. Note that it cannot be part
      --  of the normal semantic analysis of the spec since the underlying
      --  returned type may not be known yet (for private types).

      Compute_Returns_By_Ref (Subp);
   end Freeze_Subprogram;

   --------------------------
   -- Has_BIP_Extra_Formal --
   --------------------------

   function Has_BIP_Extra_Formal
     (E              : Entity_Id;
      Kind           : BIP_Formal_Kind;
      Must_Be_Frozen : Boolean := True) return Boolean
   is
      Extra_Formal : Entity_Id := Extra_Formals (E);

   begin
      --  We can only rely on the availability of the extra formals in frozen
      --  entities or in subprogram types of dispatching calls (since their
      --  extra formals are added when the target subprogram is frozen; see
      --  Expand_Dispatching_Call).

      pragma Assert ((Is_Frozen (E) or else not Must_Be_Frozen)
        or else (Ekind (E) = E_Subprogram_Type
                   and then Is_Dispatch_Table_Entity (E))
        or else (Is_Dispatching_Operation (E)
                   and then Is_Frozen (Find_Dispatching_Type (E))));

      while Present (Extra_Formal) loop
         if Is_Build_In_Place_Entity (Extra_Formal)
           and then BIP_Suffix_Kind (Extra_Formal) = Kind
         then
            return True;
         end if;

         Next_Formal_With_Extras (Extra_Formal);
      end loop;

      return False;
   end Has_BIP_Extra_Formal;

   ------------------------------
   -- Insert_Post_Call_Actions --
   ------------------------------

   procedure Insert_Post_Call_Actions (N : Node_Id; Post_Call : List_Id) is
      Context : constant Node_Id := Parent (N);

   begin
      if Is_Empty_List (Post_Call) then
         return;
      end if;

      --  Cases where the call is not a member of a statement list. This also
      --  includes the cases where the call is an actual in another function
      --  call, or is an index, or is an operand of an if-expression, i.e. is
      --  in an expression context.

      if not Is_List_Member (N)
        or else Nkind (Context) in N_Function_Call
                                 | N_If_Expression
                                 | N_Indexed_Component
      then
         --  In Ada 2012 the call may be a function call in an expression
         --  (since OUT and IN OUT parameters are now allowed for such calls).
         --  The write-back of (in)-out parameters is handled by the back-end,
         --  but the constraint checks generated when subtypes of formal and
         --  actual don't match must be inserted in the form of assignments.
         --  Also do this in the case of explicit dereferences, which can occur
         --  due to rewritings of function calls with controlled results.

         if Nkind (N) = N_Function_Call
           or else Nkind (Original_Node (N)) = N_Function_Call
           or else Nkind (N) = N_Explicit_Dereference
         then
            pragma Assert (Ada_Version >= Ada_2012);
            --  Functions with '[in] out' parameters are only allowed in Ada
            --  2012.

            --  We used to handle this by climbing up parents to a
            --  non-statement/declaration and then simply making a call to
            --  Insert_Actions_After (P, Post_Call), but that doesn't work
            --  for Ada 2012. If we are in the middle of an expression, e.g.
            --  the condition of an IF, this call would insert after the IF
            --  statement, which is much too late to be doing the write back.
            --  For example:

            --     if Clobber (X) then
            --        Put_Line (X'Img);
            --     else
            --        goto Junk
            --     end if;

            --  Now assume Clobber changes X, if we put the write back after
            --  the IF, the Put_Line gets the wrong value and the goto causes
            --  the write back to be skipped completely.

            --  To deal with this, we replace the call by
            --
            --    do
            --       Tnnn : constant function-result-type := function-call;
            --       Post_Call actions
            --    in
            --       Tnnn;
            --    end;
            --
            --   However, that doesn't work if function-result-type requires
            --   finalization (because function-call's result never gets
            --   finalized). So in that case, we instead replace the call by
            --
            --    do
            --       type Ref is access all function-result-type;
            --       Ptr : constant Ref := function-call'Reference;
            --       Tnnn : constant function-result-type := Ptr.all;
            --       Finalize (Ptr.all);
            --       Post_Call actions
            --    in
            --       Tnnn;
            --    end;
            --

            declare
               Loc   : constant Source_Ptr := Sloc (N);
               Tnnn  : constant Entity_Id := Make_Temporary (Loc, 'T');
               FRTyp : constant Entity_Id := Etype (N);
               Name  : constant Node_Id   := Relocate_Node (N);

            begin
               if Needs_Finalization (FRTyp) then
                  declare
                     Ptr_Typ : constant Entity_Id := Make_Temporary (Loc, 'A');

                     Ptr_Typ_Decl : constant Node_Id :=
                       Make_Full_Type_Declaration (Loc,
                         Defining_Identifier => Ptr_Typ,
                         Type_Definition     =>
                           Make_Access_To_Object_Definition (Loc,
                             All_Present        => True,
                             Subtype_Indication =>
                               New_Occurrence_Of (FRTyp, Loc)));

                     Ptr_Obj : constant Entity_Id :=
                       Make_Temporary (Loc, 'P');

                     Ptr_Obj_Decl : constant Node_Id :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Ptr_Obj,
                         Object_Definition   =>
                           New_Occurrence_Of (Ptr_Typ, Loc),
                         Constant_Present    => True,
                         Expression          =>
                           Make_Attribute_Reference (Loc,
                           Prefix         => Name,
                           Attribute_Name => Name_Unrestricted_Access));

                     function Ptr_Dereference return Node_Id is
                       (Make_Explicit_Dereference (Loc,
                          Prefix => New_Occurrence_Of (Ptr_Obj, Loc)));

                     Tnn_Decl : constant Node_Id :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Tnnn,
                         Object_Definition   => New_Occurrence_Of (FRTyp, Loc),
                         Constant_Present    => True,
                         Expression          => Ptr_Dereference);

                     Finalize_Call : constant Node_Id :=
                       Make_Final_Call
                         (Obj_Ref => Ptr_Dereference, Typ => FRTyp);
                  begin
                     --  Prepend in reverse order

                     Prepend_To (Post_Call, Finalize_Call);
                     Prepend_To (Post_Call, Tnn_Decl);
                     Prepend_To (Post_Call, Ptr_Obj_Decl);
                     Prepend_To (Post_Call, Ptr_Typ_Decl);
                  end;
               else
                  Prepend_To (Post_Call,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Tnnn,
                      Object_Definition   => New_Occurrence_Of (FRTyp, Loc),
                      Constant_Present    => True,
                      Expression          => Name));
               end if;

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
            end;

         --  If not the special Ada 2012 case of a function call, then we must
         --  have the triggering statement of a triggering alternative or an
         --  entry call alternative, and we can add the post call stuff to the
         --  corresponding statement list.

         else
            pragma Assert (Nkind (Context) in N_Entry_Call_Alternative
                                            | N_Triggering_Alternative);

            if Is_Non_Empty_List (Statements (Context)) then
               Insert_List_Before_And_Analyze
                 (First (Statements (Context)), Post_Call);
            else
               Set_Statements (Context, Post_Call);
            end if;
         end if;

      --  A procedure call is always part of a declarative or statement list,
      --  however a function call may appear nested within a construct. Most
      --  cases of function call nesting are handled in the special case above.
      --  The only exception is when the function call acts as an actual in a
      --  procedure call. In this case the function call is in a list, but the
      --  post-call actions must be inserted after the procedure call.
      --  What if the function call is an aggregate component ???

      elsif Nkind (Context) = N_Procedure_Call_Statement then
         Insert_Actions_After (Context, Post_Call);

      --  Otherwise, normal case where N is in a statement sequence, just put
      --  the post-call stuff after the call statement.

      else
         Insert_Actions_After (N, Post_Call);
      end if;
   end Insert_Post_Call_Actions;

   ---------------------------------------
   -- Install_Class_Preconditions_Check --
   ---------------------------------------

   procedure Install_Class_Preconditions_Check (Call_Node : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Call_Node);

      function Build_Dynamic_Check_Helper_Call return Node_Id;
      --  Build call to the helper runtime function of the nearest ancestor
      --  of the target subprogram that dynamically evaluates the merged
      --  or-else preconditions.

      function Build_Error_Message (Subp_Id : Entity_Id) return Node_Id;
      --  Build message associated with the class-wide precondition of Subp_Id
      --  indicating the call that caused it.

      function Build_Static_Check_Helper_Call return Node_Id;
      --  Build call to the helper runtime function of the nearest ancestor
      --  of the target subprogram that dynamically evaluates the merged
      --  or-else preconditions.

      function Class_Preconditions_Subprogram
        (Spec_Id : Entity_Id;
         Dynamic : Boolean) return Node_Id;
      --  Return the nearest ancestor of Spec_Id defining a helper function
      --  that evaluates a combined or-else expression containing all the
      --  inherited class-wide preconditions; Dynamic enables searching for
      --  the helper that dynamically evaluates preconditions using dispatching
      --  calls; if False it searches for the helper that statically evaluates
      --  preconditions; return Empty when not available (which means that no
      --  preconditions check is required).

      -------------------------------------
      -- Build_Dynamic_Check_Helper_Call --
      -------------------------------------

      function Build_Dynamic_Check_Helper_Call return Node_Id is
         Spec_Id   : constant Entity_Id := Entity (Name (Call_Node));
         CW_Subp   : constant Entity_Id :=
                       Class_Preconditions_Subprogram (Spec_Id,
                         Dynamic => True);
         Helper_Id : constant Entity_Id :=
                       Dynamic_Call_Helper (CW_Subp);
         Actuals   : constant List_Id := New_List;
         A         : Node_Id   := First_Actual (Call_Node);

      begin
         while Present (A) loop

            --  Ensure that the evaluation of the actuals will not produce
            --  side effects.

            Remove_Side_Effects (A);

            Append_To (Actuals, New_Copy_Tree (A));

            Next_Actual (A);
         end loop;

         return
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (Helper_Id, Loc),
             Parameter_Associations => Actuals);
      end Build_Dynamic_Check_Helper_Call;

      -------------------------
      -- Build_Error_Message --
      -------------------------

      function Build_Error_Message (Subp_Id : Entity_Id) return Node_Id is

         procedure Append_Message
           (Id       : Entity_Id;
            Is_First : in out Boolean);
         --  Build the fragment of the message associated with subprogram Id;
         --  Is_First facilitates identifying continuation messages.

         --------------------
         -- Append_Message --
         --------------------

         procedure Append_Message
           (Id       : Entity_Id;
            Is_First : in out Boolean)
         is
            Prag : constant Node_Id :=
              Get_Class_Wide_Pragma (Id, Pragma_Precondition);

         begin
            if No (Prag) or else Is_Ignored (Prag) then
               return;
            end if;

            if Is_First then
               Is_First := False;

               if Id /= Subp_Id then
                  Append
                    (Global_Name_Buffer, "failed inherited precondition ");
               else
                  Append (Global_Name_Buffer, "failed precondition ");
               end if;

            else
               Append (Global_Name_Buffer, ASCII.LF);
               Append (Global_Name_Buffer, "  or ");

               Append (Global_Name_Buffer, "failed inherited precondition ");
            end if;

            Append (Global_Name_Buffer, "from " &
              Build_Location_String
                (Sloc
                  (First_Node
                     (Expression
                        (First (Pragma_Argument_Associations (Prag)))))));
         end Append_Message;

         --  Local variables

         Str_Loc  : constant String := Build_Location_String (Loc);
         Subps    : constant Subprogram_List :=
                      Inherited_Subprograms (Subp_Id);
         Is_First : Boolean := True;

      --  Start of processing for Build_Error_Message

      begin
         Name_Len := 0;
         Append_Message (Subp_Id, Is_First);

         for Index in Subps'Range loop
            Append_Message (Subps (Index), Is_First);
         end loop;

         if Present (Controlling_Argument (Call_Node)) then
            Append (Global_Name_Buffer, " in dispatching call at ");
         else
            Append (Global_Name_Buffer, " in call at ");
         end if;

         Append (Global_Name_Buffer, Str_Loc);

         return Make_String_Literal (Loc, Name_Buffer (1 .. Name_Len));
      end Build_Error_Message;

      ------------------------------------
      -- Build_Static_Check_Helper_Call --
      ------------------------------------

      function Build_Static_Check_Helper_Call return Node_Id is
         Actuals   : constant List_Id := New_List;
         A         : Node_Id;
         Helper_Id : Entity_Id;
         F         : Entity_Id;
         CW_Subp   : Entity_Id;
         Spec_Id   : constant Entity_Id := Entity (Name (Call_Node));

      begin
         --  The target is the wrapper built to support inheriting body but
         --  overriding pre/postconditions (AI12-0195).

         if Is_Dispatch_Table_Wrapper (Spec_Id) then
            CW_Subp := Spec_Id;

         --  Common case

         else
            CW_Subp := Class_Preconditions_Subprogram (Spec_Id,
                         Dynamic => False);
         end if;

         Helper_Id := Static_Call_Helper (CW_Subp);

         F := First_Formal (Helper_Id);
         A := First_Actual (Call_Node);
         while Present (A) loop

            --  Ensure that the evaluation of the actuals will not produce
            --  side effects.

            Remove_Side_Effects (A);

            --  Ensure matching types to avoid reporting spurious errors since
            --  the called helper may have been built for a parent type.

            if Etype (F) /= Etype (A) then
               Append_To (Actuals,
                 Unchecked_Convert_To (Etype (F), New_Copy_Tree (A)));
            else
               Append_To (Actuals, New_Copy_Tree (A));
            end if;

            Next_Formal (F);
            Next_Actual (A);
         end loop;

         return
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (Helper_Id, Loc),
             Parameter_Associations => Actuals);
      end Build_Static_Check_Helper_Call;

      ------------------------------------
      -- Class_Preconditions_Subprogram --
      ------------------------------------

      function Class_Preconditions_Subprogram
        (Spec_Id : Entity_Id;
         Dynamic : Boolean) return Node_Id
      is
         Subp_Id : constant Entity_Id := Ultimate_Alias (Spec_Id);

      begin
         --  Prevent cascaded errors

         if not Is_Dispatching_Operation (Subp_Id) then
            return Empty;

         --  No need to search if this subprogram has the helper we are
         --  searching

         elsif Dynamic then
            if Present (Dynamic_Call_Helper (Subp_Id)) then
               return Subp_Id;
            end if;
         else
            if Present (Static_Call_Helper (Subp_Id)) then
               return Subp_Id;
            end if;
         end if;

         --  Process inherited subprograms looking for class-wide
         --  preconditions.

         declare
            Subps   : constant Subprogram_List :=
                        Inherited_Subprograms (Subp_Id);
            Subp_Id : Entity_Id;

         begin
            for Index in Subps'Range loop
               Subp_Id := Subps (Index);

               if Present (Alias (Subp_Id)) then
                  Subp_Id := Ultimate_Alias (Subp_Id);
               end if;

               --  Wrappers of class-wide pre/postconditions reference the
               --  parent primitive that has the inherited contract.

               if Is_Wrapper (Subp_Id)
                 and then Present (LSP_Subprogram (Subp_Id))
               then
                  Subp_Id := LSP_Subprogram (Subp_Id);
               end if;

               if Dynamic then
                  if Present (Dynamic_Call_Helper (Subp_Id)) then
                     return Subp_Id;
                  end if;
               else
                  if Present (Static_Call_Helper (Subp_Id)) then
                     return Subp_Id;
                  end if;
               end if;
            end loop;
         end;

         return Empty;
      end Class_Preconditions_Subprogram;

      --  Local variables

      Dynamic_Check : constant Boolean :=
                        Present (Controlling_Argument (Call_Node));
      Class_Subp    : Entity_Id;
      Cond          : Node_Id;
      Fail          : Node_Id;
      Subp          : Entity_Id;

   --  Start of processing for Install_Class_Preconditions_Check

   begin
      --  Do not expand the check if we are compiling under restriction
      --  No_Dispatching_Calls; the semantic analyzer has previously
      --  notified the violation of this restriction.

      if Dynamic_Check
        and then Restriction_Active (No_Dispatching_Calls)
      then
         return;

      --  Class-wide precondition check not needed in interface thunks since
      --  they are installed in the dispatching call that caused invoking the
      --  thunk.

      elsif Is_Thunk (Current_Scope) then
         return;
      end if;

      Subp := Entity (Name (Call_Node));

      --  No check needed for this subprogram call if no class-wide
      --  preconditions apply (or if the unique available preconditions
      --  are ignored preconditions).

      Class_Subp := Class_Preconditions_Subprogram (Subp, Dynamic_Check);

      if No (Class_Subp)
        or else No (Class_Preconditions (Class_Subp))
      then
         return;
      end if;

      --  Build and install the check

      if Dynamic_Check then
         Cond := Build_Dynamic_Check_Helper_Call;
      else
         Cond := Build_Static_Check_Helper_Call;
      end if;

      if Exception_Locations_Suppressed then
         Fail :=
           Make_Raise_Statement (Loc,
             Name =>
               New_Occurrence_Of
                 (RTE (RE_Assert_Failure), Loc));

      --  Failed check with message indicating the failed precondition and the
      --  call that caused it.

      else
         Fail :=
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of
                 (RTE (RE_Raise_Assert_Failure), Loc),
             Parameter_Associations =>
               New_List (Build_Error_Message (Subp)));
      end if;

      Insert_Action (Call_Node,
        Make_If_Statement (Loc,
          Condition       => Make_Op_Not (Loc, Cond),
          Then_Statements => New_List (Fail)));
   end Install_Class_Preconditions_Check;

   ------------------------------
   -- Is_Build_In_Place_Entity --
   ------------------------------

   function Is_Build_In_Place_Entity (E : Entity_Id) return Boolean is
      Nam : constant String := Get_Name_String (Chars (E));

      function Has_Suffix (Suffix : String) return Boolean;
      --  Return True if Nam has suffix Suffix

      function Has_Suffix (Suffix : String) return Boolean is
         Len : constant Natural := Suffix'Length;
      begin
         return Nam'Length > Len
           and then Nam (Nam'Last - Len + 1 .. Nam'Last) = Suffix;
      end Has_Suffix;

   --  Start of processing for Is_Build_In_Place_Entity

   begin
      return Has_Suffix (BIP_Alloc_Suffix)
        or else Has_Suffix (BIP_Storage_Pool_Suffix)
        or else Has_Suffix (BIP_Collection_Suffix)
        or else Has_Suffix (BIP_Task_Master_Suffix)
        or else Has_Suffix (BIP_Activation_Chain_Suffix)
        or else Has_Suffix (BIP_Object_Access_Suffix);
   end Is_Build_In_Place_Entity;

   --------------------------------
   -- Is_Build_In_Place_Function --
   --------------------------------

   function Is_Build_In_Place_Function (E : Entity_Id) return Boolean is
      Kind : constant Entity_Kind := Ekind (E);
      Typ  : constant Entity_Id   := Etype (E);

   begin
      --  This function is called from Expand_Subtype_From_Expr during
      --  semantic analysis, even when expansion is off. In those cases
      --  the build_in_place expansion will not take place.

      if not Expander_Active then
         return False;
      end if;

      --  We never use build-in-place if the convention is other than Ada,
      --  but note that it is OK for a build-in-place function to return a
      --  type with a foreign convention because the machinery ensures there
      --  is no copying.

      return (Kind in E_Function | E_Generic_Function
               or else
             (Kind = E_Subprogram_Type and then Typ /= Standard_Void_Type))
        and then Is_Build_In_Place_Result_Type (Typ)
        and then not Has_Foreign_Convention (E);
   end Is_Build_In_Place_Function;

   -------------------------------------
   -- Is_Build_In_Place_Function_Call --
   -------------------------------------

   function Is_Build_In_Place_Function_Call (N : Node_Id) return Boolean is
      Exp_Node    : constant Node_Id := Unqual_Conv (N);
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

      if not Expander_Active or else Nkind (Exp_Node) /= N_Function_Call then
         return False;
      end if;

      if Is_Entity_Name (Name (Exp_Node)) then
         Function_Id := Entity (Name (Exp_Node));

      --  In the case of an explicitly dereferenced call, use the subprogram
      --  type generated for the dereference.

      elsif Nkind (Name (Exp_Node)) = N_Explicit_Dereference then
         Function_Id := Etype (Name (Exp_Node));

      --  This may be a call to a protected function.

      elsif Nkind (Name (Exp_Node)) = N_Selected_Component then
         --  The selector in question might not have been analyzed due to a
         --  previous error, so analyze it here to output the appropriate
         --  error message instead of crashing when attempting to fetch its
         --  entity.

         if not Analyzed (Selector_Name (Name (Exp_Node))) then
            Analyze (Selector_Name (Name (Exp_Node)));
         end if;

         Function_Id := Etype (Entity (Selector_Name (Name (Exp_Node))));

      else
         raise Program_Error;
      end if;

      declare
         Result : constant Boolean := Is_Build_In_Place_Function (Function_Id);
         --  So we can stop here in the debugger
      begin
         return Result;
      end;
   end Is_Build_In_Place_Function_Call;

   ---------------------------------------
   -- Is_Function_Call_With_BIP_Formals --
   ---------------------------------------

   function Is_Function_Call_With_BIP_Formals (N : Node_Id) return Boolean is
      Exp_Node    : constant Node_Id := Unqual_Conv (N);
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

      if not Expander_Active or else Nkind (Exp_Node) /= N_Function_Call then
         return False;
      end if;

      if Is_Entity_Name (Name (Exp_Node)) then
         Function_Id := Entity (Name (Exp_Node));

      --  In the case of an explicitly dereferenced call, use the subprogram
      --  type generated for the dereference.

      elsif Nkind (Name (Exp_Node)) = N_Explicit_Dereference then
         Function_Id := Etype (Name (Exp_Node));

      --  This may be a call to a protected function.

      elsif Nkind (Name (Exp_Node)) = N_Selected_Component then
         --  The selector in question might not have been analyzed due to a
         --  previous error, so analyze it here to output the appropriate
         --  error message instead of crashing when attempting to fetch its
         --  entity.

         if not Analyzed (Selector_Name (Name (Exp_Node))) then
            Analyze (Selector_Name (Name (Exp_Node)));
         end if;

         Function_Id := Etype (Entity (Selector_Name (Name (Exp_Node))));

      else
         raise Program_Error;
      end if;

      if Is_Build_In_Place_Function (Function_Id) then
         return True;

      --  True also if the function has BIP Formals

      else
         declare
            Kind : constant Entity_Kind := Ekind (Function_Id);

         begin
            if (Kind in E_Function | E_Generic_Function
                  or else (Kind = E_Subprogram_Type
                             and then
                           Etype (Function_Id) /= Standard_Void_Type))
              and then Has_BIP_Formals (Function_Id)
            then
               --  So we can stop here in the debugger
               return True;
            else
               return False;
            end if;
         end;
      end if;
   end Is_Function_Call_With_BIP_Formals;

   -----------------------------------
   -- Is_Build_In_Place_Result_Type --
   -----------------------------------

   function Is_Build_In_Place_Result_Type (Typ : Entity_Id) return Boolean is
   begin
      if not Expander_Active then
         return False;
      end if;

      --  In Ada 2005 all functions with an inherently limited return type
      --  must be handled using a build-in-place profile, including the case
      --  of a function with a limited interface result, where the function
      --  may return objects of nonlimited descendants.

      return Is_Inherently_Limited_Type (Typ)
        and then Ada_Version >= Ada_2005
        and then not Debug_Flag_Dot_L;
   end Is_Build_In_Place_Result_Type;

   -------------------------------------
   -- Is_Build_In_Place_Return_Object --
   -------------------------------------

   function Is_Build_In_Place_Return_Object (E : Entity_Id) return Boolean is
   begin
      return Is_Return_Object (E)
        and then Is_Build_In_Place_Function (Return_Applies_To (Scope (E)));
   end Is_Build_In_Place_Return_Object;

   -----------------------------------
   -- Is_By_Reference_Return_Object --
   -----------------------------------

   function Is_By_Reference_Return_Object (E : Entity_Id) return Boolean is
   begin
      return Is_Return_Object (E)
        and then Is_By_Reference_Type (Etype (Return_Applies_To (Scope (E))));
   end Is_By_Reference_Return_Object;

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

   --------------------------------------
   -- Is_Secondary_Stack_Return_Object --
   --------------------------------------

   function Is_Secondary_Stack_Return_Object (E : Entity_Id) return Boolean is
   begin
      return Is_Return_Object (E)
        and then Needs_Secondary_Stack (Etype (Return_Applies_To (Scope (E))));
   end Is_Secondary_Stack_Return_Object;

   ------------------------------
   -- Is_Special_Return_Object --
   ------------------------------

   function Is_Special_Return_Object (E : Entity_Id) return Boolean is
   begin
      return Is_Build_In_Place_Return_Object (E)
         or else Is_Secondary_Stack_Return_Object (E)
         or else (Back_End_Return_Slot
                   and then Is_By_Reference_Return_Object (E));
   end Is_Special_Return_Object;

   -------------------------------------------
   -- Make_Build_In_Place_Call_In_Allocator --
   -------------------------------------------

   procedure Make_Build_In_Place_Call_In_Allocator
     (Allocator     : Node_Id;
      Function_Call : Node_Id)
   is
      Acc_Type          : constant Entity_Id := Etype (Allocator);
      Loc               : constant Source_Ptr := Sloc (Function_Call);
      Func_Call         : Node_Id := Function_Call;
      Ref_Func_Call     : Node_Id;
      Function_Id       : Entity_Id;
      Result_Subt       : Entity_Id;
      New_Allocator     : Node_Id;
      Return_Obj_Access : Entity_Id; -- temp for function result
      Temp_Init         : Node_Id; -- initial value of Return_Obj_Access
      Alloc_Form        : BIP_Allocation_Form;
      Pool_Actual       : Node_Id; -- Present if Alloc_Form = User_Storage_Pool
      Return_Obj_Actual : Node_Id; -- the temp.all, in caller-allocates case
      Chain             : Entity_Id; -- activation chain, in case of tasks

   begin
      --  Step past qualification or unchecked conversion (the latter can occur
      --  in cases of calls to 'Input).

      if Nkind (Func_Call) in N_Qualified_Expression
                            | N_Type_Conversion
                            | N_Unchecked_Type_Conversion
      then
         Func_Call := Expression (Func_Call);
      end if;

      --  Mark the call as processed as a build-in-place call

      pragma Assert (not Is_Expanded_Build_In_Place_Call (Func_Call));
      Set_Is_Expanded_Build_In_Place_Call (Func_Call);

      if Is_Entity_Name (Name (Func_Call)) then
         Function_Id := Entity (Name (Func_Call));

      elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
         Function_Id := Etype (Name (Func_Call));

      else
         raise Program_Error;
      end if;

      Warn_BIP (Func_Call);

      Result_Subt := Available_View (Etype (Function_Id));

      --  Create a temp for the function result. In the caller-allocates case,
      --  this will be initialized to the result of a new uninitialized
      --  allocator. Note: we do not use Allocator as the Related_Node of
      --  Return_Obj_Access in call to Make_Temporary below as this would
      --  create a sort of infinite "recursion".

      Return_Obj_Access := Make_Temporary (Loc, 'R');
      Set_Etype (Return_Obj_Access, Acc_Type);
      Set_Can_Never_Be_Null (Acc_Type, False);
      --  It gets initialized to null, so we can't have that

      --  When the result subtype is returned on the secondary stack or is
      --  tagged, the called function itself must perform the allocation of
      --  the return object, so we pass parameters indicating that.

      --  But that's also the case when the result subtype needs finalization
      --  actions because the caller side allocation may result in undesirable
      --  finalization. Consider the following example:
      --
      --    function Make_Lim_Ctrl return Lim_Ctrl is
      --    begin
      --       return Result : Lim_Ctrl := raise Program_Error do
      --          null;
      --       end return;
      --    end Make_Lim_Ctrl;
      --
      --    Obj : Lim_Ctrl_Ptr := new Lim_Ctrl'(Make_Lim_Ctrl);
      --
      --  Even though the size of limited controlled type Lim_Ctrl is known,
      --  allocating Obj at the caller side will chain Obj on Lim_Ctrl_Ptr's
      --  finalization collection. The subsequent call to Make_Lim_Ctrl will
      --  fail during the initialization actions for Result, which means that
      --  Result (and Obj by extension) should not be finalized. However Obj
      --  will be finalized when access type Lim_Ctrl_Ptr goes out of scope
      --  since it is already attached on the its finalization collection.

      if Needs_BIP_Alloc_Form (Function_Id) then
         Temp_Init := Empty;

         --  Case of a user-defined storage pool. Pass an allocation parameter
         --  indicating that the function should allocate its result in the
         --  pool, and pass an access to the pool. Use 'Unrestricted_Access
         --  because the pool may not be aliased.

         if Present (Associated_Storage_Pool (Acc_Type)) then
            Alloc_Form  := User_Storage_Pool;
            Pool_Actual :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of
                    (Associated_Storage_Pool (Acc_Type), Loc),
                Attribute_Name => Name_Unrestricted_Access);

         --  No user-defined pool; pass an allocation parameter indicating that
         --  the function should allocate its result on the heap. When there is
         --  a finalization collection, a pool reference is required.

         elsif Needs_BIP_Collection (Function_Id) then
            Alloc_Form  := Global_Heap;
            Pool_Actual :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (RTE (RE_Global_Pool_Object), Loc),
                Attribute_Name => Name_Unrestricted_Access);

         else
            Alloc_Form  := Global_Heap;
            Pool_Actual := Empty;
         end if;

         --  The caller does not provide the return object in this case, so we
         --  have to pass null for the object access actual.

         Return_Obj_Actual := Empty;

      --  When the result subtype neither is returned on the secondary stack
      --  nor is tagged, the return object is created on the caller side, and
      --  access to it is passed to the function.

      else
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
         --  allocator. Unchecked_Convert is needed for T'Input where T is
         --  derived from a controlled type.

         Temp_Init := Relocate_Node (Allocator);

         if Nkind (Function_Call) in
              N_Type_Conversion | N_Unchecked_Type_Conversion
         then
            Temp_Init := Unchecked_Convert_To (Acc_Type, Temp_Init);
         end if;

         --  Indicate that caller allocates, and pass in the return object

         Alloc_Form  := Caller_Allocation;
         Pool_Actual := Empty;
         Return_Obj_Actual := Unchecked_Convert_To
           (Result_Subt,
            Make_Explicit_Dereference (Loc,
              Prefix => New_Occurrence_Of (Return_Obj_Access, Loc)));
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

      --  If the types are incompatible, we need an unchecked conversion. Note
      --  that the full types will be compatible, but the types not visibly
      --  compatible.

      elsif Nkind (Function_Call)
              in N_Type_Conversion | N_Unchecked_Type_Conversion
      then
         Ref_Func_Call := Unchecked_Convert_To (Acc_Type, Ref_Func_Call);
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
         --  callee. Unchecked_Convert is needed for T'Input where T is derived
         --  from a controlled type.

         Actions : List_Id;
         --  Actions to be inserted. If there are no tasks, this is just the
         --  assignment statement. If the allocated object has tasks, we need
         --  to wrap the assignment in a block that activates them. The
         --  activation chain of that block must be passed to the function,
         --  rather than some outer chain.

      begin
         if Might_Have_Tasks (Result_Subt) then
            Actions := New_List;
            Build_Task_Allocate_Block
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
        (Func_Call,
         Function_Id,
         Alloc_Form => Alloc_Form,
         Pool_Exp   => Pool_Actual);

      Add_Collection_Actual_To_Build_In_Place_Call
        (Func_Call, Function_Id, Ptr_Typ => Acc_Type);

      Add_Task_Actuals_To_Build_In_Place_Call
        (Func_Call,
         Function_Id,
         Master_Actual => Master_Id (Acc_Type),
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
      pragma Assert (Check_Number_Of_Actuals (Func_Call, Function_Id));
      pragma Assert (Check_BIP_Actuals (Func_Call, Function_Id));
   end Make_Build_In_Place_Call_In_Allocator;

   ---------------------------------------------------
   -- Make_Build_In_Place_Call_In_Anonymous_Context --
   ---------------------------------------------------

   procedure Make_Build_In_Place_Call_In_Anonymous_Context
     (Function_Call : Node_Id)
   is
      Loc             : constant Source_Ptr := Sloc (Function_Call);
      Func_Call       : constant Node_Id := Unqual_Conv (Function_Call);
      Function_Id     : Entity_Id;
      Result_Subt     : Entity_Id;
      Return_Obj_Id   : Entity_Id;
      Return_Obj_Decl : Entity_Id;

   begin
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

      if Is_Entity_Name (Name (Func_Call)) then
         Function_Id := Entity (Name (Func_Call));

      elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
         Function_Id := Etype (Name (Func_Call));

      else
         raise Program_Error;
      end if;

      Warn_BIP (Func_Call);

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

      --  When the result subtype is definite, an object of the subtype is
      --  declared and an access value designating it is passed as an actual.

      elsif Caller_Known_Size (Func_Call, Result_Subt) then

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

         Add_Collection_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id);

         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Make_Identifier (Loc, Name_uMaster));

         --  Add an implicit actual to the function call that provides access
         --  to the caller's return object.

         Add_Access_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id, New_Occurrence_Of (Return_Obj_Id, Loc));

         pragma Assert (Check_Number_Of_Actuals (Func_Call, Function_Id));
         pragma Assert (Check_BIP_Actuals (Func_Call, Function_Id));

      --  When the result subtype is unconstrained, the function must allocate
      --  the return object in the secondary stack, so appropriate implicit
      --  parameters are added to the call to indicate that. A transient
      --  scope is established to ensure eventual cleanup of the result.

      else
         --  Pass an allocation parameter indicating that the function should
         --  allocate its result on the secondary stack.

         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Secondary_Stack);

         Add_Collection_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id);

         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Make_Identifier (Loc, Name_uMaster));

         --  Pass a null value to the function since no return object is
         --  available on the caller side.

         Add_Access_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id, Empty);

         pragma Assert (Check_Number_Of_Actuals (Func_Call, Function_Id));
         pragma Assert (Check_BIP_Actuals (Func_Call, Function_Id));
      end if;
   end Make_Build_In_Place_Call_In_Anonymous_Context;

   --------------------------------------------
   -- Make_Build_In_Place_Call_In_Assignment --
   --------------------------------------------

   procedure Make_Build_In_Place_Call_In_Assignment
     (Assign        : Node_Id;
      Function_Call : Node_Id)
   is
      Func_Call    : constant Node_Id    := Unqual_Conv (Function_Call);
      Lhs          : constant Node_Id    := Name (Assign);
      Loc          : constant Source_Ptr := Sloc (Function_Call);
      Func_Id      : Entity_Id;
      Obj_Decl     : Node_Id;
      Obj_Id       : Entity_Id;
      Ptr_Typ      : Entity_Id;
      Ptr_Typ_Decl : Node_Id;
      New_Expr     : Node_Id;
      Result_Subt  : Entity_Id;

   begin
      --  Mark the call as processed as a build-in-place call

      pragma Assert (not Is_Expanded_Build_In_Place_Call (Func_Call));
      Set_Is_Expanded_Build_In_Place_Call (Func_Call);

      if Is_Entity_Name (Name (Func_Call)) then
         Func_Id := Entity (Name (Func_Call));

      elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
         Func_Id := Etype (Name (Func_Call));

      else
         raise Program_Error;
      end if;

      Warn_BIP (Func_Call);

      Result_Subt := Etype (Func_Id);

      --  When the result subtype is unconstrained, an additional actual must
      --  be passed to indicate that the caller is providing the return object.
      --  This parameter must also be passed when the called function has a
      --  controlling result, because dispatching calls to the function needs
      --  to be treated effectively the same as calls to class-wide functions.

      Add_Unconstrained_Actuals_To_Build_In_Place_Call
        (Func_Call, Func_Id, Alloc_Form => Caller_Allocation);

      Add_Collection_Actual_To_Build_In_Place_Call
        (Func_Call, Func_Id);

      Add_Task_Actuals_To_Build_In_Place_Call
        (Func_Call, Func_Id, Make_Identifier (Loc, Name_uMaster));

      --  Add an implicit actual to the function call that provides access to
      --  the caller's return object.

      Add_Access_Actual_To_Build_In_Place_Call
        (Func_Call, Func_Id, Unchecked_Convert_To (Result_Subt, Lhs));

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

      --  Add a conversion if it's the wrong type

      New_Expr := Unchecked_Convert_To (Ptr_Typ, New_Expr);

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
      pragma Assert (Check_Number_Of_Actuals (Func_Call, Func_Id));
      pragma Assert (Check_BIP_Actuals (Func_Call, Func_Id));
   end Make_Build_In_Place_Call_In_Assignment;

   ----------------------------------------------------
   -- Make_Build_In_Place_Call_In_Object_Declaration --
   ----------------------------------------------------

   procedure Make_Build_In_Place_Call_In_Object_Declaration
     (Obj_Decl      : Node_Id;
      Function_Call : Node_Id)
   is
      function Get_Function_Id (Func_Call : Node_Id) return Entity_Id;
      --  Get the value of Function_Id, below

      ---------------------
      -- Get_Function_Id --
      ---------------------

      function Get_Function_Id (Func_Call : Node_Id) return Entity_Id is
      begin
         if Is_Entity_Name (Name (Func_Call)) then
            return Entity (Name (Func_Call));

         elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
            return Etype (Name (Func_Call));

         else
            raise Program_Error;
         end if;
      end Get_Function_Id;

      --  Local variables

      Func_Call   : constant Node_Id    := Unqual_Conv (Function_Call);
      Function_Id : constant Entity_Id  := Get_Function_Id (Func_Call);
      Loc         : constant Source_Ptr := Sloc (Function_Call);
      Obj_Loc     : constant Source_Ptr := Sloc (Obj_Decl);
      Obj_Def_Id  : constant Entity_Id  := Defining_Identifier (Obj_Decl);
      Obj_Typ     : constant Entity_Id  := Etype (Obj_Def_Id);
      Encl_Func   : constant Entity_Id  := Enclosing_Subprogram (Obj_Def_Id);
      Result_Subt : constant Entity_Id  := Etype (Function_Id);

      Call_Deref        : Node_Id;
      Caller_Object     : Node_Id;
      Collection_Actual : Node_Id := Empty;
      Def_Id            : Entity_Id;
      Designated_Type   : Entity_Id;
      Pool_Actual       : Node_Id;
      Ptr_Typ           : Entity_Id;
      Ptr_Typ_Decl      : Node_Id;
      Pass_Caller_Acc   : Boolean := False;
      Res_Decl          : Node_Id;

      Definite : constant Boolean :=
                   Caller_Known_Size (Func_Call, Result_Subt)
                     and then not Is_Class_Wide_Type (Obj_Typ);
      --  In the case of "X : T'Class := F(...);", where F returns a
      --  Caller_Known_Size (specific) tagged type, we treat it as
      --  indefinite, because the code for the Definite case below sets the
      --  initialization expression of the object to Empty, which would be
      --  illegal Ada, and would cause gigi to misallocate X.

      Is_OK_Return_Object : constant Boolean :=
        Is_Return_Object (Obj_Def_Id)
          and then
        not Has_Foreign_Convention (Return_Applies_To (Scope (Obj_Def_Id)));

   --  Start of processing for Make_Build_In_Place_Call_In_Object_Declaration

   begin
      --  If the call has already been processed to add build-in-place actuals
      --  then return.

      if Is_Expanded_Build_In_Place_Call (Func_Call) then
         return;
      end if;

      --  Mark the call as processed as a build-in-place call

      Set_Is_Expanded_Build_In_Place_Call (Func_Call);

      Warn_BIP (Func_Call);

      --  Create an access type designating the function's result subtype.
      --  We use the type of the original call because it may be a call to an
      --  inherited operation, which the expansion has replaced with the parent
      --  operation that yields the parent type. Note that this access type
      --  must be declared before we establish a transient scope, so that it
      --  receives the proper accessibility level.

      if Is_Class_Wide_Type (Obj_Typ)
        and then not Is_Interface (Obj_Typ)
        and then not Is_Class_Wide_Type (Etype (Function_Call))
      then
         Designated_Type := Obj_Typ;
      else
         Designated_Type := Etype (Function_Call);
      end if;

      Ptr_Typ := Make_Temporary (Loc, 'A');
      Ptr_Typ_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Ptr_Typ,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication =>
                New_Occurrence_Of (Designated_Type, Loc)));

      --  The access type and its accompanying object must be inserted after
      --  the object declaration in the constrained case, so that the function
      --  call can be passed access to the object. In the indefinite case, or
      --  if the object declaration is for a return object, the access type and
      --  object must be inserted before the object, since the object
      --  declaration is rewritten to be a renaming of a dereference of the
      --  access object. Note: we need to freeze Ptr_Typ explicitly, because
      --  the result object is in a different (transient) scope, so won't cause
      --  freezing.

      if Definite and then not Is_OK_Return_Object then

         --  The presence of an address clause complicates the build-in-place
         --  expansion because the indicated address must be processed before
         --  the indirect call is generated (including the definition of a
         --  local pointer to the object). The address clause may come from
         --  an aspect specification or from an explicit attribute
         --  specification appearing after the object declaration. These two
         --  cases require different processing.

         if Has_Aspect (Obj_Def_Id, Aspect_Address) then

            --  Skip non-delayed pragmas that correspond to other aspects, if
            --  any, to find proper insertion point for freeze node of object.

            declare
               D : Node_Id := Obj_Decl;
               N : Node_Id := Next (D);

            begin
               while Present (N)
                 and then Nkind (N) in N_Attribute_Reference | N_Pragma
               loop
                  Analyze (N);
                  D := N;
                  Next (N);
               end loop;

               Insert_After (D, Ptr_Typ_Decl);

               --  Freeze object before pointer declaration, to ensure that
               --  generated attribute for address is inserted at the proper
               --  place.

               Freeze_Before (Ptr_Typ_Decl, Obj_Def_Id);
            end;

            Analyze (Ptr_Typ_Decl);

         elsif Present (Following_Address_Clause (Obj_Decl)) then

            --  Locate explicit address clause, which may also follow pragmas
            --  generated by other aspect specifications.

            declare
               Addr : constant Node_Id := Following_Address_Clause (Obj_Decl);
               D    : Node_Id := Next (Obj_Decl);

            begin
               while Present (D) loop
                  Analyze (D);
                  exit when D = Addr;
                  Next (D);
               end loop;

               Insert_After_And_Analyze (Addr, Ptr_Typ_Decl);
            end;

         else
            Insert_After_And_Analyze (Obj_Decl, Ptr_Typ_Decl);
         end if;
      else
         Insert_Action (Obj_Decl, Ptr_Typ_Decl);
      end if;

      --  Force immediate freezing of Ptr_Typ because Res_Decl will be
      --  elaborated in an inner (transient) scope and thus won't cause
      --  freezing by itself. It's not an itype, but it needs to be frozen
      --  inside the current subprogram (see Freeze_Outside in freeze.adb).

      Freeze_Itype (Ptr_Typ, Ptr_Typ_Decl);

      --  If the object is a return object of an enclosing build-in-place
      --  function, then the implicit build-in-place parameters of the
      --  enclosing function are simply passed along to the called function.
      --  (Unfortunately, this won't cover the case of extension aggregates
      --  where the ancestor part is a build-in-place indefinite function
      --  call that should be passed along the caller's parameters.
      --  Currently those get mishandled by reassigning the result of the
      --  call to the aggregate return object, when the call result should
      --  really be directly built in place in the aggregate and not in a
      --  temporary. ???)

      if Is_OK_Return_Object then
         Pass_Caller_Acc := True;

         --  When the enclosing function has a BIP_Alloc_Form formal then we
         --  pass it along to the callee (such as when the enclosing function
         --  has an unconstrained or tagged result type).

         if Needs_BIP_Alloc_Form (Encl_Func) then
            if RTE_Available (RE_Root_Storage_Pool_Ptr) then
               Pool_Actual :=
                 New_Occurrence_Of
                   (Build_In_Place_Formal
                     (Encl_Func, BIP_Storage_Pool), Loc);

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
               Pool_Exp       => Pool_Actual);

         --  Otherwise, if enclosing function has a definite result subtype,
         --  then caller allocation will be used.

         else
            Add_Unconstrained_Actuals_To_Build_In_Place_Call
              (Func_Call, Function_Id, Alloc_Form => Caller_Allocation);
         end if;

         if Needs_BIP_Collection (Encl_Func) then
            Collection_Actual :=
              New_Occurrence_Of
                (Build_In_Place_Formal
                   (Encl_Func, BIP_Collection), Loc);
         end if;

         --  Retrieve the BIPacc formal from the enclosing function and convert
         --  it to the access type of the callee's BIP_Object_Access formal.

         Caller_Object :=
           Unchecked_Convert_To
             (Etype (Build_In_Place_Formal (Function_Id, BIP_Object_Access)),
              New_Occurrence_Of
                (Build_In_Place_Formal (Encl_Func, BIP_Object_Access), Loc));

      --  In the definite case, add an implicit actual to the function call
      --  that provides access to the declared object. An unchecked conversion
      --  to the (specific) result type of the function is inserted to handle
      --  the case where the object is declared with a class-wide type.

      elsif Definite then
         Caller_Object := Unchecked_Convert_To
           (Result_Subt, New_Occurrence_Of (Obj_Def_Id, Loc));

         --  When the function has a controlling result, an allocation-form
         --  parameter must be passed indicating that the caller is allocating
         --  the result object. This is needed because such a function can be
         --  called as a dispatching operation and must be treated similarly to
         --  functions with indefinite result subtypes.

         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Caller_Allocation);

      --  The allocation for indefinite library-level objects occurs on the
      --  heap as opposed to the secondary stack. This accommodates DLLs where
      --  the secondary stack is destroyed after each library unload. This is a
      --  hybrid mechanism where a stack-allocated object lives on the heap.

      elsif Is_Library_Level_Entity (Obj_Def_Id)
        and then not Restriction_Active (No_Implicit_Heap_Allocations)
      then
         --  Create a finalization collection for the access result type to
         --  ensure that the heap allocation can properly chain the object
         --  and later finalize it when the library unit goes out of scope.

         if Needs_BIP_Collection (Func_Call) then
            Build_Finalization_Collection
              (Typ            => Ptr_Typ,
               For_Lib_Level  => True,
               Insertion_Node => Ptr_Typ_Decl);

            Collection_Actual :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (Finalization_Collection (Ptr_Typ), Loc),
                Attribute_Name => Name_Unrestricted_Access);

            Pool_Actual :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (RTE (RE_Global_Pool_Object), Loc),
                Attribute_Name => Name_Unrestricted_Access);

         else
            Pool_Actual := Empty;
         end if;

         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call,
            Function_Id,
            Alloc_Form => Global_Heap,
            Pool_Exp   => Pool_Actual);
         Caller_Object := Empty;

      --  In other indefinite cases, pass an indication to do the allocation
      --  on the secondary stack and set Caller_Object to Empty so that a null
      --  value will be passed for the caller's object address. A transient
      --  scope is established to ensure eventual cleanup of the result.

      else
         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Secondary_Stack);
         Caller_Object := Empty;

         Establish_Transient_Scope (Obj_Decl, Manage_Sec_Stack => True);
      end if;

      --  Pass along any finalization collection actual, which is needed in
      --  the case where the called function initializes a return object of
      --  an enclosing build-in-place function.

      Add_Collection_Actual_To_Build_In_Place_Call
        (Func_Call, Function_Id, Collection_Exp => Collection_Actual);

      if Nkind (Parent (Obj_Decl)) = N_Extended_Return_Statement
        and then Needs_BIP_Task_Actuals (Function_Id)
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
        (Func_Call,
         Function_Id,
         Caller_Object,
         Is_Access => Pass_Caller_Acc);

      --  Finally, create an access object initialized to a reference to the
      --  function call. We know this access value cannot be null, so mark the
      --  entity accordingly to suppress the access check. We need to suppress
      --  warnings, because this can be part of the expansion of "for ... of"
      --  and similar constructs that generate finalization actions. Such
      --  finalization actions are safe, because they check a count that
      --  indicates which objects should be finalized, but the back end
      --  nonetheless warns about uninitialized objects.

      Def_Id := Make_Temporary (Loc, 'R', Func_Call);
      Set_Warnings_Off (Def_Id);
      Set_Etype (Def_Id, Ptr_Typ);
      Set_Is_Known_Non_Null (Def_Id);

      if Nkind (Function_Call) in N_Type_Conversion
                                | N_Unchecked_Type_Conversion
      then
         Res_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Ptr_Typ, Loc),
             Expression          =>
               Unchecked_Convert_To
                 (Ptr_Typ, Make_Reference (Loc, Relocate_Node (Func_Call))));
      else
         Res_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Ptr_Typ, Loc),
             Expression          =>
               Make_Reference (Loc, Relocate_Node (Func_Call)));
      end if;

      Insert_After_And_Analyze (Ptr_Typ_Decl, Res_Decl);

      --  If the result subtype of the called function is definite and is not
      --  itself the return expression of an enclosing BIP function, then mark
      --  the object as having no initialization.

      if Definite and then not Is_OK_Return_Object then

         --  The related object declaration is encased in a transient block
         --  because the build-in-place function call contains at least one
         --  nested function call that produces a controlled transient
         --  temporary:

         --    Obj : ... := BIP_Func_Call (Ctrl_Func_Call);

         --  Since the build-in-place expansion decouples the call from the
         --  object declaration, the finalization machinery lacks the context
         --  which prompted the generation of the transient block. To resolve
         --  this scenario, store the build-in-place call.

         if Scope_Is_Transient then
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
             Subtype_Mark        =>
               New_Occurrence_Of (Designated_Type, Obj_Loc),
             Name                => Call_Deref));

         --  At this point, Defining_Identifier (Obj_Decl) is no longer equal
         --  to Obj_Def_Id.

         pragma Assert (Ekind (Defining_Identifier (Obj_Decl)) = E_Void);
         Set_Renamed_Object_Of_Possibly_Void
           (Defining_Identifier (Obj_Decl), Call_Deref);

         --  If the original entity comes from source, then mark the new
         --  entity as needing debug information, even though it's defined
         --  by a generated renaming that does not come from source, so that
         --  the Materialize_Entity flag will be set on the entity when
         --  Debug_Renaming_Declaration is called during analysis.

         if Comes_From_Source (Obj_Def_Id) then
            Set_Debug_Info_Needed (Defining_Identifier (Obj_Decl));
         end if;

         Analyze (Obj_Decl);
         Replace_Renaming_Declaration_Id
           (Obj_Decl, Original_Node (Obj_Decl));
      end if;

      pragma Assert (Check_Number_Of_Actuals (Func_Call, Function_Id));
      pragma Assert (Check_BIP_Actuals (Func_Call, Function_Id));
   end Make_Build_In_Place_Call_In_Object_Declaration;

   -------------------------------------------------
   -- Make_Build_In_Place_Iface_Call_In_Allocator --
   -------------------------------------------------

   procedure Make_Build_In_Place_Iface_Call_In_Allocator
     (Allocator     : Node_Id;
      Function_Call : Node_Id)
   is
      BIP_Func_Call : constant Node_Id :=
                        Unqual_BIP_Iface_Function_Call (Function_Call);
      Loc           : constant Source_Ptr := Sloc (Function_Call);

      Anon_Type : Entity_Id;
      Tmp_Decl  : Node_Id;
      Tmp_Id    : Entity_Id;

   begin
      --  No action if the call has already been processed

      if Is_Expanded_Build_In_Place_Call (BIP_Func_Call) then
         return;
      end if;

      Tmp_Id := Make_Temporary (Loc, 'D');

      --  Insert a temporary before N initialized with the BIP function call
      --  without its enclosing type conversions and analyze it without its
      --  expansion. This temporary facilitates us reusing the BIP machinery,
      --  which takes care of adding the extra build-in-place actuals and
      --  transforms this object declaration into an object renaming
      --  declaration.

      Anon_Type := Create_Itype (E_Anonymous_Access_Type, Function_Call);
      Set_Directly_Designated_Type (Anon_Type, Etype (BIP_Func_Call));
      Set_Etype (Anon_Type, Anon_Type);
      Build_Class_Wide_Master (Anon_Type);

      Tmp_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Tmp_Id,
          Object_Definition   => New_Occurrence_Of (Anon_Type, Loc),
          Expression          =>
            Make_Allocator (Loc,
              Expression =>
                Make_Qualified_Expression (Loc,
                  Subtype_Mark =>
                    New_Occurrence_Of (Etype (BIP_Func_Call), Loc),
                  Expression   => New_Copy_Tree (BIP_Func_Call))));

      --  Manually set the associated node for the anonymous access type to
      --  be its local declaration, to avoid confusing and complicating
      --  the accessibility machinery.

      Set_Associated_Node_For_Itype (Anon_Type, Tmp_Decl);

      Expander_Mode_Save_And_Set (False);
      Insert_Action (Allocator, Tmp_Decl);
      Expander_Mode_Restore;

      Make_Build_In_Place_Call_In_Allocator
        (Allocator     => Expression (Tmp_Decl),
         Function_Call => Expression (Expression (Tmp_Decl)));

      --  Add a conversion to displace the pointer to the allocated object
      --  to reference the corresponding dispatch table.

      Rewrite (Allocator,
        Convert_To (Etype (Allocator),
          New_Occurrence_Of (Tmp_Id, Loc)));
   end Make_Build_In_Place_Iface_Call_In_Allocator;

   ---------------------------------------------------------
   -- Make_Build_In_Place_Iface_Call_In_Anonymous_Context --
   ---------------------------------------------------------

   procedure Make_Build_In_Place_Iface_Call_In_Anonymous_Context
     (Function_Call : Node_Id)
   is
      BIP_Func_Call : constant Node_Id :=
                        Unqual_BIP_Iface_Function_Call (Function_Call);
      Loc           : constant Source_Ptr := Sloc (Function_Call);

      Tmp_Decl : Node_Id;
      Tmp_Id   : Entity_Id;

   begin
      --  No action of the call has already been processed

      if Is_Expanded_Build_In_Place_Call (BIP_Func_Call) then
         return;
      end if;

      pragma Assert (Needs_Finalization (Etype (BIP_Func_Call)));

      --  Insert a temporary before the call initialized with function call to
      --  reuse the BIP machinery which takes care of adding the extra build-in
      --  place actuals and transforms this object declaration into an object
      --  renaming declaration.

      Tmp_Id := Make_Temporary (Loc, 'D');

      Tmp_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Tmp_Id,
          Object_Definition   =>
            New_Occurrence_Of (Etype (Function_Call), Loc),
          Expression          => Relocate_Node (Function_Call));

      Expander_Mode_Save_And_Set (False);
      Insert_Action (Function_Call, Tmp_Decl);
      Expander_Mode_Restore;

      Make_Build_In_Place_Iface_Call_In_Object_Declaration
        (Obj_Decl      => Tmp_Decl,
         Function_Call => Expression (Tmp_Decl));
   end Make_Build_In_Place_Iface_Call_In_Anonymous_Context;

   ----------------------------------------------------------
   -- Make_Build_In_Place_Iface_Call_In_Object_Declaration --
   ----------------------------------------------------------

   procedure Make_Build_In_Place_Iface_Call_In_Object_Declaration
     (Obj_Decl      : Node_Id;
      Function_Call : Node_Id)
   is
      BIP_Func_Call : constant Node_Id :=
                        Unqual_BIP_Iface_Function_Call (Function_Call);
      Loc           : constant Source_Ptr := Sloc (Function_Call);
      Obj_Id        : constant Entity_Id := Defining_Entity (Obj_Decl);

      Tmp_Decl : Node_Id;
      Tmp_Id   : Entity_Id;

   begin
      --  No action of the call has already been processed

      if Is_Expanded_Build_In_Place_Call (BIP_Func_Call) then
         return;
      end if;

      Tmp_Id := Make_Temporary (Loc, 'D');

      --  Insert a temporary before N initialized with the BIP function call
      --  without its enclosing type conversions and analyze it without its
      --  expansion. This temporary facilitates us reusing the BIP machinery,
      --  which takes care of adding the extra build-in-place actuals and
      --  transforms this object declaration into an object renaming
      --  declaration.

      Tmp_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Tmp_Id,
          Object_Definition   =>
            New_Occurrence_Of (Etype (BIP_Func_Call), Loc),
          Expression          => New_Copy_Tree (BIP_Func_Call));

      Expander_Mode_Save_And_Set (False);
      Insert_Action (Obj_Decl, Tmp_Decl);
      Expander_Mode_Restore;

      --  Inherit Is_Return_Object from the parent object to the temp object,
      --  so that Make_In_Build_Place_Call_In_Object_Declaration will handle
      --  the temp properly in cases where there's a BIP_Alloc_Form formal of
      --  an enclosing function that should be passed along (and which also
      --  ensures that if the BIP call is used as a function result and it
      --  requires finalization, then it will not be finalized prematurely
      --  or redundantly).

      Set_Is_Return_Object (Tmp_Id, Is_Return_Object (Obj_Id));

      Make_Build_In_Place_Call_In_Object_Declaration
        (Obj_Decl      => Tmp_Decl,
         Function_Call => Expression (Tmp_Decl));

      pragma Assert (Nkind (Tmp_Decl) = N_Object_Renaming_Declaration);

      --  Replace the original build-in-place function call by a reference to
      --  the resulting temporary object renaming declaration. In this way,
      --  all the interface conversions performed in the original Function_Call
      --  on the build-in-place object are preserved.

      Rewrite (BIP_Func_Call, New_Occurrence_Of (Tmp_Id, Loc));

      --  Replace the original object declaration by an internal object
      --  renaming declaration. This leaves the generated code more clean (the
      --  build-in-place function call in an object renaming declaration and
      --  displacements of the pointer to the build-in-place object in another
      --  renaming declaration) and allows us to invoke the routine that takes
      --  care of replacing the identifier of the renaming declaration (routine
      --  originally developed for the regular build-in-place management).

      Rewrite (Obj_Decl,
        Make_Object_Renaming_Declaration (Loc,
          Defining_Identifier => Make_Temporary (Loc, 'D'),
          Subtype_Mark        => New_Occurrence_Of (Etype (Obj_Id), Loc),
          Name                => Function_Call));
      Analyze (Obj_Decl);

      Replace_Renaming_Declaration_Id (Obj_Decl, Original_Node (Obj_Decl));
   end Make_Build_In_Place_Iface_Call_In_Object_Declaration;

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
        Build_Initialization_Call (Allocator,
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

   ----------------------
   -- Might_Have_Tasks --
   ----------------------

   function Might_Have_Tasks (Typ : Entity_Id) return Boolean is
   begin
      return not Global_No_Tasking
        and then not No_Run_Time_Mode
        and then (Has_Task (Typ)
                    or else (Is_Class_Wide_Type (Typ)
                               and then Is_Limited_Record (Typ)
                               and then not Has_Aspect
                                 (Etype (Typ), Aspect_No_Task_Parts)));
   end Might_Have_Tasks;

   ----------------------------
   -- Needs_BIP_Task_Actuals --
   ----------------------------

   function Needs_BIP_Task_Actuals (Func_Id : Entity_Id) return Boolean is
      Subp_Id  : Entity_Id;
      Func_Typ : Entity_Id;

   begin
      if Global_No_Tasking or else No_Run_Time_Mode then
         return False;
      end if;

      --  For thunks we must rely on their target entity; otherwise, given that
      --  the profile of thunks for functions returning a limited interface
      --  type returns a class-wide type, we would erroneously add these extra
      --  formals.

      if Is_Thunk (Func_Id) then
         Subp_Id := Thunk_Target (Func_Id);

      --  Common case

      else
         Subp_Id := Func_Id;
      end if;

      Func_Typ := Underlying_Type (Etype (Subp_Id));

      --  Functions returning types with foreign convention don't have extra
      --  formals.

      if Has_Foreign_Convention (Func_Typ) then
         return False;

      --  At first sight, for all the following cases, we could add assertions
      --  to ensure that if Func_Id is frozen then the computed result matches
      --  with the availability of the task master extra formal; unfortunately
      --  this is not feasible because we may be precisely freezing this entity
      --  (that is, Is_Frozen has been set by Freeze_Entity but it has not
      --  completed its work).

      elsif Has_Task (Func_Typ) then
         return True;

      elsif Ekind (Func_Id) = E_Function then
         return Might_Have_Tasks (Func_Typ);

      --  Handle subprogram type internally generated for dispatching call. We
      --  cannot rely on the return type of the subprogram type of dispatching
      --  calls since it is always a class-wide type (cf. Expand_Dispatching_
      --  Call).

      elsif Ekind (Func_Id) = E_Subprogram_Type then
         if Is_Dispatch_Table_Entity (Func_Id) then
            return Has_BIP_Extra_Formal (Func_Id, BIP_Task_Master);
         else
            return Might_Have_Tasks (Func_Typ);
         end if;

      else
         raise Program_Error;
      end if;
   end Needs_BIP_Task_Actuals;

   --------------------------
   -- Needs_BIP_Collection --
   --------------------------

   function Needs_BIP_Collection (Func_Id : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Underlying_Type (Etype (Func_Id));

   begin
      --  A formal for the finalization collection is needed for build-in-place
      --  functions whose result type needs finalization or is a tagged type.
      --  Tagged primitive build-in-place functions need such a formal because
      --  they can be called by a dispatching call, and extensions may require
      --  finalization even if the root type doesn't. This means nonprimitive
      --  build-in-place functions with tagged results also need it, since such
      --  functions can be called via access-to-function types, and those can
      --  be used to call primitives, so the formal needs to be passed to all
      --  such build-in-place functions, primitive or not.

      return not Restriction_Active (No_Finalization)
        and then ((Needs_Finalization (Typ)
                    and then not Has_Relaxed_Finalization (Typ))
                  or else Is_Tagged_Type (Typ))
        and then not Has_Foreign_Convention (Typ);
   end Needs_BIP_Collection;

   --------------------------
   -- Needs_BIP_Alloc_Form --
   --------------------------

   function Needs_BIP_Alloc_Form (Func_Id : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Underlying_Type (Etype (Func_Id));

   begin
      --  See Make_Build_In_Place_Call_In_Allocator for the rationale

      if Needs_BIP_Collection (Func_Id) then
         return True;
      end if;

      --  A formal giving the allocation method is needed for build-in-place
      --  functions whose result type is returned on the secondary stack or
      --  is a tagged type. Tagged primitive build-in-place functions need
      --  such a formal because they can be called by a dispatching call, and
      --  the secondary stack is always used for dispatching-on-result calls.
      --  This means nonprimitive build-in-place functions with tagged results
      --  also need it, as such functions can be called via access-to-function
      --  types, and those can be used to call primitives, so the formal needs
      --  to be passed to all such build-in-place functions, primitive or not.

      --  We never use build-in-place if the function has foreign convention,
      --  but note that it is OK for a build-in-place function to return a
      --  type with a foreign convention because the machinery ensures there
      --  is no copying.

      return not Restriction_Active (No_Secondary_Stack)
        and then (Needs_Secondary_Stack (Typ) or else Is_Tagged_Type (Typ))
        and then not Has_Foreign_Convention (Func_Id);
   end Needs_BIP_Alloc_Form;

   -------------------------------------
   -- Replace_Renaming_Declaration_Id --
   -------------------------------------

   procedure Replace_Renaming_Declaration_Id
      (New_Decl  : Node_Id;
       Orig_Decl : Node_Id)
   is
      New_Id  : constant Entity_Id := Defining_Entity (New_Decl);
      Orig_Id : constant Entity_Id := Defining_Entity (Orig_Decl);

   begin
      Set_Chars (New_Id, Chars (Orig_Id));

      --  Swap next entity links in preparation for exchanging entities

      declare
         Next_Id : constant Entity_Id := Next_Entity (New_Id);
      begin
         Link_Entities (New_Id, Next_Entity (Orig_Id));
         Link_Entities (Orig_Id, Next_Id);
      end;

      Set_Homonym (New_Id, Homonym (Orig_Id));
      Exchange_Entities (New_Id, Orig_Id);

      --  Preserve source indication of original declaration, so that xref
      --  information is properly generated for the right entity.

      Preserve_Comes_From_Source (New_Decl, Orig_Decl);
      Preserve_Comes_From_Source (Orig_Id, Orig_Decl);

      Set_Comes_From_Source (New_Id, False);

      --  Preserve aliased indication

      Set_Is_Aliased (Orig_Id, Is_Aliased (New_Id));
   end Replace_Renaming_Declaration_Id;

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

         if Nkind (P) in N_Block_Statement | N_Loop_Statement then
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

   ------------------------------------
   -- Unqual_BIP_Iface_Function_Call --
   ------------------------------------

   function Unqual_BIP_Iface_Function_Call (Expr : Node_Id) return Node_Id is
      Has_Pointer_Displacement : Boolean := False;
      On_Object_Declaration    : Boolean := False;
      --  Remember if processing the renaming expressions on recursion we have
      --  traversed an object declaration, since we can traverse many object
      --  declaration renamings but just one regular object declaration.

      function Unqual_BIP_Function_Call (Expr : Node_Id) return Node_Id;
      --  Search for a build-in-place function call skipping any qualification
      --  including qualified expressions, type conversions, references, calls
      --  to displace the pointer to the object, and renamings. Return Empty if
      --  no build-in-place function call is found.

      ------------------------------
      -- Unqual_BIP_Function_Call --
      ------------------------------

      function Unqual_BIP_Function_Call (Expr : Node_Id) return Node_Id is
      begin
         --  Recurse to handle case of multiple levels of qualification and/or
         --  conversion.

         if Nkind (Expr) in N_Qualified_Expression
                          | N_Type_Conversion
                          | N_Unchecked_Type_Conversion
         then
            return Unqual_BIP_Function_Call (Expression (Expr));

         --  Recurse to handle case of multiple levels of references and
         --  explicit dereferences.

         elsif Nkind (Expr) in N_Attribute_Reference
                             | N_Explicit_Dereference
                             | N_Reference
         then
            return Unqual_BIP_Function_Call (Prefix (Expr));

         --  Recurse on object renamings

         elsif Nkind (Expr) = N_Identifier
           and then Present (Entity (Expr))
           and then Ekind (Entity (Expr)) in E_Constant | E_Variable
           and then Nkind (Parent (Entity (Expr))) =
                      N_Object_Renaming_Declaration
           and then Present (Renamed_Object (Entity (Expr)))
         then
            return Unqual_BIP_Function_Call (Renamed_Object (Entity (Expr)));

         --  Recurse on the initializing expression of the first reference of
         --  an object declaration.

         elsif not On_Object_Declaration
           and then Nkind (Expr) = N_Identifier
           and then Present (Entity (Expr))
           and then Ekind (Entity (Expr)) in E_Constant | E_Variable
           and then Nkind (Parent (Entity (Expr))) = N_Object_Declaration
           and then Present (Expression (Parent (Entity (Expr))))
         then
            On_Object_Declaration := True;
            return
              Unqual_BIP_Function_Call (Expression (Parent (Entity (Expr))));

         --  Recurse to handle calls to displace the pointer to the object to
         --  reference a secondary dispatch table.

         elsif Nkind (Expr) = N_Function_Call
           and then Nkind (Name (Expr)) in N_Has_Entity
           and then Present (Entity (Name (Expr)))
           and then Is_RTE (Entity (Name (Expr)), RE_Displace)
         then
            Has_Pointer_Displacement := True;
            return
              Unqual_BIP_Function_Call (First (Parameter_Associations (Expr)));

         --  Normal case: check if the inner expression is a BIP function call
         --  and the pointer to the object is displaced.

         elsif Has_Pointer_Displacement
           and then Is_Build_In_Place_Function_Call (Expr)
         then
            return Expr;

         else
            return Empty;
         end if;
      end Unqual_BIP_Function_Call;

   --  Start of processing for Unqual_BIP_Iface_Function_Call

   begin
      if Nkind (Expr) = N_Identifier and then No (Entity (Expr)) then

         --  Can happen for X'Elab_Spec in the binder-generated file

         return Empty;
      end if;

      return Unqual_BIP_Function_Call (Expr);
   end Unqual_BIP_Iface_Function_Call;

   -------------------------------
   -- Validate_Subprogram_Calls --
   -------------------------------

   procedure Validate_Subprogram_Calls (N : Node_Id) is

      function Process_Node (Nod : Node_Id) return Traverse_Result;
      --  Function to traverse the subtree of N using Traverse_Proc.

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (Nod : Node_Id) return Traverse_Result is
      begin
         case Nkind (Nod) is
            when N_Entry_Call_Statement
               | N_Procedure_Call_Statement
               | N_Function_Call
              =>
               declare
                  Call_Node : Node_Id renames Nod;
                  Subp      : Entity_Id;

               begin
                  --  Call using access to subprogram with explicit dereference

                  if Nkind (Name (Call_Node)) = N_Explicit_Dereference then
                     Subp := Etype (Name (Call_Node));

                  --  Prefix notation calls

                  elsif Nkind (Name (Call_Node)) = N_Selected_Component then
                     Subp := Entity (Selector_Name (Name (Call_Node)));

                  --  Call to member of entry family, where Name is an indexed
                  --  component, with the prefix being a selected component
                  --  giving the task and entry family name, and the index
                  --  being the entry index.

                  elsif Nkind (Name (Call_Node)) = N_Indexed_Component then
                     Subp :=
                       Entity (Selector_Name (Prefix (Name (Call_Node))));

                  --  Normal case

                  else
                     Subp := Entity (Name (Call_Node));
                  end if;

                  pragma Assert (Check_BIP_Actuals (Call_Node, Subp));
               end;

            --  Skip generic bodies

            when N_Package_Body =>
               if Ekind (Unique_Defining_Entity (Nod)) = E_Generic_Package then
                  return Skip;
               end if;

            when N_Subprogram_Body =>
               if Ekind (Unique_Defining_Entity (Nod)) in E_Generic_Function
                                                        | E_Generic_Procedure
               then
                  return Skip;
               end if;

            --  Nodes we want to ignore

            --  Skip calls placed in the full declaration of record types since
            --  the call will be performed by their Init Proc; for example,
            --  calls initializing default values of discriminants or calls
            --  providing the initial value of record type components. Other
            --  full type declarations are processed because they may have
            --  calls that must be checked. For example:

            --    type T is array (1 .. Some_Function_Call (...)) of Some_Type;

            --  ??? More work needed here to handle the following case:

            --    type Rec is record
            --       F : String (1 .. <some complicated expression>);
            --    end record;

            when N_Full_Type_Declaration =>
               if Is_Record_Type (Defining_Entity (Nod)) then
                  return Skip;
               end if;

            --  Skip calls placed in subprogram specifications since function
            --  calls initializing default parameter values will be processed
            --  when the call to the subprogram is found (if the default actual
            --  parameter is required), and calls found in aspects will be
            --  processed when their corresponding pragma is found, or in the
            --  specific case of class-wide pre-/postconditions, when their
            --  helpers are found.

            when N_Procedure_Specification
               | N_Function_Specification
              =>
               return Skip;

            when N_Abstract_Subprogram_Declaration
               | N_Aspect_Specification
               | N_At_Clause
               | N_Call_Marker
               | N_Empty
               | N_Enumeration_Representation_Clause
               | N_Enumeration_Type_Definition
               | N_Function_Instantiation
               | N_Freeze_Generic_Entity
               | N_Generic_Function_Renaming_Declaration
               | N_Generic_Package_Renaming_Declaration
               | N_Generic_Procedure_Renaming_Declaration
               | N_Generic_Package_Declaration
               | N_Generic_Subprogram_Declaration
               | N_Itype_Reference
               | N_Number_Declaration
               | N_Package_Instantiation
               | N_Package_Renaming_Declaration
               | N_Pragma
               | N_Procedure_Instantiation
               | N_Protected_Type_Declaration
               | N_Record_Representation_Clause
               | N_Validate_Unchecked_Conversion
               | N_Variable_Reference_Marker
               | N_Use_Package_Clause
               | N_Use_Type_Clause
               | N_With_Clause
              =>
               return Skip;

            when others =>
               null;
         end case;

         return OK;
      end Process_Node;

      procedure Check_Calls is new Traverse_Proc (Process_Node);

   --  Start of processing for Validate_Subprogram_Calls

   begin
      --  No action required if we are not generating code or compiling sources
      --  that have errors.

      if Serious_Errors_Detected > 0
        or else Operating_Mode /= Generate_Code
      then
         return;
      end if;

      Check_Calls (N);
   end Validate_Subprogram_Calls;

   --------------
   -- Warn_BIP --
   --------------

   procedure Warn_BIP (Func_Call : Node_Id) is
   begin
      if Debug_Flag_Underscore_BB then
         Error_Msg_N ("build-in-place function call??", Func_Call);
      end if;
   end Warn_BIP;

end Exp_Ch6;
