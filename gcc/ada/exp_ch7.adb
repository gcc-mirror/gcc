------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 7                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This package contains virtually all expansion mechanisms related to
--    - controlled types
--    - transient scopes

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Ch11; use Exp_Ch11;
with Exp_Dbug; use Exp_Dbug;
with Exp_Dist; use Exp_Dist;
with Exp_Disp; use Exp_Disp;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sinfo;    use Sinfo;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_SCIL; use Sem_SCIL;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch7 is

   --------------------------------
   -- Transient Scope Management --
   --------------------------------

   --  A transient scope is created when temporary objects are created by the
   --  compiler. These temporary objects are allocated on the secondary stack
   --  and the transient scope is responsible for finalizing the object when
   --  appropriate and reclaiming the memory at the right time. The temporary
   --  objects are generally the objects allocated to store the result of a
   --  function returning an unconstrained or a tagged value. Expressions
   --  needing to be wrapped in a transient scope (functions calls returning
   --  unconstrained or tagged values) may appear in 3 different contexts which
   --  lead to 3 different kinds of transient scope expansion:

   --   1. In a simple statement (procedure call, assignment, ...). In
   --      this case the instruction is wrapped into a transient block.
   --      (See Wrap_Transient_Statement for details)

   --   2. In an expression of a control structure (test in a IF statement,
   --      expression in a CASE statement, ...).
   --      (See Wrap_Transient_Expression for details)

   --   3. In a expression of an object_declaration. No wrapping is possible
   --      here, so the finalization actions, if any, are done right after the
   --      declaration and the secondary stack deallocation is done in the
   --      proper enclosing scope (see Wrap_Transient_Declaration for details)

   --  Note about functions returning tagged types: it has been decided to
   --  always allocate their result in the secondary stack, even though is not
   --  absolutely mandatory when the tagged type is constrained because the
   --  caller knows the size of the returned object and thus could allocate the
   --  result in the primary stack. An exception to this is when the function
   --  builds its result in place, as is done for functions with inherently
   --  limited result types for Ada 2005. In that case, certain callers may
   --  pass the address of a constrained object as the target object for the
   --  function result.

   --  By allocating tagged results in the secondary stack a number of
   --  implementation difficulties are avoided:

   --    - If it is a dispatching function call, the computation of the size of
   --      the result is possible but complex from the outside.

   --    - If the returned type is controlled, the assignment of the returned
   --      value to the anonymous object involves an Adjust, and we have no
   --      easy way to access the anonymous object created by the back end.

   --    - If the returned type is class-wide, this is an unconstrained type
   --      anyway.

   --  Furthermore, the small loss in efficiency which is the result of this
   --  decision is not such a big deal because functions returning tagged types
   --  are not as common in practice compared to functions returning access to
   --  a tagged type.

   --------------------------------------------------
   -- Transient Blocks and Finalization Management --
   --------------------------------------------------

   function Find_Node_To_Be_Wrapped (N : Node_Id) return Node_Id;
   --  N is a node which may generate a transient scope. Loop over the parent
   --  pointers of N until it find the appropriate node to wrap. If it returns
   --  Empty, it means that no transient scope is needed in this context.

   function Make_Clean
     (N                          : Node_Id;
      Clean                      : Entity_Id;
      Mark                       : Entity_Id;
      Flist                      : Entity_Id;
      Is_Task                    : Boolean;
      Is_Master                  : Boolean;
      Is_Protected_Subprogram    : Boolean;
      Is_Task_Allocation_Block   : Boolean;
      Is_Asynchronous_Call_Block : Boolean;
      Chained_Cleanup_Action     : Node_Id) return Node_Id;
   --  Expand the clean-up procedure for a controlled and/or transient block,
   --  and/or task master or task body, or a block used to  implement task
   --  allocation or asynchronous entry calls, or a procedure used to implement
   --  protected procedures. Clean is the entity for such a procedure. Mark
   --  is the entity for the secondary stack mark, if empty only controlled
   --  block clean-up will be performed. Flist is the entity for the local
   --  final list, if empty only transient scope clean-up will be performed.
   --  The flags Is_Task and Is_Master control the calls to the corresponding
   --  finalization actions for a task body or for an entity that is a task
   --  master. Finally if Chained_Cleanup_Action is present, it is a reference
   --  to a previous cleanup procedure, a call to which is appended at the
   --  end of the generated one.

   procedure Set_Node_To_Be_Wrapped (N : Node_Id);
   --  Set the field Node_To_Be_Wrapped of the current scope

   procedure Insert_Actions_In_Scope_Around (N : Node_Id);
   --  Insert the before-actions kept in the scope stack before N, and the
   --  after-actions after N, which must be a member of a list.

   function Make_Transient_Block
     (Loc    : Source_Ptr;
      Action : Node_Id) return Node_Id;
   --  Create a transient block whose name is Scope, which is also a controlled
   --  block if Flist is not empty and whose only code is Action (either a
   --  single statement or single declaration).

   type Final_Primitives is (Initialize_Case, Adjust_Case, Finalize_Case);
   --  This enumeration type is defined in order to ease sharing code for
   --  building finalization procedures for composite types.

   Name_Of      : constant array (Final_Primitives) of Name_Id :=
                    (Initialize_Case => Name_Initialize,
                     Adjust_Case     => Name_Adjust,
                     Finalize_Case   => Name_Finalize);

   Deep_Name_Of : constant array (Final_Primitives) of TSS_Name_Type :=
                    (Initialize_Case => TSS_Deep_Initialize,
                     Adjust_Case     => TSS_Deep_Adjust,
                     Finalize_Case   => TSS_Deep_Finalize);

   procedure Build_Record_Deep_Procs (Typ : Entity_Id);
   --  Build the deep Initialize/Adjust/Finalize for a record Typ with
   --  Has_Component_Component set and store them using the TSS mechanism.

   procedure Build_Array_Deep_Procs (Typ : Entity_Id);
   --  Build the deep Initialize/Adjust/Finalize for a record Typ with
   --  Has_Controlled_Component set and store them using the TSS mechanism.

   function Make_Deep_Proc
     (Prim  : Final_Primitives;
      Typ   : Entity_Id;
      Stmts : List_Id) return Node_Id;
   --  This function generates the tree for Deep_Initialize, Deep_Adjust or
   --  Deep_Finalize procedures according to the first parameter, these
   --  procedures operate on the type Typ. The Stmts parameter gives the body
   --  of the procedure.

   function Make_Deep_Array_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id) return List_Id;
   --  This function generates the list of statements for implementing
   --  Deep_Initialize, Deep_Adjust or Deep_Finalize procedures according to
   --  the first parameter, these procedures operate on the array type Typ.

   function Make_Deep_Record_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id) return List_Id;
   --  This function generates the list of statements for implementing
   --  Deep_Initialize, Deep_Adjust or Deep_Finalize procedures according to
   --  the first parameter, these procedures operate on the record type Typ.

   procedure Check_Visibly_Controlled
     (Prim : Final_Primitives;
      Typ  : Entity_Id;
      E    : in out Entity_Id;
      Cref : in out Node_Id);
   --  The controlled operation declared for a derived type may not be
   --  overriding, if the controlled operations of the parent type are
   --  hidden, for example when the parent is a private type whose full
   --  view is controlled. For other primitive operations we modify the
   --  name of the operation to indicate that it is not overriding, but
   --  this is not possible for Initialize, etc. because they have to be
   --  retrievable by name. Before generating the proper call to one of
   --  these operations we check whether Typ is known to be controlled at
   --  the point of definition. If it is not then we must retrieve the
   --  hidden operation of the parent and use it instead.  This is one
   --  case that might be solved more cleanly once Overriding pragmas or
   --  declarations are in place.

   function Convert_View
     (Proc : Entity_Id;
      Arg  : Node_Id;
      Ind  : Pos := 1) return Node_Id;
   --  Proc is one of the Initialize/Adjust/Finalize operations, and
   --  Arg is the argument being passed to it. Ind indicates which
   --  formal of procedure Proc we are trying to match. This function
   --  will, if necessary, generate an conversion between the partial
   --  and full view of Arg to match the type of the formal of Proc,
   --  or force a conversion to the class-wide type in the case where
   --  the operation is abstract.

   -----------------------------
   -- Finalization Management --
   -----------------------------

   --  This part describe how Initialization/Adjustment/Finalization procedures
   --  are generated and called. Two cases must be considered, types that are
   --  Controlled (Is_Controlled flag set) and composite types that contain
   --  controlled components (Has_Controlled_Component flag set). In the first
   --  case the procedures to call are the user-defined primitive operations
   --  Initialize/Adjust/Finalize. In the second case, GNAT generates
   --  Deep_Initialize, Deep_Adjust and Deep_Finalize that are in charge
   --  of calling the former procedures on the controlled components.

   --  For records with Has_Controlled_Component set, a hidden "controller"
   --  component is inserted. This controller component contains its own
   --  finalization list on which all controlled components are attached
   --  creating an indirection on the upper-level Finalization list. This
   --  technique facilitates the management of objects whose number of
   --  controlled components changes during execution. This controller
   --  component is itself controlled and is attached to the upper-level
   --  finalization chain. Its adjust primitive is in charge of calling adjust
   --  on the components and adjusting the finalization pointer to match their
   --  new location (see a-finali.adb).

   --  It is not possible to use a similar technique for arrays that have
   --  Has_Controlled_Component set. In this case, deep procedures are
   --  generated that call initialize/adjust/finalize + attachment or
   --  detachment on the finalization list for all component.

   --  Initialize calls: they are generated for declarations or dynamic
   --  allocations of Controlled objects with no initial value. They are always
   --  followed by an attachment to the current Finalization Chain. For the
   --  dynamic allocation case this the chain attached to the scope of the
   --  access type definition otherwise, this is the chain of the current
   --  scope.

   --  Adjust Calls: They are generated on 2 occasions: (1) for
   --  declarations or dynamic allocations of Controlled objects with an
   --  initial value. (2) after an assignment. In the first case they are
   --  followed by an attachment to the final chain, in the second case
   --  they are not.

   --  Finalization Calls: They are generated on (1) scope exit, (2)
   --  assignments, (3) unchecked deallocations. In case (3) they have to
   --  be detached from the final chain, in case (2) they must not and in
   --  case (1) this is not important since we are exiting the scope anyway.

   --  Other details:

   --    Type extensions will have a new record controller at each derivation
   --    level containing controlled components. The record controller for
   --    the parent/ancestor is attached to the finalization list of the
   --    extension's record controller (i.e. the parent is like a component
   --    of the extension).

   --    For types that are both Is_Controlled and Has_Controlled_Components,
   --    the record controller and the object itself are handled separately.
   --    It could seem simpler to attach the object at the end of its record
   --    controller but this would not tackle view conversions properly.

   --    A classwide type can always potentially have controlled components
   --    but the record controller of the corresponding actual type may not
   --    be known at compile time so the dispatch table contains a special
   --    field that allows to compute the offset of the record controller
   --    dynamically. See s-finimp.Deep_Tag_Attach and a-tags.RC_Offset.

   --  Here is a simple example of the expansion of a controlled block :

   --    declare
   --       X : Controlled;
   --       Y : Controlled := Init;
   --
   --       type R is record
   --          C : Controlled;
   --       end record;
   --       W : R;
   --       Z : R := (C => X);
   --    begin
   --       X := Y;
   --       W := Z;
   --    end;
   --
   --  is expanded into
   --
   --    declare
   --       _L : System.FI.Finalizable_Ptr;

   --       procedure _Clean is
   --       begin
   --          Abort_Defer;
   --          System.FI.Finalize_List (_L);
   --          Abort_Undefer;
   --       end _Clean;

   --       X : Controlled;
   --       begin
   --          Abort_Defer;
   --          Initialize (X);
   --          Attach_To_Final_List (_L, Finalizable (X), 1);
   --       at end: Abort_Undefer;
   --       Y : Controlled := Init;
   --       Adjust (Y);
   --       Attach_To_Final_List (_L, Finalizable (Y), 1);
   --
   --       type R is record
   --         _C : Record_Controller;
   --          C : Controlled;
   --       end record;
   --       W : R;
   --       begin
   --          Abort_Defer;
   --          Deep_Initialize (W, _L, 1);
   --       at end: Abort_Under;
   --       Z : R := (C => X);
   --       Deep_Adjust (Z, _L, 1);

   --    begin
   --       _Assign (X, Y);
   --       Deep_Finalize (W, False);
   --       <save W's final pointers>
   --       W := Z;
   --       <restore W's final pointers>
   --       Deep_Adjust (W, _L, 0);
   --    at end
   --       _Clean;
   --    end;

   function Global_Flist_Ref (Flist_Ref : Node_Id) return Boolean;
   --  Return True if Flist_Ref refers to a global final list, either the
   --  object Global_Final_List which is used to attach standalone objects,
   --  or any of the list controllers associated with library-level access
   --  to controlled objects.

   procedure Clean_Simple_Protected_Objects (N : Node_Id);
   --  Protected objects without entries are not controlled types, and the
   --  locks have to be released explicitly when such an object goes out
   --  of scope. Traverse declarations in scope to determine whether such
   --  objects are present.

   ----------------------------
   -- Build_Array_Deep_Procs --
   ----------------------------

   procedure Build_Array_Deep_Procs (Typ : Entity_Id) is
   begin
      Set_TSS (Typ,
        Make_Deep_Proc (
          Prim  => Initialize_Case,
          Typ   => Typ,
          Stmts => Make_Deep_Array_Body (Initialize_Case, Typ)));

      if not Is_Inherently_Limited_Type (Typ) then
         Set_TSS (Typ,
           Make_Deep_Proc (
             Prim  => Adjust_Case,
             Typ   => Typ,
             Stmts => Make_Deep_Array_Body (Adjust_Case, Typ)));
      end if;

      Set_TSS (Typ,
        Make_Deep_Proc (
          Prim  => Finalize_Case,
          Typ   => Typ,
          Stmts => Make_Deep_Array_Body (Finalize_Case, Typ)));
   end Build_Array_Deep_Procs;

   -----------------------------
   -- Build_Controlling_Procs --
   -----------------------------

   procedure Build_Controlling_Procs (Typ : Entity_Id) is
   begin
      if Is_Array_Type (Typ) then
         Build_Array_Deep_Procs (Typ);

      else pragma Assert (Is_Record_Type (Typ));
         Build_Record_Deep_Procs (Typ);
      end if;
   end Build_Controlling_Procs;

   ----------------------
   -- Build_Final_List --
   ----------------------

   procedure Build_Final_List (N : Node_Id; Typ : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Decl : Node_Id;

   begin
      Set_Associated_Final_Chain (Typ,
        Make_Defining_Identifier (Loc,
          New_External_Name (Chars (Typ), 'L')));

      Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
             Associated_Final_Chain (Typ),
          Object_Definition   =>
            New_Reference_To
              (RTE (RE_List_Controller), Loc));

      --  If the type is declared in a package declaration and designates a
      --  Taft amendment type that requires finalization, place declaration
      --  of finalization list in the body, because no client of the package
      --  can create objects of the type and thus make use of this list. This
      --  ensures the tree for the spec is identical whenever it is compiled.

      if Has_Completion_In_Body (Directly_Designated_Type (Typ))
        and then In_Package_Body (Current_Scope)
        and then Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Body
        and then
          Nkind (Parent (Declaration_Node (Typ))) = N_Package_Specification
      then
         Insert_Action (Parent (Designated_Type (Typ)), Decl);

      --  The type may have been frozen already, and this is a late freezing
      --  action, in which case the declaration must be elaborated at once.
      --  If the call is for an allocator, the chain must also be created now,
      --  because the freezing of the type does not build one. Otherwise, the
      --  declaration is one of the freezing actions for a user-defined type.

      elsif Is_Frozen (Typ)
        or else (Nkind (N) = N_Allocator
                  and then Ekind (Etype (N)) = E_Anonymous_Access_Type)
      then
         Insert_Action (N, Decl);

      else
         Append_Freeze_Action (Typ, Decl);
      end if;
   end Build_Final_List;

   ---------------------
   -- Build_Late_Proc --
   ---------------------

   procedure Build_Late_Proc (Typ : Entity_Id; Nam : Name_Id) is
   begin
      for Final_Prim in Name_Of'Range loop
         if Name_Of (Final_Prim) = Nam then
            Set_TSS (Typ,
              Make_Deep_Proc (
                Prim  => Final_Prim,
                Typ   => Typ,
                Stmts => Make_Deep_Record_Body (Final_Prim, Typ)));
         end if;
      end loop;
   end Build_Late_Proc;

   -----------------------------
   -- Build_Record_Deep_Procs --
   -----------------------------

   procedure Build_Record_Deep_Procs (Typ : Entity_Id) is
   begin
      Set_TSS (Typ,
        Make_Deep_Proc (
          Prim  => Initialize_Case,
          Typ   => Typ,
          Stmts => Make_Deep_Record_Body (Initialize_Case, Typ)));

      if not Is_Inherently_Limited_Type (Typ) then
         Set_TSS (Typ,
           Make_Deep_Proc (
             Prim  => Adjust_Case,
             Typ   => Typ,
             Stmts => Make_Deep_Record_Body (Adjust_Case, Typ)));
      end if;

      Set_TSS (Typ,
        Make_Deep_Proc (
          Prim  => Finalize_Case,
          Typ   => Typ,
          Stmts => Make_Deep_Record_Body (Finalize_Case, Typ)));
   end Build_Record_Deep_Procs;

   -------------------
   -- Cleanup_Array --
   -------------------

   function Cleanup_Array
     (N    : Node_Id;
      Obj  : Node_Id;
      Typ  : Entity_Id) return List_Id
   is
      Loc        : constant Source_Ptr := Sloc (N);
      Index_List : constant List_Id := New_List;

      function Free_Component return List_Id;
      --  Generate the code to finalize the task or protected  subcomponents
      --  of a single component of the array.

      function Free_One_Dimension (Dim : Int) return List_Id;
      --  Generate a loop over one dimension of the array

      --------------------
      -- Free_Component --
      --------------------

      function Free_Component return List_Id is
         Stmts : List_Id := New_List;
         Tsk   : Node_Id;
         C_Typ : constant Entity_Id := Component_Type (Typ);

      begin
         --  Component type is known to contain tasks or protected objects

         Tsk :=
           Make_Indexed_Component (Loc,
             Prefix        => Duplicate_Subexpr_No_Checks (Obj),
             Expressions   => Index_List);

         Set_Etype (Tsk, C_Typ);

         if Is_Task_Type (C_Typ) then
            Append_To (Stmts, Cleanup_Task (N, Tsk));

         elsif Is_Simple_Protected_Type (C_Typ) then
            Append_To (Stmts, Cleanup_Protected_Object (N, Tsk));

         elsif Is_Record_Type (C_Typ) then
            Stmts := Cleanup_Record (N, Tsk, C_Typ);

         elsif Is_Array_Type (C_Typ) then
            Stmts := Cleanup_Array (N, Tsk, C_Typ);
         end if;

         return Stmts;
      end Free_Component;

      ------------------------
      -- Free_One_Dimension --
      ------------------------

      function Free_One_Dimension (Dim : Int) return List_Id is
         Index      : Entity_Id;

      begin
         if Dim > Number_Dimensions (Typ) then
            return Free_Component;

         --  Here we generate the required loop

         else
            Index :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('J'));

            Append (New_Reference_To (Index, Loc), Index_List);

            return New_List (
              Make_Implicit_Loop_Statement (N,
                Identifier => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix => Duplicate_Subexpr (Obj),
                            Attribute_Name  => Name_Range,
                            Expressions => New_List (
                              Make_Integer_Literal (Loc, Dim))))),
                Statements =>  Free_One_Dimension (Dim + 1)));
         end if;
      end Free_One_Dimension;

   --  Start of processing for Cleanup_Array

   begin
      return Free_One_Dimension (1);
   end Cleanup_Array;

   --------------------
   -- Cleanup_Record --
   --------------------

   function Cleanup_Record
     (N    : Node_Id;
      Obj  : Node_Id;
      Typ  : Entity_Id) return List_Id
   is
      Loc   : constant Source_Ptr := Sloc (N);
      Tsk   : Node_Id;
      Comp  : Entity_Id;
      Stmts : constant List_Id    := New_List;
      U_Typ : constant Entity_Id  := Underlying_Type (Typ);

   begin
      if Has_Discriminants (U_Typ)
        and then Nkind (Parent (U_Typ)) = N_Full_Type_Declaration
        and then
          Nkind (Type_Definition (Parent (U_Typ))) = N_Record_Definition
        and then
          Present
            (Variant_Part
              (Component_List (Type_Definition (Parent (U_Typ)))))
      then
         --  For now, do not attempt to free a component that may appear in
         --  a variant, and instead issue a warning. Doing this "properly"
         --  would require building a case statement and would be quite a
         --  mess. Note that the RM only requires that free "work" for the
         --  case of a task access value, so already we go way beyond this
         --  in that we deal with the array case and non-discriminated
         --  record cases.

         Error_Msg_N
           ("task/protected object in variant record will not be freed?", N);
         return New_List (Make_Null_Statement (Loc));
      end if;

      Comp := First_Component (Typ);

      while Present (Comp) loop
         if Has_Task (Etype (Comp))
           or else Has_Simple_Protected_Object (Etype (Comp))
         then
            Tsk :=
              Make_Selected_Component (Loc,
                Prefix        => Duplicate_Subexpr_No_Checks (Obj),
                Selector_Name => New_Occurrence_Of (Comp, Loc));
            Set_Etype (Tsk, Etype (Comp));

            if Is_Task_Type (Etype (Comp)) then
               Append_To (Stmts, Cleanup_Task (N, Tsk));

            elsif Is_Simple_Protected_Type (Etype (Comp)) then
               Append_To (Stmts, Cleanup_Protected_Object (N, Tsk));

            elsif Is_Record_Type (Etype (Comp)) then

               --  Recurse, by generating the prefix of the argument to
               --  the eventual cleanup call.

               Append_List_To
                 (Stmts, Cleanup_Record (N, Tsk, Etype (Comp)));

            elsif Is_Array_Type (Etype (Comp)) then
               Append_List_To
                 (Stmts, Cleanup_Array (N, Tsk, Etype (Comp)));
            end if;
         end if;

         Next_Component (Comp);
      end loop;

      return Stmts;
   end Cleanup_Record;

   ------------------------------
   -- Cleanup_Protected_Object --
   ------------------------------

   function Cleanup_Protected_Object
     (N   : Node_Id;
      Ref : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE_Finalize_Protection), Loc),
          Parameter_Associations => New_List (
            Concurrent_Ref (Ref)));
   end Cleanup_Protected_Object;

   ------------------------------------
   -- Clean_Simple_Protected_Objects --
   ------------------------------------

   procedure Clean_Simple_Protected_Objects (N : Node_Id) is
      Stmts : constant List_Id := Statements (Handled_Statement_Sequence (N));
      Stmt  : Node_Id          := Last (Stmts);
      E     : Entity_Id;

   begin
      E := First_Entity (Current_Scope);
      while Present (E) loop
         if (Ekind (E) = E_Variable
              or else Ekind (E) = E_Constant)
           and then Has_Simple_Protected_Object (Etype (E))
           and then not Has_Task (Etype (E))
           and then Nkind (Parent (E)) /= N_Object_Renaming_Declaration
         then
            declare
               Typ : constant Entity_Id := Etype (E);
               Ref : constant Node_Id := New_Occurrence_Of (E, Sloc (Stmt));

            begin
               if Is_Simple_Protected_Type (Typ) then
                  Append_To (Stmts, Cleanup_Protected_Object (N, Ref));

               elsif Has_Simple_Protected_Object (Typ) then
                  if Is_Record_Type (Typ) then
                     Append_List_To (Stmts, Cleanup_Record (N, Ref, Typ));

                  elsif Is_Array_Type (Typ) then
                     Append_List_To (Stmts, Cleanup_Array (N, Ref, Typ));
                  end if;
               end if;
            end;
         end if;

         Next_Entity (E);
      end loop;

      --   Analyze inserted cleanup statements

      if Present (Stmt) then
         Stmt := Next (Stmt);

         while Present (Stmt) loop
            Analyze (Stmt);
            Next (Stmt);
         end loop;
      end if;
   end Clean_Simple_Protected_Objects;

   ------------------
   -- Cleanup_Task --
   ------------------

   function Cleanup_Task
     (N   : Node_Id;
      Ref : Node_Id) return Node_Id
   is
      Loc  : constant Source_Ptr := Sloc (N);
   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE_Free_Task), Loc),
          Parameter_Associations =>
            New_List (Concurrent_Ref (Ref)));
   end Cleanup_Task;

   ---------------------------------
   -- Has_Simple_Protected_Object --
   ---------------------------------

   function Has_Simple_Protected_Object (T : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if Is_Simple_Protected_Type (T) then
         return True;

      elsif Is_Array_Type (T) then
         return Has_Simple_Protected_Object (Component_Type (T));

      elsif Is_Record_Type (T) then
         Comp := First_Component (T);

         while Present (Comp) loop
            if Has_Simple_Protected_Object (Etype (Comp)) then
               return True;
            end if;

            Next_Component (Comp);
         end loop;

         return False;

      else
         return False;
      end if;
   end Has_Simple_Protected_Object;

   ------------------------------
   -- Is_Simple_Protected_Type --
   ------------------------------

   function Is_Simple_Protected_Type (T : Entity_Id) return Boolean is
   begin
      return Is_Protected_Type (T) and then not Has_Entries (T);
   end Is_Simple_Protected_Type;

   ------------------------------
   -- Check_Visibly_Controlled --
   ------------------------------

   procedure Check_Visibly_Controlled
     (Prim : Final_Primitives;
      Typ  : Entity_Id;
      E    : in out Entity_Id;
      Cref : in out Node_Id)
   is
      Parent_Type : Entity_Id;
      Op          : Entity_Id;

   begin
      if Is_Derived_Type (Typ)
        and then Comes_From_Source (E)
        and then not Is_Overriding_Operation (E)
      then
         --  We know that the explicit operation on the type does not override
         --  the inherited operation of the parent, and that the derivation
         --  is from a private type that is not visibly controlled.

         Parent_Type := Etype (Typ);
         Op := Find_Prim_Op (Parent_Type, Name_Of (Prim));

         if Present (Op) then
            E := Op;

            --  Wrap the object to be initialized into the proper
            --  unchecked conversion, to be compatible with the operation
            --  to be called.

            if Nkind (Cref) = N_Unchecked_Type_Conversion then
               Cref := Unchecked_Convert_To (Parent_Type, Expression (Cref));
            else
               Cref := Unchecked_Convert_To (Parent_Type, Cref);
            end if;
         end if;
      end if;
   end Check_Visibly_Controlled;

   -------------------------------
   -- CW_Or_Has_Controlled_Part --
   -------------------------------

   function CW_Or_Has_Controlled_Part (T : Entity_Id) return Boolean is
   begin
      return Is_Class_Wide_Type (T) or else Needs_Finalization (T);
   end CW_Or_Has_Controlled_Part;

   --------------------------
   -- Controller_Component --
   --------------------------

   function Controller_Component (Typ : Entity_Id) return Entity_Id is
      T         : Entity_Id := Base_Type (Typ);
      Comp      : Entity_Id;
      Comp_Scop : Entity_Id;
      Res       : Entity_Id := Empty;
      Res_Scop  : Entity_Id := Empty;

   begin
      if Is_Class_Wide_Type (T) then
         T := Root_Type (T);
      end if;

      if Is_Private_Type (T) then
         T := Underlying_Type (T);
      end if;

      --  Fetch the outermost controller

      Comp := First_Entity (T);
      while Present (Comp) loop
         if Chars (Comp) = Name_uController then
            Comp_Scop := Scope (Original_Record_Component (Comp));

            --  If this controller is at the outermost level, no need to
            --  look for another one

            if Comp_Scop = T then
               return Comp;

            --  Otherwise record the outermost one and continue looking

            elsif Res = Empty or else Is_Ancestor (Res_Scop, Comp_Scop) then
               Res      := Comp;
               Res_Scop := Comp_Scop;
            end if;
         end if;

         Next_Entity (Comp);
      end loop;

      --  If we fall through the loop, there is no controller component

      return Res;
   end Controller_Component;

   ------------------
   -- Convert_View --
   ------------------

   function Convert_View
     (Proc : Entity_Id;
      Arg  : Node_Id;
      Ind  : Pos := 1) return Node_Id
   is
      Fent : Entity_Id := First_Entity (Proc);
      Ftyp : Entity_Id;
      Atyp : Entity_Id;

   begin
      for J in 2 .. Ind loop
         Next_Entity (Fent);
      end loop;

      Ftyp := Etype (Fent);

      if Nkind_In (Arg, N_Type_Conversion, N_Unchecked_Type_Conversion) then
         Atyp := Entity (Subtype_Mark (Arg));
      else
         Atyp := Etype (Arg);
      end if;

      if Is_Abstract_Subprogram (Proc) and then Is_Tagged_Type (Ftyp) then
         return Unchecked_Convert_To (Class_Wide_Type (Ftyp), Arg);

      elsif Ftyp /= Atyp
        and then Present (Atyp)
        and then
          (Is_Private_Type (Ftyp) or else Is_Private_Type (Atyp))
        and then
           Base_Type (Underlying_Type (Atyp)) =
             Base_Type (Underlying_Type (Ftyp))
      then
         return Unchecked_Convert_To (Ftyp, Arg);

      --  If the argument is already a conversion, as generated by
      --  Make_Init_Call, set the target type to the type of the formal
      --  directly, to avoid spurious typing problems.

      elsif Nkind_In (Arg, N_Unchecked_Type_Conversion, N_Type_Conversion)
        and then not Is_Class_Wide_Type (Atyp)
      then
         Set_Subtype_Mark (Arg, New_Occurrence_Of (Ftyp, Sloc (Arg)));
         Set_Etype (Arg, Ftyp);
         return Arg;

      else
         return Arg;
      end if;
   end Convert_View;

   -------------------------------
   -- Establish_Transient_Scope --
   -------------------------------

   --  This procedure is called each time a transient block has to be inserted
   --  that is to say for each call to a function with unconstrained or tagged
   --  result. It creates a new scope on the stack scope in order to enclose
   --  all transient variables generated

   procedure Establish_Transient_Scope (N : Node_Id; Sec_Stack : Boolean) is
      Loc       : constant Source_Ptr := Sloc (N);
      Wrap_Node : Node_Id;

   begin
      --  Nothing to do for virtual machines where memory is GCed

      if VM_Target /= No_VM then
         return;
      end if;

      --  Do not create a transient scope if we are already inside one

      for S in reverse Scope_Stack.First .. Scope_Stack.Last loop
         if Scope_Stack.Table (S).Is_Transient then
            if Sec_Stack then
               Set_Uses_Sec_Stack (Scope_Stack.Table (S).Entity);
            end if;

            return;

         --  If we have encountered Standard there are no enclosing
         --  transient scopes.

         elsif Scope_Stack.Table (S).Entity = Standard_Standard then
            exit;

         end if;
      end loop;

      Wrap_Node := Find_Node_To_Be_Wrapped (N);

      --  Case of no wrap node, false alert, no transient scope needed

      if No (Wrap_Node) then
         null;

      --  If the node to wrap is an iteration_scheme, the expression is
      --  one of the bounds, and the expansion will make an explicit
      --  declaration for it (see Analyze_Iteration_Scheme, sem_ch5.adb),
      --  so do not apply any transformations here.

      elsif Nkind (Wrap_Node) = N_Iteration_Scheme then
         null;

      else
         Push_Scope (New_Internal_Entity (E_Block, Current_Scope, Loc, 'B'));
         Set_Scope_Is_Transient;

         if Sec_Stack then
            Set_Uses_Sec_Stack (Current_Scope);
            Check_Restriction (No_Secondary_Stack, N);
         end if;

         Set_Etype (Current_Scope, Standard_Void_Type);
         Set_Node_To_Be_Wrapped (Wrap_Node);

         if Debug_Flag_W then
            Write_Str ("    <Transient>");
            Write_Eol;
         end if;
      end if;
   end Establish_Transient_Scope;

   ----------------------------
   -- Expand_Cleanup_Actions --
   ----------------------------

   procedure Expand_Cleanup_Actions (N : Node_Id) is
      S       : constant Entity_Id  := Current_Scope;
      Flist   : constant Entity_Id := Finalization_Chain_Entity (S);
      Is_Task : constant Boolean := Nkind (Original_Node (N)) = N_Task_Body;

      Is_Master            : constant Boolean :=
                               Nkind (N) /= N_Entry_Body
                                 and then Is_Task_Master (N);
      Is_Protected         : constant Boolean :=
                               Nkind (N) = N_Subprogram_Body
                                 and then Is_Protected_Subprogram_Body (N);
      Is_Task_Allocation   : constant Boolean :=
                               Nkind (N) = N_Block_Statement
                                 and then Is_Task_Allocation_Block (N);
      Is_Asynchronous_Call : constant Boolean :=
                               Nkind (N) = N_Block_Statement
                                 and then Is_Asynchronous_Call_Block (N);

      Previous_At_End_Proc : constant Node_Id :=
                               At_End_Proc (Handled_Statement_Sequence (N));

      Clean     : Entity_Id;
      Loc       : Source_Ptr;
      Mark      : Entity_Id := Empty;
      New_Decls : constant List_Id := New_List;
      Blok      : Node_Id;
      End_Lab   : Node_Id;
      Wrapped   : Boolean;
      Chain     : Entity_Id := Empty;
      Decl      : Node_Id;
      Old_Poll  : Boolean;

   begin
      --  If we are generating expanded code for debugging purposes, use
      --  the Sloc of the point of insertion for the cleanup code. The Sloc
      --  will be updated subsequently to reference the proper line in the
      --  .dg file.  If we are not debugging generated code, use instead
      --  No_Location, so that no debug information is generated for the
      --  cleanup code. This makes the behavior of the NEXT command in GDB
      --  monotonic, and makes the placement of breakpoints more accurate.

      if Debug_Generated_Code then
         Loc := Sloc (S);
      else
         Loc := No_Location;
      end if;

      --  There are cleanup actions only if the secondary stack needs
      --  releasing or some finalizations are needed or in the context
      --  of tasking

      if Uses_Sec_Stack  (Current_Scope)
        and then not Sec_Stack_Needed_For_Return (Current_Scope)
      then
         null;
      elsif No (Flist)
        and then not Is_Master
        and then not Is_Task
        and then not Is_Protected
        and then not Is_Task_Allocation
        and then not Is_Asynchronous_Call
      then
         Clean_Simple_Protected_Objects (N);
         return;
      end if;

      --  If the current scope is the subprogram body that is the rewriting
      --  of a task body, and the descriptors have not been delayed (due to
      --  some nested instantiations) do not generate redundant cleanup
      --  actions: the cleanup procedure already exists for this body.

      if Nkind (N) = N_Subprogram_Body
        and then Nkind (Original_Node (N)) = N_Task_Body
        and then not Delay_Subprogram_Descriptors (Corresponding_Spec (N))
      then
         return;
      end if;

      --  Set polling off, since we don't need to poll during cleanup
      --  actions, and indeed for the cleanup routine, which is executed
      --  with aborts deferred, we don't want polling.

      Old_Poll := Polling_Required;
      Polling_Required := False;

      --  Make sure we have a declaration list, since we will add to it

      if No (Declarations (N)) then
         Set_Declarations (N, New_List);
      end if;

      --  The task activation call has already been built for task
      --  allocation blocks.

      if not Is_Task_Allocation then
         Build_Task_Activation_Call (N);
      end if;

      if Is_Master then
         Establish_Task_Master (N);
      end if;

      --  If secondary stack is in use, expand:
      --    _Mxx : constant Mark_Id := SS_Mark;

      --  Suppress calls to SS_Mark and SS_Release if VM_Target,
      --  since we never use the secondary stack on the VM.

      if Uses_Sec_Stack (Current_Scope)
        and then not Sec_Stack_Needed_For_Return (Current_Scope)
        and then VM_Target = No_VM
      then
         Mark := Make_Defining_Identifier (Loc, New_Internal_Name ('M'));
         Append_To (New_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Mark,
             Object_Definition   => New_Reference_To (RTE (RE_Mark_Id), Loc),
             Expression =>
               Make_Function_Call (Loc,
                 Name => New_Reference_To (RTE (RE_SS_Mark), Loc))));

         Set_Uses_Sec_Stack (Current_Scope, False);
      end if;

      --  If finalization list is present then expand:
      --   Local_Final_List : System.FI.Finalizable_Ptr;

      if Present (Flist) then
         Append_To (New_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Flist,
             Object_Definition   =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), Loc)));
      end if;

      --  Clean-up procedure definition

      Clean := Make_Defining_Identifier (Loc, Name_uClean);
      Set_Suppress_Elaboration_Warnings (Clean);
      Append_To (New_Decls,
        Make_Clean (N, Clean, Mark, Flist,
          Is_Task,
          Is_Master,
          Is_Protected,
          Is_Task_Allocation,
          Is_Asynchronous_Call,
          Previous_At_End_Proc));

      --  The previous AT END procedure, if any, has been captured in Clean:
      --  reset it to Empty now because we check further on that we never
      --  overwrite an existing AT END call.

      Set_At_End_Proc (Handled_Statement_Sequence (N), Empty);

      --  If exception handlers are present, wrap the Sequence of statements in
      --  a block because it is not possible to get exception handlers and an
      --  AT END call in the same scope.

      if Present (Exception_Handlers (Handled_Statement_Sequence (N))) then

         --  Preserve end label to provide proper cross-reference information

         End_Lab := End_Label (Handled_Statement_Sequence (N));
         Blok :=
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence => Handled_Statement_Sequence (N));
         Set_Handled_Statement_Sequence (N,
           Make_Handled_Sequence_Of_Statements (Loc, New_List (Blok)));
         Set_End_Label (Handled_Statement_Sequence (N), End_Lab);
         Wrapped := True;

         --  Comment needed here, see RH for 1.306 ???

         if Nkind (N) = N_Subprogram_Body then
            Set_Has_Nested_Block_With_Handler (Current_Scope);
         end if;

      --  Otherwise we do not wrap

      else
         Wrapped := False;
         Blok    := Empty;
      end if;

      --  Don't move the _chain Activation_Chain declaration in task
      --  allocation blocks. Task allocation blocks use this object
      --  in their cleanup handlers, and gigi complains if it is declared
      --  in the sequence of statements of the scope that declares the
      --  handler.

      if Is_Task_Allocation then
         Chain := Activation_Chain_Entity (N);

         Decl := First (Declarations (N));
         while Nkind (Decl) /= N_Object_Declaration
           or else Defining_Identifier (Decl) /= Chain
         loop
            Next (Decl);
            pragma Assert (Present (Decl));
         end loop;

         Remove (Decl);
         Prepend_To (New_Decls, Decl);
      end if;

      --  Now we move the declarations into the Sequence of statements
      --  in order to get them protected by the AT END call. It may seem
      --  weird to put declarations in the sequence of statement but in
      --  fact nothing forbids that at the tree level. We also set the
      --  First_Real_Statement field so that we remember where the real
      --  statements (i.e. original statements) begin. Note that if we
      --  wrapped the statements, the first real statement is inside the
      --  inner block. If the First_Real_Statement is already set (as is
      --  the case for subprogram bodies that are expansions of task bodies)
      --  then do not reset it, because its declarative part would migrate
      --  to the statement part.

      if not Wrapped then
         if No (First_Real_Statement (Handled_Statement_Sequence (N))) then
            Set_First_Real_Statement (Handled_Statement_Sequence (N),
              First (Statements (Handled_Statement_Sequence (N))));
         end if;

      else
         Set_First_Real_Statement (Handled_Statement_Sequence (N), Blok);
      end if;

      Append_List_To (Declarations (N),
        Statements (Handled_Statement_Sequence (N)));
      Set_Statements (Handled_Statement_Sequence (N), Declarations (N));

      --  We need to reset the Sloc of the handled statement sequence to
      --  properly reflect the new initial "statement" in the sequence.

      Set_Sloc
        (Handled_Statement_Sequence (N), Sloc (First (Declarations (N))));

      --  The declarations of the _Clean procedure and finalization chain
      --  replace the old declarations that have been moved inward.

      Set_Declarations (N, New_Decls);
      Analyze_Declarations (New_Decls);

      --  The At_End call is attached to the sequence of statements

      declare
         HSS : Node_Id;

      begin
         --  If the construct is a protected subprogram, then the call to
         --  the corresponding unprotected subprogram appears in a block which
         --  is the last statement in the body, and it is this block that must
         --  be covered by the At_End handler.

         if Is_Protected then
            HSS := Handled_Statement_Sequence
              (Last (Statements (Handled_Statement_Sequence (N))));
         else
            HSS := Handled_Statement_Sequence (N);
         end if;

         --  Never overwrite an existing AT END call

         pragma Assert (No (At_End_Proc (HSS)));

         Set_At_End_Proc (HSS, New_Occurrence_Of (Clean, Loc));
         Expand_At_End_Handler (HSS, Empty);
      end;

      --  Restore saved polling mode

      Polling_Required := Old_Poll;
   end Expand_Cleanup_Actions;

   -------------------------------
   -- Expand_Ctrl_Function_Call --
   -------------------------------

   procedure Expand_Ctrl_Function_Call (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Rtype   : constant Entity_Id  := Etype (N);
      Utype   : constant Entity_Id  := Underlying_Type (Rtype);
      Ref     : Node_Id;
      Action  : Node_Id;
      Action2 : Node_Id := Empty;

      Attach_Level : Uint    := Uint_1;
      Len_Ref      : Node_Id := Empty;

      function Last_Array_Component
        (Ref : Node_Id;
         Typ : Entity_Id) return Node_Id;
      --  Creates a reference to the last component of the array object
      --  designated by Ref whose type is Typ.

      --------------------------
      -- Last_Array_Component --
      --------------------------

      function Last_Array_Component
        (Ref : Node_Id;
         Typ : Entity_Id) return Node_Id
      is
         Index_List : constant List_Id := New_List;

      begin
         for N in 1 .. Number_Dimensions (Typ) loop
            Append_To (Index_List,
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr_No_Checks (Ref),
                Attribute_Name => Name_Last,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, N))));
         end loop;

         return
           Make_Indexed_Component (Loc,
             Prefix      => Duplicate_Subexpr (Ref),
             Expressions => Index_List);
      end Last_Array_Component;

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
      Ref := Duplicate_Subexpr_No_Checks (N);

      --  Now we can generate the Attach Call. Note that this value is always
      --  on the (secondary) stack and thus is attached to a singly linked
      --  final list:

      --    Resx := F (X)'reference;
      --    Attach_To_Final_List (_Lx, Resx.all, 1);

      --  or when there are controlled components:

      --    Attach_To_Final_List (_Lx, Resx._controller, 1);

      --  or when it is both Is_Controlled and Has_Controlled_Components:

      --    Attach_To_Final_List (_Lx, Resx._controller, 1);
      --    Attach_To_Final_List (_Lx, Resx, 1);

      --  or if it is an array with Is_Controlled (and Has_Controlled)

      --    Attach_To_Final_List (_Lx, Resx (Resx'last), 3);

      --    An attach level of 3 means that a whole array is to be attached to
      --    the finalization list (including the controlled components).

      --  or if it is an array with Has_Controlled_Components but not
      --  Is_Controlled:

      --    Attach_To_Final_List (_Lx, Resx (Resx'last)._controller, 3);

      --  Case where type has controlled components

      if Has_Controlled_Component (Rtype) then
         declare
            T1 : Entity_Id := Rtype;
            T2 : Entity_Id := Utype;

         begin
            if Is_Array_Type (T2) then
               Len_Ref :=
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     Duplicate_Subexpr_Move_Checks
                       (Unchecked_Convert_To (T2, Ref)),
                   Attribute_Name => Name_Length);
            end if;

            while Is_Array_Type (T2) loop
               if T1 /= T2 then
                  Ref := Unchecked_Convert_To (T2, Ref);
               end if;

               Ref := Last_Array_Component (Ref, T2);
               Attach_Level := Uint_3;
               T1 := Component_Type (T2);
               T2 := Underlying_Type (T1);
            end loop;

            --  If the type has controlled components, go to the controller
            --  except in the case of arrays of controlled objects since in
            --  this case objects and their components are already chained
            --  and the head of the chain is the last array element.

            if Is_Array_Type (Rtype) and then Is_Controlled (T2) then
               null;

            elsif Has_Controlled_Component (T2) then
               if T1 /= T2 then
                  Ref := Unchecked_Convert_To (T2, Ref);
               end if;

               Ref :=
                 Make_Selected_Component (Loc,
                   Prefix        => Ref,
                   Selector_Name => Make_Identifier (Loc, Name_uController));
            end if;
         end;

         --  Here we know that 'Ref' has a controller so we may as well attach
         --  it directly.

         Action :=
           Make_Attach_Call (
             Obj_Ref      => Ref,
             Flist_Ref    => Find_Final_List (Current_Scope),
             With_Attach  => Make_Integer_Literal (Loc, Attach_Level));

         --  If it is also Is_Controlled we need to attach the global object

         if Is_Controlled (Rtype) then
            Action2 :=
              Make_Attach_Call (
                Obj_Ref      => Duplicate_Subexpr_No_Checks (N),
                Flist_Ref    => Find_Final_List (Current_Scope),
                With_Attach  => Make_Integer_Literal (Loc, Attach_Level));
         end if;

      --  Here, we have a controlled type that does not seem to have controlled
      --  components but it could be a class wide type whose further
      --  derivations have controlled components. So we don't know if the
      --  object itself needs to be attached or if it has a record controller.
      --  We need to call a runtime function (Deep_Tag_Attach) which knows what
      --  to do thanks to the RC_Offset in the dispatch table.

      else
         Action :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Deep_Tag_Attach), Loc),
             Parameter_Associations => New_List (
               Find_Final_List (Current_Scope),

               Make_Attribute_Reference (Loc,
                   Prefix => Ref,
                   Attribute_Name => Name_Address),

               Make_Integer_Literal (Loc, Attach_Level)));
      end if;

      if Present (Len_Ref) then
         Action :=
           Make_Implicit_If_Statement (N,
             Condition => Make_Op_Gt (Loc,
               Left_Opnd  => Len_Ref,
               Right_Opnd => Make_Integer_Literal (Loc, 0)),
             Then_Statements => New_List (Action));
      end if;

      Insert_Action (N, Action);
      if Present (Action2) then
         Insert_Action (N, Action2);
      end if;
   end Expand_Ctrl_Function_Call;

   ---------------------------
   -- Expand_N_Package_Body --
   ---------------------------

   --  Add call to Activate_Tasks if body is an activator (actual processing
   --  is in chapter 9).

   --  Generate subprogram descriptor for elaboration routine

   --  Encode entity names in package body

   procedure Expand_N_Package_Body (N : Node_Id) is
      Ent : constant Entity_Id := Corresponding_Spec (N);

   begin
      --  This is done only for non-generic packages

      if Ekind (Ent) = E_Package then
         Push_Scope (Corresponding_Spec (N));

         --  Build dispatch tables of library level tagged types

         if Is_Library_Level_Entity (Ent) then
            Build_Static_Dispatch_Tables (N);
         end if;

         Build_Task_Activation_Call (N);
         Pop_Scope;
      end if;

      Set_Elaboration_Flag (N, Corresponding_Spec (N));
      Set_In_Package_Body (Ent, False);

      --  Set to encode entity names in package body before gigi is called

      Qualify_Entity_Names (N);
   end Expand_N_Package_Body;

   ----------------------------------
   -- Expand_N_Package_Declaration --
   ----------------------------------

   --  Add call to Activate_Tasks if there are tasks declared and the package
   --  has no body. Note that in Ada83, this may result in premature activation
   --  of some tasks, given that we cannot tell whether a body will eventually
   --  appear.

   procedure Expand_N_Package_Declaration (N : Node_Id) is
      Spec    : constant Node_Id   := Specification (N);
      Id      : constant Entity_Id := Defining_Entity (N);
      Decls   : List_Id;
      No_Body : Boolean := False;
      --  True in the case of a package declaration that is a compilation unit
      --  and for which no associated body will be compiled in
      --  this compilation.

   begin
      --  Case of a package declaration other than a compilation unit

      if Nkind (Parent (N)) /= N_Compilation_Unit then
         null;

      --  Case of a compilation unit that does not require a body

      elsif not Body_Required (Parent (N))
        and then not Unit_Requires_Body (Id)
      then
         No_Body := True;

      --  Special case of generating calling stubs for a remote call interface
      --  package: even though the package declaration requires one, the
      --  body won't be processed in this compilation (so any stubs for RACWs
      --  declared in the package must be generated here, along with the
      --  spec).

      elsif Parent (N) = Cunit (Main_Unit)
        and then Is_Remote_Call_Interface (Id)
        and then Distribution_Stub_Mode = Generate_Caller_Stub_Body
      then
         No_Body := True;
      end if;

      --  For a package declaration that implies no associated body, generate
      --  task activation call and RACW supporting bodies now (since we won't
      --  have a specific separate compilation unit for that).

      if No_Body then
         Push_Scope (Id);

         if Has_RACW (Id) then

            --  Generate RACW subprogram bodies

            Decls := Private_Declarations (Spec);

            if No (Decls) then
               Decls := Visible_Declarations (Spec);
            end if;

            if No (Decls) then
               Decls := New_List;
               Set_Visible_Declarations (Spec, Decls);
            end if;

            Append_RACW_Bodies (Decls, Id);
            Analyze_List (Decls);
         end if;

         if Present (Activation_Chain_Entity (N)) then

            --  Generate task activation call as last step of elaboration

            Build_Task_Activation_Call (N);
         end if;

         Pop_Scope;
      end if;

      --  Build dispatch tables of library level tagged types

      if Is_Compilation_Unit (Id)
        or else (Is_Generic_Instance (Id)
                   and then Is_Library_Level_Entity (Id))
      then
         Build_Static_Dispatch_Tables (N);
      end if;

      --  Note: it is not necessary to worry about generating a subprogram
      --  descriptor, since the only way to get exception handlers into a
      --  package spec is to include instantiations, and that would cause
      --  generation of subprogram descriptors to be delayed in any case.

      --  Set to encode entity names in package spec before gigi is called

      Qualify_Entity_Names (N);
   end Expand_N_Package_Declaration;

   ---------------------
   -- Find_Final_List --
   ---------------------

   function Find_Final_List
     (E   : Entity_Id;
      Ref : Node_Id := Empty) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Ref);
      S   : Entity_Id;
      Id  : Entity_Id;
      R   : Node_Id;

   begin
      --  If the restriction No_Finalization applies, then there's not any
      --  finalization list available to return, so return Empty.

      if Restriction_Active (No_Finalization) then
         return Empty;

      --  Case of an internal component. The Final list is the record
      --  controller of the enclosing record.

      elsif Present (Ref) then
         R := Ref;
         loop
            case Nkind (R) is
               when N_Unchecked_Type_Conversion | N_Type_Conversion =>
                  R := Expression (R);

               when N_Indexed_Component | N_Explicit_Dereference =>
                  R := Prefix (R);

               when  N_Selected_Component =>
                  R := Prefix (R);
                  exit;

               when  N_Identifier =>
                  exit;

               when others =>
                  raise Program_Error;
            end case;
         end loop;

         return
           Make_Selected_Component (Loc,
             Prefix =>
               Make_Selected_Component (Loc,
                 Prefix        => R,
                 Selector_Name => Make_Identifier (Loc, Name_uController)),
             Selector_Name => Make_Identifier (Loc, Name_F));

      --  Case of a dynamically allocated object whose access type has an
      --  Associated_Final_Chain. The final list is the corresponding list
      --  controller (the next entity in the scope of the access type with
      --  the right type). If the type comes from a With_Type clause, no
      --  controller was created, we use the global chain instead. (The code
      --  related to with_type clauses should presumably be removed at some
      --  point since that feature is obsolete???)

      --  An anonymous access type either has a list created for it when the
      --  allocator is a for an access parameter or an access discriminant,
      --  or else it uses the list of the enclosing dynamic scope, when the
      --  context is a declaration or an assignment.

      elsif Is_Access_Type (E)
        and then (Present (Associated_Final_Chain (E))
                   or else From_With_Type (E))
      then
         if From_With_Type (E) then
            return New_Reference_To (RTE (RE_Global_Final_List), Sloc (E));

         --  Use the access type's associated finalization chain

         else
            return
              Make_Selected_Component (Loc,
                Prefix        =>
                  New_Reference_To
                    (Associated_Final_Chain (Base_Type (E)), Loc),
                Selector_Name => Make_Identifier (Loc, Name_F));
         end if;

      else
         if Is_Dynamic_Scope (E) then
            S := E;
         else
            S := Enclosing_Dynamic_Scope (E);
         end if;

         --  When the finalization chain entity is 'Error', it means that there
         --  should not be any chain at that level and that the enclosing one
         --  should be used.

         --  This is a nasty kludge, see ??? note in exp_ch11

         while Finalization_Chain_Entity (S) = Error loop
            S := Enclosing_Dynamic_Scope (S);
         end loop;

         if S = Standard_Standard then
            return New_Reference_To (RTE (RE_Global_Final_List), Sloc (E));
         else
            if No (Finalization_Chain_Entity (S)) then

               --  In the case where the scope is a subprogram, retrieve the
               --  Sloc of subprogram's body for association with the chain,
               --  since using the Sloc of the spec would be confusing during
               --  source-line stepping within the debugger.

               declare
                  Flist_Loc : Source_Ptr := Sloc (S);
                  Subp_Body : Node_Id;

               begin
                  if Ekind (S) in Subprogram_Kind then
                     Subp_Body := Unit_Declaration_Node (S);

                     if Nkind (Subp_Body) /= N_Subprogram_Body then
                        Subp_Body := Corresponding_Body (Subp_Body);
                     end if;

                     if Present (Subp_Body) then
                        Flist_Loc := Sloc (Subp_Body);
                     end if;
                  end if;

                  Id :=
                    Make_Defining_Identifier (Flist_Loc,
                      Chars => New_Internal_Name ('F'));
               end;

               Set_Finalization_Chain_Entity (S, Id);

               --  Set momentarily some semantics attributes to allow normal
               --  analysis of expansions containing references to this chain.
               --  Will be fully decorated during the expansion of the scope
               --  itself.

               Set_Ekind (Id, E_Variable);
               Set_Etype (Id, RTE (RE_Finalizable_Ptr));
            end if;

            return New_Reference_To (Finalization_Chain_Entity (S), Sloc (E));
         end if;
      end if;
   end Find_Final_List;

   -----------------------------
   -- Find_Node_To_Be_Wrapped --
   -----------------------------

   function Find_Node_To_Be_Wrapped (N : Node_Id) return Node_Id is
      P          : Node_Id;
      The_Parent : Node_Id;

   begin
      The_Parent := N;
      loop
         P := The_Parent;
         pragma Assert (P /= Empty);
         The_Parent := Parent (P);

         case Nkind (The_Parent) is

            --  Simple statement can be wrapped

            when N_Pragma =>
               return The_Parent;

            --  Usually assignments are good candidate for wrapping
            --  except when they have been generated as part of a
            --  controlled aggregate where the wrapping should take
            --  place more globally.

            when N_Assignment_Statement =>
               if No_Ctrl_Actions (The_Parent) then
                  null;
               else
                  return The_Parent;
               end if;

            --  An entry call statement is a special case if it occurs in
            --  the context of a Timed_Entry_Call. In this case we wrap
            --  the entire timed entry call.

            when N_Entry_Call_Statement     |
                 N_Procedure_Call_Statement =>
               if Nkind (Parent (The_Parent)) = N_Entry_Call_Alternative
                 and then Nkind_In (Parent (Parent (The_Parent)),
                                    N_Timed_Entry_Call,
                                    N_Conditional_Entry_Call)
               then
                  return Parent (Parent (The_Parent));
               else
                  return The_Parent;
               end if;

            --  Object declarations are also a boundary for the transient scope
            --  even if they are not really wrapped
            --  (see Wrap_Transient_Declaration)

            when N_Object_Declaration          |
                 N_Object_Renaming_Declaration |
                 N_Subtype_Declaration         =>
               return The_Parent;

            --  The expression itself is to be wrapped if its parent is a
            --  compound statement or any other statement where the expression
            --  is known to be scalar

            when N_Accept_Alternative               |
                 N_Attribute_Definition_Clause      |
                 N_Case_Statement                   |
                 N_Code_Statement                   |
                 N_Delay_Alternative                |
                 N_Delay_Until_Statement            |
                 N_Delay_Relative_Statement         |
                 N_Discriminant_Association         |
                 N_Elsif_Part                       |
                 N_Entry_Body_Formal_Part           |
                 N_Exit_Statement                   |
                 N_If_Statement                     |
                 N_Iteration_Scheme                 |
                 N_Terminate_Alternative            =>
               return P;

            when N_Attribute_Reference =>

               if Is_Procedure_Attribute_Name
                    (Attribute_Name (The_Parent))
               then
                  return The_Parent;
               end if;

            --  A raise statement can be wrapped. This will arise when the
            --  expression in a raise_with_expression uses the secondary
            --  stack, for example.

            when N_Raise_Statement =>
               return The_Parent;

            --  If the expression is within the iteration scheme of a loop,
            --  we must create a declaration for it, followed by an assignment
            --  in order to have a usable statement to wrap.

            when N_Loop_Parameter_Specification =>
               return Parent (The_Parent);

            --  The following nodes contains "dummy calls" which don't
            --  need to be wrapped.

            when N_Parameter_Specification     |
                 N_Discriminant_Specification  |
                 N_Component_Declaration       =>
               return Empty;

            --  The return statement is not to be wrapped when the function
            --  itself needs wrapping at the outer-level

            when N_Simple_Return_Statement =>
               declare
                  Applies_To : constant Entity_Id :=
                                 Return_Applies_To
                                   (Return_Statement_Entity (The_Parent));
                  Return_Type : constant Entity_Id := Etype (Applies_To);
               begin
                  if Requires_Transient_Scope (Return_Type) then
                     return Empty;
                  else
                     return The_Parent;
                  end if;
               end;

            --  If we leave a scope without having been able to find a node to
            --  wrap, something is going wrong but this can happen in error
            --  situation that are not detected yet (such as a dynamic string
            --  in a pragma export)

            when N_Subprogram_Body     |
                 N_Package_Declaration |
                 N_Package_Body        |
                 N_Block_Statement     =>
               return Empty;

            --  otherwise continue the search

            when others =>
               null;
         end case;
      end loop;
   end Find_Node_To_Be_Wrapped;

   ----------------------
   -- Global_Flist_Ref --
   ----------------------

   function Global_Flist_Ref  (Flist_Ref : Node_Id) return Boolean is
      Flist : Entity_Id;

   begin
      --  Look for the Global_Final_List

      if Is_Entity_Name (Flist_Ref) then
         Flist := Entity (Flist_Ref);

      --  Look for the final list associated with an access to controlled

      elsif  Nkind (Flist_Ref) = N_Selected_Component
        and then Is_Entity_Name (Prefix (Flist_Ref))
      then
         Flist :=  Entity (Prefix (Flist_Ref));
      else
         return False;
      end if;

      return Present (Flist)
        and then Present (Scope (Flist))
        and then Enclosing_Dynamic_Scope (Flist) = Standard_Standard;
   end Global_Flist_Ref;

   ----------------------------------
   -- Has_New_Controlled_Component --
   ----------------------------------

   function Has_New_Controlled_Component (E : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if not Is_Tagged_Type (E) then
         return Has_Controlled_Component (E);
      elsif not Is_Derived_Type (E) then
         return Has_Controlled_Component (E);
      end if;

      Comp := First_Component (E);
      while Present (Comp) loop

         if Chars (Comp) = Name_uParent then
            null;

         elsif Scope (Original_Record_Component (Comp)) = E
           and then Needs_Finalization (Etype (Comp))
         then
            return True;
         end if;

         Next_Component (Comp);
      end loop;

      return False;
   end Has_New_Controlled_Component;

   --------------------------
   -- In_Finalization_Root --
   --------------------------

   --  It would seem simpler to test Scope (RTE (RE_Root_Controlled)) but
   --  the purpose of this function is to avoid a circular call to Rtsfind
   --  which would been caused by such a test.

   function In_Finalization_Root (E : Entity_Id) return Boolean is
      S : constant Entity_Id := Scope (E);

   begin
      return Chars (Scope (S))     = Name_System
        and then Chars (S)         = Name_Finalization_Root
        and then Scope (Scope (S)) = Standard_Standard;
   end  In_Finalization_Root;

   ------------------------------------
   -- Insert_Actions_In_Scope_Around --
   ------------------------------------

   procedure Insert_Actions_In_Scope_Around (N : Node_Id) is
      SE     : Scope_Stack_Entry renames Scope_Stack.Table (Scope_Stack.Last);
      Target : Node_Id;

   begin
      --  If the node to be wrapped is the triggering statement of an
      --  asynchronous select, it is not part of a statement list. The
      --  actions must be inserted before the Select itself, which is
      --  part of some list of statements. Note that the triggering
      --  alternative includes the triggering statement and an optional
      --  statement list. If the node to be wrapped is part of that list,
      --  the normal insertion applies.

      if Nkind (Parent (Node_To_Be_Wrapped)) = N_Triggering_Alternative
        and then not Is_List_Member (Node_To_Be_Wrapped)
      then
         Target := Parent (Parent (Node_To_Be_Wrapped));
      else
         Target := N;
      end if;

      if Present (SE.Actions_To_Be_Wrapped_Before) then
         Insert_List_Before (Target, SE.Actions_To_Be_Wrapped_Before);
         SE.Actions_To_Be_Wrapped_Before := No_List;
      end if;

      if Present (SE.Actions_To_Be_Wrapped_After) then
         Insert_List_After (Target, SE.Actions_To_Be_Wrapped_After);
         SE.Actions_To_Be_Wrapped_After := No_List;
      end if;
   end Insert_Actions_In_Scope_Around;

   -----------------------
   -- Make_Adjust_Call --
   -----------------------

   function Make_Adjust_Call
     (Ref         : Node_Id;
      Typ         : Entity_Id;
      Flist_Ref   : Node_Id;
      With_Attach : Node_Id;
      Allocator   : Boolean := False) return List_Id
   is
      Loc    : constant Source_Ptr := Sloc (Ref);
      Res    : constant List_Id    := New_List;
      Utyp   : Entity_Id;
      Proc   : Entity_Id;
      Cref   : Node_Id := Ref;
      Cref2  : Node_Id;
      Attach : Node_Id := With_Attach;

   begin
      if Is_Class_Wide_Type (Typ) then
         Utyp := Underlying_Type (Base_Type (Root_Type (Typ)));
      else
         Utyp := Underlying_Type (Base_Type (Typ));
      end if;

      Set_Assignment_OK (Cref);

      --  Deal with non-tagged derivation of private views

      if Is_Untagged_Derivation (Typ) then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Cref := Unchecked_Convert_To (Utyp, Cref);
         Set_Assignment_OK (Cref);
         --  To prevent problems with UC see 1.156 RH ???
      end if;

      --  If the underlying_type is a subtype, we are dealing with
      --  the completion of a private type. We need to access
      --  the base type and generate a conversion to it.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));
         Utyp := Base_Type (Utyp);
         Cref := Unchecked_Convert_To (Utyp, Cref);
      end if;

      --  If the object is unanalyzed, set its expected type for use
      --  in Convert_View in case an additional conversion is needed.

      if No (Etype (Cref))
        and then Nkind (Cref) /= N_Unchecked_Type_Conversion
      then
         Set_Etype (Cref, Typ);
      end if;

      --  We do not need to attach to one of the Global Final Lists
      --  the objects whose type is Finalize_Storage_Only

      if Finalize_Storage_Only (Typ)
        and then (Global_Flist_Ref (Flist_Ref)
          or else Entity (Constant_Value (RTE (RE_Garbage_Collected)))
                  = Standard_True)
      then
         Attach := Make_Integer_Literal (Loc, 0);
      end if;

      --  Special case for allocators: need initialization of the chain
      --  pointers. For the 0 case, reset them to null.

      if Allocator then
         pragma Assert (Nkind (Attach) = N_Integer_Literal);

         if Intval (Attach) = 0 then
            Set_Intval (Attach, Uint_4);
         end if;
      end if;

      --  Generate:
      --    Deep_Adjust (Flist_Ref, Ref, Attach);

      if Has_Controlled_Component (Utyp)
        or else Is_Class_Wide_Type (Typ)
      then
         if Is_Tagged_Type (Utyp) then
            Proc := Find_Prim_Op (Utyp, TSS_Deep_Adjust);

         else
            Proc := TSS (Utyp, TSS_Deep_Adjust);
         end if;

         Cref := Convert_View (Proc, Cref, 2);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Proc, Loc),
             Parameter_Associations =>
               New_List (Flist_Ref, Cref, Attach)));

      --  Generate:
      --    if With_Attach then
      --       Attach_To_Final_List (Ref, Flist_Ref);
      --    end if;
      --    Adjust (Ref);

      else -- Is_Controlled (Utyp)

         Proc  := Find_Prim_Op (Utyp, Name_Of (Adjust_Case));
         Cref  := Convert_View (Proc, Cref);
         Cref2 := New_Copy_Tree (Cref);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
           Name => New_Reference_To (Proc, Loc),
           Parameter_Associations => New_List (Cref2)));

         Append_To (Res, Make_Attach_Call (Cref, Flist_Ref, Attach));
      end if;

      return Res;
   end Make_Adjust_Call;

   ----------------------
   -- Make_Attach_Call --
   ----------------------

   --  Generate:
   --    System.FI.Attach_To_Final_List (Flist, Ref, Nb_Link)

   function Make_Attach_Call
     (Obj_Ref     : Node_Id;
      Flist_Ref   : Node_Id;
      With_Attach : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Obj_Ref);

   begin
      --  Optimization: If the number of links is statically '0', don't
      --  call the attach_proc.

      if Nkind (With_Attach) = N_Integer_Literal
        and then Intval (With_Attach) = Uint_0
      then
         return Make_Null_Statement (Loc);
      end if;

      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE_Attach_To_Final_List), Loc),
          Parameter_Associations => New_List (
            Flist_Ref,
            OK_Convert_To (RTE (RE_Finalizable), Obj_Ref),
            With_Attach));
   end Make_Attach_Call;

   ----------------
   -- Make_Clean --
   ----------------

   function Make_Clean
     (N                          : Node_Id;
      Clean                      : Entity_Id;
      Mark                       : Entity_Id;
      Flist                      : Entity_Id;
      Is_Task                    : Boolean;
      Is_Master                  : Boolean;
      Is_Protected_Subprogram    : Boolean;
      Is_Task_Allocation_Block   : Boolean;
      Is_Asynchronous_Call_Block : Boolean;
      Chained_Cleanup_Action     : Node_Id) return Node_Id
   is
      Loc  : constant Source_Ptr := Sloc (Clean);
      Stmt : constant List_Id    := New_List;

      Sbody        : Node_Id;
      Spec         : Node_Id;
      Name         : Node_Id;
      Param        : Node_Id;
      Param_Type   : Entity_Id;
      Pid          : Entity_Id := Empty;
      Cancel_Param : Entity_Id;

   begin
      if Is_Task then
         if Restricted_Profile then
            Append_To
              (Stmt, Build_Runtime_Call (Loc, RE_Complete_Restricted_Task));
         else
            Append_To (Stmt, Build_Runtime_Call (Loc, RE_Complete_Task));
         end if;

      elsif Is_Master then
         if Restriction_Active (No_Task_Hierarchy) = False then
            Append_To (Stmt, Build_Runtime_Call (Loc, RE_Complete_Master));
         end if;

      elsif Is_Protected_Subprogram then

         --  Add statements to the cleanup handler of the (ordinary)
         --  subprogram expanded to implement a protected subprogram,
         --  unlocking the protected object parameter and undeferring abort.
         --  If this is a protected procedure, and the object contains
         --  entries, this also calls the entry service routine.

         --  NOTE: This cleanup handler references _object, a parameter
         --        to the procedure.

         --  Find the _object parameter representing the protected object

         Spec := Parent (Corresponding_Spec (N));

         Param := First (Parameter_Specifications (Spec));
         loop
            Param_Type := Etype (Parameter_Type (Param));

            if Ekind (Param_Type) = E_Record_Type then
               Pid := Corresponding_Concurrent_Type (Param_Type);
            end if;

            exit when No (Param) or else Present (Pid);
            Next (Param);
         end loop;

         pragma Assert (Present (Param));

         --  If the associated protected object declares entries,
         --  a protected procedure has to service entry queues.
         --  In this case, add

         --  Service_Entries (_object._object'Access);

         --  _object is the record used to implement the protected object.
         --  It is a parameter to the protected subprogram.

         if Nkind (Specification (N)) = N_Procedure_Specification
           and then Has_Entries (Pid)
         then
            case Corresponding_Runtime_Package (Pid) is
               when System_Tasking_Protected_Objects_Entries =>
                  Name := New_Reference_To (RTE (RE_Service_Entries), Loc);

               when System_Tasking_Protected_Objects_Single_Entry =>
                  Name := New_Reference_To (RTE (RE_Service_Entry), Loc);

               when others =>
                  raise Program_Error;
            end case;

            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name => Name,
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      Make_Selected_Component (Loc,
                        Prefix => New_Reference_To (
                          Defining_Identifier (Param), Loc),
                        Selector_Name =>
                          Make_Identifier (Loc, Name_uObject)),
                    Attribute_Name => Name_Unchecked_Access))));

         else
            --  Unlock (_object._object'Access);

            --  object is the record used to implement the protected object.
            --  It is a parameter to the protected subprogram.

            case Corresponding_Runtime_Package (Pid) is
               when System_Tasking_Protected_Objects_Entries =>
                  Name := New_Reference_To (RTE (RE_Unlock_Entries), Loc);

               when System_Tasking_Protected_Objects_Single_Entry =>
                  Name := New_Reference_To (RTE (RE_Unlock_Entry), Loc);

               when System_Tasking_Protected_Objects =>
                  Name := New_Reference_To (RTE (RE_Unlock), Loc);

               when others =>
                  raise Program_Error;
            end case;

            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name => Name,
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      Make_Selected_Component (Loc,
                        Prefix =>
                          New_Reference_To (Defining_Identifier (Param), Loc),
                        Selector_Name =>
                          Make_Identifier (Loc, Name_uObject)),
                    Attribute_Name => Name_Unchecked_Access))));
         end if;

         if Abort_Allowed then

            --  Abort_Undefer;

            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (
                    RTE (RE_Abort_Undefer), Loc),
                Parameter_Associations => Empty_List));
         end if;

      elsif Is_Task_Allocation_Block then

         --  Add a call to Expunge_Unactivated_Tasks to the cleanup
         --  handler of a block created for the dynamic allocation of
         --  tasks:

         --  Expunge_Unactivated_Tasks (_chain);

         --  where _chain is the list of tasks created by the allocator
         --  but not yet activated. This list will be empty unless
         --  the block completes abnormally.

         --  This only applies to dynamically allocated tasks;
         --  other unactivated tasks are completed by Complete_Task or
         --  Complete_Master.

         --  NOTE: This cleanup handler references _chain, a local
         --        object.

         Append_To (Stmt,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Reference_To (
                 RTE (RE_Expunge_Unactivated_Tasks), Loc),
             Parameter_Associations => New_List (
               New_Reference_To (Activation_Chain_Entity (N), Loc))));

      elsif Is_Asynchronous_Call_Block then

         --  Add a call to attempt to cancel the asynchronous entry call
         --  whenever the block containing the abortable part is exited.

         --  NOTE: This cleanup handler references C, a local object

         --  Get the argument to the Cancel procedure
         Cancel_Param := Entry_Cancel_Parameter (Entity (Identifier (N)));

         --  If it is of type Communication_Block, this must be a
         --  protected entry call.

         if Is_RTE (Etype (Cancel_Param), RE_Communication_Block) then

            Append_To (Stmt,

            --  if Enqueued (Cancel_Parameter) then

              Make_Implicit_If_Statement (Clean,
                Condition => Make_Function_Call (Loc,
                  Name => New_Reference_To (
                    RTE (RE_Enqueued), Loc),
                  Parameter_Associations => New_List (
                    New_Reference_To (Cancel_Param, Loc))),
                Then_Statements => New_List (

            --  Cancel_Protected_Entry_Call (Cancel_Param);

                  Make_Procedure_Call_Statement (Loc,
                    Name => New_Reference_To (
                      RTE (RE_Cancel_Protected_Entry_Call), Loc),
                    Parameter_Associations => New_List (
                      New_Reference_To (Cancel_Param, Loc))))));

         --  Asynchronous delay

         elsif Is_RTE (Etype (Cancel_Param), RE_Delay_Block) then
            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Cancel_Async_Delay), Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Reference_To (Cancel_Param, Loc),
                    Attribute_Name => Name_Unchecked_Access))));

         --  Task entry call

         else
            --  Append call to Cancel_Task_Entry_Call (C);

            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (
                  RTE (RE_Cancel_Task_Entry_Call),
                  Loc),
                Parameter_Associations => New_List (
                  New_Reference_To (Cancel_Param, Loc))));

         end if;
      end if;

      if Present (Flist) then
         Append_To (Stmt,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Finalize_List), Loc),
             Parameter_Associations => New_List (
                    New_Reference_To (Flist, Loc))));
      end if;

      if Present (Mark) then
         Append_To (Stmt,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_SS_Release), Loc),
             Parameter_Associations => New_List (
                    New_Reference_To (Mark, Loc))));
      end if;

      if Present (Chained_Cleanup_Action) then
         Append_To (Stmt,
           Make_Procedure_Call_Statement (Loc,
             Name => Chained_Cleanup_Action));
      end if;

      Sbody :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Clean),

          Declarations  => New_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmt));

      if Present (Flist) or else Is_Task or else Is_Master then
         Wrap_Cleanup_Procedure (Sbody);
      end if;

      --  We do not want debug information for _Clean routines,
      --  since it just confuses the debugging operation unless
      --  we are debugging generated code.

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (Clean, True);
      end if;

      return Sbody;
   end Make_Clean;

   --------------------------
   -- Make_Deep_Array_Body --
   --------------------------

   --  Array components are initialized and adjusted in the normal order
   --  and finalized in the reverse order. Exceptions are handled and
   --  Program_Error is re-raise in the Adjust and Finalize case
   --  (RM 7.6.1(12)). Generate the following code :
   --
   --  procedure Deep_<P>   --  with <P> being Initialize or Adjust or Finalize
   --   (L : in out Finalizable_Ptr;
   --    V : in out Typ)
   --  is
   --  begin
   --     for J1 in             Typ'First (1) .. Typ'Last (1) loop
   --               ^ reverse ^  --  in the finalization case
   --        ...
   --           for J2 in Typ'First (n) .. Typ'Last (n) loop
   --                 Make_<P>_Call (Typ, V (J1, .. , Jn), L, V);
   --           end loop;
   --        ...
   --     end loop;
   --  exception                                --  not in the
   --     when others => raise Program_Error;   --     Initialize case
   --  end Deep_<P>;

   function Make_Deep_Array_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id) return List_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);

      Index_List : constant List_Id := New_List;
      --  Stores the list of references to the indexes (one per dimension)

      function One_Component return List_Id;
      --  Create one statement to initialize/adjust/finalize one array
      --  component, designated by a full set of indices.

      function One_Dimension (N : Int) return List_Id;
      --  Create loop to deal with one dimension of the array. The single
      --  statement in the body of the loop initializes the inner dimensions if
      --  any, or else a single component.

      -------------------
      -- One_Component --
      -------------------

      function One_Component return List_Id is
         Comp_Typ : constant Entity_Id := Component_Type (Typ);
         Comp_Ref : constant Node_Id :=
                      Make_Indexed_Component (Loc,
                        Prefix      => Make_Identifier (Loc, Name_V),
                        Expressions => Index_List);

      begin
         --  Set the etype of the component Reference, which is used to
         --  determine whether a conversion to a parent type is needed.

         Set_Etype (Comp_Ref, Comp_Typ);

         case Prim is
            when Initialize_Case =>
               return Make_Init_Call (Comp_Ref, Comp_Typ,
                        Make_Identifier (Loc, Name_L),
                        Make_Identifier (Loc, Name_B));

            when Adjust_Case =>
               return Make_Adjust_Call (Comp_Ref, Comp_Typ,
                        Make_Identifier (Loc, Name_L),
                        Make_Identifier (Loc, Name_B));

            when Finalize_Case =>
               return Make_Final_Call (Comp_Ref, Comp_Typ,
                        Make_Identifier (Loc, Name_B));
         end case;
      end One_Component;

      -------------------
      -- One_Dimension --
      -------------------

      function One_Dimension (N : Int) return List_Id is
         Index : Entity_Id;

      begin
         if N > Number_Dimensions (Typ) then
            return One_Component;

         else
            Index :=
              Make_Defining_Identifier (Loc, New_External_Name ('J', N));

            Append_To (Index_List, New_Reference_To (Index, Loc));

            return New_List (
              Make_Implicit_Loop_Statement (Typ,
                Identifier => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_V),
                            Attribute_Name  => Name_Range,
                            Expressions => New_List (
                              Make_Integer_Literal (Loc, N))),
                        Reverse_Present => Prim = Finalize_Case)),
                Statements => One_Dimension (N + 1)));
         end if;
      end One_Dimension;

   --  Start of processing for Make_Deep_Array_Body

   begin
      return One_Dimension (1);
   end Make_Deep_Array_Body;

   --------------------
   -- Make_Deep_Proc --
   --------------------

   --  Generate:
   --    procedure DEEP_<prim>
   --      (L : IN OUT Finalizable_Ptr;    -- not for Finalize
   --       V : IN OUT <typ>;
   --       B : IN Short_Short_Integer) is
   --    begin
   --       <stmts>;
   --    exception                   --  Finalize and Adjust Cases only
   --       raise Program_Error;     --  idem
   --    end DEEP_<prim>;

   function Make_Deep_Proc
     (Prim  : Final_Primitives;
      Typ   : Entity_Id;
      Stmts : List_Id) return Entity_Id
   is
      Loc       : constant Source_Ptr := Sloc (Typ);
      Formals   : List_Id;
      Proc_Name : Entity_Id;
      Handler   : List_Id := No_List;
      Type_B    : Entity_Id;

   begin
      if Prim = Finalize_Case then
         Formals := New_List;
         Type_B := Standard_Boolean;

      else
         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_L),
             In_Present          => True,
             Out_Present         => True,
             Parameter_Type      =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), Loc)));
         Type_B := Standard_Short_Short_Integer;
      end if;

      Append_To (Formals,
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
          In_Present          => True,
          Out_Present         => True,
          Parameter_Type      => New_Reference_To (Typ, Loc)));

      Append_To (Formals,
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_B),
          Parameter_Type      => New_Reference_To (Type_B, Loc)));

      if Prim = Finalize_Case or else Prim = Adjust_Case then
         Handler := New_List (Make_Handler_For_Ctrl_Operation (Loc));
      end if;

      Proc_Name :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name (Typ, Deep_Name_Of (Prim)));

      Discard_Node (
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Proc_Name,
              Parameter_Specifications => Formals),

          Declarations =>  Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements         => Stmts,
              Exception_Handlers => Handler)));

      return Proc_Name;
   end Make_Deep_Proc;

   ---------------------------
   -- Make_Deep_Record_Body --
   ---------------------------

   --  The Deep procedures call the appropriate Controlling proc on the
   --  the controller component. In the init case, it also attach the
   --  controller to the current finalization list.

   function Make_Deep_Record_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id) return List_Id
   is
      Loc            : constant Source_Ptr := Sloc (Typ);
      Controller_Typ : Entity_Id;
      Obj_Ref        : constant Node_Id := Make_Identifier (Loc, Name_V);
      Controller_Ref : constant Node_Id :=
                         Make_Selected_Component (Loc,
                           Prefix        => Obj_Ref,
                           Selector_Name =>
                             Make_Identifier (Loc, Name_uController));
      Res            : constant List_Id := New_List;

   begin
      if Is_Inherently_Limited_Type (Typ) then
         Controller_Typ := RTE (RE_Limited_Record_Controller);
      else
         Controller_Typ := RTE (RE_Record_Controller);
      end if;

      case Prim is
         when Initialize_Case =>
            Append_List_To (Res,
              Make_Init_Call (
                Ref          => Controller_Ref,
                Typ          => Controller_Typ,
                Flist_Ref    => Make_Identifier (Loc, Name_L),
                With_Attach  => Make_Identifier (Loc, Name_B)));

            --  When the type is also a controlled type by itself,
            --  initialize it and attach it to the finalization chain.

            if Is_Controlled (Typ) then
               Append_To (Res,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Reference_To (
                     Find_Prim_Op (Typ, Name_Of (Prim)), Loc),
                   Parameter_Associations =>
                     New_List (New_Copy_Tree (Obj_Ref))));

               Append_To (Res, Make_Attach_Call (
                 Obj_Ref      => New_Copy_Tree (Obj_Ref),
                 Flist_Ref    => Make_Identifier (Loc, Name_L),
                 With_Attach => Make_Identifier (Loc, Name_B)));
            end if;

         when Adjust_Case =>
            Append_List_To (Res,
              Make_Adjust_Call (Controller_Ref, Controller_Typ,
                Make_Identifier (Loc, Name_L),
                Make_Identifier (Loc, Name_B)));

            --  When the type is also a controlled type by itself,
            --  adjust it and attach it to the finalization chain.

            if Is_Controlled (Typ) then
               Append_To (Res,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Reference_To (
                     Find_Prim_Op (Typ, Name_Of (Prim)), Loc),
                   Parameter_Associations =>
                     New_List (New_Copy_Tree (Obj_Ref))));

               Append_To (Res, Make_Attach_Call (
                 Obj_Ref      => New_Copy_Tree (Obj_Ref),
                 Flist_Ref    => Make_Identifier (Loc, Name_L),
                 With_Attach => Make_Identifier (Loc, Name_B)));
            end if;

         when Finalize_Case =>
            if Is_Controlled (Typ) then
               Append_To (Res,
                 Make_Implicit_If_Statement (Obj_Ref,
                   Condition => Make_Identifier (Loc, Name_B),
                   Then_Statements => New_List (
                     Make_Procedure_Call_Statement (Loc,
                       Name => New_Reference_To (RTE (RE_Finalize_One), Loc),
                       Parameter_Associations => New_List (
                         OK_Convert_To (RTE (RE_Finalizable),
                           New_Copy_Tree (Obj_Ref))))),

                   Else_Statements => New_List (
                     Make_Procedure_Call_Statement (Loc,
                       Name => New_Reference_To (
                         Find_Prim_Op (Typ, Name_Of (Prim)), Loc),
                       Parameter_Associations =>
                        New_List (New_Copy_Tree (Obj_Ref))))));
            end if;

            Append_List_To (Res,
              Make_Final_Call (Controller_Ref, Controller_Typ,
                Make_Identifier (Loc, Name_B)));
      end case;
      return Res;
   end Make_Deep_Record_Body;

   ----------------------
   -- Make_Final_Call --
   ----------------------

   function Make_Final_Call
     (Ref         : Node_Id;
      Typ         : Entity_Id;
      With_Detach : Node_Id) return List_Id
   is
      Loc   : constant Source_Ptr := Sloc (Ref);
      Res   : constant List_Id    := New_List;
      Cref  : Node_Id;
      Cref2 : Node_Id;
      Proc  : Entity_Id;
      Utyp  : Entity_Id;

   begin
      if Is_Class_Wide_Type (Typ) then
         Utyp := Root_Type (Typ);
         Cref := Ref;

      elsif Is_Concurrent_Type (Typ) then
         Utyp := Corresponding_Record_Type (Typ);
         Cref := Convert_Concurrent (Ref, Typ);

      elsif Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
        and then Is_Concurrent_Type (Full_View (Typ))
      then
         Utyp := Corresponding_Record_Type (Full_View (Typ));
         Cref := Convert_Concurrent (Ref, Full_View (Typ));
      else
         Utyp := Typ;
         Cref := Ref;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));
      Set_Assignment_OK (Cref);

      --  Deal with non-tagged derivation of private views. If the parent is
      --  now known to be protected, the finalization routine is the one
      --  defined on the corresponding record of the ancestor (corresponding
      --  records do not automatically inherit operations, but maybe they
      --  should???)

      if Is_Untagged_Derivation (Typ) then
         if Is_Protected_Type (Typ) then
            Utyp := Corresponding_Record_Type (Root_Type (Base_Type (Typ)));
         else
            Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         end if;

         Cref := Unchecked_Convert_To (Utyp, Cref);

         --  We need to set Assignment_OK to prevent problems with unchecked
         --  conversions, where we do not want them to be converted back in the
         --  case of untagged record derivation (see code in Make_*_Call
         --  procedures for similar situations).

         Set_Assignment_OK (Cref);
      end if;

      --  If the underlying_type is a subtype, we are dealing with
      --  the completion of a private type. We need to access
      --  the base type and generate a conversion to it.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));
         Utyp := Base_Type (Utyp);
         Cref := Unchecked_Convert_To (Utyp, Cref);
      end if;

      --  Generate:
      --    Deep_Finalize (Ref, With_Detach);

      if Has_Controlled_Component (Utyp)
        or else Is_Class_Wide_Type (Typ)
      then
         if Is_Tagged_Type (Utyp) then
            Proc := Find_Prim_Op (Utyp, TSS_Deep_Finalize);
         else
            Proc := TSS (Utyp, TSS_Deep_Finalize);
         end if;

         Cref := Convert_View (Proc, Cref);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Proc, Loc),
             Parameter_Associations =>
               New_List (Cref, With_Detach)));

      --  Generate:
      --    if With_Detach then
      --       Finalize_One (Ref);
      --    else
      --       Finalize (Ref);
      --    end if;

      else
         Proc := Find_Prim_Op (Utyp, Name_Of (Finalize_Case));

         if Chars (With_Detach) = Chars (Standard_True) then
            Append_To (Res,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Finalize_One), Loc),
                Parameter_Associations => New_List (
                  OK_Convert_To (RTE (RE_Finalizable), Cref))));

         elsif Chars (With_Detach) = Chars (Standard_False) then
            Append_To (Res,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (Proc, Loc),
                Parameter_Associations =>
                  New_List (Convert_View (Proc, Cref))));

         else
            Cref2 := New_Copy_Tree (Cref);
            Append_To (Res,
              Make_Implicit_If_Statement (Ref,
                Condition => With_Detach,
                Then_Statements => New_List (
                  Make_Procedure_Call_Statement (Loc,
                    Name => New_Reference_To (RTE (RE_Finalize_One), Loc),
                    Parameter_Associations => New_List (
                      OK_Convert_To (RTE (RE_Finalizable), Cref)))),

                Else_Statements => New_List (
                  Make_Procedure_Call_Statement (Loc,
                    Name => New_Reference_To (Proc, Loc),
                    Parameter_Associations =>
                      New_List (Convert_View (Proc, Cref2))))));
         end if;
      end if;

      return Res;
   end Make_Final_Call;

   -------------------------------------
   -- Make_Handler_For_Ctrl_Operation --
   -------------------------------------

   --  Generate:

   --    when E : others =>
   --      Raise_From_Controlled_Operation (X => E);

   --  or:

   --    when others =>
   --      raise Program_Error [finalize raised exception];

   --  depending on whether Raise_From_Controlled_Operation is available

   function Make_Handler_For_Ctrl_Operation
     (Loc : Source_Ptr) return Node_Id
   is
      E_Occ : Entity_Id;
      --  Choice parameter (for the first case above)

      Raise_Node : Node_Id;
      --  Procedure call or raise statement

   begin
      if RTE_Available (RE_Raise_From_Controlled_Operation) then

         --  Standard runtime: add choice parameter E, and pass it to
         --  Raise_From_Controlled_Operation so that the original exception
         --  name and message can be recorded in the exception message for
         --  Program_Error.

         E_Occ := Make_Defining_Identifier (Loc, Name_E);
         Raise_Node := Make_Procedure_Call_Statement (Loc,
                         Name =>
                           New_Occurrence_Of (
                             RTE (RE_Raise_From_Controlled_Operation), Loc),
                         Parameter_Associations => New_List (
                           New_Occurrence_Of (E_Occ, Loc)));

      else
         --  Restricted runtime: exception messages are not supported

         E_Occ := Empty;
         Raise_Node := Make_Raise_Program_Error (Loc,
                         Reason => PE_Finalize_Raised_Exception);
      end if;

      return Make_Implicit_Exception_Handler (Loc,
               Exception_Choices => New_List (Make_Others_Choice (Loc)),
               Choice_Parameter  => E_Occ,
               Statements        => New_List (Raise_Node));
   end Make_Handler_For_Ctrl_Operation;

   --------------------
   -- Make_Init_Call --
   --------------------

   function Make_Init_Call
     (Ref          : Node_Id;
      Typ          : Entity_Id;
      Flist_Ref    : Node_Id;
      With_Attach  : Node_Id) return List_Id
   is
      Loc     : constant Source_Ptr := Sloc (Ref);
      Is_Conc : Boolean;
      Res     : constant List_Id := New_List;
      Proc    : Entity_Id;
      Utyp    : Entity_Id;
      Cref    : Node_Id;
      Cref2   : Node_Id;
      Attach  : Node_Id := With_Attach;

   begin
      if Is_Concurrent_Type (Typ) then
         Is_Conc := True;
         Utyp    := Corresponding_Record_Type (Typ);
         Cref    := Convert_Concurrent (Ref, Typ);

      elsif Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
        and then Is_Concurrent_Type (Underlying_Type (Typ))
      then
         Is_Conc := True;
         Utyp    := Corresponding_Record_Type (Underlying_Type (Typ));
         Cref    := Convert_Concurrent (Ref, Underlying_Type (Typ));

      else
         Is_Conc := False;
         Utyp    := Typ;
         Cref    := Ref;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));

      Set_Assignment_OK (Cref);

      --  Deal with non-tagged derivation of private views

      if Is_Untagged_Derivation (Typ)
        and then not Is_Conc
      then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Cref := Unchecked_Convert_To (Utyp, Cref);
         Set_Assignment_OK (Cref);
         --  To prevent problems with UC see 1.156 RH ???
      end if;

      --  If the underlying_type is a subtype, we are dealing with
      --  the completion of a private type. We need to access
      --  the base type and generate a conversion to it.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));
         Utyp := Base_Type (Utyp);
         Cref := Unchecked_Convert_To (Utyp, Cref);
      end if;

      --  We do not need to attach to one of the Global Final Lists
      --  the objects whose type is Finalize_Storage_Only

      if Finalize_Storage_Only (Typ)
        and then (Global_Flist_Ref (Flist_Ref)
          or else Entity (Constant_Value (RTE (RE_Garbage_Collected)))
                  = Standard_True)
      then
         Attach := Make_Integer_Literal (Loc, 0);
      end if;

      --  Generate:
      --    Deep_Initialize (Ref, Flist_Ref);

      if Has_Controlled_Component (Utyp) then
         Proc := TSS (Utyp, Deep_Name_Of (Initialize_Case));

         Cref := Convert_View (Proc, Cref, 2);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Proc, Loc),
             Parameter_Associations => New_List (
               Node1 => Flist_Ref,
               Node2 => Cref,
               Node3 => Attach)));

      --  Generate:
      --    Attach_To_Final_List (Ref, Flist_Ref);
      --    Initialize (Ref);

      else -- Is_Controlled (Utyp)
         Proc  := Find_Prim_Op (Utyp, Name_Of (Initialize_Case));
         Check_Visibly_Controlled (Initialize_Case, Typ, Proc, Cref);

         Cref  := Convert_View (Proc, Cref);
         Cref2 := New_Copy_Tree (Cref);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
           Name => New_Reference_To (Proc, Loc),
           Parameter_Associations => New_List (Cref2)));

         Append_To (Res,
           Make_Attach_Call (Cref, Flist_Ref, Attach));
      end if;

      return Res;
   end Make_Init_Call;

   --------------------------
   -- Make_Transient_Block --
   --------------------------

   --  If finalization is involved, this function just wraps the instruction
   --  into a block whose name is the transient block entity, and then
   --  Expand_Cleanup_Actions (called on the expansion of the handled
   --  sequence of statements will do the necessary expansions for
   --  cleanups).

   function Make_Transient_Block
     (Loc    : Source_Ptr;
      Action : Node_Id) return Node_Id
   is
      Flist  : constant Entity_Id := Finalization_Chain_Entity (Current_Scope);
      Decls  : constant List_Id   := New_List;
      Par    : constant Node_Id   := Parent (Action);
      Instrs : constant List_Id   := New_List (Action);
      Blk    : Node_Id;

   begin
      --  Case where only secondary stack use is involved

      if VM_Target = No_VM
        and then Uses_Sec_Stack (Current_Scope)
        and then No (Flist)
        and then Nkind (Action) /= N_Simple_Return_Statement
        and then Nkind (Par) /= N_Exception_Handler
      then
         declare
            S  : Entity_Id;
            K  : Entity_Kind;

         begin
            S := Scope (Current_Scope);
            loop
               K := Ekind (S);

               --  At the outer level, no need to release the sec stack

               if S = Standard_Standard then
                  Set_Uses_Sec_Stack (Current_Scope, False);
                  exit;

               --  In a function, only release the sec stack if the
               --  function does not return on the sec stack otherwise
               --  the result may be lost. The caller is responsible for
               --  releasing.

               elsif K = E_Function then
                  Set_Uses_Sec_Stack (Current_Scope, False);

                  if not Requires_Transient_Scope (Etype (S)) then
                     Set_Uses_Sec_Stack (S, True);
                     Check_Restriction (No_Secondary_Stack, Action);
                  end if;

                  exit;

               --  In a loop or entry we should install a block encompassing
               --  all the construct. For now just release right away.

               elsif K = E_Loop or else K = E_Entry then
                  exit;

               --  In a procedure or a block, we release on exit of the
               --  procedure or block. ??? memory leak can be created by
               --  recursive calls.

               elsif K = E_Procedure
                 or else K = E_Block
               then
                  Set_Uses_Sec_Stack (S, True);
                  Check_Restriction (No_Secondary_Stack, Action);
                  Set_Uses_Sec_Stack (Current_Scope, False);
                  exit;

               else
                  S := Scope (S);
               end if;
            end loop;
         end;
      end if;

      --  Insert actions stuck in the transient scopes as well as all
      --  freezing nodes needed by those actions

      Insert_Actions_In_Scope_Around (Action);

      declare
         Last_Inserted : Node_Id := Prev (Action);
      begin
         if Present (Last_Inserted) then
            Freeze_All (First_Entity (Current_Scope), Last_Inserted);
         end if;
      end;

      Blk :=
        Make_Block_Statement (Loc,
          Identifier => New_Reference_To (Current_Scope, Loc),
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Instrs),
          Has_Created_Identifier => True);

      --  When the transient scope was established, we pushed the entry for
      --  the transient scope onto the scope stack, so that the scope was
      --  active for the installation of finalizable entities etc. Now we
      --  must remove this entry, since we have constructed a proper block.

      Pop_Scope;

      return Blk;
   end Make_Transient_Block;

   ------------------------
   -- Needs_Finalization --
   ------------------------

   function Needs_Finalization (T : Entity_Id) return Boolean is

      function Has_Some_Controlled_Component (Rec : Entity_Id) return Boolean;
      --  If type is not frozen yet, check explicitly among its components,
      --  because the Has_Controlled_Component flag is not necessarily set.

      -----------------------------------
      -- Has_Some_Controlled_Component --
      -----------------------------------

      function Has_Some_Controlled_Component
        (Rec : Entity_Id) return Boolean
      is
         Comp : Entity_Id;

      begin
         if Has_Controlled_Component (Rec) then
            return True;

         elsif not Is_Frozen (Rec) then
            if Is_Record_Type (Rec) then
               Comp := First_Entity (Rec);

               while Present (Comp) loop
                  if not Is_Type (Comp)
                    and then Needs_Finalization (Etype (Comp))
                  then
                     return True;
                  end if;

                  Next_Entity (Comp);
               end loop;

               return False;

            elsif Is_Array_Type (Rec) then
               return Needs_Finalization (Component_Type (Rec));

            else
               return Has_Controlled_Component (Rec);
            end if;
         else
            return False;
         end if;
      end Has_Some_Controlled_Component;

   --  Start of processing for Needs_Finalization

   begin
      --  Class-wide types must be treated as controlled because they may
      --  contain an extension that has controlled components

      --  We can skip this if finalization is not available

      return (Is_Class_Wide_Type (T)
                and then not In_Finalization_Root (T)
                and then not Restriction_Active (No_Finalization))
        or else Is_Controlled (T)
        or else Has_Some_Controlled_Component (T)
        or else (Is_Concurrent_Type (T)
                  and then Present (Corresponding_Record_Type (T))
                  and then Needs_Finalization (Corresponding_Record_Type (T)));
   end Needs_Finalization;

   ------------------------
   -- Node_To_Be_Wrapped --
   ------------------------

   function Node_To_Be_Wrapped return Node_Id is
   begin
      return Scope_Stack.Table (Scope_Stack.Last).Node_To_Be_Wrapped;
   end Node_To_Be_Wrapped;

   ----------------------------
   -- Set_Node_To_Be_Wrapped --
   ----------------------------

   procedure Set_Node_To_Be_Wrapped (N : Node_Id) is
   begin
      Scope_Stack.Table (Scope_Stack.Last).Node_To_Be_Wrapped := N;
   end Set_Node_To_Be_Wrapped;

   ----------------------------------
   -- Store_After_Actions_In_Scope --
   ----------------------------------

   procedure Store_After_Actions_In_Scope (L : List_Id) is
      SE : Scope_Stack_Entry renames Scope_Stack.Table (Scope_Stack.Last);

   begin
      if Present (SE.Actions_To_Be_Wrapped_After) then
         Insert_List_Before_And_Analyze (
          First (SE.Actions_To_Be_Wrapped_After), L);

      else
         SE.Actions_To_Be_Wrapped_After := L;

         if Is_List_Member (SE.Node_To_Be_Wrapped) then
            Set_Parent (L, Parent (SE.Node_To_Be_Wrapped));
         else
            Set_Parent (L, SE.Node_To_Be_Wrapped);
         end if;

         Analyze_List (L);
      end if;
   end Store_After_Actions_In_Scope;

   -----------------------------------
   -- Store_Before_Actions_In_Scope --
   -----------------------------------

   procedure Store_Before_Actions_In_Scope (L : List_Id) is
      SE : Scope_Stack_Entry renames Scope_Stack.Table (Scope_Stack.Last);

   begin
      if Present (SE.Actions_To_Be_Wrapped_Before) then
         Insert_List_After_And_Analyze (
           Last (SE.Actions_To_Be_Wrapped_Before), L);

      else
         SE.Actions_To_Be_Wrapped_Before := L;

         if Is_List_Member (SE.Node_To_Be_Wrapped) then
            Set_Parent (L, Parent (SE.Node_To_Be_Wrapped));
         else
            Set_Parent (L, SE.Node_To_Be_Wrapped);
         end if;

         Analyze_List (L);
      end if;
   end Store_Before_Actions_In_Scope;

   --------------------------------
   -- Wrap_Transient_Declaration --
   --------------------------------

   --  If a transient scope has been established during the processing of the
   --  Expression of an Object_Declaration, it is not possible to wrap the
   --  declaration into a transient block as usual case, otherwise the object
   --  would be itself declared in the wrong scope. Therefore, all entities (if
   --  any) defined in the transient block are moved to the proper enclosing
   --  scope, furthermore, if they are controlled variables they are finalized
   --  right after the declaration. The finalization list of the transient
   --  scope is defined as a renaming of the enclosing one so during their
   --  initialization they will be attached to the proper finalization
   --  list. For instance, the following declaration :

   --        X : Typ := F (G (A), G (B));

   --  (where G(A) and G(B) return controlled values, expanded as _v1 and _v2)
   --  is expanded into :

   --    _local_final_list_1 : Finalizable_Ptr;
   --    X : Typ := [ complex Expression-Action ];
   --    Finalize_One(_v1);
   --    Finalize_One (_v2);

   procedure Wrap_Transient_Declaration (N : Node_Id) is
      S              : Entity_Id;
      LC             : Entity_Id := Empty;
      Nodes          : List_Id;
      Loc            : constant Source_Ptr := Sloc (N);
      First_Decl_Loc : Source_Ptr;
      Enclosing_S    : Entity_Id;
      Uses_SS        : Boolean;
      Next_N         : constant Node_Id := Next (N);

   begin
      S := Current_Scope;
      Enclosing_S := Scope (S);

      --  Insert Actions kept in the Scope stack

      Insert_Actions_In_Scope_Around (N);

      --  If the declaration is consuming some secondary stack, mark the
      --  Enclosing scope appropriately.

      Uses_SS := Uses_Sec_Stack (S);
      Pop_Scope;

      --  Create a List controller and rename the final list to be its
      --  internal final pointer:
      --       Lxxx : Simple_List_Controller;
      --       Fxxx : Finalizable_Ptr renames Lxxx.F;

      if Present (Finalization_Chain_Entity (S)) then
         LC := Make_Defining_Identifier (Loc, New_Internal_Name ('L'));

         --  Use the Sloc of the first declaration of N's containing list, to
         --  maintain monotonicity of source-line stepping during debugging.

         First_Decl_Loc := Sloc (First (List_Containing (N)));

         Nodes := New_List (
           Make_Object_Declaration (First_Decl_Loc,
             Defining_Identifier => LC,
             Object_Definition   =>
               New_Reference_To
                 (RTE (RE_Simple_List_Controller), First_Decl_Loc)),

           Make_Object_Renaming_Declaration (First_Decl_Loc,
             Defining_Identifier => Finalization_Chain_Entity (S),
             Subtype_Mark =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), First_Decl_Loc),
             Name =>
               Make_Selected_Component (Loc,
                 Prefix        => New_Reference_To (LC, First_Decl_Loc),
                 Selector_Name => Make_Identifier (First_Decl_Loc, Name_F))));

         --  Put the declaration at the beginning of the declaration part
         --  to make sure it will be before all other actions that have been
         --  inserted before N.

         Insert_List_Before_And_Analyze (First (List_Containing (N)), Nodes);

         --  Generate the Finalization calls by finalizing the list controller
         --  right away. It will be re-finalized on scope exit but it doesn't
         --  matter. It cannot be done when the call initializes a renaming
         --  object though because in this case, the object becomes a pointer
         --  to the temporary and thus increases its life span. Ditto if this
         --  is a renaming of a component of an expression (such as a function
         --  call).

         --  Note that there is a problem if an actual in the call needs
         --  finalization, because in that case the call itself is the master,
         --  and the actual should be finalized on return from the call ???

         if Nkind (N) = N_Object_Renaming_Declaration
           and then Needs_Finalization (Etype (Defining_Identifier (N)))
         then
            null;

         elsif Nkind (N) = N_Object_Renaming_Declaration
           and then
             Nkind_In (Renamed_Object (Defining_Identifier (N)),
                       N_Selected_Component,
                       N_Indexed_Component)
           and then
             Needs_Finalization
               (Etype (Prefix (Renamed_Object (Defining_Identifier (N)))))
         then
            null;

         else
            Nodes :=
              Make_Final_Call
                (Ref         => New_Reference_To (LC, Loc),
                 Typ         => Etype (LC),
                 With_Detach => New_Reference_To (Standard_False, Loc));

            if Present (Next_N) then
               Insert_List_Before_And_Analyze (Next_N, Nodes);
            else
               Append_List_To (List_Containing (N), Nodes);
            end if;
         end if;
      end if;

      --  Put the local entities back in the enclosing scope, and set the
      --  Is_Public flag appropriately.

      Transfer_Entities (S, Enclosing_S);

      --  Mark the enclosing dynamic scope so that the sec stack will be
      --  released upon its exit unless this is a function that returns on
      --  the sec stack in which case this will be done by the caller.

      if VM_Target = No_VM and then Uses_SS then
         S := Enclosing_Dynamic_Scope (S);

         if Ekind (S) = E_Function
           and then Requires_Transient_Scope (Etype (S))
         then
            null;
         else
            Set_Uses_Sec_Stack (S);
            Check_Restriction (No_Secondary_Stack, N);
         end if;
      end if;
   end Wrap_Transient_Declaration;

   -------------------------------
   -- Wrap_Transient_Expression --
   -------------------------------

   --  Insert actions before <Expression>:

   --  (lines marked with <CTRL> are expanded only in presence of Controlled
   --   objects needing finalization)

   --     _E : Etyp;
   --     declare
   --        _M : constant Mark_Id := SS_Mark;
   --        Local_Final_List : System.FI.Finalizable_Ptr;    <CTRL>

   --        procedure _Clean is
   --        begin
   --           Abort_Defer;
   --           System.FI.Finalize_List (Local_Final_List);   <CTRL>
   --           SS_Release (M);
   --           Abort_Undefer;
   --        end _Clean;

   --     begin
   --        _E := <Expression>;
   --     at end
   --        _Clean;
   --     end;

   --    then expression is replaced by _E

   procedure Wrap_Transient_Expression (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      E    : constant Entity_Id  := Make_Temporary (Loc, 'E', N);
      Etyp : constant Entity_Id  := Etype (N);

      Expr : constant Node_Id := Relocate_Node (N);
      --  Capture this node because the call to Adjust_SCIL_Node can ???

   begin
      --  If the relocated node is a function call then check if some SCIL
      --  node references it and needs readjustment.

      if Generate_SCIL
        and then Nkind (N) = N_Function_Call
      then
         Adjust_SCIL_Node (N, Expr);
      end if;

      Insert_Actions (N, New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => E,
          Object_Definition   => New_Reference_To (Etyp, Loc)),

        Make_Transient_Block (Loc,
          Action =>
            Make_Assignment_Statement (Loc,
              Name       => New_Reference_To (E, Loc),
              Expression => Expr))));

      Rewrite (N, New_Reference_To (E, Loc));
      Analyze_And_Resolve (N, Etyp);
   end Wrap_Transient_Expression;

   ------------------------------
   -- Wrap_Transient_Statement --
   ------------------------------

   --  Transform <Instruction> into

   --  (lines marked with <CTRL> are expanded only in presence of Controlled
   --   objects needing finalization)

   --    declare
   --       _M : Mark_Id := SS_Mark;
   --       Local_Final_List : System.FI.Finalizable_Ptr ;    <CTRL>

   --       procedure _Clean is
   --       begin
   --          Abort_Defer;
   --          System.FI.Finalize_List (Local_Final_List);    <CTRL>
   --          SS_Release (_M);
   --          Abort_Undefer;
   --       end _Clean;

   --    begin
   --       <Instruction>;
   --    at end
   --       _Clean;
   --    end;

   procedure Wrap_Transient_Statement (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      New_Statement : constant Node_Id := Relocate_Node (N);

   begin
      --  If the relocated node is a procedure call then check if some SCIL
      --  node references it and needs readjustment.

      if Generate_SCIL
        and then Nkind (New_Statement) = N_Procedure_Call_Statement
      then
         Adjust_SCIL_Node (N, New_Statement);
      end if;

      Rewrite (N, Make_Transient_Block (Loc, New_Statement));

      --  With the scope stack back to normal, we can call analyze on the
      --  resulting block. At this point, the transient scope is being
      --  treated like a perfectly normal scope, so there is nothing
      --  special about it.

      --  Note: Wrap_Transient_Statement is called with the node already
      --  analyzed (i.e. Analyzed (N) is True). This is important, since
      --  otherwise we would get a recursive processing of the node when
      --  we do this Analyze call.

      Analyze (N);
   end Wrap_Transient_Statement;

end Exp_Ch7;
