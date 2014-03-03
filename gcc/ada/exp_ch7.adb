------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 7                               --
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

--  This package contains virtually all expansion mechanisms related to
--    - controlled types
--    - transient scopes

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Ch6;  use Exp_Ch6;
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
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
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

   --   1. In a simple statement (procedure call, assignment, ...). In this
   --      case the instruction is wrapped into a transient block. See
   --      Wrap_Transient_Statement for details.

   --   2. In an expression of a control structure (test in a IF statement,
   --      expression in a CASE statement, ...). See Wrap_Transient_Expression
   --      for details.

   --   3. In a expression of an object_declaration. No wrapping is possible
   --      here, so the finalization actions, if any, are done right after the
   --      declaration and the secondary stack deallocation is done in the
   --      proper enclosing scope. See Wrap_Transient_Declaration for details.

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

   procedure Insert_Actions_In_Scope_Around (N : Node_Id);
   --  Insert the before-actions kept in the scope stack before N, and the
   --  after-actions after N, which must be a member of a list.

   function Make_Transient_Block
     (Loc    : Source_Ptr;
      Action : Node_Id;
      Par    : Node_Id) return Node_Id;
   --  Action is a single statement or object declaration. Par is the proper
   --  parent of the generated block. Create a transient block whose name is
   --  the current scope and the only handled statement is Action. If Action
   --  involves controlled objects or secondary stack usage, the corresponding
   --  cleanup actions are performed at the end of the block.

   procedure Set_Node_To_Be_Wrapped (N : Node_Id);
   --  Set the field Node_To_Be_Wrapped of the current scope

   --  ??? The entire comment needs to be rewritten
   --  ??? which entire comment?

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

   --  Adjust Calls: They are generated on 2 occasions: (1) for declarations
   --  or dynamic allocations of Controlled objects with an initial value.
   --  (2) after an assignment. In the first case they are followed by an
   --  attachment to the final chain, in the second case they are not.

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

   type Final_Primitives is
     (Initialize_Case, Adjust_Case, Finalize_Case, Address_Case);
   --  This enumeration type is defined in order to ease sharing code for
   --  building finalization procedures for composite types.

   Name_Of      : constant array (Final_Primitives) of Name_Id :=
                    (Initialize_Case => Name_Initialize,
                     Adjust_Case     => Name_Adjust,
                     Finalize_Case   => Name_Finalize,
                     Address_Case    => Name_Finalize_Address);
   Deep_Name_Of : constant array (Final_Primitives) of TSS_Name_Type :=
                    (Initialize_Case => TSS_Deep_Initialize,
                     Adjust_Case     => TSS_Deep_Adjust,
                     Finalize_Case   => TSS_Deep_Finalize,
                     Address_Case    => TSS_Finalize_Address);

   procedure Build_Array_Deep_Procs (Typ : Entity_Id);
   --  Build the deep Initialize/Adjust/Finalize for a record Typ with
   --  Has_Controlled_Component set and store them using the TSS mechanism.

   function Build_Cleanup_Statements (N : Node_Id) return List_Id;
   --  Create the clean up calls for an asynchronous call block, task master,
   --  protected subprogram body, task allocation block or task body. If the
   --  context does not contain the above constructs, the routine returns an
   --  empty list.

   procedure Build_Finalizer
     (N           : Node_Id;
      Clean_Stmts : List_Id;
      Mark_Id     : Entity_Id;
      Top_Decls   : List_Id;
      Defer_Abort : Boolean;
      Fin_Id      : out Entity_Id);
   --  N may denote an accept statement, block, entry body, package body,
   --  package spec, protected body, subprogram body, or a task body. Create
   --  a procedure which contains finalization calls for all controlled objects
   --  declared in the declarative or statement region of N. The calls are
   --  built in reverse order relative to the original declarations. In the
   --  case of a task body, the routine delays the creation of the finalizer
   --  until all statements have been moved to the task body procedure.
   --  Clean_Stmts may contain additional context-dependent code used to abort
   --  asynchronous calls or complete tasks (see Build_Cleanup_Statements).
   --  Mark_Id is the secondary stack used in the current context or Empty if
   --  missing. Top_Decls is the list on which the declaration of the finalizer
   --  is attached in the non-package case. Defer_Abort indicates that the
   --  statements passed in perform actions that require abort to be deferred,
   --  such as for task termination. Fin_Id is the finalizer declaration
   --  entity.

   procedure Build_Finalizer_Call (N : Node_Id; Fin_Id : Entity_Id);
   --  N is a construct which contains a handled sequence of statements, Fin_Id
   --  is the entity of a finalizer. Create an At_End handler which covers the
   --  statements of N and calls Fin_Id. If the handled statement sequence has
   --  an exception handler, the statements will be wrapped in a block to avoid
   --  unwanted interaction with the new At_End handler.

   procedure Build_Record_Deep_Procs (Typ : Entity_Id);
   --  Build the deep Initialize/Adjust/Finalize for a record Typ with
   --  Has_Component_Component set and store them using the TSS mechanism.

   procedure Check_Visibly_Controlled
     (Prim : Final_Primitives;
      Typ  : Entity_Id;
      E    : in out Entity_Id;
      Cref : in out Node_Id);
   --  The controlled operation declared for a derived type may not be
   --  overriding, if the controlled operations of the parent type are hidden,
   --  for example when the parent is a private type whose full view is
   --  controlled. For other primitive operations we modify the name of the
   --  operation to indicate that it is not overriding, but this is not
   --  possible for Initialize, etc. because they have to be retrievable by
   --  name. Before generating the proper call to one of these operations we
   --  check whether Typ is known to be controlled at the point of definition.
   --  If it is not then we must retrieve the hidden operation of the parent
   --  and use it instead.  This is one case that might be solved more cleanly
   --  once Overriding pragmas or declarations are in place.

   function Convert_View
     (Proc : Entity_Id;
      Arg  : Node_Id;
      Ind  : Pos := 1) return Node_Id;
   --  Proc is one of the Initialize/Adjust/Finalize operations, and Arg is the
   --  argument being passed to it. Ind indicates which formal of procedure
   --  Proc we are trying to match. This function will, if necessary, generate
   --  a conversion between the partial and full view of Arg to match the type
   --  of the formal of Proc, or force a conversion to the class-wide type in
   --  the case where the operation is abstract.

   function Enclosing_Function (E : Entity_Id) return Entity_Id;
   --  Given an arbitrary entity, traverse the scope chain looking for the
   --  first enclosing function. Return Empty if no function was found.

   procedure Expand_Pragma_Initial_Condition (N : Node_Id);
   --  Subsidiary to the expansion of package specs and bodies. Generate a
   --  runtime check needed to verify the assumption introduced by pragma
   --  Initial_Condition. N denotes the package spec or body.

   function Make_Call
     (Loc        : Source_Ptr;
      Proc_Id    : Entity_Id;
      Param      : Node_Id;
      For_Parent : Boolean := False) return Node_Id;
   --  Subsidiary to Make_Adjust_Call and Make_Final_Call. Given the entity of
   --  routine [Deep_]Adjust / Finalize and an object parameter, create an
   --  adjust / finalization call. Flag For_Parent should be set when field
   --  _parent is being processed.

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
     (Prim     : Final_Primitives;
      Typ      : Entity_Id;
      Is_Local : Boolean := False) return List_Id;
   --  This function generates the list of statements for implementing
   --  Deep_Initialize, Deep_Adjust or Deep_Finalize procedures according to
   --  the first parameter, these procedures operate on the record type Typ.
   --  Flag Is_Local is used in conjunction with Deep_Finalize to designate
   --  whether the inner logic should be dictated by state counters.

   function Make_Finalize_Address_Stmts (Typ : Entity_Id) return List_Id;
   --  Subsidiary to Make_Finalize_Address_Body, Make_Deep_Array_Body and
   --  Make_Deep_Record_Body. Generate the following statements:
   --
   --    declare
   --       type Acc_Typ is access all Typ;
   --       for Acc_Typ'Storage_Size use 0;
   --    begin
   --       [Deep_]Finalize (Acc_Typ (V).all);
   --    end;

   ----------------------------
   -- Build_Array_Deep_Procs --
   ----------------------------

   procedure Build_Array_Deep_Procs (Typ : Entity_Id) is
   begin
      Set_TSS (Typ,
        Make_Deep_Proc
          (Prim  => Initialize_Case,
           Typ   => Typ,
           Stmts => Make_Deep_Array_Body (Initialize_Case, Typ)));

      if not Is_Limited_View (Typ) then
         Set_TSS (Typ,
           Make_Deep_Proc
             (Prim  => Adjust_Case,
              Typ   => Typ,
              Stmts => Make_Deep_Array_Body (Adjust_Case, Typ)));
      end if;

      --  Do not generate Deep_Finalize and Finalize_Address if finalization is
      --  suppressed since these routine will not be used.

      if not Restriction_Active (No_Finalization) then
         Set_TSS (Typ,
           Make_Deep_Proc
             (Prim  => Finalize_Case,
              Typ   => Typ,
              Stmts => Make_Deep_Array_Body (Finalize_Case, Typ)));

         --  Create TSS primitive Finalize_Address for non-VM targets. JVM and
         --  .NET do not support address arithmetic and unchecked conversions.

         if VM_Target = No_VM then
            Set_TSS (Typ,
              Make_Deep_Proc
                (Prim  => Address_Case,
                 Typ   => Typ,
                 Stmts => Make_Deep_Array_Body (Address_Case, Typ)));
         end if;
      end if;
   end Build_Array_Deep_Procs;

   ------------------------------
   -- Build_Cleanup_Statements --
   ------------------------------

   function Build_Cleanup_Statements (N : Node_Id) return List_Id is
      Is_Asynchronous_Call : constant Boolean :=
                               Nkind (N) = N_Block_Statement
                                 and then Is_Asynchronous_Call_Block (N);
      Is_Master            : constant Boolean :=
                               Nkind (N) /= N_Entry_Body
                                 and then Is_Task_Master (N);
      Is_Protected_Body    : constant Boolean :=
                               Nkind (N) = N_Subprogram_Body
                                 and then Is_Protected_Subprogram_Body (N);
      Is_Task_Allocation   : constant Boolean :=
                               Nkind (N) = N_Block_Statement
                                 and then Is_Task_Allocation_Block (N);
      Is_Task_Body         : constant Boolean :=
                               Nkind (Original_Node (N)) = N_Task_Body;

      Loc   : constant Source_Ptr := Sloc (N);
      Stmts : constant List_Id    := New_List;

   begin
      if Is_Task_Body then
         if Restricted_Profile then
            Append_To (Stmts,
              Build_Runtime_Call (Loc, RE_Complete_Restricted_Task));
         else
            Append_To (Stmts, Build_Runtime_Call (Loc, RE_Complete_Task));
         end if;

      elsif Is_Master then
         if Restriction_Active (No_Task_Hierarchy) = False then
            Append_To (Stmts, Build_Runtime_Call (Loc, RE_Complete_Master));
         end if;

      --  Add statements to unlock the protected object parameter and to
      --  undefer abort. If the context is a protected procedure and the object
      --  has entries, call the entry service routine.

      --  NOTE: The generated code references _object, a parameter to the
      --  procedure.

      elsif Is_Protected_Body then
         declare
            Spec      : constant Node_Id := Parent (Corresponding_Spec (N));
            Conc_Typ  : Entity_Id;
            Param     : Node_Id;
            Param_Typ : Entity_Id;

         begin
            --  Find the _object parameter representing the protected object

            Param := First (Parameter_Specifications (Spec));
            loop
               Param_Typ := Etype (Parameter_Type (Param));

               if Ekind (Param_Typ) = E_Record_Type then
                  Conc_Typ := Corresponding_Concurrent_Type (Param_Typ);
               end if;

               exit when No (Param) or else Present (Conc_Typ);
               Next (Param);
            end loop;

            pragma Assert (Present (Param));

            --  Historical note: In earlier versions of GNAT, there was code
            --  at this point to generate stuff to service entry queues. It is
            --  now abstracted in Build_Protected_Subprogram_Call_Cleanup.

            Build_Protected_Subprogram_Call_Cleanup
              (Specification (N), Conc_Typ, Loc, Stmts);
         end;

      --  Add a call to Expunge_Unactivated_Tasks for dynamically allocated
      --  tasks. Other unactivated tasks are completed by Complete_Task or
      --  Complete_Master.

      --  NOTE: The generated code references _chain, a local object

      elsif Is_Task_Allocation then

         --  Generate:
         --     Expunge_Unactivated_Tasks (_chain);

         --  where _chain is the list of tasks created by the allocator but not
         --  yet activated. This list will be empty unless the block completes
         --  abnormally.

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of
                 (RTE (RE_Expunge_Unactivated_Tasks), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Activation_Chain_Entity (N), Loc))));

      --  Attempt to cancel an asynchronous entry call whenever the block which
      --  contains the abortable part is exited.

      --  NOTE: The generated code references Cnn, a local object

      elsif Is_Asynchronous_Call then
         declare
            Cancel_Param : constant Entity_Id :=
                             Entry_Cancel_Parameter (Entity (Identifier (N)));

         begin
            --  If it is of type Communication_Block, this must be a protected
            --  entry call. Generate:

            --    if Enqueued (Cancel_Param) then
            --       Cancel_Protected_Entry_Call (Cancel_Param);
            --    end if;

            if Is_RTE (Etype (Cancel_Param), RE_Communication_Block) then
               Append_To (Stmts,
                 Make_If_Statement (Loc,
                   Condition =>
                     Make_Function_Call (Loc,
                       Name                   =>
                         New_Occurrence_Of (RTE (RE_Enqueued), Loc),
                       Parameter_Associations => New_List (
                         New_Occurrence_Of (Cancel_Param, Loc))),

                   Then_Statements => New_List (
                     Make_Procedure_Call_Statement (Loc,
                       Name =>
                         New_Occurrence_Of
                           (RTE (RE_Cancel_Protected_Entry_Call), Loc),
                         Parameter_Associations => New_List (
                           New_Occurrence_Of (Cancel_Param, Loc))))));

            --  Asynchronous delay, generate:
            --    Cancel_Async_Delay (Cancel_Param);

            elsif Is_RTE (Etype (Cancel_Param), RE_Delay_Block) then
               Append_To (Stmts,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (RTE (RE_Cancel_Async_Delay), Loc),
                   Parameter_Associations => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         New_Occurrence_Of (Cancel_Param, Loc),
                       Attribute_Name => Name_Unchecked_Access))));

            --  Task entry call, generate:
            --    Cancel_Task_Entry_Call (Cancel_Param);

            else
               Append_To (Stmts,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (RTE (RE_Cancel_Task_Entry_Call), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Cancel_Param, Loc))));
            end if;
         end;
      end if;

      return Stmts;
   end Build_Cleanup_Statements;

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

   -----------------------------
   -- Build_Exception_Handler --
   -----------------------------

   function Build_Exception_Handler
     (Data        : Finalization_Exception_Data;
      For_Library : Boolean := False) return Node_Id
   is
      Actuals      : List_Id;
      Proc_To_Call : Entity_Id;
      Except       : Node_Id;
      Stmts        : List_Id;

   begin
      pragma Assert (Present (Data.Raised_Id));

      if Exception_Extra_Info
        or else (For_Library and not Restricted_Profile)
      then
         if Exception_Extra_Info then

            --  Generate:

            --    Get_Current_Excep.all

            Except :=
              Make_Function_Call (Data.Loc,
                Name =>
                  Make_Explicit_Dereference (Data.Loc,
                    Prefix =>
                      New_Occurrence_Of
                        (RTE (RE_Get_Current_Excep), Data.Loc)));

         else
            --  Generate:

            --    null

            Except := Make_Null (Data.Loc);
         end if;

         if For_Library and then not Restricted_Profile then
            Proc_To_Call := RTE (RE_Save_Library_Occurrence);
            Actuals := New_List (Except);

         else
            Proc_To_Call := RTE (RE_Save_Occurrence);

            --  The dereference occurs only when Exception_Extra_Info is true,
            --  and therefore Except is not null.

            Actuals :=
              New_List (
                New_Occurrence_Of (Data.E_Id, Data.Loc),
                Make_Explicit_Dereference (Data.Loc, Except));
         end if;

         --  Generate:

         --    when others =>
         --       if not Raised_Id then
         --          Raised_Id := True;

         --          Save_Occurrence (E_Id, Get_Current_Excep.all.all);
         --            or
         --          Save_Library_Occurrence (Get_Current_Excep.all);
         --       end if;

         Stmts :=
           New_List (
             Make_If_Statement (Data.Loc,
               Condition       =>
                 Make_Op_Not (Data.Loc,
                   Right_Opnd => New_Occurrence_Of (Data.Raised_Id, Data.Loc)),

               Then_Statements => New_List (
                 Make_Assignment_Statement (Data.Loc,
                   Name       => New_Occurrence_Of (Data.Raised_Id, Data.Loc),
                   Expression => New_Occurrence_Of (Standard_True, Data.Loc)),

                 Make_Procedure_Call_Statement (Data.Loc,
                   Name                   =>
                     New_Occurrence_Of (Proc_To_Call, Data.Loc),
                   Parameter_Associations => Actuals))));

      else
         --  Generate:

         --    Raised_Id := True;

         Stmts := New_List (
           Make_Assignment_Statement (Data.Loc,
             Name       => New_Occurrence_Of (Data.Raised_Id, Data.Loc),
             Expression => New_Occurrence_Of (Standard_True, Data.Loc)));
      end if;

      --  Generate:

      --    when others =>

      return
        Make_Exception_Handler (Data.Loc,
          Exception_Choices => New_List (Make_Others_Choice (Data.Loc)),
          Statements        => Stmts);
   end Build_Exception_Handler;

   -------------------------------
   -- Build_Finalization_Master --
   -------------------------------

   procedure Build_Finalization_Master
     (Typ        : Entity_Id;
      Ins_Node   : Node_Id := Empty;
      Encl_Scope : Entity_Id := Empty)
   is
      Desig_Typ : constant Entity_Id := Directly_Designated_Type (Typ);
      Ptr_Typ   : Entity_Id := Root_Type (Base_Type (Typ));

      function In_Deallocation_Instance (E : Entity_Id) return Boolean;
      --  Determine whether entity E is inside a wrapper package created for
      --  an instance of Ada.Unchecked_Deallocation.

      ------------------------------
      -- In_Deallocation_Instance --
      ------------------------------

      function In_Deallocation_Instance (E : Entity_Id) return Boolean is
         Pkg : constant Entity_Id := Scope (E);
         Par : Node_Id := Empty;

      begin
         if Ekind (Pkg) = E_Package
           and then Present (Related_Instance (Pkg))
           and then Ekind (Related_Instance (Pkg)) = E_Procedure
         then
            Par := Generic_Parent (Parent (Related_Instance (Pkg)));

            return
              Present (Par)
                and then Chars (Par) = Name_Unchecked_Deallocation
                and then Chars (Scope (Par)) = Name_Ada
                and then Scope (Scope (Par)) = Standard_Standard;
         end if;

         return False;
      end In_Deallocation_Instance;

   --  Start of processing for Build_Finalization_Master

   begin
      if Is_Private_Type (Ptr_Typ)
        and then Present (Full_View (Ptr_Typ))
      then
         Ptr_Typ := Full_View (Ptr_Typ);
      end if;

      --  Certain run-time configurations and targets do not provide support
      --  for controlled types.

      if Restriction_Active (No_Finalization) then
         return;

      --  Do not process C, C++, CIL and Java types since it is assumend that
      --  the non-Ada side will handle their clean up.

      elsif Convention (Desig_Typ) = Convention_C
        or else Convention (Desig_Typ) = Convention_CIL
        or else Convention (Desig_Typ) = Convention_CPP
        or else Convention (Desig_Typ) = Convention_Java
      then
         return;

      --  Various machinery such as freezing may have already created a
      --  finalization master.

      elsif Present (Finalization_Master (Ptr_Typ)) then
         return;

      --  Do not process types that return on the secondary stack

      elsif Present (Associated_Storage_Pool (Ptr_Typ))
        and then Is_RTE (Associated_Storage_Pool (Ptr_Typ), RE_SS_Pool)
      then
         return;

      --  Do not process types which may never allocate an object

      elsif No_Pool_Assigned (Ptr_Typ) then
         return;

      --  Do not process access types coming from Ada.Unchecked_Deallocation
      --  instances. Even though the designated type may be controlled, the
      --  access type will never participate in allocation.

      elsif In_Deallocation_Instance (Ptr_Typ) then
         return;

      --  Ignore the general use of anonymous access types unless the context
      --  requires a finalization master.

      elsif Ekind (Ptr_Typ) = E_Anonymous_Access_Type
        and then No (Ins_Node)
      then
         return;

      --  Do not process non-library access types when restriction No_Nested_
      --  Finalization is in effect since masters are controlled objects.

      elsif Restriction_Active (No_Nested_Finalization)
        and then not Is_Library_Level_Entity (Ptr_Typ)
      then
         return;

      --  For .NET/JVM targets, allow the processing of access-to-controlled
      --  types where the designated type is explicitly derived from [Limited_]
      --  Controlled.

      elsif VM_Target /= No_VM
        and then not Is_Controlled (Desig_Typ)
      then
         return;

      --  Do not create finalization masters in SPARK mode because they result
      --  in unwanted expansion.

      --  More detail would be useful here ???

      elsif GNATprove_Mode then
         return;
      end if;

      declare
         Loc        : constant Source_Ptr := Sloc (Ptr_Typ);
         Actions    : constant List_Id := New_List;
         Fin_Mas_Id : Entity_Id;
         Pool_Id    : Entity_Id;

      begin
         --  Generate:
         --    Fnn : aliased Finalization_Master;

         --  Source access types use fixed master names since the master is
         --  inserted in the same source unit only once. The only exception to
         --  this are instances using the same access type as generic actual.

         if Comes_From_Source (Ptr_Typ)
           and then not Inside_A_Generic
         then
            Fin_Mas_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Ptr_Typ), "FM"));

         --  Internally generated access types use temporaries as their names
         --  due to possible collision with identical names coming from other
         --  packages.

         else
            Fin_Mas_Id := Make_Temporary (Loc, 'F');
         end if;

         Append_To (Actions,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Fin_Mas_Id,
             Aliased_Present     => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Finalization_Master), Loc)));

         --  Storage pool selection and attribute decoration of the generated
         --  master. Since .NET/JVM compilers do not support pools, this step
         --  is skipped.

         if VM_Target = No_VM then

            --  If the access type has a user-defined pool, use it as the base
            --  storage medium for the finalization pool.

            if Present (Associated_Storage_Pool (Ptr_Typ)) then
               Pool_Id := Associated_Storage_Pool (Ptr_Typ);

            --  The default choice is the global pool

            else
               Pool_Id := Get_Global_Pool_For_Access_Type (Ptr_Typ);
               Set_Associated_Storage_Pool (Ptr_Typ, Pool_Id);
            end if;

            --  Generate:
            --    Set_Base_Pool (Fnn, Pool_Id'Unchecked_Access);

            Append_To (Actions,
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_Set_Base_Pool), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Fin_Mas_Id, Loc),
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Occurrence_Of (Pool_Id, Loc),
                    Attribute_Name => Name_Unrestricted_Access))));
         end if;

         Set_Finalization_Master (Ptr_Typ, Fin_Mas_Id);

         --  A finalization master created for an anonymous access type must be
         --  inserted before a context-dependent node.

         if Present (Ins_Node) then
            Push_Scope (Encl_Scope);

            --  Treat use clauses as declarations and insert directly in front
            --  of them.

            if Nkind_In (Ins_Node, N_Use_Package_Clause,
                                   N_Use_Type_Clause)
            then
               Insert_List_Before_And_Analyze (Ins_Node, Actions);
            else
               Insert_Actions (Ins_Node, Actions);
            end if;

            Pop_Scope;

         elsif Ekind (Desig_Typ) = E_Incomplete_Type
           and then Has_Completion_In_Body (Desig_Typ)
         then
            Insert_Actions (Parent (Ptr_Typ), Actions);

         --  If the designated type is not yet frozen, then append the actions
         --  to that type's freeze actions. The actions need to be appended to
         --  whichever type is frozen later, similarly to what Freeze_Type does
         --  for appending the storage pool declaration for an access type.
         --  Otherwise, the call to Set_Storage_Pool_Ptr might reference the
         --  pool object before it's declared. However, it's not clear that
         --  this is exactly the right test to accomplish that here. ???

         elsif Present (Freeze_Node (Desig_Typ))
           and then not Analyzed (Freeze_Node (Desig_Typ))
         then
            Append_Freeze_Actions (Desig_Typ, Actions);

         elsif Present (Freeze_Node (Ptr_Typ))
           and then not Analyzed (Freeze_Node (Ptr_Typ))
         then
            Append_Freeze_Actions (Ptr_Typ, Actions);

         --  If there's a pool created locally for the access type, then we
         --  need to ensure that the master gets created after the pool object,
         --  because otherwise we can have a forward reference, so we force the
         --  master actions to be inserted and analyzed after the pool entity.
         --  Note that both the access type and its designated type may have
         --  already been frozen and had their freezing actions analyzed at
         --  this point. (This seems a little unclean.???)

         elsif VM_Target = No_VM
           and then Scope (Pool_Id) = Scope (Ptr_Typ)
         then
            Insert_List_After_And_Analyze (Parent (Pool_Id), Actions);

         else
            Insert_Actions (Parent (Ptr_Typ), Actions);
         end if;
      end;
   end Build_Finalization_Master;

   ---------------------
   -- Build_Finalizer --
   ---------------------

   procedure Build_Finalizer
     (N           : Node_Id;
      Clean_Stmts : List_Id;
      Mark_Id     : Entity_Id;
      Top_Decls   : List_Id;
      Defer_Abort : Boolean;
      Fin_Id      : out Entity_Id)
   is
      Acts_As_Clean    : constant Boolean :=
                           Present (Mark_Id)
                             or else
                               (Present (Clean_Stmts)
                                 and then Is_Non_Empty_List (Clean_Stmts));
      Exceptions_OK    : constant Boolean :=
                           not Restriction_Active (No_Exception_Propagation);
      For_Package_Body : constant Boolean := Nkind (N) = N_Package_Body;
      For_Package_Spec : constant Boolean := Nkind (N) = N_Package_Declaration;
      For_Package      : constant Boolean :=
                           For_Package_Body or else For_Package_Spec;
      Loc              : constant Source_Ptr := Sloc (N);

      --  NOTE: Local variable declarations are conservative and do not create
      --  structures right from the start. Entities and lists are created once
      --  it has been established that N has at least one controlled object.

      Components_Built : Boolean := False;
      --  A flag used to avoid double initialization of entities and lists. If
      --  the flag is set then the following variables have been initialized:
      --    Counter_Id
      --    Finalizer_Decls
      --    Finalizer_Stmts
      --    Jump_Alts

      Counter_Id  : Entity_Id := Empty;
      Counter_Val : Int       := 0;
      --  Name and value of the state counter

      Decls : List_Id := No_List;
      --  Declarative region of N (if available). If N is a package declaration
      --  Decls denotes the visible declarations.

      Finalizer_Data : Finalization_Exception_Data;
      --  Data for the exception

      Finalizer_Decls : List_Id := No_List;
      --  Local variable declarations. This list holds the label declarations
      --  of all jump block alternatives as well as the declaration of the
      --  local exception occurence and the raised flag:
      --     E : Exception_Occurrence;
      --     Raised : Boolean := False;
      --     L<counter value> : label;

      Finalizer_Insert_Nod : Node_Id := Empty;
      --  Insertion point for the finalizer body. Depending on the context
      --  (Nkind of N) and the individual grouping of controlled objects, this
      --  node may denote a package declaration or body, package instantiation,
      --  block statement or a counter update statement.

      Finalizer_Stmts : List_Id := No_List;
      --  The statement list of the finalizer body. It contains the following:
      --
      --    Abort_Defer;               --  Added if abort is allowed
      --    <call to Prev_At_End>      --  Added if exists
      --    <cleanup statements>       --  Added if Acts_As_Clean
      --    <jump block>               --  Added if Has_Ctrl_Objs
      --    <finalization statements>  --  Added if Has_Ctrl_Objs
      --    <stack release>            --  Added if Mark_Id exists
      --    Abort_Undefer;             --  Added if abort is allowed

      Has_Ctrl_Objs : Boolean := False;
      --  A general flag which denotes whether N has at least one controlled
      --  object.

      Has_Tagged_Types : Boolean := False;
      --  A general flag which indicates whether N has at least one library-
      --  level tagged type declaration.

      HSS : Node_Id := Empty;
      --  The sequence of statements of N (if available)

      Jump_Alts : List_Id := No_List;
      --  Jump block alternatives. Depending on the value of the state counter,
      --  the control flow jumps to a sequence of finalization statements. This
      --  list contains the following:
      --
      --     when <counter value> =>
      --        goto L<counter value>;

      Jump_Block_Insert_Nod : Node_Id := Empty;
      --  Specific point in the finalizer statements where the jump block is
      --  inserted.

      Last_Top_Level_Ctrl_Construct : Node_Id := Empty;
      --  The last controlled construct encountered when processing the top
      --  level lists of N. This can be a nested package, an instantiation or
      --  an object declaration.

      Prev_At_End : Entity_Id := Empty;
      --  The previous at end procedure of the handled statements block of N

      Priv_Decls : List_Id := No_List;
      --  The private declarations of N if N is a package declaration

      Spec_Id    : Entity_Id := Empty;
      Spec_Decls : List_Id   := Top_Decls;
      Stmts      : List_Id   := No_List;

      Tagged_Type_Stmts : List_Id := No_List;
      --  Contains calls to Ada.Tags.Unregister_Tag for all library-level
      --  tagged types found in N.

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Build_Components;
      --  Create all entites and initialize all lists used in the creation of
      --  the finalizer.

      procedure Create_Finalizer;
      --  Create the spec and body of the finalizer and insert them in the
      --  proper place in the tree depending on the context.

      procedure Process_Declarations
        (Decls      : List_Id;
         Preprocess : Boolean := False;
         Top_Level  : Boolean := False);
      --  Inspect a list of declarations or statements which may contain
      --  objects that need finalization. When flag Preprocess is set, the
      --  routine will simply count the total number of controlled objects in
      --  Decls. Flag Top_Level denotes whether the processing is done for
      --  objects in nested package declarations or instances.

      procedure Process_Object_Declaration
        (Decl         : Node_Id;
         Has_No_Init  : Boolean := False;
         Is_Protected : Boolean := False);
      --  Generate all the machinery associated with the finalization of a
      --  single object. Flag Has_No_Init is used to denote certain contexts
      --  where Decl does not have initialization call(s). Flag Is_Protected
      --  is set when Decl denotes a simple protected object.

      procedure Process_Tagged_Type_Declaration (Decl : Node_Id);
      --  Generate all the code necessary to unregister the external tag of a
      --  tagged type.

      ----------------------
      -- Build_Components --
      ----------------------

      procedure Build_Components is
         Counter_Decl     : Node_Id;
         Counter_Typ      : Entity_Id;
         Counter_Typ_Decl : Node_Id;

      begin
         pragma Assert (Present (Decls));

         --  This routine might be invoked several times when dealing with
         --  constructs that have two lists (either two declarative regions
         --  or declarations and statements). Avoid double initialization.

         if Components_Built then
            return;
         end if;

         Components_Built := True;

         if Has_Ctrl_Objs then

            --  Create entities for the counter, its type, the local exception
            --  and the raised flag.

            Counter_Id  := Make_Temporary (Loc, 'C');
            Counter_Typ := Make_Temporary (Loc, 'T');

            Finalizer_Decls := New_List;

            Build_Object_Declarations
              (Finalizer_Data, Finalizer_Decls, Loc, For_Package);

            --  Since the total number of controlled objects is always known,
            --  build a subtype of Natural with precise bounds. This allows
            --  the backend to optimize the case statement. Generate:
            --
            --    subtype Tnn is Natural range 0 .. Counter_Val;

            Counter_Typ_Decl :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => Counter_Typ,
                Subtype_Indication  =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark => New_Occurrence_Of (Standard_Natural, Loc),
                    Constraint   =>
                      Make_Range_Constraint (Loc,
                        Range_Expression =>
                          Make_Range (Loc,
                            Low_Bound  =>
                              Make_Integer_Literal (Loc, Uint_0),
                            High_Bound =>
                              Make_Integer_Literal (Loc, Counter_Val)))));

            --  Generate the declaration of the counter itself:
            --
            --    Counter : Integer := 0;

            Counter_Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Counter_Id,
                Object_Definition   => New_Occurrence_Of (Counter_Typ, Loc),
                Expression          => Make_Integer_Literal (Loc, 0));

            --  Set the type of the counter explicitly to prevent errors when
            --  examining object declarations later on.

            Set_Etype (Counter_Id, Counter_Typ);

            --  The counter and its type are inserted before the source
            --  declarations of N.

            Prepend_To (Decls, Counter_Decl);
            Prepend_To (Decls, Counter_Typ_Decl);

            --  The counter and its associated type must be manually analized
            --  since N has already been analyzed. Use the scope of the spec
            --  when inserting in a package.

            if For_Package then
               Push_Scope (Spec_Id);
               Analyze (Counter_Typ_Decl);
               Analyze (Counter_Decl);
               Pop_Scope;

            else
               Analyze (Counter_Typ_Decl);
               Analyze (Counter_Decl);
            end if;

            Jump_Alts := New_List;
         end if;

         --  If the context requires additional clean up, the finalization
         --  machinery is added after the clean up code.

         if Acts_As_Clean then
            Finalizer_Stmts       := Clean_Stmts;
            Jump_Block_Insert_Nod := Last (Finalizer_Stmts);
         else
            Finalizer_Stmts := New_List;
         end if;

         if Has_Tagged_Types then
            Tagged_Type_Stmts := New_List;
         end if;
      end Build_Components;

      ----------------------
      -- Create_Finalizer --
      ----------------------

      procedure Create_Finalizer is
         Body_Id    : Entity_Id;
         Fin_Body   : Node_Id;
         Fin_Spec   : Node_Id;
         Jump_Block : Node_Id;
         Label      : Node_Id;
         Label_Id   : Entity_Id;

         function New_Finalizer_Name return Name_Id;
         --  Create a fully qualified name of a package spec or body finalizer.
         --  The generated name is of the form: xx__yy__finalize_[spec|body].

         ------------------------
         -- New_Finalizer_Name --
         ------------------------

         function New_Finalizer_Name return Name_Id is
            procedure New_Finalizer_Name (Id : Entity_Id);
            --  Place "__<name-of-Id>" in the name buffer. If the identifier
            --  has a non-standard scope, process the scope first.

            ------------------------
            -- New_Finalizer_Name --
            ------------------------

            procedure New_Finalizer_Name (Id : Entity_Id) is
            begin
               if Scope (Id) = Standard_Standard then
                  Get_Name_String (Chars (Id));

               else
                  New_Finalizer_Name (Scope (Id));
                  Add_Str_To_Name_Buffer ("__");
                  Add_Str_To_Name_Buffer (Get_Name_String (Chars (Id)));
               end if;
            end New_Finalizer_Name;

         --  Start of processing for New_Finalizer_Name

         begin
            --  Create the fully qualified name of the enclosing scope

            New_Finalizer_Name (Spec_Id);

            --  Generate:
            --    __finalize_[spec|body]

            Add_Str_To_Name_Buffer ("__finalize_");

            if For_Package_Spec then
               Add_Str_To_Name_Buffer ("spec");
            else
               Add_Str_To_Name_Buffer ("body");
            end if;

            return Name_Find;
         end New_Finalizer_Name;

      --  Start of processing for Create_Finalizer

      begin
         --  Step 1: Creation of the finalizer name

         --  Packages must use a distinct name for their finalizers since the
         --  binder will have to generate calls to them by name. The name is
         --  of the following form:

         --    xx__yy__finalize_[spec|body]

         if For_Package then
            Fin_Id := Make_Defining_Identifier (Loc, New_Finalizer_Name);
            Set_Has_Qualified_Name       (Fin_Id);
            Set_Has_Fully_Qualified_Name (Fin_Id);

         --  The default name is _finalizer

         else
            Fin_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Name_uFinalizer));

            --  The visibility semantics of AT_END handlers force a strange
            --  separation of spec and body for stack-related finalizers:

            --     declare : Enclosing_Scope
            --        procedure _finalizer;
            --     begin
            --        <controlled objects>
            --        procedure _finalizer is
            --           ...
            --     at end
            --        _finalizer;
            --     end;

            --  Both spec and body are within the same construct and scope, but
            --  the body is part of the handled sequence of statements. This
            --  placement confuses the elaboration mechanism on targets where
            --  AT_END handlers are expanded into "when all others" handlers:

            --     exception
            --        when all others =>
            --           _finalizer;  --  appears to require elab checks
            --     at end
            --        _finalizer;
            --     end;

            --  Since the compiler guarantees that the body of a _finalizer is
            --  always inserted in the same construct where the AT_END handler
            --  resides, there is no need for elaboration checks.

            Set_Kill_Elaboration_Checks (Fin_Id);
         end if;

         --  Step 2: Creation of the finalizer specification

         --  Generate:
         --    procedure Fin_Id;

         Fin_Spec :=
           Make_Subprogram_Declaration (Loc,
             Specification =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Fin_Id));

         --  Step 3: Creation of the finalizer body

         if Has_Ctrl_Objs then

            --  Add L0, the default destination to the jump block

            Label_Id := Make_Identifier (Loc, New_External_Name ('L', 0));
            Set_Entity (Label_Id,
              Make_Defining_Identifier (Loc, Chars (Label_Id)));
            Label := Make_Label (Loc, Label_Id);

            --  Generate:
            --    L0 : label;

            Prepend_To (Finalizer_Decls,
              Make_Implicit_Label_Declaration (Loc,
                Defining_Identifier => Entity (Label_Id),
                Label_Construct     => Label));

            --  Generate:
            --    when others =>
            --       goto L0;

            Append_To (Jump_Alts,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices => New_List (Make_Others_Choice (Loc)),
                Statements       => New_List (
                  Make_Goto_Statement (Loc,
                    Name => New_Occurrence_Of (Entity (Label_Id), Loc)))));

            --  Generate:
            --    <<L0>>

            Append_To (Finalizer_Stmts, Label);

            --  Create the jump block which controls the finalization flow
            --  depending on the value of the state counter.

            Jump_Block :=
              Make_Case_Statement (Loc,
                Expression   => Make_Identifier (Loc, Chars (Counter_Id)),
                Alternatives => Jump_Alts);

            if Acts_As_Clean
              and then Present (Jump_Block_Insert_Nod)
            then
               Insert_After (Jump_Block_Insert_Nod, Jump_Block);
            else
               Prepend_To (Finalizer_Stmts, Jump_Block);
            end if;
         end if;

         --  Add the library-level tagged type unregistration machinery before
         --  the jump block circuitry. This ensures that external tags will be
         --  removed even if a finalization exception occurs at some point.

         if Has_Tagged_Types then
            Prepend_List_To (Finalizer_Stmts, Tagged_Type_Stmts);
         end if;

         --  Add a call to the previous At_End handler if it exists. The call
         --  must always precede the jump block.

         if Present (Prev_At_End) then
            Prepend_To (Finalizer_Stmts,
              Make_Procedure_Call_Statement (Loc, Prev_At_End));

            --  Clear the At_End handler since we have already generated the
            --  proper replacement call for it.

            Set_At_End_Proc (HSS, Empty);
         end if;

         --  Release the secondary stack mark

         if Present (Mark_Id) then
            Append_To (Finalizer_Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_SS_Release), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Mark_Id, Loc))));
         end if;

         --  Protect the statements with abort defer/undefer. This is only when
         --  aborts are allowed and the clean up statements require deferral or
         --  there are controlled objects to be finalized.

         if Abort_Allowed
           and then
             (Defer_Abort or else Has_Ctrl_Objs)
         then
            Prepend_To (Finalizer_Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (RTE (RE_Abort_Defer), Loc)));

            Append_To (Finalizer_Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (RTE (RE_Abort_Undefer), Loc)));
         end if;

         --  The local exception does not need to be reraised for library-level
         --  finalizers. Note that this action must be carried out after object
         --  clean up, secondary stack release and abort undeferral. Generate:

         --    if Raised and then not Abort then
         --       Raise_From_Controlled_Operation (E);
         --    end if;

         if Has_Ctrl_Objs
           and then Exceptions_OK
           and then not For_Package
         then
            Append_To (Finalizer_Stmts,
              Build_Raise_Statement (Finalizer_Data));
         end if;

         --  Generate:
         --    procedure Fin_Id is
         --       Abort  : constant Boolean := Triggered_By_Abort;
         --         <or>
         --       Abort  : constant Boolean := False;  --  no abort

         --       E      : Exception_Occurrence;  --  All added if flag
         --       Raised : Boolean := False;      --  Has_Ctrl_Objs is set
         --       L0     : label;
         --       ...
         --       Lnn    : label;

         --    begin
         --       Abort_Defer;               --  Added if abort is allowed
         --       <call to Prev_At_End>      --  Added if exists
         --       <cleanup statements>       --  Added if Acts_As_Clean
         --       <jump block>               --  Added if Has_Ctrl_Objs
         --       <finalization statements>  --  Added if Has_Ctrl_Objs
         --       <stack release>            --  Added if Mark_Id exists
         --       Abort_Undefer;             --  Added if abort is allowed
         --       <exception propagation>    --  Added if Has_Ctrl_Objs
         --    end Fin_Id;

         --  Create the body of the finalizer

         Body_Id := Make_Defining_Identifier (Loc, Chars (Fin_Id));

         if For_Package then
            Set_Has_Qualified_Name       (Body_Id);
            Set_Has_Fully_Qualified_Name (Body_Id);
         end if;

         Fin_Body :=
           Make_Subprogram_Body (Loc,
             Specification              =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Body_Id),
             Declarations               => Finalizer_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Finalizer_Stmts));

         --  Step 4: Spec and body insertion, analysis

         if For_Package then

            --  If the package spec has private declarations, the finalizer
            --  body must be added to the end of the list in order to have
            --  visibility of all private controlled objects.

            if For_Package_Spec then
               if Present (Priv_Decls) then
                  Append_To (Priv_Decls, Fin_Spec);
                  Append_To (Priv_Decls, Fin_Body);
               else
                  Append_To (Decls, Fin_Spec);
                  Append_To (Decls, Fin_Body);
               end if;

            --  For package bodies, both the finalizer spec and body are
            --  inserted at the end of the package declarations.

            else
               Append_To (Decls, Fin_Spec);
               Append_To (Decls, Fin_Body);
            end if;

            --  Push the name of the package

            Push_Scope (Spec_Id);
            Analyze (Fin_Spec);
            Analyze (Fin_Body);
            Pop_Scope;

         --  Non-package case

         else
            --  Create the spec for the finalizer. The At_End handler must be
            --  able to call the body which resides in a nested structure.

            --  Generate:
            --    declare
            --       procedure Fin_Id;                  --  Spec
            --    begin
            --       <objects and possibly statements>
            --       procedure Fin_Id is ...            --  Body
            --       <statements>
            --    at end
            --       Fin_Id;                            --  At_End handler
            --    end;

            pragma Assert (Present (Spec_Decls));

            Append_To (Spec_Decls, Fin_Spec);
            Analyze (Fin_Spec);

            --  When the finalizer acts solely as a clean up routine, the body
            --  is inserted right after the spec.

            if Acts_As_Clean
              and then not Has_Ctrl_Objs
            then
               Insert_After (Fin_Spec, Fin_Body);

            --  In all other cases the body is inserted after either:
            --
            --    1) The counter update statement of the last controlled object
            --    2) The last top level nested controlled package
            --    3) The last top level controlled instantiation

            else
               --  Manually freeze the spec. This is somewhat of a hack because
               --  a subprogram is frozen when its body is seen and the freeze
               --  node appears right before the body. However, in this case,
               --  the spec must be frozen earlier since the At_End handler
               --  must be able to call it.
               --
               --    declare
               --       procedure Fin_Id;               --  Spec
               --       [Fin_Id]                        --  Freeze node
               --    begin
               --       ...
               --    at end
               --       Fin_Id;                         --  At_End handler
               --    end;

               Ensure_Freeze_Node (Fin_Id);
               Insert_After (Fin_Spec, Freeze_Node (Fin_Id));
               Set_Is_Frozen (Fin_Id);

               --  In the case where the last construct to contain a controlled
               --  object is either a nested package, an instantiation or a
               --  freeze node, the body must be inserted directly after the
               --  construct.

               if Nkind_In (Last_Top_Level_Ctrl_Construct,
                              N_Freeze_Entity,
                              N_Package_Declaration,
                              N_Package_Body)
               then
                  Finalizer_Insert_Nod := Last_Top_Level_Ctrl_Construct;
               end if;

               Insert_After (Finalizer_Insert_Nod, Fin_Body);
            end if;

            Analyze (Fin_Body);
         end if;
      end Create_Finalizer;

      --------------------------
      -- Process_Declarations --
      --------------------------

      procedure Process_Declarations
        (Decls      : List_Id;
         Preprocess : Boolean := False;
         Top_Level  : Boolean := False)
      is
         Decl    : Node_Id;
         Expr    : Node_Id;
         Obj_Id  : Entity_Id;
         Obj_Typ : Entity_Id;
         Pack_Id : Entity_Id;
         Spec    : Node_Id;
         Typ     : Entity_Id;

         Old_Counter_Val : Int;
         --  This variable is used to determine whether a nested package or
         --  instance contains at least one controlled object.

         procedure Processing_Actions
           (Has_No_Init  : Boolean := False;
            Is_Protected : Boolean := False);
         --  Depending on the mode of operation of Process_Declarations, either
         --  increment the controlled object counter, set the controlled object
         --  flag and store the last top level construct or process the current
         --  declaration. Flag Has_No_Init is used to propagate scenarios where
         --  the current declaration may not have initialization proc(s). Flag
         --  Is_Protected should be set when the current declaration denotes a
         --  simple protected object.

         ------------------------
         -- Processing_Actions --
         ------------------------

         procedure Processing_Actions
           (Has_No_Init  : Boolean := False;
            Is_Protected : Boolean := False)
         is
         begin
            --  Library-level tagged type

            if Nkind (Decl) = N_Full_Type_Declaration then
               if Preprocess then
                  Has_Tagged_Types := True;

                  if Top_Level
                    and then No (Last_Top_Level_Ctrl_Construct)
                  then
                     Last_Top_Level_Ctrl_Construct := Decl;
                  end if;

               else
                  Process_Tagged_Type_Declaration (Decl);
               end if;

            --  Controlled object declaration

            else
               if Preprocess then
                  Counter_Val   := Counter_Val + 1;
                  Has_Ctrl_Objs := True;

                  if Top_Level
                    and then No (Last_Top_Level_Ctrl_Construct)
                  then
                     Last_Top_Level_Ctrl_Construct := Decl;
                  end if;

               else
                  Process_Object_Declaration (Decl, Has_No_Init, Is_Protected);
               end if;
            end if;
         end Processing_Actions;

      --  Start of processing for Process_Declarations

      begin
         if No (Decls) or else Is_Empty_List (Decls) then
            return;
         end if;

         --  Process all declarations in reverse order

         Decl := Last_Non_Pragma (Decls);
         while Present (Decl) loop

            --  Library-level tagged types

            if Nkind (Decl) = N_Full_Type_Declaration then
               Typ := Defining_Identifier (Decl);

               if Is_Tagged_Type (Typ)
                 and then Is_Library_Level_Entity (Typ)
                 and then Convention (Typ) = Convention_Ada
                 and then Present (Access_Disp_Table (Typ))
                 and then RTE_Available (RE_Register_Tag)
                 and then not No_Run_Time_Mode
                 and then not Is_Abstract_Type (Typ)
               then
                  Processing_Actions;
               end if;

            --  Regular object declarations

            elsif Nkind (Decl) = N_Object_Declaration then
               Obj_Id  := Defining_Identifier (Decl);
               Obj_Typ := Base_Type (Etype (Obj_Id));
               Expr    := Expression (Decl);

               --  Bypass any form of processing for objects which have their
               --  finalization disabled. This applies only to objects at the
               --  library level.

               if For_Package
                 and then Finalize_Storage_Only (Obj_Typ)
               then
                  null;

               --  Transient variables are treated separately in order to
               --  minimize the size of the generated code. For details, see
               --  Process_Transient_Objects.

               elsif Is_Processed_Transient (Obj_Id) then
                  null;

               --  The object is of the form:
               --    Obj : Typ [:= Expr];

               --  Do not process the incomplete view of a deferred constant.
               --  Do not consider tag-to-class-wide conversions.

               elsif not Is_Imported (Obj_Id)
                 and then Needs_Finalization (Obj_Typ)
                 and then not (Ekind (Obj_Id) = E_Constant
                                and then not Has_Completion (Obj_Id))
                 and then not Is_Tag_To_Class_Wide_Conversion (Obj_Id)
               then
                  Processing_Actions;

               --  The object is of the form:
               --    Obj : Access_Typ := Non_BIP_Function_Call'reference;

               --    Obj : Access_Typ :=
               --            BIP_Function_Call (BIPalloc => 2, ...)'reference;

               elsif Is_Access_Type (Obj_Typ)
                 and then Needs_Finalization
                            (Available_View (Designated_Type (Obj_Typ)))
                 and then Present (Expr)
                 and then
                   (Is_Secondary_Stack_BIP_Func_Call (Expr)
                     or else
                       (Is_Non_BIP_Func_Call (Expr)
                         and then not Is_Related_To_Func_Return (Obj_Id)))
               then
                  Processing_Actions (Has_No_Init => True);

               --  Processing for "hook" objects generated for controlled
               --  transients declared inside an Expression_With_Actions.

               elsif Is_Access_Type (Obj_Typ)
                 and then Present (Status_Flag_Or_Transient_Decl (Obj_Id))
                 and then Nkind (Status_Flag_Or_Transient_Decl (Obj_Id)) =
                                   N_Object_Declaration
                 and then Is_Finalizable_Transient
                            (Status_Flag_Or_Transient_Decl (Obj_Id), Decl)
               then
                  Processing_Actions (Has_No_Init => True);

               --  Process intermediate results of an if expression with one
               --  of the alternatives using a controlled function call.

               elsif Is_Access_Type (Obj_Typ)
                 and then Present (Status_Flag_Or_Transient_Decl (Obj_Id))
                 and then Nkind (Status_Flag_Or_Transient_Decl (Obj_Id)) =
                                                       N_Defining_Identifier
                 and then Present (Expr)
                 and then Nkind (Expr) = N_Null
               then
                  Processing_Actions (Has_No_Init => True);

               --  Simple protected objects which use type System.Tasking.
               --  Protected_Objects.Protection to manage their locks should
               --  be treated as controlled since they require manual cleanup.
               --  The only exception is illustrated in the following example:

               --     package Pkg is
               --        type Ctrl is new Controlled ...
               --        procedure Finalize (Obj : in out Ctrl);
               --        Lib_Obj : Ctrl;
               --     end Pkg;

               --     package body Pkg is
               --        protected Prot is
               --           procedure Do_Something (Obj : in out Ctrl);
               --        end Prot;

               --        protected body Prot is
               --           procedure Do_Something (Obj : in out Ctrl) is ...
               --        end Prot;

               --        procedure Finalize (Obj : in out Ctrl) is
               --        begin
               --           Prot.Do_Something (Obj);
               --        end Finalize;
               --     end Pkg;

               --  Since for the most part entities in package bodies depend on
               --  those in package specs, Prot's lock should be cleaned up
               --  first. The subsequent cleanup of the spec finalizes Lib_Obj.
               --  This act however attempts to invoke Do_Something and fails
               --  because the lock has disappeared.

               elsif Ekind (Obj_Id) = E_Variable
                 and then not In_Library_Level_Package_Body (Obj_Id)
                 and then
                   (Is_Simple_Protected_Type (Obj_Typ)
                     or else Has_Simple_Protected_Object (Obj_Typ))
               then
                  Processing_Actions (Is_Protected => True);
               end if;

            --  Specific cases of object renamings

            elsif Nkind (Decl) = N_Object_Renaming_Declaration then
               Obj_Id  := Defining_Identifier (Decl);
               Obj_Typ := Base_Type (Etype (Obj_Id));

               --  Bypass any form of processing for objects which have their
               --  finalization disabled. This applies only to objects at the
               --  library level.

               if For_Package
                 and then Finalize_Storage_Only (Obj_Typ)
               then
                  null;

               --  Return object of a build-in-place function. This case is
               --  recognized and marked by the expansion of an extended return
               --  statement (see Expand_N_Extended_Return_Statement).

               elsif Needs_Finalization (Obj_Typ)
                 and then Is_Return_Object (Obj_Id)
                 and then Present (Status_Flag_Or_Transient_Decl (Obj_Id))
               then
                  Processing_Actions (Has_No_Init => True);

               --  Detect a case where a source object has been initialized by
               --  a controlled function call or another object which was later
               --  rewritten as a class-wide conversion of Ada.Tags.Displace.

               --     Obj1 : CW_Type := Src_Obj;
               --     Obj2 : CW_Type := Function_Call (...);

               --     Obj1 : CW_Type renames (... Ada.Tags.Displace (Src_Obj));
               --     Tmp  : ... := Function_Call (...)'reference;
               --     Obj2 : CW_Type renames (... Ada.Tags.Displace (Tmp));

               elsif Is_Displacement_Of_Object_Or_Function_Result (Obj_Id) then
                  Processing_Actions (Has_No_Init => True);
               end if;

            --  Inspect the freeze node of an access-to-controlled type and
            --  look for a delayed finalization master. This case arises when
            --  the freeze actions are inserted at a later time than the
            --  expansion of the context. Since Build_Finalizer is never called
            --  on a single construct twice, the master will be ultimately
            --  left out and never finalized. This is also needed for freeze
            --  actions of designated types themselves, since in some cases the
            --  finalization master is associated with a designated type's
            --  freeze node rather than that of the access type (see handling
            --  for freeze actions in Build_Finalization_Master).

            elsif Nkind (Decl) = N_Freeze_Entity
              and then Present (Actions (Decl))
            then
               Typ := Entity (Decl);

               if (Is_Access_Type (Typ)
                    and then not Is_Access_Subprogram_Type (Typ)
                    and then Needs_Finalization
                               (Available_View (Designated_Type (Typ))))
                 or else (Is_Type (Typ) and then Needs_Finalization (Typ))
               then
                  Old_Counter_Val := Counter_Val;

                  --  Freeze nodes are considered to be identical to packages
                  --  and blocks in terms of nesting. The difference is that
                  --  a finalization master created inside the freeze node is
                  --  at the same nesting level as the node itself.

                  Process_Declarations (Actions (Decl), Preprocess);

                  --  The freeze node contains a finalization master

                  if Preprocess
                    and then Top_Level
                    and then No (Last_Top_Level_Ctrl_Construct)
                    and then Counter_Val > Old_Counter_Val
                  then
                     Last_Top_Level_Ctrl_Construct := Decl;
                  end if;
               end if;

            --  Nested package declarations, avoid generics

            elsif Nkind (Decl) = N_Package_Declaration then
               Spec    := Specification (Decl);
               Pack_Id := Defining_Unit_Name (Spec);

               if Nkind (Pack_Id) = N_Defining_Program_Unit_Name then
                  Pack_Id := Defining_Identifier (Pack_Id);
               end if;

               if Ekind (Pack_Id) /= E_Generic_Package then
                  Old_Counter_Val := Counter_Val;
                  Process_Declarations
                    (Private_Declarations (Spec), Preprocess);
                  Process_Declarations
                    (Visible_Declarations (Spec), Preprocess);

                  --  Either the visible or the private declarations contain a
                  --  controlled object. The nested package declaration is the
                  --  last such construct.

                  if Preprocess
                    and then Top_Level
                    and then No (Last_Top_Level_Ctrl_Construct)
                    and then Counter_Val > Old_Counter_Val
                  then
                     Last_Top_Level_Ctrl_Construct := Decl;
                  end if;
               end if;

            --  Nested package bodies, avoid generics

            elsif Nkind (Decl) = N_Package_Body then
               Spec := Corresponding_Spec (Decl);

               if Ekind (Spec) /= E_Generic_Package then
                  Old_Counter_Val := Counter_Val;
                  Process_Declarations (Declarations (Decl), Preprocess);

                  --  The nested package body is the last construct to contain
                  --  a controlled object.

                  if Preprocess
                    and then Top_Level
                    and then No (Last_Top_Level_Ctrl_Construct)
                    and then Counter_Val > Old_Counter_Val
                  then
                     Last_Top_Level_Ctrl_Construct := Decl;
                  end if;
               end if;

            --  Handle a rare case caused by a controlled transient variable
            --  created as part of a record init proc. The variable is wrapped
            --  in a block, but the block is not associated with a transient
            --  scope.

            elsif Nkind (Decl) = N_Block_Statement
              and then Inside_Init_Proc
            then
               Old_Counter_Val := Counter_Val;

               if Present (Handled_Statement_Sequence (Decl)) then
                  Process_Declarations
                    (Statements (Handled_Statement_Sequence (Decl)),
                     Preprocess);
               end if;

               Process_Declarations (Declarations (Decl), Preprocess);

               --  Either the declaration or statement list of the block has a
               --  controlled object.

               if Preprocess
                 and then Top_Level
                 and then No (Last_Top_Level_Ctrl_Construct)
                 and then Counter_Val > Old_Counter_Val
               then
                  Last_Top_Level_Ctrl_Construct := Decl;
               end if;

            --  Handle the case where the original context has been wrapped in
            --  a block to avoid interference between exception handlers and
            --  At_End handlers. Treat the block as transparent and process its
            --  contents.

            elsif Nkind (Decl) = N_Block_Statement
              and then Is_Finalization_Wrapper (Decl)
            then
               if Present (Handled_Statement_Sequence (Decl)) then
                  Process_Declarations
                    (Statements (Handled_Statement_Sequence (Decl)),
                     Preprocess);
               end if;

               Process_Declarations (Declarations (Decl), Preprocess);
            end if;

            Prev_Non_Pragma (Decl);
         end loop;
      end Process_Declarations;

      --------------------------------
      -- Process_Object_Declaration --
      --------------------------------

      procedure Process_Object_Declaration
        (Decl         : Node_Id;
         Has_No_Init  : Boolean := False;
         Is_Protected : Boolean := False)
      is
         Obj_Id    : constant Entity_Id := Defining_Identifier (Decl);
         Loc       : constant Source_Ptr := Sloc (Decl);
         Body_Ins  : Node_Id;
         Count_Ins : Node_Id;
         Fin_Call  : Node_Id;
         Fin_Stmts : List_Id;
         Inc_Decl  : Node_Id;
         Label     : Node_Id;
         Label_Id  : Entity_Id;
         Obj_Ref   : Node_Id;
         Obj_Typ   : Entity_Id;

         function Build_BIP_Cleanup_Stmts (Func_Id : Entity_Id) return Node_Id;
         --  Once it has been established that the current object is in fact a
         --  return object of build-in-place function Func_Id, generate the
         --  following cleanup code:
         --
         --    if BIPallocfrom > Secondary_Stack'Pos
         --      and then BIPfinalizationmaster /= null
         --    then
         --       declare
         --          type Ptr_Typ is access Obj_Typ;
         --          for Ptr_Typ'Storage_Pool
         --            use Base_Pool (BIPfinalizationmaster);
         --       begin
         --          Free (Ptr_Typ (Temp));
         --       end;
         --    end if;
         --
         --  Obj_Typ is the type of the current object, Temp is the original
         --  allocation which Obj_Id renames.

         procedure Find_Last_Init
           (Decl        : Node_Id;
            Typ         : Entity_Id;
            Last_Init   : out Node_Id;
            Body_Insert : out Node_Id);
         --  An object declaration has at least one and at most two init calls:
         --  that of the type and the user-defined initialize. Given an object
         --  declaration, Last_Init denotes the last initialization call which
         --  follows the declaration. Body_Insert denotes the place where the
         --  finalizer body could be potentially inserted.

         -----------------------------
         -- Build_BIP_Cleanup_Stmts --
         -----------------------------

         function Build_BIP_Cleanup_Stmts
           (Func_Id : Entity_Id) return Node_Id
         is
            Decls      : constant List_Id := New_List;
            Fin_Mas_Id : constant Entity_Id :=
                           Build_In_Place_Formal
                             (Func_Id, BIP_Finalization_Master);
            Obj_Typ    : constant Entity_Id := Etype (Func_Id);
            Temp_Id    : constant Entity_Id :=
                           Entity (Prefix (Name (Parent (Obj_Id))));

            Cond      : Node_Id;
            Free_Blk  : Node_Id;
            Free_Stmt : Node_Id;
            Pool_Id   : Entity_Id;
            Ptr_Typ   : Entity_Id;

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
            --  caller's finalization master.

            --  Generate:
            --    type Ptr_Typ is access Obj_Typ;

            Ptr_Typ := Make_Temporary (Loc, 'P');

            Append_To (Decls,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Ptr_Typ,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    Subtype_Indication => New_Occurrence_Of (Obj_Typ, Loc))));

            --  Perform minor decoration in order to set the master and the
            --  storage pool attributes.

            Set_Ekind (Ptr_Typ, E_Access_Type);
            Set_Finalization_Master     (Ptr_Typ, Fin_Mas_Id);
            Set_Associated_Storage_Pool (Ptr_Typ, Pool_Id);

            --  Create an explicit free statement. Note that the free uses the
            --  caller's pool expressed as a renaming.

            Free_Stmt :=
              Make_Free_Statement (Loc,
                Expression =>
                  Unchecked_Convert_To (Ptr_Typ,
                    New_Occurrence_Of (Temp_Id, Loc)));

            Set_Storage_Pool (Free_Stmt, Pool_Id);

            --  Create a block to house the dummy type and the instantiation as
            --  well as to perform the cleanup the temporary.

            --  Generate:
            --    declare
            --       <Decls>
            --    begin
            --       Free (Ptr_Typ (Temp_Id));
            --    end;

            Free_Blk :=
              Make_Block_Statement (Loc,
                Declarations               => Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (Free_Stmt)));

            --  Generate:
            --    if BIPfinalizationmaster /= null then

            Cond :=
              Make_Op_Ne (Loc,
                Left_Opnd  => New_Occurrence_Of (Fin_Mas_Id, Loc),
                Right_Opnd => Make_Null (Loc));

            --  For constrained or tagged results escalate the condition to
            --  include the allocation format. Generate:
            --
            --    if BIPallocform > Secondary_Stack'Pos
            --      and then BIPfinalizationmaster /= null
            --    then

            if not Is_Constrained (Obj_Typ)
              or else Is_Tagged_Type (Obj_Typ)
            then
               declare
                  Alloc : constant Entity_Id :=
                            Build_In_Place_Formal (Func_Id, BIP_Alloc_Form);
               begin
                  Cond :=
                    Make_And_Then (Loc,
                      Left_Opnd  =>
                        Make_Op_Gt (Loc,
                          Left_Opnd  => New_Occurrence_Of (Alloc, Loc),
                          Right_Opnd =>
                            Make_Integer_Literal (Loc,
                              UI_From_Int
                                (BIP_Allocation_Form'Pos (Secondary_Stack)))),

                      Right_Opnd => Cond);
               end;
            end if;

            --  Generate:
            --    if <Cond> then
            --       <Free_Blk>
            --    end if;

            return
              Make_If_Statement (Loc,
                Condition       => Cond,
                Then_Statements => New_List (Free_Blk));
         end Build_BIP_Cleanup_Stmts;

         --------------------
         -- Find_Last_Init --
         --------------------

         procedure Find_Last_Init
           (Decl        : Node_Id;
            Typ         : Entity_Id;
            Last_Init   : out Node_Id;
            Body_Insert : out Node_Id)
         is
            Nod_1 : Node_Id := Empty;
            Nod_2 : Node_Id := Empty;
            Utyp  : Entity_Id;

            function Is_Init_Call
              (N   : Node_Id;
               Typ : Entity_Id) return Boolean;
            --  Given an arbitrary node, determine whether N is a procedure
            --  call and if it is, try to match the name of the call with the
            --  [Deep_]Initialize proc of Typ.

            function Next_Suitable_Statement (Stmt : Node_Id) return Node_Id;
            --  Given a statement which is part of a list, return the next
            --  real statement while skipping over dynamic elab checks.

            ------------------
            -- Is_Init_Call --
            ------------------

            function Is_Init_Call
              (N   : Node_Id;
               Typ : Entity_Id) return Boolean
            is
            begin
               --  A call to [Deep_]Initialize is always direct

               if Nkind (N) = N_Procedure_Call_Statement
                 and then Nkind (Name (N)) = N_Identifier
               then
                  declare
                     Call_Ent  : constant Entity_Id := Entity (Name (N));
                     Deep_Init : constant Entity_Id :=
                                   TSS (Typ, TSS_Deep_Initialize);
                     Init      : Entity_Id := Empty;

                  begin
                     --  A type may have controlled components but not be
                     --  controlled.

                     if Is_Controlled (Typ) then
                        Init := Find_Prim_Op (Typ, Name_Initialize);

                        if Present (Init) then
                           Init := Ultimate_Alias (Init);
                        end if;
                     end if;

                     return
                       (Present (Deep_Init) and then Call_Ent = Deep_Init)
                         or else
                       (Present (Init)      and then Call_Ent = Init);
                  end;
               end if;

               return False;
            end Is_Init_Call;

            -----------------------------
            -- Next_Suitable_Statement --
            -----------------------------

            function Next_Suitable_Statement (Stmt : Node_Id) return Node_Id is
               Result : Node_Id := Next (Stmt);

            begin
               --  Skip over access-before-elaboration checks

               if Dynamic_Elaboration_Checks
                 and then Nkind (Result) = N_Raise_Program_Error
               then
                  Result := Next (Result);
               end if;

               return Result;
            end Next_Suitable_Statement;

         --  Start of processing for Find_Last_Init

         begin
            Last_Init   := Decl;
            Body_Insert := Empty;

            --  Object renamings and objects associated with controlled
            --  function results do not have initialization calls.

            if Has_No_Init then
               return;
            end if;

            if Is_Concurrent_Type (Typ) then
               Utyp := Corresponding_Record_Type (Typ);
            else
               Utyp := Typ;
            end if;

            if Is_Private_Type (Utyp)
              and then Present (Full_View (Utyp))
            then
               Utyp := Full_View (Utyp);
            end if;

            --  The init procedures are arranged as follows:

            --    Object : Controlled_Type;
            --    Controlled_TypeIP (Object);
            --    [[Deep_]Initialize (Object);]

            --  where the user-defined initialize may be optional or may appear
            --  inside a block when abort deferral is needed.

            Nod_1 := Next_Suitable_Statement (Decl);
            if Present (Nod_1) then
               Nod_2 := Next_Suitable_Statement (Nod_1);

               --  The statement following an object declaration is always a
               --  call to the type init proc.

               Last_Init := Nod_1;
            end if;

            --  Optional user-defined init or deep init processing

            if Present (Nod_2) then

               --  The statement following the type init proc may be a block
               --  statement in cases where abort deferral is required.

               if Nkind (Nod_2) = N_Block_Statement then
                  declare
                     HSS  : constant Node_Id :=
                              Handled_Statement_Sequence (Nod_2);
                     Stmt : Node_Id;

                  begin
                     if Present (HSS)
                       and then Present (Statements (HSS))
                     then
                        Stmt := First (Statements (HSS));

                        --  Examine individual block statements and locate the
                        --  call to [Deep_]Initialze.

                        while Present (Stmt) loop
                           if Is_Init_Call (Stmt, Utyp) then
                              Last_Init   := Stmt;
                              Body_Insert := Nod_2;

                              exit;
                           end if;

                           Next (Stmt);
                        end loop;
                     end if;
                  end;

               elsif Is_Init_Call (Nod_2, Utyp) then
                  Last_Init := Nod_2;
               end if;
            end if;
         end Find_Last_Init;

      --  Start of processing for Process_Object_Declaration

      begin
         Obj_Ref := New_Occurrence_Of (Obj_Id, Loc);
         Obj_Typ := Base_Type (Etype (Obj_Id));

         --  Handle access types

         if Is_Access_Type (Obj_Typ) then
            Obj_Ref := Make_Explicit_Dereference (Loc, Obj_Ref);
            Obj_Typ := Directly_Designated_Type (Obj_Typ);
         end if;

         Set_Etype (Obj_Ref, Obj_Typ);

         --  Set a new value for the state counter and insert the statement
         --  after the object declaration. Generate:
         --
         --    Counter := <value>;

         Inc_Decl :=
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Counter_Id, Loc),
             Expression => Make_Integer_Literal (Loc, Counter_Val));

         --  Insert the counter after all initialization has been done. The
         --  place of insertion depends on the context. If an object is being
         --  initialized via an aggregate, then the counter must be inserted
         --  after the last aggregate assignment.

         if Ekind (Obj_Id) = E_Variable
           and then Present (Last_Aggregate_Assignment (Obj_Id))
         then
            Count_Ins := Last_Aggregate_Assignment (Obj_Id);
            Body_Ins  := Empty;

         --  In all other cases the counter is inserted after the last call to
         --  either [Deep_]Initialize or the type specific init proc.

         else
            Find_Last_Init (Decl, Obj_Typ, Count_Ins, Body_Ins);
         end if;

         Insert_After (Count_Ins, Inc_Decl);
         Analyze (Inc_Decl);

         --  If the current declaration is the last in the list, the finalizer
         --  body needs to be inserted after the set counter statement for the
         --  current object declaration. This is complicated by the fact that
         --  the set counter statement may appear in abort deferred block. In
         --  that case, the proper insertion place is after the block.

         if No (Finalizer_Insert_Nod) then

            --  Insertion after an abort deffered block

            if Present (Body_Ins) then
               Finalizer_Insert_Nod := Body_Ins;
            else
               Finalizer_Insert_Nod := Inc_Decl;
            end if;
         end if;

         --  Create the associated label with this object, generate:
         --
         --    L<counter> : label;

         Label_Id :=
           Make_Identifier (Loc, New_External_Name ('L', Counter_Val));
         Set_Entity
           (Label_Id, Make_Defining_Identifier (Loc, Chars (Label_Id)));
         Label := Make_Label (Loc, Label_Id);

         Prepend_To (Finalizer_Decls,
           Make_Implicit_Label_Declaration (Loc,
             Defining_Identifier => Entity (Label_Id),
             Label_Construct     => Label));

         --  Create the associated jump with this object, generate:
         --
         --    when <counter> =>
         --       goto L<counter>;

         Prepend_To (Jump_Alts,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (
               Make_Integer_Literal (Loc, Counter_Val)),
             Statements       => New_List (
               Make_Goto_Statement (Loc,
                 Name => New_Occurrence_Of (Entity (Label_Id), Loc)))));

         --  Insert the jump destination, generate:
         --
         --     <<L<counter>>>

         Append_To (Finalizer_Stmts, Label);

         --  Processing for simple protected objects. Such objects require
         --  manual finalization of their lock managers.

         if Is_Protected then
            Fin_Stmts := No_List;

            if Is_Simple_Protected_Type (Obj_Typ) then
               Fin_Call := Cleanup_Protected_Object (Decl, Obj_Ref);

               if Present (Fin_Call) then
                  Fin_Stmts := New_List (Fin_Call);
               end if;

            elsif Has_Simple_Protected_Object (Obj_Typ) then
               if Is_Record_Type (Obj_Typ) then
                  Fin_Stmts := Cleanup_Record (Decl, Obj_Ref, Obj_Typ);
               elsif Is_Array_Type (Obj_Typ) then
                  Fin_Stmts := Cleanup_Array (Decl, Obj_Ref, Obj_Typ);
               end if;
            end if;

            --  Generate:
            --    begin
            --       System.Tasking.Protected_Objects.Finalize_Protection
            --         (Obj._object);

            --    exception
            --       when others =>
            --          null;
            --    end;

            if Present (Fin_Stmts) then
               Append_To (Finalizer_Stmts,
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements         => Fin_Stmts,

                       Exception_Handlers => New_List (
                         Make_Exception_Handler (Loc,
                           Exception_Choices => New_List (
                             Make_Others_Choice (Loc)),

                           Statements     => New_List (
                             Make_Null_Statement (Loc)))))));
            end if;

         --  Processing for regular controlled objects

         else
            --  Generate:
            --    [Deep_]Finalize (Obj);  --  No_Exception_Propagation

            --    begin                   --  Exception handlers allowed
            --       [Deep_]Finalize (Obj);

            --    exception
            --       when Id : others =>
            --          if not Raised then
            --             Raised := True;
            --             Save_Occurrence (E, Id);
            --          end if;
            --    end;

            Fin_Call :=
              Make_Final_Call (
                Obj_Ref => Obj_Ref,
                Typ     => Obj_Typ);

            --  For CodePeer, the exception handlers normally generated here
            --  generate complex flowgraphs which result in capacity problems.
            --  Omitting these handlers for CodePeer is justified as follows:

            --    If a handler is dead, then omitting it is surely ok

            --    If a handler is live, then CodePeer should flag the
            --      potentially-exception-raising construct that causes it
            --      to be live. That is what we are interested in, not what
            --      happens after the exception is raised.

            if Exceptions_OK and not CodePeer_Mode then
               Fin_Stmts := New_List (
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (Fin_Call),

                    Exception_Handlers => New_List (
                      Build_Exception_Handler
                        (Finalizer_Data, For_Package)))));

            --  When exception handlers are prohibited, the finalization call
            --  appears unprotected. Any exception raised during finalization
            --  will bypass the circuitry which ensures the cleanup of all
            --  remaining objects.

            else
               Fin_Stmts := New_List (Fin_Call);
            end if;

            --  If we are dealing with a return object of a build-in-place
            --  function, generate the following cleanup statements:

            --    if BIPallocfrom > Secondary_Stack'Pos
            --      and then BIPfinalizationmaster /= null
            --    then
            --       declare
            --          type Ptr_Typ is access Obj_Typ;
            --          for Ptr_Typ'Storage_Pool use
            --                Base_Pool (BIPfinalizationmaster.all).all;
            --       begin
            --          Free (Ptr_Typ (Temp));
            --       end;
            --    end if;
            --
            --  The generated code effectively detaches the temporary from the
            --  caller finalization master and deallocates the object. This is
            --  disabled on .NET/JVM because pools are not supported.

            if VM_Target = No_VM and then Is_Return_Object (Obj_Id) then
               declare
                  Func_Id : constant Entity_Id := Enclosing_Function (Obj_Id);
               begin
                  if Is_Build_In_Place_Function (Func_Id)
                    and then Needs_BIP_Finalization_Master (Func_Id)
                  then
                     Append_To (Fin_Stmts, Build_BIP_Cleanup_Stmts (Func_Id));
                  end if;
               end;
            end if;

            if Ekind_In (Obj_Id, E_Constant, E_Variable)
              and then Present (Status_Flag_Or_Transient_Decl (Obj_Id))
            then
               --  Temporaries created for the purpose of "exporting" a
               --  controlled transient out of an Expression_With_Actions (EWA)
               --  need guards. The following illustrates the usage of such
               --  temporaries.

               --    Access_Typ : access [all] Obj_Typ;
               --    Temp       : Access_Typ := null;
               --    <Counter>  := ...;

               --    do
               --       Ctrl_Trans : [access [all]] Obj_Typ := ...;
               --       Temp := Access_Typ (Ctrl_Trans);  --  when a pointer
               --         <or>
               --       Temp := Ctrl_Trans'Unchecked_Access;
               --    in ... end;

               --  The finalization machinery does not process EWA nodes as
               --  this may lead to premature finalization of expressions. Note
               --  that Temp is marked as being properly initialized regardless
               --  of whether the initialization of Ctrl_Trans succeeded. Since
               --  a failed initialization may leave Temp with a value of null,
               --  add a guard to handle this case:

               --    if Obj /= null then
               --       <object finalization statements>
               --    end if;

               if Nkind (Status_Flag_Or_Transient_Decl (Obj_Id)) =
                                                      N_Object_Declaration
               then
                  Fin_Stmts := New_List (
                    Make_If_Statement (Loc,
                      Condition       =>
                        Make_Op_Ne (Loc,
                          Left_Opnd  => New_Occurrence_Of (Obj_Id, Loc),
                          Right_Opnd => Make_Null (Loc)),
                      Then_Statements => Fin_Stmts));

               --  Return objects use a flag to aid in processing their
               --  potential finalization when the enclosing function fails
               --  to return properly. Generate:

               --    if not Flag then
               --       <object finalization statements>
               --    end if;

               else
                  Fin_Stmts := New_List (
                    Make_If_Statement (Loc,
                      Condition     =>
                        Make_Op_Not (Loc,
                          Right_Opnd =>
                            New_Occurrence_Of
                              (Status_Flag_Or_Transient_Decl (Obj_Id), Loc)),

                    Then_Statements => Fin_Stmts));
               end if;
            end if;
         end if;

         Append_List_To (Finalizer_Stmts, Fin_Stmts);

         --  Since the declarations are examined in reverse, the state counter
         --  must be decremented in order to keep with the true position of
         --  objects.

         Counter_Val := Counter_Val - 1;
      end Process_Object_Declaration;

      -------------------------------------
      -- Process_Tagged_Type_Declaration --
      -------------------------------------

      procedure Process_Tagged_Type_Declaration (Decl : Node_Id) is
         Typ    : constant Entity_Id := Defining_Identifier (Decl);
         DT_Ptr : constant Entity_Id :=
                    Node (First_Elmt (Access_Disp_Table (Typ)));
      begin
         --  Generate:
         --    Ada.Tags.Unregister_Tag (<Typ>P);

         Append_To (Tagged_Type_Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Unregister_Tag), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (DT_Ptr, Loc))));
      end Process_Tagged_Type_Declaration;

   --  Start of processing for Build_Finalizer

   begin
      Fin_Id := Empty;

      --  Do not perform this expansion in SPARK mode because it is not
      --  necessary.

      if GNATprove_Mode then
         return;
      end if;

      --  Step 1: Extract all lists which may contain controlled objects or
      --  library-level tagged types.

      if For_Package_Spec then
         Decls      := Visible_Declarations (Specification (N));
         Priv_Decls := Private_Declarations (Specification (N));

         --  Retrieve the package spec id

         Spec_Id := Defining_Unit_Name (Specification (N));

         if Nkind (Spec_Id) = N_Defining_Program_Unit_Name then
            Spec_Id := Defining_Identifier (Spec_Id);
         end if;

      --  Accept statement, block, entry body, package body, protected body,
      --  subprogram body or task body.

      else
         Decls := Declarations (N);
         HSS   := Handled_Statement_Sequence (N);

         if Present (HSS) then
            if Present (Statements (HSS)) then
               Stmts := Statements (HSS);
            end if;

            if Present (At_End_Proc (HSS)) then
               Prev_At_End := At_End_Proc (HSS);
            end if;
         end if;

         --  Retrieve the package spec id for package bodies

         if For_Package_Body then
            Spec_Id := Corresponding_Spec (N);
         end if;
      end if;

      --  Do not process nested packages since those are handled by the
      --  enclosing scope's finalizer. Do not process non-expanded package
      --  instantiations since those will be re-analyzed and re-expanded.

      if For_Package
        and then
          (not Is_Library_Level_Entity (Spec_Id)

             --  Nested packages are considered to be library level entities,
             --  but do not need to be processed separately. True library level
             --  packages have a scope value of 1.

             or else Scope_Depth_Value (Spec_Id) /= Uint_1
             or else (Is_Generic_Instance (Spec_Id)
                       and then Package_Instantiation (Spec_Id) /= N))
      then
         return;
      end if;

      --  Step 2: Object [pre]processing

      if For_Package then

         --  Preprocess the visible declarations now in order to obtain the
         --  correct number of controlled object by the time the private
         --  declarations are processed.

         Process_Declarations (Decls, Preprocess => True, Top_Level => True);

         --  From all the possible contexts, only package specifications may
         --  have private declarations.

         if For_Package_Spec then
            Process_Declarations
              (Priv_Decls, Preprocess => True, Top_Level => True);
         end if;

         --  The current context may lack controlled objects, but require some
         --  other form of completion (task termination for instance). In such
         --  cases, the finalizer must be created and carry the additional
         --  statements.

         if Acts_As_Clean or Has_Ctrl_Objs or Has_Tagged_Types then
            Build_Components;
         end if;

         --  The preprocessing has determined that the context has controlled
         --  objects or library-level tagged types.

         if Has_Ctrl_Objs or Has_Tagged_Types then

            --  Private declarations are processed first in order to preserve
            --  possible dependencies between public and private objects.

            if For_Package_Spec then
               Process_Declarations (Priv_Decls);
            end if;

            Process_Declarations (Decls);
         end if;

      --  Non-package case

      else
         --  Preprocess both declarations and statements

         Process_Declarations (Decls, Preprocess => True, Top_Level => True);
         Process_Declarations (Stmts, Preprocess => True, Top_Level => True);

         --  At this point it is known that N has controlled objects. Ensure
         --  that N has a declarative list since the finalizer spec will be
         --  attached to it.

         if Has_Ctrl_Objs and then No (Decls) then
            Set_Declarations (N, New_List);
            Decls      := Declarations (N);
            Spec_Decls := Decls;
         end if;

         --  The current context may lack controlled objects, but require some
         --  other form of completion (task termination for instance). In such
         --  cases, the finalizer must be created and carry the additional
         --  statements.

         if Acts_As_Clean or Has_Ctrl_Objs or Has_Tagged_Types then
            Build_Components;
         end if;

         if Has_Ctrl_Objs or Has_Tagged_Types then
            Process_Declarations (Stmts);
            Process_Declarations (Decls);
         end if;
      end if;

      --  Step 3: Finalizer creation

      if Acts_As_Clean or Has_Ctrl_Objs or Has_Tagged_Types then
         Create_Finalizer;
      end if;
   end Build_Finalizer;

   --------------------------
   -- Build_Finalizer_Call --
   --------------------------

   procedure Build_Finalizer_Call (N : Node_Id; Fin_Id : Entity_Id) is
      Is_Prot_Body : constant Boolean :=
                       Nkind (N) = N_Subprogram_Body
                         and then Is_Protected_Subprogram_Body (N);
      --  Determine whether N denotes the protected version of a subprogram
      --  which belongs to a protected type.

      Loc : constant Source_Ptr := Sloc (N);
      HSS : Node_Id;

   begin
      --  Do not perform this expansion in SPARK mode because we do not create
      --  finalizers in the first place.

      if GNATprove_Mode then
         return;
      end if;

      --  The At_End handler should have been assimilated by the finalizer

      HSS := Handled_Statement_Sequence (N);
      pragma Assert (No (At_End_Proc (HSS)));

      --  If the construct to be cleaned up is a protected subprogram body, the
      --  finalizer call needs to be associated with the block which wraps the
      --  unprotected version of the subprogram. The following illustrates this
      --  scenario:

      --     procedure Prot_SubpP is
      --        procedure finalizer is
      --        begin
      --           Service_Entries (Prot_Obj);
      --           Abort_Undefer;
      --        end finalizer;

      --     begin
      --        . . .
      --        begin
      --           Prot_SubpN (Prot_Obj);
      --        at end
      --           finalizer;
      --        end;
      --     end Prot_SubpP;

      if Is_Prot_Body then
         HSS := Handled_Statement_Sequence (Last (Statements (HSS)));

      --  An At_End handler and regular exception handlers cannot coexist in
      --  the same statement sequence. Wrap the original statements in a block.

      elsif Present (Exception_Handlers (HSS)) then
         declare
            End_Lab : constant Node_Id := End_Label (HSS);
            Block   : Node_Id;

         begin
            Block :=
              Make_Block_Statement (Loc, Handled_Statement_Sequence => HSS);

            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Loc, New_List (Block)));

            HSS := Handled_Statement_Sequence (N);
            Set_End_Label (HSS, End_Lab);
         end;
      end if;

      Set_At_End_Proc (HSS, New_Occurrence_Of (Fin_Id, Loc));

      Analyze (At_End_Proc (HSS));
      Expand_At_End_Handler (HSS, Empty);
   end Build_Finalizer_Call;

   ---------------------
   -- Build_Late_Proc --
   ---------------------

   procedure Build_Late_Proc (Typ : Entity_Id; Nam : Name_Id) is
   begin
      for Final_Prim in Name_Of'Range loop
         if Name_Of (Final_Prim) = Nam then
            Set_TSS (Typ,
              Make_Deep_Proc
                (Prim  => Final_Prim,
                 Typ   => Typ,
                 Stmts => Make_Deep_Record_Body (Final_Prim, Typ)));
         end if;
      end loop;
   end Build_Late_Proc;

   -------------------------------
   -- Build_Object_Declarations --
   -------------------------------

   procedure Build_Object_Declarations
     (Data        : out Finalization_Exception_Data;
      Decls       : List_Id;
      Loc         : Source_Ptr;
      For_Package : Boolean := False)
   is
      A_Expr : Node_Id;
      E_Decl : Node_Id;

   begin
      pragma Assert (Decls /= No_List);

      --  Always set the proper location as it may be needed even when
      --  exception propagation is forbidden.

      Data.Loc := Loc;

      if Restriction_Active (No_Exception_Propagation) then
         Data.Abort_Id  := Empty;
         Data.E_Id      := Empty;
         Data.Raised_Id := Empty;
         return;
      end if;

      Data.Raised_Id := Make_Temporary (Loc, 'R');

      --  In certain scenarios, finalization can be triggered by an abort. If
      --  the finalization itself fails and raises an exception, the resulting
      --  Program_Error must be supressed and replaced by an abort signal. In
      --  order to detect this scenario, save the state of entry into the
      --  finalization code.

      --  No need to do this for VM case, since VM version of Ada.Exceptions
      --  does not include routine Raise_From_Controlled_Operation which is the
      --  the sole user of flag Abort.

      --  This is not needed for library-level finalizers as they are called
      --  by the environment task and cannot be aborted.

      if Abort_Allowed
        and then VM_Target = No_VM
        and then not For_Package
      then
         Data.Abort_Id  := Make_Temporary (Loc, 'A');

         A_Expr := New_Occurrence_Of (RTE (RE_Triggered_By_Abort), Loc);

         --  Generate:

         --    Abort_Id : constant Boolean := <A_Expr>;

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Data.Abort_Id,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
             Expression          => A_Expr));

      else
         --  No abort, .NET/JVM or library-level finalizers

         Data.Abort_Id  := Empty;
      end if;

      if Exception_Extra_Info then
         Data.E_Id      := Make_Temporary (Loc, 'E');

         --  Generate:

         --    E_Id : Exception_Occurrence;

         E_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Data.E_Id,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc));
         Set_No_Initialization (E_Decl);

         Append_To (Decls, E_Decl);

      else
         Data.E_Id      := Empty;
      end if;

      --  Generate:

      --    Raised_Id : Boolean := False;

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Data.Raised_Id,
          Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
          Expression          => New_Occurrence_Of (Standard_False, Loc)));
   end Build_Object_Declarations;

   ---------------------------
   -- Build_Raise_Statement --
   ---------------------------

   function Build_Raise_Statement
     (Data : Finalization_Exception_Data) return Node_Id
   is
      Stmt : Node_Id;
      Expr : Node_Id;

   begin
      --  Standard run-time and .NET/JVM targets use the specialized routine
      --  Raise_From_Controlled_Operation.

      if Exception_Extra_Info
        and then RTE_Available (RE_Raise_From_Controlled_Operation)
      then
         Stmt :=
           Make_Procedure_Call_Statement (Data.Loc,
              Name                   =>
                New_Occurrence_Of
                  (RTE (RE_Raise_From_Controlled_Operation), Data.Loc),
              Parameter_Associations =>
                New_List (New_Occurrence_Of (Data.E_Id, Data.Loc)));

      --  Restricted run-time: exception messages are not supported and hence
      --  Raise_From_Controlled_Operation is not supported. Raise Program_Error
      --  instead.

      else
         Stmt :=
           Make_Raise_Program_Error (Data.Loc,
             Reason => PE_Finalize_Raised_Exception);
      end if;

      --  Generate:

      --    Raised_Id and then not Abort_Id
      --      <or>
      --    Raised_Id

      Expr := New_Occurrence_Of (Data.Raised_Id, Data.Loc);

      if Present (Data.Abort_Id) then
         Expr := Make_And_Then (Data.Loc,
           Left_Opnd  => Expr,
           Right_Opnd =>
             Make_Op_Not (Data.Loc,
               Right_Opnd => New_Occurrence_Of (Data.Abort_Id, Data.Loc)));
      end if;

      --  Generate:

      --    if Raised_Id and then not Abort_Id then
      --       Raise_From_Controlled_Operation (E_Id);
      --         <or>
      --       raise Program_Error;  --  restricted runtime
      --    end if;

      return
        Make_If_Statement (Data.Loc,
          Condition       => Expr,
          Then_Statements => New_List (Stmt));
   end Build_Raise_Statement;

   -----------------------------
   -- Build_Record_Deep_Procs --
   -----------------------------

   procedure Build_Record_Deep_Procs (Typ : Entity_Id) is
   begin
      Set_TSS (Typ,
        Make_Deep_Proc
          (Prim  => Initialize_Case,
           Typ   => Typ,
           Stmts => Make_Deep_Record_Body (Initialize_Case, Typ)));

      if not Is_Limited_View (Typ) then
         Set_TSS (Typ,
           Make_Deep_Proc
             (Prim  => Adjust_Case,
              Typ   => Typ,
              Stmts => Make_Deep_Record_Body (Adjust_Case, Typ)));
      end if;

      --  Do not generate Deep_Finalize and Finalize_Address if finalization is
      --  suppressed since these routine will not be used.

      if not Restriction_Active (No_Finalization) then
         Set_TSS (Typ,
           Make_Deep_Proc
             (Prim  => Finalize_Case,
              Typ   => Typ,
              Stmts => Make_Deep_Record_Body (Finalize_Case, Typ)));

         --  Create TSS primitive Finalize_Address for non-VM targets. JVM and
         --  .NET do not support address arithmetic and unchecked conversions.

         if VM_Target = No_VM then
            Set_TSS (Typ,
              Make_Deep_Proc
                (Prim  => Address_Case,
                 Typ   => Typ,
                 Stmts => Make_Deep_Record_Body (Address_Case, Typ)));
         end if;
      end if;
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
         Index : Entity_Id;

      begin
         if Dim > Number_Dimensions (Typ) then
            return Free_Component;

         --  Here we generate the required loop

         else
            Index := Make_Temporary (Loc, 'J');
            Append (New_Occurrence_Of (Index, Loc), Index_List);

            return New_List (
              Make_Implicit_Loop_Statement (N,
                Identifier       => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier         => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix          => Duplicate_Subexpr (Obj),
                            Attribute_Name  => Name_Range,
                            Expressions     => New_List (
                              Make_Integer_Literal (Loc, Dim))))),
                Statements       =>  Free_One_Dimension (Dim + 1)));
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
            (Variant_Part (Component_List (Type_Definition (Parent (U_Typ)))))
      then
         --  For now, do not attempt to free a component that may appear in a
         --  variant, and instead issue a warning. Doing this "properly" would
         --  require building a case statement and would be quite a mess. Note
         --  that the RM only requires that free "work" for the case of a task
         --  access value, so already we go way beyond this in that we deal
         --  with the array case and non-discriminated record cases.

         Error_Msg_N
           ("task/protected object in variant record will not be freed??", N);
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

               Append_List_To (Stmts, Cleanup_Record (N, Tsk, Etype (Comp)));

            elsif Is_Array_Type (Etype (Comp)) then
               Append_List_To (Stmts, Cleanup_Array (N, Tsk, Etype (Comp)));
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
      --  For restricted run-time libraries (Ravenscar), tasks are
      --  non-terminating, and protected objects can only appear at library
      --  level, so we do not want finalization of protected objects.

      if Restricted_Profile then
         return Empty;

      else
         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Finalize_Protection), Loc),
             Parameter_Associations => New_List (Concurrent_Ref (Ref)));
      end if;
   end Cleanup_Protected_Object;

   ------------------
   -- Cleanup_Task --
   ------------------

   function Cleanup_Task
     (N   : Node_Id;
      Ref : Node_Id) return Node_Id
   is
      Loc  : constant Source_Ptr := Sloc (N);

   begin
      --  For restricted run-time libraries (Ravenscar), tasks are
      --  non-terminating and they can only appear at library level, so we do
      --  not want finalization of task objects.

      if Restricted_Profile then
         return Empty;

      else
         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Free_Task), Loc),
             Parameter_Associations => New_List (Concurrent_Ref (Ref)));
      end if;
   end Cleanup_Task;

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
        and then not Present (Overridden_Operation (E))
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
        and then (Is_Private_Type (Ftyp) or else Is_Private_Type (Atyp))
        and then Base_Type (Underlying_Type (Atyp)) =
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

   ------------------------
   -- Enclosing_Function --
   ------------------------

   function Enclosing_Function (E : Entity_Id) return Entity_Id is
      Func_Id : Entity_Id;

   begin
      Func_Id := E;
      while Present (Func_Id)
        and then Func_Id /= Standard_Standard
      loop
         if Ekind (Func_Id) = E_Function then
            return Func_Id;
         end if;

         Func_Id := Scope (Func_Id);
      end loop;

      return Empty;
   end Enclosing_Function;

   -------------------------------
   -- Establish_Transient_Scope --
   -------------------------------

   --  This procedure is called each time a transient block has to be inserted
   --  that is to say for each call to a function with unconstrained or tagged
   --  result. It creates a new scope on the stack scope in order to enclose
   --  all transient variables generated.

   procedure Establish_Transient_Scope (N : Node_Id; Sec_Stack : Boolean) is
      Loc       : constant Source_Ptr := Sloc (N);
      Iter_Loop : Entity_Id;
      Wrap_Node : Node_Id;

   begin
      --  Do not create a transient scope if we are already inside one

      for S in reverse Scope_Stack.First .. Scope_Stack.Last loop
         if Scope_Stack.Table (S).Is_Transient then
            if Sec_Stack then
               Set_Uses_Sec_Stack (Scope_Stack.Table (S).Entity);
            end if;

            return;

         --  If we encounter Standard there are no enclosing transient scopes

         elsif Scope_Stack.Table (S).Entity = Standard_Standard then
            exit;
         end if;
      end loop;

      Wrap_Node := Find_Node_To_Be_Wrapped (N);

      --  The context does not contain a node that requires a transient scope,
      --  nothing to do.

      if No (Wrap_Node) then
         null;

      --  If the node to wrap is an iteration_scheme, the expression is one of
      --  the bounds, and the expansion will make an explicit declaration for
      --  it (see Analyze_Iteration_Scheme, sem_ch5.adb), so do not apply any
      --  transformations here. Same for an Ada 2012 iterator specification,
      --  where a block is created for the expression that build the container.

      elsif Nkind_In (Wrap_Node, N_Iteration_Scheme,
                                 N_Iterator_Specification)
      then
         null;

      --  In formal verification mode, if the node to wrap is a pragma check,
      --  this node and enclosed expression are not expanded, so do not apply
      --  any transformations here.

      elsif GNATprove_Mode
        and then Nkind (Wrap_Node) = N_Pragma
        and then Get_Pragma_Id (Wrap_Node) = Pragma_Check
      then
         null;

      --  Create a block entity to act as a transient scope. Note that when the
      --  node to be wrapped is an expression or a statement, a real physical
      --  block is constructed (see routines Wrap_Transient_Expression and
      --  Wrap_Transient_Statement) and inserted into the tree.

      else
         Push_Scope (New_Internal_Entity (E_Block, Current_Scope, Loc, 'B'));
         Set_Scope_Is_Transient;

         --  The transient scope must also take care of the secondary stack
         --  management.

         if Sec_Stack then
            Set_Uses_Sec_Stack (Current_Scope);
            Check_Restriction (No_Secondary_Stack, N);

            --  The expansion of iterator loops generates references to objects
            --  in order to extract elements from a container:

            --    Ref : Reference_Type_Ptr := Reference (Container, Cursor);
            --    Obj : <object type> renames Ref.all.Element.all;

            --  These references are controlled and returned on the secondary
            --  stack. A new reference is created at each iteration of the loop
            --  and as a result it must be finalized and the space occupied by
            --  it on the secondary stack reclaimed at the end of the current
            --  iteration.

            --  When the context that requires a transient scope is a call to
            --  routine Reference, the node to be wrapped is the source object:

            --    for Obj of Container loop

            --  Routine Wrap_Transient_Declaration however does not generate a
            --  physical block as wrapping a declaration will kill it too ealy.
            --  To handle this peculiar case, mark the related iterator loop as
            --  requiring the secondary stack. This signals the finalization
            --  machinery to manage the secondary stack (see routine
            --  Process_Statements_For_Controlled_Objects).

            Iter_Loop := Find_Enclosing_Iterator_Loop (Current_Scope);

            if Present (Iter_Loop) then
               Set_Uses_Sec_Stack (Iter_Loop);
            end if;
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
      Scop : constant Entity_Id := Current_Scope;

      Is_Asynchronous_Call : constant Boolean :=
                               Nkind (N) = N_Block_Statement
                                 and then Is_Asynchronous_Call_Block (N);
      Is_Master            : constant Boolean :=
                               Nkind (N) /= N_Entry_Body
                                 and then Is_Task_Master (N);
      Is_Protected_Body    : constant Boolean :=
                               Nkind (N) = N_Subprogram_Body
                                 and then Is_Protected_Subprogram_Body (N);
      Is_Task_Allocation   : constant Boolean :=
                               Nkind (N) = N_Block_Statement
                                 and then Is_Task_Allocation_Block (N);
      Is_Task_Body         : constant Boolean :=
                               Nkind (Original_Node (N)) = N_Task_Body;
      Needs_Sec_Stack_Mark : constant Boolean :=
                               Uses_Sec_Stack (Scop)
                                 and then
                                   not Sec_Stack_Needed_For_Return (Scop)
                                 and then VM_Target = No_VM;

      Actions_Required     : constant Boolean :=
                               Requires_Cleanup_Actions (N, True)
                                 or else Is_Asynchronous_Call
                                 or else Is_Master
                                 or else Is_Protected_Body
                                 or else Is_Task_Allocation
                                 or else Is_Task_Body
                                 or else Needs_Sec_Stack_Mark;

      HSS : Node_Id := Handled_Statement_Sequence (N);
      Loc : Source_Ptr;

      procedure Wrap_HSS_In_Block;
      --  Move HSS inside a new block along with the original exception
      --  handlers. Make the newly generated block the sole statement of HSS.

      -----------------------
      -- Wrap_HSS_In_Block --
      -----------------------

      procedure Wrap_HSS_In_Block is
         Block   : Node_Id;
         End_Lab : Node_Id;

      begin
         --  Preserve end label to provide proper cross-reference information

         End_Lab := End_Label (HSS);
         Block :=
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence => HSS);

         --  Signal the finalization machinery that this particular block
         --  contains the original context.

         Set_Is_Finalization_Wrapper (Block);

         Set_Handled_Statement_Sequence (N,
           Make_Handled_Sequence_Of_Statements (Loc, New_List (Block)));
         HSS := Handled_Statement_Sequence (N);

         Set_First_Real_Statement (HSS, Block);
         Set_End_Label (HSS, End_Lab);

         --  Comment needed here, see RH for 1.306 ???

         if Nkind (N) = N_Subprogram_Body then
            Set_Has_Nested_Block_With_Handler (Scop);
         end if;
      end Wrap_HSS_In_Block;

   --  Start of processing for Expand_Cleanup_Actions

   begin
      --  The current construct does not need any form of servicing

      if not Actions_Required then
         return;

      --  If the current node is a rewritten task body and the descriptors have
      --  not been delayed (due to some nested instantiations), do not generate
      --  redundant cleanup actions.

      elsif Is_Task_Body
        and then Nkind (N) = N_Subprogram_Body
        and then not Delay_Subprogram_Descriptors (Corresponding_Spec (N))
      then
         return;
      end if;

      declare
         Decls     : List_Id := Declarations (N);
         Fin_Id    : Entity_Id;
         Mark      : Entity_Id := Empty;
         New_Decls : List_Id;
         Old_Poll  : Boolean;

      begin
         --  If we are generating expanded code for debugging purposes, use the
         --  Sloc of the point of insertion for the cleanup code. The Sloc will
         --  be updated subsequently to reference the proper line in .dg files.
         --  If we are not debugging generated code, use No_Location instead,
         --  so that no debug information is generated for the cleanup code.
         --  This makes the behavior of the NEXT command in GDB monotonic, and
         --  makes the placement of breakpoints more accurate.

         if Debug_Generated_Code then
            Loc := Sloc (Scop);
         else
            Loc := No_Location;
         end if;

         --  Set polling off. The finalization and cleanup code is executed
         --  with aborts deferred.

         Old_Poll := Polling_Required;
         Polling_Required := False;

         --  A task activation call has already been built for a task
         --  allocation block.

         if not Is_Task_Allocation then
            Build_Task_Activation_Call (N);
         end if;

         if Is_Master then
            Establish_Task_Master (N);
         end if;

         New_Decls := New_List;

         --  If secondary stack is in use, generate:
         --
         --    Mnn : constant Mark_Id := SS_Mark;

         --  Suppress calls to SS_Mark and SS_Release if VM_Target, since the
         --  secondary stack is never used on a VM.

         if Needs_Sec_Stack_Mark then
            Mark := Make_Temporary (Loc, 'M');

            Append_To (New_Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Mark,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Mark_Id), Loc),
                Expression          =>
                  Make_Function_Call (Loc,
                    Name => New_Occurrence_Of (RTE (RE_SS_Mark), Loc))));

            Set_Uses_Sec_Stack (Scop, False);
         end if;

         --  If exception handlers are present, wrap the sequence of statements
         --  in a block since it is not possible to have exception handlers and
         --  an At_End handler in the same construct.

         if Present (Exception_Handlers (HSS)) then
            Wrap_HSS_In_Block;

         --  Ensure that the First_Real_Statement field is set

         elsif No (First_Real_Statement (HSS)) then
            Set_First_Real_Statement (HSS, First (Statements (HSS)));
         end if;

         --  Do not move the Activation_Chain declaration in the context of
         --  task allocation blocks. Task allocation blocks use _chain in their
         --  cleanup handlers and gigi complains if it is declared in the
         --  sequence of statements of the scope that declares the handler.

         if Is_Task_Allocation then
            declare
               Chain : constant Entity_Id := Activation_Chain_Entity (N);
               Decl  : Node_Id;

            begin
               Decl := First (Decls);
               while Nkind (Decl) /= N_Object_Declaration
                 or else Defining_Identifier (Decl) /= Chain
               loop
                  Next (Decl);

                  --  A task allocation block should always include a _chain
                  --  declaration.

                  pragma Assert (Present (Decl));
               end loop;

               Remove (Decl);
               Prepend_To (New_Decls, Decl);
            end;
         end if;

         --  Ensure the presence of a declaration list in order to successfully
         --  append all original statements to it.

         if No (Decls) then
            Set_Declarations (N, New_List);
            Decls := Declarations (N);
         end if;

         --  Move the declarations into the sequence of statements in order to
         --  have them protected by the At_End handler. It may seem weird to
         --  put declarations in the sequence of statement but in fact nothing
         --  forbids that at the tree level.

         Append_List_To (Decls, Statements (HSS));
         Set_Statements (HSS, Decls);

         --  Reset the Sloc of the handled statement sequence to properly
         --  reflect the new initial "statement" in the sequence.

         Set_Sloc (HSS, Sloc (First (Decls)));

         --  The declarations of finalizer spec and auxiliary variables replace
         --  the old declarations that have been moved inward.

         Set_Declarations (N, New_Decls);
         Analyze_Declarations (New_Decls);

         --  Generate finalization calls for all controlled objects appearing
         --  in the statements of N. Add context specific cleanup for various
         --  constructs.

         Build_Finalizer
           (N           => N,
            Clean_Stmts => Build_Cleanup_Statements (N),
            Mark_Id     => Mark,
            Top_Decls   => New_Decls,
            Defer_Abort => Nkind (Original_Node (N)) = N_Task_Body
                             or else Is_Master,
            Fin_Id      => Fin_Id);

         if Present (Fin_Id) then
            Build_Finalizer_Call (N, Fin_Id);
         end if;

         --  Restore saved polling mode

         Polling_Required := Old_Poll;
      end;
   end Expand_Cleanup_Actions;

   ---------------------------
   -- Expand_N_Package_Body --
   ---------------------------

   --  Add call to Activate_Tasks if body is an activator (actual processing
   --  is in chapter 9).

   --  Generate subprogram descriptor for elaboration routine

   --  Encode entity names in package body

   procedure Expand_N_Package_Body (N : Node_Id) is
      Spec_Ent : constant Entity_Id := Corresponding_Spec (N);
      Fin_Id   : Entity_Id;

   begin
      --  This is done only for non-generic packages

      if Ekind (Spec_Ent) = E_Package then
         Push_Scope (Corresponding_Spec (N));

         --  Build dispatch tables of library level tagged types

         if Tagged_Type_Expansion
           and then Is_Library_Level_Entity (Spec_Ent)
         then
            Build_Static_Dispatch_Tables (N);
         end if;

         Build_Task_Activation_Call (N);

         --  When the package is subject to pragma Initial_Condition, the
         --  assertion expression must be verified at the end of the body
         --  statements.

         if Present (Get_Pragma (Spec_Ent, Pragma_Initial_Condition)) then
            Expand_Pragma_Initial_Condition (N);
         end if;

         Pop_Scope;
      end if;

      Set_Elaboration_Flag (N, Corresponding_Spec (N));
      Set_In_Package_Body (Spec_Ent, False);

      --  Set to encode entity names in package body before gigi is called

      Qualify_Entity_Names (N);

      if Ekind (Spec_Ent) /= E_Generic_Package then
         Build_Finalizer
           (N           => N,
            Clean_Stmts => No_List,
            Mark_Id     => Empty,
            Top_Decls   => No_List,
            Defer_Abort => False,
            Fin_Id      => Fin_Id);

         if Present (Fin_Id) then
            declare
               Body_Ent : Node_Id := Defining_Unit_Name (N);

            begin
               if Nkind (Body_Ent) = N_Defining_Program_Unit_Name then
                  Body_Ent := Defining_Identifier (Body_Ent);
               end if;

               Set_Finalizer (Body_Ent, Fin_Id);
            end;
         end if;
      end if;
   end Expand_N_Package_Body;

   ----------------------------------
   -- Expand_N_Package_Declaration --
   ----------------------------------

   --  Add call to Activate_Tasks if there are tasks declared and the package
   --  has no body. Note that in Ada 83 this may result in premature activation
   --  of some tasks, given that we cannot tell whether a body will eventually
   --  appear.

   procedure Expand_N_Package_Declaration (N : Node_Id) is
      Id     : constant Entity_Id := Defining_Entity (N);
      Spec   : constant Node_Id   := Specification (N);
      Decls  : List_Id;
      Fin_Id : Entity_Id;

      No_Body : Boolean := False;
      --  True in the case of a package declaration that is a compilation
      --  unit and for which no associated body will be compiled in this
      --  compilation.

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
      --  package: even though the package declaration requires one, the body
      --  won't be processed in this compilation (so any stubs for RACWs
      --  declared in the package must be generated here, along with the spec).

      elsif Parent (N) = Cunit (Main_Unit)
        and then Is_Remote_Call_Interface (Id)
        and then Distribution_Stub_Mode = Generate_Caller_Stub_Body
      then
         No_Body := True;
      end if;

      --  For a nested instance, delay processing until freeze point

      if Has_Delayed_Freeze (Id)
        and then Nkind (Parent (N)) /= N_Compilation_Unit
      then
         return;
      end if;

      --  For a package declaration that implies no associated body, generate
      --  task activation call and RACW supporting bodies now (since we won't
      --  have a specific separate compilation unit for that).

      if No_Body then
         Push_Scope (Id);

         --  Generate RACW subprogram bodies

         if Has_RACW (Id) then
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

         --  Generate task activation call as last step of elaboration

         if Present (Activation_Chain_Entity (N)) then
            Build_Task_Activation_Call (N);
         end if;

         --  When the package is subject to pragma Initial_Condition and lacks
         --  a body, the assertion expression must be verified at the end of
         --  the visible declarations. Otherwise the check is performed at the
         --  end of the body statements (see Expand_N_Package_Body).

         if Present (Get_Pragma (Id, Pragma_Initial_Condition)) then
            Expand_Pragma_Initial_Condition (N);
         end if;

         Pop_Scope;
      end if;

      --  Build dispatch tables of library level tagged types

      if Tagged_Type_Expansion
        and then (Is_Compilation_Unit (Id)
                   or else (Is_Generic_Instance (Id)
                             and then Is_Library_Level_Entity (Id)))
      then
         Build_Static_Dispatch_Tables (N);
      end if;

      --  Note: it is not necessary to worry about generating a subprogram
      --  descriptor, since the only way to get exception handlers into a
      --  package spec is to include instantiations, and that would cause
      --  generation of subprogram descriptors to be delayed in any case.

      --  Set to encode entity names in package spec before gigi is called

      Qualify_Entity_Names (N);

      if Ekind (Id) /= E_Generic_Package then
         Build_Finalizer
           (N           => N,
            Clean_Stmts => No_List,
            Mark_Id     => Empty,
            Top_Decls   => No_List,
            Defer_Abort => False,
            Fin_Id      => Fin_Id);

         Set_Finalizer (Id, Fin_Id);
      end if;
   end Expand_N_Package_Declaration;

   -------------------------------------
   -- Expand_Pragma_Initial_Condition --
   -------------------------------------

   procedure Expand_Pragma_Initial_Condition (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Check     : Node_Id;
      Expr      : Node_Id;
      Init_Cond : Node_Id;
      List      : List_Id;
      Pack_Id   : Entity_Id;

   begin
      if Nkind (N) = N_Package_Body then
         Pack_Id := Corresponding_Spec (N);

         if Present (Handled_Statement_Sequence (N)) then
            List := Statements (Handled_Statement_Sequence (N));

         --  The package body lacks statements, create an empty list

         else
            List := New_List;

            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Loc, Statements => List));
         end if;

      elsif Nkind (N) = N_Package_Declaration then
         Pack_Id := Defining_Entity (N);

         if Present (Visible_Declarations (Specification (N))) then
            List := Visible_Declarations (Specification (N));

         --  The package lacks visible declarations, create an empty list

         else
            List := New_List;

            Set_Visible_Declarations (Specification (N), List);
         end if;

      --  This routine should not be used on anything other than packages

      else
         raise Program_Error;
      end if;

      Init_Cond := Get_Pragma (Pack_Id, Pragma_Initial_Condition);

      --  The caller should check whether the package is subject to pragma
      --  Initial_Condition.

      pragma Assert (Present (Init_Cond));

      Expr :=
        Get_Pragma_Arg (First (Pragma_Argument_Associations (Init_Cond)));

      --  The assertion expression was found to be illegal, do not generate the
      --  runtime check as it will repeat the illegality.

      if Error_Posted (Init_Cond) or else Error_Posted (Expr) then
         return;
      end if;

      --  Generate:
      --    pragma Check (Initial_Condition, <Expr>);

      Check :=
        Make_Pragma (Loc,
          Chars                        => Name_Check,
          Pragma_Argument_Associations => New_List (
            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Name_Initial_Condition)),

            Make_Pragma_Argument_Association (Loc,
              Expression => New_Copy_Tree (Expr))));

      Append_To (List, Check);
      Analyze (Check);
   end Expand_Pragma_Initial_Condition;

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

            --  Usually assignments are good candidate for wrapping except
            --  when they have been generated as part of a controlled aggregate
            --  where the wrapping should take place more globally. Note that
            --  No_Ctrl_Actions may be set also for non-controlled assignements
            --  in order to disable the use of dispatching _assign, so we need
            --  to test explicitly for a controlled type here.

            when N_Assignment_Statement =>
               if No_Ctrl_Actions (The_Parent)
                 and then Needs_Finalization (Etype (Name (The_Parent)))
               then
                  null;
               else
                  return The_Parent;
               end if;

            --  An entry call statement is a special case if it occurs in the
            --  context of a Timed_Entry_Call. In this case we wrap the entire
            --  timed entry call.

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
            --  even if they are not really wrapped. For further details, see
            --  Wrap_Transient_Declaration.

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

            --  The following nodes contains "dummy calls" which don't need to
            --  be wrapped.

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

            --  Otherwise continue the search

            when others =>
               null;
         end case;
      end loop;
   end Find_Node_To_Be_Wrapped;

   -------------------------------------
   -- Get_Global_Pool_For_Access_Type --
   -------------------------------------

   function Get_Global_Pool_For_Access_Type (T : Entity_Id) return Entity_Id is
   begin
      --  Access types whose size is smaller than System.Address size can exist
      --  only on VMS. We can't use the usual global pool which returns an
      --  object of type Address as truncation will make it invalid. To handle
      --  this case, VMS has a dedicated global pool that returns addresses
      --  that fit into 32 bit accesses.

      if Opt.True_VMS_Target and then Esize (T) = 32 then
         return RTE (RE_Global_Pool_32_Object);
      else
         return RTE (RE_Global_Pool_Object);
      end if;
   end Get_Global_Pool_For_Access_Type;

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

   ---------------------------------
   -- Has_Simple_Protected_Object --
   ---------------------------------

   function Has_Simple_Protected_Object (T : Entity_Id) return Boolean is
   begin
      if Has_Task (T) then
         return False;

      elsif Is_Simple_Protected_Type (T) then
         return True;

      elsif Is_Array_Type (T) then
         return Has_Simple_Protected_Object (Component_Type (T));

      elsif Is_Record_Type (T) then
         declare
            Comp : Entity_Id;

         begin
            Comp := First_Component (T);
            while Present (Comp) loop
               if Has_Simple_Protected_Object (Etype (Comp)) then
                  return True;
               end if;

               Next_Component (Comp);
            end loop;

            return False;
         end;

      else
         return False;
      end if;
   end Has_Simple_Protected_Object;

   ------------------------------------
   -- Insert_Actions_In_Scope_Around --
   ------------------------------------

   procedure Insert_Actions_In_Scope_Around (N : Node_Id) is
      After  : constant List_Id :=
        Scope_Stack.Table (Scope_Stack.Last).Actions_To_Be_Wrapped_After;
      Before : constant List_Id :=
        Scope_Stack.Table (Scope_Stack.Last).Actions_To_Be_Wrapped_Before;
      --  Note: We used to use renamings of Scope_Stack.Table (Scope_Stack.
      --  Last), but this was incorrect as Process_Transient_Object may
      --  introduce new scopes and cause a reallocation of Scope_Stack.Table.

      procedure Process_Transient_Objects
        (First_Object : Node_Id;
         Last_Object  : Node_Id;
         Related_Node : Node_Id);
      --  First_Object and Last_Object define a list which contains potential
      --  controlled transient objects. Finalization flags are inserted before
      --  First_Object and finalization calls are inserted after Last_Object.
      --  Related_Node is the node for which transient objects have been
      --  created.

      -------------------------------
      -- Process_Transient_Objects --
      -------------------------------

      procedure Process_Transient_Objects
        (First_Object : Node_Id;
         Last_Object  : Node_Id;
         Related_Node : Node_Id)
      is
         Must_Hook : Boolean := False;
         --  Flag denoting whether the context requires transient variable
         --  export to the outer finalizer.

         function Is_Subprogram_Call (N : Node_Id) return Traverse_Result;
         --  Determine whether an arbitrary node denotes a subprogram call

         procedure Detect_Subprogram_Call is
           new Traverse_Proc (Is_Subprogram_Call);

         ------------------------
         -- Is_Subprogram_Call --
         ------------------------

         function Is_Subprogram_Call (N : Node_Id) return Traverse_Result is
         begin
            --  Complex constructs are factored out by the expander and their
            --  occurrences are replaced with references to temporaries. Due to
            --  this expansion activity, inspect the original tree to detect
            --  subprogram calls.

            if Nkind (N) = N_Identifier and then Original_Node (N) /= N then
               Detect_Subprogram_Call (Original_Node (N));

               --  The original construct contains a subprogram call, there is
               --  no point in continuing the tree traversal.

               if Must_Hook then
                  return Abandon;
               else
                  return OK;
               end if;

            --  The original construct contains a subprogram call, there is no
            --  point in continuing the tree traversal.

            elsif Nkind (N) = N_Object_Declaration
              and then Present (Expression (N))
              and then Nkind (Original_Node (Expression (N))) = N_Function_Call
            then
               Must_Hook := True;
               return Abandon;

            --  A regular procedure or function call

            elsif Nkind (N) in N_Subprogram_Call then
               Must_Hook := True;
               return Abandon;

            --  Keep searching

            else
               return OK;
            end if;
         end Is_Subprogram_Call;

         --  Local variables

         Built     : Boolean := False;
         Desig_Typ : Entity_Id;
         Expr      : Node_Id;
         Fin_Block : Node_Id;
         Fin_Data  : Finalization_Exception_Data;
         Fin_Decls : List_Id;
         Fin_Insrt : Node_Id;
         Last_Fin  : Node_Id := Empty;
         Loc       : Source_Ptr;
         Obj_Id    : Entity_Id;
         Obj_Ref   : Node_Id;
         Obj_Typ   : Entity_Id;
         Prev_Fin  : Node_Id := Empty;
         Ptr_Id    : Entity_Id;
         Stmt      : Node_Id;
         Stmts     : List_Id;
         Temp_Id   : Entity_Id;
         Temp_Ins  : Node_Id;

      --  Start of processing for Process_Transient_Objects

      begin
         --  Recognize a scenario where the transient context is an object
         --  declaration initialized by a build-in-place function call:

         --    Obj : ... := BIP_Function_Call (Ctrl_Func_Call);

         --  The rough expansion of the above is:

         --    Temp : ... := Ctrl_Func_Call;
         --    Obj  : ...;
         --    Res  : ... := BIP_Func_Call (..., Obj, ...);

         --  The finalization of any controlled transient must happen after
         --  the build-in-place function call is executed.

         if Nkind (N) = N_Object_Declaration
           and then Present (BIP_Initialization_Call (Defining_Identifier (N)))
         then
            Must_Hook := True;
            Fin_Insrt := BIP_Initialization_Call (Defining_Identifier (N));

         --  Search the context for at least one subprogram call. If found, the
         --  machinery exports all transient objects to the enclosing finalizer
         --  due to the possibility of abnormal call termination.

         else
            Detect_Subprogram_Call (N);
            Fin_Insrt := Last_Object;
         end if;

         --  Examine all objects in the list First_Object .. Last_Object

         Stmt := First_Object;
         while Present (Stmt) loop
            if Nkind (Stmt) = N_Object_Declaration
              and then Analyzed (Stmt)
              and then Is_Finalizable_Transient (Stmt, N)

              --  Do not process the node to be wrapped since it will be
              --  handled by the enclosing finalizer.

              and then Stmt /= Related_Node
            then
               Loc       := Sloc (Stmt);
               Obj_Id    := Defining_Identifier (Stmt);
               Obj_Typ   := Base_Type (Etype (Obj_Id));
               Desig_Typ := Obj_Typ;

               Set_Is_Processed_Transient (Obj_Id);

               --  Handle access types

               if Is_Access_Type (Desig_Typ) then
                  Desig_Typ := Available_View (Designated_Type (Desig_Typ));
               end if;

               --  Create the necessary entities and declarations the first
               --  time around.

               if not Built then
                  Built     := True;
                  Fin_Decls := New_List;

                  Build_Object_Declarations (Fin_Data, Fin_Decls, Loc);
               end if;

               --  Transient variables associated with subprogram calls need
               --  extra processing. These variables are usually created right
               --  before the call and finalized immediately after the call.
               --  If an exception occurs during the call, the clean up code
               --  is skipped due to the sudden change in control and the
               --  transient is never finalized.

               --  To handle this case, such variables are "exported" to the
               --  enclosing sequence of statements where their corresponding
               --  "hooks" are picked up by the finalization machinery.

               if Must_Hook then

                  --  Step 1: Create an access type which provides a reference
                  --  to the transient object. Generate:

                  --    Ann : access [all] <Desig_Typ>;

                  Ptr_Id := Make_Temporary (Loc, 'A');

                  Insert_Action (Stmt,
                    Make_Full_Type_Declaration (Loc,
                      Defining_Identifier => Ptr_Id,
                      Type_Definition     =>
                        Make_Access_To_Object_Definition (Loc,
                          All_Present        =>
                            Ekind (Obj_Typ) = E_General_Access_Type,
                          Subtype_Indication =>
                            New_Occurrence_Of (Desig_Typ, Loc))));

                  --  Step 2: Create a temporary which acts as a hook to the
                  --  transient object. Generate:

                  --    Temp : Ptr_Id := null;

                  Temp_Id := Make_Temporary (Loc, 'T');

                  Insert_Action (Stmt,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Temp_Id,
                      Object_Definition   =>
                        New_Occurrence_Of (Ptr_Id, Loc)));

                  --  Mark the temporary as a transient hook. This signals the
                  --  machinery in Build_Finalizer to recognize this special
                  --  case.

                  Set_Status_Flag_Or_Transient_Decl (Temp_Id, Stmt);

                  --  Step 3: Hook the transient object to the temporary

                  if Is_Access_Type (Obj_Typ) then
                     Expr :=
                       Convert_To (Ptr_Id, New_Occurrence_Of (Obj_Id, Loc));
                  else
                     Expr :=
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Obj_Id, Loc),
                         Attribute_Name => Name_Unrestricted_Access);
                  end if;

                  --  Generate:
                  --    Temp := Ptr_Id (Obj_Id);
                  --      <or>
                  --    Temp := Obj_Id'Unrestricted_Access;

                  --  When the transient object is initialized by an aggregate,
                  --  the hook must capture the object after the last component
                  --  assignment takes place. Only then is the object fully
                  --  initialized.

                  if Ekind (Obj_Id) = E_Variable
                    and then Present (Last_Aggregate_Assignment (Obj_Id))
                  then
                     Temp_Ins := Last_Aggregate_Assignment (Obj_Id);

                  --  Otherwise the hook seizes the related object immediately

                  else
                     Temp_Ins := Stmt;
                  end if;

                  Insert_After_And_Analyze (Temp_Ins,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Temp_Id, Loc),
                      Expression => Expr));
               end if;

               Stmts := New_List;

               --  The transient object is about to be finalized by the clean
               --  up code following the subprogram call. In order to avoid
               --  double finalization, clear the hook.

               --  Generate:
               --    Temp := null;

               if Must_Hook then
                  Append_To (Stmts,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Temp_Id, Loc),
                      Expression => Make_Null (Loc)));
               end if;

               --  Generate:
               --    [Deep_]Finalize (Obj_Ref);

               Obj_Ref := New_Occurrence_Of (Obj_Id, Loc);

               if Is_Access_Type (Obj_Typ) then
                  Obj_Ref := Make_Explicit_Dereference (Loc, Obj_Ref);
               end if;

               Append_To (Stmts,
                 Make_Final_Call (Obj_Ref => Obj_Ref, Typ => Desig_Typ));

               --  Generate:
               --    [Temp := null;]
               --    begin
               --       [Deep_]Finalize (Obj_Ref);

               --    exception
               --       when others =>
               --          if not Raised then
               --             Raised := True;
               --             Save_Occurrence
               --               (Enn, Get_Current_Excep.all.all);
               --          end if;
               --    end;

               Fin_Block :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => Stmts,
                       Exception_Handlers => New_List (
                         Build_Exception_Handler (Fin_Data))));

               --  The single raise statement must be inserted after all the
               --  finalization blocks, and we put everything into a wrapper
               --  block to clearly expose the construct to the back-end.

               if Present (Prev_Fin) then
                  Insert_Before_And_Analyze (Prev_Fin, Fin_Block);
               else
                  Insert_After_And_Analyze (Fin_Insrt,
                    Make_Block_Statement (Loc,
                      Declarations => Fin_Decls,
                      Handled_Statement_Sequence =>
                        Make_Handled_Sequence_Of_Statements (Loc,
                          Statements => New_List (Fin_Block))));

                  Last_Fin := Fin_Block;
               end if;

               Prev_Fin := Fin_Block;
            end if;

            --  Terminate the scan after the last object has been processed to
            --  avoid touching unrelated code.

            if Stmt = Last_Object then
               exit;
            end if;

            Next (Stmt);
         end loop;

         --  Generate:
         --    if Raised and then not Abort then
         --       Raise_From_Controlled_Operation (E);
         --    end if;

         if Built and then Present (Last_Fin) then
            Insert_After_And_Analyze (Last_Fin,
              Build_Raise_Statement (Fin_Data));
         end if;
      end Process_Transient_Objects;

   --  Start of processing for Insert_Actions_In_Scope_Around

   begin
      if No (Before) and then No (After) then
         return;
      end if;

      declare
         Node_To_Wrap : constant Node_Id := Node_To_Be_Wrapped;
         First_Obj    : Node_Id;
         Last_Obj     : Node_Id;
         Target       : Node_Id;

      begin
         --  If the node to be wrapped is the trigger of an asynchronous
         --  select, it is not part of a statement list. The actions must be
         --  inserted before the select itself, which is part of some list of
         --  statements. Note that the triggering alternative includes the
         --  triggering statement and an optional statement list. If the node
         --  to be wrapped is part of that list, the normal insertion applies.

         if Nkind (Parent (Node_To_Wrap)) = N_Triggering_Alternative
           and then not Is_List_Member (Node_To_Wrap)
         then
            Target := Parent (Parent (Node_To_Wrap));
         else
            Target := N;
         end if;

         First_Obj := Target;
         Last_Obj  := Target;

         --  Add all actions associated with a transient scope into the main
         --  tree. There are several scenarios here:

         --       +--- Before ----+        +----- After ---+
         --    1) First_Obj ....... Target ........ Last_Obj

         --    2) First_Obj ....... Target

         --    3)                   Target ........ Last_Obj

         if Present (Before) then

            --  Flag declarations are inserted before the first object

            First_Obj := First (Before);

            Insert_List_Before (Target, Before);
         end if;

         if Present (After) then

            --  Finalization calls are inserted after the last object

            Last_Obj := Last (After);

            Insert_List_After (Target, After);
         end if;

         --  Check for transient controlled objects associated with Target and
         --  generate the appropriate finalization actions for them.

         Process_Transient_Objects
           (First_Object => First_Obj,
            Last_Object  => Last_Obj,
            Related_Node => Target);

         --  Reset the action lists

         if Present (Before) then
            Scope_Stack.Table (Scope_Stack.Last).
              Actions_To_Be_Wrapped_Before := No_List;
         end if;

         if Present (After) then
            Scope_Stack.Table (Scope_Stack.Last).
              Actions_To_Be_Wrapped_After := No_List;
         end if;
      end;
   end Insert_Actions_In_Scope_Around;

   ------------------------------
   -- Is_Simple_Protected_Type --
   ------------------------------

   function Is_Simple_Protected_Type (T : Entity_Id) return Boolean is
   begin
      return
        Is_Protected_Type (T)
          and then not Uses_Lock_Free (T)
          and then not Has_Entries (T)
          and then Is_RTE (Find_Protection_Type (T), RE_Protection);
   end Is_Simple_Protected_Type;

   -----------------------
   -- Make_Adjust_Call --
   -----------------------

   function Make_Adjust_Call
     (Obj_Ref    : Node_Id;
      Typ        : Entity_Id;
      For_Parent : Boolean := False) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Obj_Ref);
      Adj_Id : Entity_Id := Empty;
      Ref    : Node_Id   := Obj_Ref;
      Utyp   : Entity_Id;

   begin
      --  Recover the proper type which contains Deep_Adjust

      if Is_Class_Wide_Type (Typ) then
         Utyp := Root_Type (Typ);
      else
         Utyp := Typ;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));
      Set_Assignment_OK (Ref);

      --  Deal with non-tagged derivation of private views

      if Is_Untagged_Derivation (Typ) then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Ref  := Unchecked_Convert_To (Utyp, Ref);
         Set_Assignment_OK (Ref);
      end if;

      --  When dealing with the completion of a private type, use the base
      --  type instead.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));

         Utyp := Base_Type (Utyp);
         Ref  := Unchecked_Convert_To (Utyp, Ref);
      end if;

      --  Select the appropriate version of adjust

      if For_Parent then
         if Has_Controlled_Component (Utyp) then
            Adj_Id := Find_Prim_Op (Utyp, TSS_Deep_Adjust);
         end if;

      --  Class-wide types, interfaces and types with controlled components

      elsif Is_Class_Wide_Type (Typ)
        or else Is_Interface (Typ)
        or else Has_Controlled_Component (Utyp)
      then
         if Is_Tagged_Type (Utyp) then
            Adj_Id := Find_Prim_Op (Utyp, TSS_Deep_Adjust);
         else
            Adj_Id := TSS (Utyp, TSS_Deep_Adjust);
         end if;

      --  Derivations from [Limited_]Controlled

      elsif Is_Controlled (Utyp) then
         if Has_Controlled_Component (Utyp) then
            Adj_Id := Find_Prim_Op (Utyp, TSS_Deep_Adjust);
         else
            Adj_Id := Find_Prim_Op (Utyp, Name_Of (Adjust_Case));
         end if;

      --  Tagged types

      elsif Is_Tagged_Type (Utyp) then
         Adj_Id := Find_Prim_Op (Utyp, TSS_Deep_Adjust);

      else
         raise Program_Error;
      end if;

      if Present (Adj_Id) then

         --  If the object is unanalyzed, set its expected type for use in
         --  Convert_View in case an additional conversion is needed.

         if No (Etype (Ref))
           and then Nkind (Ref) /= N_Unchecked_Type_Conversion
         then
            Set_Etype (Ref, Typ);
         end if;

         --  The object reference may need another conversion depending on the
         --  type of the formal and that of the actual.

         if not Is_Class_Wide_Type (Typ) then
            Ref := Convert_View (Adj_Id, Ref);
         end if;

         return Make_Call (Loc, Adj_Id, New_Copy_Tree (Ref), For_Parent);
      else
         return Empty;
      end if;
   end Make_Adjust_Call;

   ----------------------
   -- Make_Attach_Call --
   ----------------------

   function Make_Attach_Call
     (Obj_Ref : Node_Id;
      Ptr_Typ : Entity_Id) return Node_Id
   is
      pragma Assert (VM_Target /= No_VM);

      Loc : constant Source_Ptr := Sloc (Obj_Ref);
   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Attach), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Finalization_Master (Ptr_Typ), Loc),
            Unchecked_Convert_To (RTE (RE_Root_Controlled_Ptr), Obj_Ref)));
   end Make_Attach_Call;

   ----------------------
   -- Make_Detach_Call --
   ----------------------

   function Make_Detach_Call (Obj_Ref : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (Obj_Ref);

   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Detach), Loc),
          Parameter_Associations => New_List (
            Unchecked_Convert_To (RTE (RE_Root_Controlled_Ptr), Obj_Ref)));
   end Make_Detach_Call;

   ---------------
   -- Make_Call --
   ---------------

   function Make_Call
     (Loc        : Source_Ptr;
      Proc_Id    : Entity_Id;
      Param      : Node_Id;
      For_Parent : Boolean := False) return Node_Id
   is
      Params : constant List_Id := New_List (Param);

   begin
      --  When creating a call to Deep_Finalize for a _parent field of a
      --  derived type, disable the invocation of the nested Finalize by giving
      --  the corresponding flag a False value.

      if For_Parent then
         Append_To (Params, New_Occurrence_Of (Standard_False, Loc));
      end if;

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc_Id, Loc),
          Parameter_Associations => Params);
   end Make_Call;

   --------------------------
   -- Make_Deep_Array_Body --
   --------------------------

   function Make_Deep_Array_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id) return List_Id
   is
      function Build_Adjust_Or_Finalize_Statements
        (Typ : Entity_Id) return List_Id;
      --  Create the statements necessary to adjust or finalize an array of
      --  controlled elements. Generate:
      --
      --    declare
      --       Abort  : constant Boolean := Triggered_By_Abort;
      --         <or>
      --       Abort  : constant Boolean := False;  --  no abort
      --
      --       E      : Exception_Occurrence;
      --       Raised : Boolean := False;
      --
      --    begin
      --       for J1 in [reverse] Typ'First (1) .. Typ'Last (1) loop
      --                 ^--  in the finalization case
      --          ...
      --          for Jn in [reverse] Typ'First (n) .. Typ'Last (n) loop
      --             begin
      --                [Deep_]Adjust / Finalize (V (J1, ..., Jn));
      --
      --             exception
      --                when others =>
      --                   if not Raised then
      --                      Raised := True;
      --                      Save_Occurrence (E, Get_Current_Excep.all.all);
      --                   end if;
      --             end;
      --          end loop;
      --          ...
      --       end loop;
      --
      --       if Raised and then not Abort then
      --          Raise_From_Controlled_Operation (E);
      --       end if;
      --    end;

      function Build_Initialize_Statements (Typ : Entity_Id) return List_Id;
      --  Create the statements necessary to initialize an array of controlled
      --  elements. Include a mechanism to carry out partial finalization if an
      --  exception occurs. Generate:
      --
      --    declare
      --       Counter : Integer := 0;
      --
      --    begin
      --       for J1 in V'Range (1) loop
      --          ...
      --          for JN in V'Range (N) loop
      --             begin
      --                [Deep_]Initialize (V (J1, ..., JN));
      --
      --                Counter := Counter + 1;
      --
      --             exception
      --                when others =>
      --                   declare
      --                      Abort  : constant Boolean := Triggered_By_Abort;
      --                        <or>
      --                      Abort  : constant Boolean := False; --  no abort
      --                      E      : Exception_Occurence;
      --                      Raised : Boolean := False;

      --                   begin
      --                      Counter :=
      --                        V'Length (1) *
      --                        V'Length (2) *
      --                        ...
      --                        V'Length (N) - Counter;

      --                      for F1 in reverse V'Range (1) loop
      --                         ...
      --                         for FN in reverse V'Range (N) loop
      --                            if Counter > 0 then
      --                               Counter := Counter - 1;
      --                            else
      --                               begin
      --                                  [Deep_]Finalize (V (F1, ..., FN));

      --                               exception
      --                                  when others =>
      --                                     if not Raised then
      --                                        Raised := True;
      --                                        Save_Occurrence (E,
      --                                          Get_Current_Excep.all.all);
      --                                     end if;
      --                               end;
      --                            end if;
      --                         end loop;
      --                         ...
      --                      end loop;
      --                   end;
      --
      --                   if Raised and then not Abort then
      --                      Raise_From_Controlled_Operation (E);
      --                   end if;
      --
      --                   raise;
      --             end;
      --          end loop;
      --       end loop;
      --    end;

      function New_References_To
        (L   : List_Id;
         Loc : Source_Ptr) return List_Id;
      --  Given a list of defining identifiers, return a list of references to
      --  the original identifiers, in the same order as they appear.

      -----------------------------------------
      -- Build_Adjust_Or_Finalize_Statements --
      -----------------------------------------

      function Build_Adjust_Or_Finalize_Statements
        (Typ : Entity_Id) return List_Id
      is
         Comp_Typ        : constant Entity_Id  := Component_Type (Typ);
         Index_List      : constant List_Id    := New_List;
         Loc             : constant Source_Ptr := Sloc (Typ);
         Num_Dims        : constant Int        := Number_Dimensions (Typ);
         Finalizer_Decls : List_Id := No_List;
         Finalizer_Data  : Finalization_Exception_Data;
         Call            : Node_Id;
         Comp_Ref        : Node_Id;
         Core_Loop       : Node_Id;
         Dim             : Int;
         J               : Entity_Id;
         Loop_Id         : Entity_Id;
         Stmts           : List_Id;

         Exceptions_OK : constant Boolean :=
                           not Restriction_Active (No_Exception_Propagation);

         procedure Build_Indexes;
         --  Generate the indexes used in the dimension loops

         -------------------
         -- Build_Indexes --
         -------------------

         procedure Build_Indexes is
         begin
            --  Generate the following identifiers:
            --    Jnn  -  for initialization

            for Dim in 1 .. Num_Dims loop
               Append_To (Index_List,
                 Make_Defining_Identifier (Loc, New_External_Name ('J', Dim)));
            end loop;
         end Build_Indexes;

      --  Start of processing for Build_Adjust_Or_Finalize_Statements

      begin
         Finalizer_Decls := New_List;

         Build_Indexes;
         Build_Object_Declarations (Finalizer_Data, Finalizer_Decls, Loc);

         Comp_Ref :=
           Make_Indexed_Component (Loc,
             Prefix      => Make_Identifier (Loc, Name_V),
             Expressions => New_References_To (Index_List, Loc));
         Set_Etype (Comp_Ref, Comp_Typ);

         --  Generate:
         --    [Deep_]Adjust (V (J1, ..., JN))

         if Prim = Adjust_Case then
            Call := Make_Adjust_Call (Obj_Ref => Comp_Ref, Typ => Comp_Typ);

         --  Generate:
         --    [Deep_]Finalize (V (J1, ..., JN))

         else pragma Assert (Prim = Finalize_Case);
            Call := Make_Final_Call (Obj_Ref => Comp_Ref, Typ => Comp_Typ);
         end if;

         --  Generate the block which houses the adjust or finalize call:

         --    <adjust or finalize call>;  --  No_Exception_Propagation

         --    begin                       --  Exception handlers allowed
         --       <adjust or finalize call>

         --    exception
         --       when others =>
         --          if not Raised then
         --             Raised := True;
         --             Save_Occurrence (E, Get_Current_Excep.all.all);
         --          end if;
         --    end;

         if Exceptions_OK then
            Core_Loop :=
              Make_Block_Statement (Loc,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements         => New_List (Call),
                    Exception_Handlers => New_List (
                      Build_Exception_Handler (Finalizer_Data))));
         else
            Core_Loop := Call;
         end if;

         --  Generate the dimension loops starting from the innermost one

         --    for Jnn in [reverse] V'Range (Dim) loop
         --       <core loop>
         --    end loop;

         J := Last (Index_List);
         Dim := Num_Dims;
         while Present (J) and then Dim > 0 loop
            Loop_Id := J;
            Prev (J);
            Remove (Loop_Id);

            Core_Loop :=
              Make_Loop_Statement (Loc,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier         => Loop_Id,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => Make_Identifier (Loc, Name_V),
                            Attribute_Name => Name_Range,
                            Expressions    => New_List (
                              Make_Integer_Literal (Loc, Dim))),

                        Reverse_Present => Prim = Finalize_Case)),

                Statements => New_List (Core_Loop),
                End_Label  => Empty);

            Dim := Dim - 1;
         end loop;

         --  Generate the block which contains the core loop, the declarations
         --  of the abort flag, the exception occurrence, the raised flag and
         --  the conditional raise:

         --    declare
         --       Abort  : constant Boolean := Triggered_By_Abort;
         --         <or>
         --       Abort  : constant Boolean := False;  --  no abort

         --       E      : Exception_Occurrence;
         --       Raised : Boolean := False;

         --    begin
         --       <core loop>

         --       if Raised and then not Abort then  --  Expection handlers OK
         --          Raise_From_Controlled_Operation (E);
         --       end if;
         --    end;

         Stmts := New_List (Core_Loop);

         if Exceptions_OK then
            Append_To (Stmts,
              Build_Raise_Statement (Finalizer_Data));
         end if;

         return
           New_List (
             Make_Block_Statement (Loc,
               Declarations               =>
                 Finalizer_Decls,
               Handled_Statement_Sequence =>
                 Make_Handled_Sequence_Of_Statements (Loc, Stmts)));
      end Build_Adjust_Or_Finalize_Statements;

      ---------------------------------
      -- Build_Initialize_Statements --
      ---------------------------------

      function Build_Initialize_Statements (Typ : Entity_Id) return List_Id is
         Comp_Typ        : constant Entity_Id  := Component_Type (Typ);
         Final_List      : constant List_Id    := New_List;
         Index_List      : constant List_Id    := New_List;
         Loc             : constant Source_Ptr := Sloc (Typ);
         Num_Dims        : constant Int        := Number_Dimensions (Typ);
         Counter_Id      : Entity_Id;
         Dim             : Int;
         F               : Node_Id;
         Fin_Stmt        : Node_Id;
         Final_Block     : Node_Id;
         Final_Loop      : Node_Id;
         Finalizer_Data  : Finalization_Exception_Data;
         Finalizer_Decls : List_Id := No_List;
         Init_Loop       : Node_Id;
         J               : Node_Id;
         Loop_Id         : Node_Id;
         Stmts           : List_Id;

         Exceptions_OK : constant Boolean :=
                           not Restriction_Active (No_Exception_Propagation);

         function Build_Counter_Assignment return Node_Id;
         --  Generate the following assignment:
         --    Counter := V'Length (1) *
         --               ...
         --               V'Length (N) - Counter;

         function Build_Finalization_Call return Node_Id;
         --  Generate a deep finalization call for an array element

         procedure Build_Indexes;
         --  Generate the initialization and finalization indexes used in the
         --  dimension loops.

         function Build_Initialization_Call return Node_Id;
         --  Generate a deep initialization call for an array element

         ------------------------------
         -- Build_Counter_Assignment --
         ------------------------------

         function Build_Counter_Assignment return Node_Id is
            Dim  : Int;
            Expr : Node_Id;

         begin
            --  Start from the first dimension and generate:
            --    V'Length (1)

            Dim := 1;
            Expr :=
              Make_Attribute_Reference (Loc,
                Prefix         => Make_Identifier (Loc, Name_V),
                Attribute_Name => Name_Length,
                Expressions    => New_List (Make_Integer_Literal (Loc, Dim)));

            --  Process the rest of the dimensions, generate:
            --    Expr * V'Length (N)

            Dim := Dim + 1;
            while Dim <= Num_Dims loop
               Expr :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd  => Expr,
                   Right_Opnd =>
                     Make_Attribute_Reference (Loc,
                       Prefix         => Make_Identifier (Loc, Name_V),
                       Attribute_Name => Name_Length,
                       Expressions    => New_List (
                         Make_Integer_Literal (Loc, Dim))));

               Dim := Dim + 1;
            end loop;

            --  Generate:
            --    Counter := Expr - Counter;

            return
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Counter_Id, Loc),
                Expression =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd  => Expr,
                    Right_Opnd => New_Occurrence_Of (Counter_Id, Loc)));
         end Build_Counter_Assignment;

         -----------------------------
         -- Build_Finalization_Call --
         -----------------------------

         function Build_Finalization_Call return Node_Id is
            Comp_Ref : constant Node_Id :=
                         Make_Indexed_Component (Loc,
                           Prefix      => Make_Identifier (Loc, Name_V),
                           Expressions => New_References_To (Final_List, Loc));

         begin
            Set_Etype (Comp_Ref, Comp_Typ);

            --  Generate:
            --    [Deep_]Finalize (V);

            return Make_Final_Call (Obj_Ref => Comp_Ref, Typ => Comp_Typ);
         end Build_Finalization_Call;

         -------------------
         -- Build_Indexes --
         -------------------

         procedure Build_Indexes is
         begin
            --  Generate the following identifiers:
            --    Jnn  -  for initialization
            --    Fnn  -  for finalization

            for Dim in 1 .. Num_Dims loop
               Append_To (Index_List,
                 Make_Defining_Identifier (Loc, New_External_Name ('J', Dim)));

               Append_To (Final_List,
                 Make_Defining_Identifier (Loc, New_External_Name ('F', Dim)));
            end loop;
         end Build_Indexes;

         -------------------------------
         -- Build_Initialization_Call --
         -------------------------------

         function Build_Initialization_Call return Node_Id is
            Comp_Ref : constant Node_Id :=
                         Make_Indexed_Component (Loc,
                           Prefix      => Make_Identifier (Loc, Name_V),
                           Expressions => New_References_To (Index_List, Loc));

         begin
            Set_Etype (Comp_Ref, Comp_Typ);

            --  Generate:
            --    [Deep_]Initialize (V (J1, ..., JN));

            return Make_Init_Call (Obj_Ref => Comp_Ref, Typ => Comp_Typ);
         end Build_Initialization_Call;

      --  Start of processing for Build_Initialize_Statements

      begin
         Counter_Id := Make_Temporary (Loc, 'C');
         Finalizer_Decls := New_List;

         Build_Indexes;
         Build_Object_Declarations (Finalizer_Data, Finalizer_Decls, Loc);

         --  Generate the block which houses the finalization call, the index
         --  guard and the handler which triggers Program_Error later on.

         --    if Counter > 0 then
         --       Counter := Counter - 1;
         --    else
         --       [Deep_]Finalize (V (F1, ..., FN));  --  No_Except_Propagation

         --       begin                               --  Exceptions allowed
         --          [Deep_]Finalize (V (F1, ..., FN));
         --       exception
         --          when others =>
         --             if not Raised then
         --                Raised := True;
         --                Save_Occurrence (E, Get_Current_Excep.all.all);
         --             end if;
         --       end;
         --    end if;

         if Exceptions_OK then
            Fin_Stmt :=
              Make_Block_Statement (Loc,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements         => New_List (Build_Finalization_Call),
                    Exception_Handlers => New_List (
                      Build_Exception_Handler (Finalizer_Data))));
         else
            Fin_Stmt := Build_Finalization_Call;
         end if;

         --  This is the core of the loop, the dimension iterators are added
         --  one by one in reverse.

         Final_Loop :=
           Make_If_Statement (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => New_Occurrence_Of (Counter_Id, Loc),
                 Right_Opnd => Make_Integer_Literal (Loc, 0)),

             Then_Statements => New_List (
               Make_Assignment_Statement (Loc,
                 Name       => New_Occurrence_Of (Counter_Id, Loc),
                 Expression =>
                   Make_Op_Subtract (Loc,
                     Left_Opnd  => New_Occurrence_Of (Counter_Id, Loc),
                     Right_Opnd => Make_Integer_Literal (Loc, 1)))),

             Else_Statements => New_List (Fin_Stmt));

         --  Generate all finalization loops starting from the innermost
         --  dimension.

         --    for Fnn in reverse V'Range (Dim) loop
         --       <final loop>
         --    end loop;

         F := Last (Final_List);
         Dim := Num_Dims;
         while Present (F) and then Dim > 0 loop
            Loop_Id := F;
            Prev (F);
            Remove (Loop_Id);

            Final_Loop :=
              Make_Loop_Statement (Loc,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Loop_Id,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => Make_Identifier (Loc, Name_V),
                            Attribute_Name => Name_Range,
                            Expressions    => New_List (
                              Make_Integer_Literal (Loc, Dim))),

                        Reverse_Present => True)),

                Statements => New_List (Final_Loop),
                End_Label => Empty);

            Dim := Dim - 1;
         end loop;

         --  Generate the block which contains the finalization loops, the
         --  declarations of the abort flag, the exception occurrence, the
         --  raised flag and the conditional raise.

         --    declare
         --       Abort  : constant Boolean := Triggered_By_Abort;
         --         <or>
         --       Abort  : constant Boolean := False;  --  no abort

         --       E      : Exception_Occurrence;
         --       Raised : Boolean := False;

         --    begin
         --       Counter :=
         --         V'Length (1) *
         --         ...
         --         V'Length (N) - Counter;

         --       <final loop>

         --       if Raised and then not Abort then  --  Exception handlers OK
         --          Raise_From_Controlled_Operation (E);
         --       end if;

         --       raise;  --  Exception handlers OK
         --    end;

         Stmts := New_List (Build_Counter_Assignment, Final_Loop);

         if Exceptions_OK then
            Append_To (Stmts,
              Build_Raise_Statement (Finalizer_Data));
            Append_To (Stmts, Make_Raise_Statement (Loc));
         end if;

         Final_Block :=
           Make_Block_Statement (Loc,
             Declarations               =>
               Finalizer_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Statements => Stmts));

         --  Generate the block which contains the initialization call and
         --  the partial finalization code.

         --    begin
         --       [Deep_]Initialize (V (J1, ..., JN));

         --       Counter := Counter + 1;

         --    exception
         --       when others =>
         --          <finalization code>
         --    end;

         Init_Loop :=
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements         => New_List (Build_Initialization_Call),
                 Exception_Handlers => New_List (
                   Make_Exception_Handler (Loc,
                     Exception_Choices => New_List (Make_Others_Choice (Loc)),
                     Statements        => New_List (Final_Block)))));

         Append_To (Statements (Handled_Statement_Sequence (Init_Loop)),
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Counter_Id, Loc),
             Expression =>
               Make_Op_Add (Loc,
                 Left_Opnd  => New_Occurrence_Of (Counter_Id, Loc),
                 Right_Opnd => Make_Integer_Literal (Loc, 1))));

         --  Generate all initialization loops starting from the innermost
         --  dimension.

         --    for Jnn in V'Range (Dim) loop
         --       <init loop>
         --    end loop;

         J := Last (Index_List);
         Dim := Num_Dims;
         while Present (J) and then Dim > 0 loop
            Loop_Id := J;
            Prev (J);
            Remove (Loop_Id);

            Init_Loop :=
              Make_Loop_Statement (Loc,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Loop_Id,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => Make_Identifier (Loc, Name_V),
                            Attribute_Name => Name_Range,
                            Expressions    => New_List (
                              Make_Integer_Literal (Loc, Dim))))),

                Statements => New_List (Init_Loop),
                End_Label => Empty);

            Dim := Dim - 1;
         end loop;

         --  Generate the block which contains the counter variable and the
         --  initialization loops.

         --    declare
         --       Counter : Integer := 0;
         --    begin
         --       <init loop>
         --    end;

         return
           New_List (
             Make_Block_Statement (Loc,
               Declarations               => New_List (
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Counter_Id,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Integer, Loc),
                   Expression          => Make_Integer_Literal (Loc, 0))),

               Handled_Statement_Sequence =>
                 Make_Handled_Sequence_Of_Statements (Loc,
                   Statements => New_List (Init_Loop))));
      end Build_Initialize_Statements;

      -----------------------
      -- New_References_To --
      -----------------------

      function New_References_To
        (L   : List_Id;
         Loc : Source_Ptr) return List_Id
      is
         Refs : constant List_Id := New_List;
         Id   : Node_Id;

      begin
         Id := First (L);
         while Present (Id) loop
            Append_To (Refs, New_Occurrence_Of (Id, Loc));
            Next (Id);
         end loop;

         return Refs;
      end New_References_To;

   --  Start of processing for Make_Deep_Array_Body

   begin
      case Prim is
         when Address_Case =>
            return Make_Finalize_Address_Stmts (Typ);

         when Adjust_Case   |
              Finalize_Case =>
            return Build_Adjust_Or_Finalize_Statements (Typ);

         when Initialize_Case =>
            return Build_Initialize_Statements (Typ);
      end case;
   end Make_Deep_Array_Body;

   --------------------
   -- Make_Deep_Proc --
   --------------------

   function Make_Deep_Proc
     (Prim  : Final_Primitives;
      Typ   : Entity_Id;
      Stmts : List_Id) return Entity_Id
   is
      Loc     : constant Source_Ptr := Sloc (Typ);
      Formals : List_Id;
      Proc_Id : Entity_Id;

   begin
      --  Create the object formal, generate:
      --    V : System.Address

      if Prim = Address_Case then
         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Parameter_Type      =>
               New_Occurrence_Of (RTE (RE_Address), Loc)));

      --  Default case

      else
         --  V : in out Typ

         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             In_Present          => True,
             Out_Present         => True,
             Parameter_Type      => New_Occurrence_Of (Typ, Loc)));

         --  F : Boolean := True

         if Prim = Adjust_Case
           or else Prim = Finalize_Case
         then
            Append_To (Formals,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Make_Defining_Identifier (Loc, Name_F),
                Parameter_Type      =>
                  New_Occurrence_Of (Standard_Boolean, Loc),
                Expression          =>
                  New_Occurrence_Of (Standard_True, Loc)));
         end if;
      end if;

      Proc_Id :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name (Typ, Deep_Name_Of (Prim)));

      --  Generate:
      --    procedure Deep_Initialize / Adjust / Finalize (V : in out <typ>) is
      --    begin
      --       <stmts>
      --    exception                --  Finalize and Adjust cases only
      --       raise Program_Error;
      --    end Deep_Initialize / Adjust / Finalize;

      --       or

      --    procedure Finalize_Address (V : System.Address) is
      --    begin
      --       <stmts>
      --    end Finalize_Address;

      Discard_Node (
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Proc_Id,
              Parameter_Specifications => Formals),

          Declarations => Empty_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Stmts)));

      return Proc_Id;
   end Make_Deep_Proc;

   ---------------------------
   -- Make_Deep_Record_Body --
   ---------------------------

   function Make_Deep_Record_Body
     (Prim     : Final_Primitives;
      Typ      : Entity_Id;
      Is_Local : Boolean := False) return List_Id
   is
      function Build_Adjust_Statements (Typ : Entity_Id) return List_Id;
      --  Build the statements necessary to adjust a record type. The type may
      --  have discriminants and contain variant parts. Generate:
      --
      --    begin
      --       begin
      --          [Deep_]Adjust (V.Comp_1);
      --       exception
      --          when Id : others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E, Get_Current_Excep.all.all);
      --             end if;
      --       end;
      --       .  .  .
      --       begin
      --          [Deep_]Adjust (V.Comp_N);
      --       exception
      --          when Id : others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E, Get_Current_Excep.all.all);
      --             end if;
      --       end;
      --
      --       begin
      --          Deep_Adjust (V._parent, False);  --  If applicable
      --       exception
      --          when Id : others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E, Get_Current_Excep.all.all);
      --             end if;
      --       end;
      --
      --       if F then
      --          begin
      --             Adjust (V);  --  If applicable
      --          exception
      --             when others =>
      --                if not Raised then
      --                   Raised := True;
      --                   Save_Occurence (E, Get_Current_Excep.all.all);
      --                end if;
      --          end;
      --       end if;
      --
      --       if Raised and then not Abort then
      --          Raise_From_Controlled_Operation (E);
      --       end if;
      --    end;

      function Build_Finalize_Statements (Typ : Entity_Id) return List_Id;
      --  Build the statements necessary to finalize a record type. The type
      --  may have discriminants and contain variant parts. Generate:
      --
      --    declare
      --       Abort  : constant Boolean := Triggered_By_Abort;
      --         <or>
      --       Abort  : constant Boolean := False;  --  no abort
      --       E      : Exception_Occurence;
      --       Raised : Boolean := False;
      --
      --    begin
      --       if F then
      --          begin
      --             Finalize (V);  --  If applicable
      --          exception
      --             when others =>
      --                if not Raised then
      --                   Raised := True;
      --                   Save_Occurence (E, Get_Current_Excep.all.all);
      --                end if;
      --          end;
      --       end if;
      --
      --       case Variant_1 is
      --          when Value_1 =>
      --             case State_Counter_N =>  --  If Is_Local is enabled
      --                when N =>                 .
      --                   goto LN;               .
      --                ...                       .
      --                when 1 =>                 .
      --                   goto L1;               .
      --                when others =>            .
      --                   goto L0;               .
      --             end case;                    .
      --
      --             <<LN>>                   --  If Is_Local is enabled
      --             begin
      --                [Deep_]Finalize (V.Comp_N);
      --             exception
      --                when others =>
      --                   if not Raised then
      --                      Raised := True;
      --                      Save_Occurence (E, Get_Current_Excep.all.all);
      --                   end if;
      --             end;
      --             .  .  .
      --             <<L1>>
      --             begin
      --                [Deep_]Finalize (V.Comp_1);
      --             exception
      --                when others =>
      --                   if not Raised then
      --                      Raised := True;
      --                      Save_Occurence (E, Get_Current_Excep.all.all);
      --                   end if;
      --             end;
      --             <<L0>>
      --       end case;
      --
      --       case State_Counter_1 =>  --  If Is_Local is enabled
      --          when M =>                 .
      --             goto LM;               .
      --       ...
      --
      --       begin
      --          Deep_Finalize (V._parent, False);  --  If applicable
      --       exception
      --          when Id : others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E, Get_Current_Excep.all.all);
      --             end if;
      --       end;
      --
      --       if Raised and then not Abort then
      --          Raise_From_Controlled_Operation (E);
      --       end if;
      --    end;

      function Parent_Field_Type (Typ : Entity_Id) return Entity_Id;
      --  Given a derived tagged type Typ, traverse all components, find field
      --  _parent and return its type.

      procedure Preprocess_Components
        (Comps     : Node_Id;
         Num_Comps : out Int;
         Has_POC   : out Boolean);
      --  Examine all components in component list Comps, count all controlled
      --  components and determine whether at least one of them is per-object
      --  constrained. Component _parent is always skipped.

      -----------------------------
      -- Build_Adjust_Statements --
      -----------------------------

      function Build_Adjust_Statements (Typ : Entity_Id) return List_Id is
         Loc             : constant Source_Ptr := Sloc (Typ);
         Typ_Def         : constant Node_Id := Type_Definition (Parent (Typ));
         Bod_Stmts       : List_Id;
         Finalizer_Data  : Finalization_Exception_Data;
         Finalizer_Decls : List_Id := No_List;
         Rec_Def         : Node_Id;
         Var_Case        : Node_Id;

         Exceptions_OK : constant Boolean :=
                           not Restriction_Active (No_Exception_Propagation);

         function Process_Component_List_For_Adjust
           (Comps : Node_Id) return List_Id;
         --  Build all necessary adjust statements for a single component list

         ---------------------------------------
         -- Process_Component_List_For_Adjust --
         ---------------------------------------

         function Process_Component_List_For_Adjust
           (Comps : Node_Id) return List_Id
         is
            Stmts     : constant List_Id := New_List;
            Decl      : Node_Id;
            Decl_Id   : Entity_Id;
            Decl_Typ  : Entity_Id;
            Has_POC   : Boolean;
            Num_Comps : Int;

            procedure Process_Component_For_Adjust (Decl : Node_Id);
            --  Process the declaration of a single controlled component

            ----------------------------------
            -- Process_Component_For_Adjust --
            ----------------------------------

            procedure Process_Component_For_Adjust (Decl : Node_Id) is
               Id       : constant Entity_Id := Defining_Identifier (Decl);
               Typ      : constant Entity_Id := Etype (Id);
               Adj_Stmt : Node_Id;

            begin
               --  Generate:
               --    [Deep_]Adjust (V.Id);  --  No_Exception_Propagation

               --    begin                  --  Exception handlers allowed
               --       [Deep_]Adjust (V.Id);
               --    exception
               --       when others =>
               --          if not Raised then
               --             Raised := True;
               --             Save_Occurrence (E, Get_Current_Excep.all.all);
               --          end if;
               --    end;

               Adj_Stmt :=
                 Make_Adjust_Call (
                   Obj_Ref =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_V),
                       Selector_Name => Make_Identifier (Loc, Chars (Id))),
                   Typ     => Typ);

               if Exceptions_OK then
                  Adj_Stmt :=
                    Make_Block_Statement (Loc,
                      Handled_Statement_Sequence =>
                        Make_Handled_Sequence_Of_Statements (Loc,
                          Statements         => New_List (Adj_Stmt),
                          Exception_Handlers => New_List (
                            Build_Exception_Handler (Finalizer_Data))));
               end if;

               Append_To (Stmts, Adj_Stmt);
            end Process_Component_For_Adjust;

         --  Start of processing for Process_Component_List_For_Adjust

         begin
            --  Perform an initial check, determine the number of controlled
            --  components in the current list and whether at least one of them
            --  is per-object constrained.

            Preprocess_Components (Comps, Num_Comps, Has_POC);

            --  The processing in this routine is done in the following order:
            --    1) Regular components
            --    2) Per-object constrained components
            --    3) Variant parts

            if Num_Comps > 0 then

               --  Process all regular components in order of declarations

               Decl := First_Non_Pragma (Component_Items (Comps));
               while Present (Decl) loop
                  Decl_Id  := Defining_Identifier (Decl);
                  Decl_Typ := Etype (Decl_Id);

                  --  Skip _parent as well as per-object constrained components

                  if Chars (Decl_Id) /= Name_uParent
                    and then Needs_Finalization (Decl_Typ)
                  then
                     if Has_Access_Constraint (Decl_Id)
                       and then No (Expression (Decl))
                     then
                        null;
                     else
                        Process_Component_For_Adjust (Decl);
                     end if;
                  end if;

                  Next_Non_Pragma (Decl);
               end loop;

               --  Process all per-object constrained components in order of
               --  declarations.

               if Has_POC then
                  Decl := First_Non_Pragma (Component_Items (Comps));
                  while Present (Decl) loop
                     Decl_Id  := Defining_Identifier (Decl);
                     Decl_Typ := Etype (Decl_Id);

                     --  Skip _parent

                     if Chars (Decl_Id) /= Name_uParent
                       and then Needs_Finalization (Decl_Typ)
                       and then Has_Access_Constraint (Decl_Id)
                       and then No (Expression (Decl))
                     then
                        Process_Component_For_Adjust (Decl);
                     end if;

                     Next_Non_Pragma (Decl);
                  end loop;
               end if;
            end if;

            --  Process all variants, if any

            Var_Case := Empty;
            if Present (Variant_Part (Comps)) then
               declare
                  Var_Alts : constant List_Id := New_List;
                  Var      : Node_Id;

               begin
                  Var := First_Non_Pragma (Variants (Variant_Part (Comps)));
                  while Present (Var) loop

                     --  Generate:
                     --     when <discrete choices> =>
                     --        <adjust statements>

                     Append_To (Var_Alts,
                       Make_Case_Statement_Alternative (Loc,
                         Discrete_Choices =>
                           New_Copy_List (Discrete_Choices (Var)),
                         Statements       =>
                           Process_Component_List_For_Adjust (
                             Component_List (Var))));

                     Next_Non_Pragma (Var);
                  end loop;

                  --  Generate:
                  --     case V.<discriminant> is
                  --        when <discrete choices 1> =>
                  --           <adjust statements 1>
                  --        ...
                  --        when <discrete choices N> =>
                  --           <adjust statements N>
                  --     end case;

                  Var_Case :=
                    Make_Case_Statement (Loc,
                      Expression =>
                        Make_Selected_Component (Loc,
                          Prefix        => Make_Identifier (Loc, Name_V),
                          Selector_Name =>
                            Make_Identifier (Loc,
                              Chars => Chars (Name (Variant_Part (Comps))))),
                      Alternatives => Var_Alts);
               end;
            end if;

            --  Add the variant case statement to the list of statements

            if Present (Var_Case) then
               Append_To (Stmts, Var_Case);
            end if;

            --  If the component list did not have any controlled components
            --  nor variants, return null.

            if Is_Empty_List (Stmts) then
               Append_To (Stmts, Make_Null_Statement (Loc));
            end if;

            return Stmts;
         end Process_Component_List_For_Adjust;

      --  Start of processing for Build_Adjust_Statements

      begin
         Finalizer_Decls := New_List;
         Build_Object_Declarations (Finalizer_Data, Finalizer_Decls, Loc);

         if Nkind (Typ_Def) = N_Derived_Type_Definition then
            Rec_Def := Record_Extension_Part (Typ_Def);
         else
            Rec_Def := Typ_Def;
         end if;

         --  Create an adjust sequence for all record components

         if Present (Component_List (Rec_Def)) then
            Bod_Stmts :=
              Process_Component_List_For_Adjust (Component_List (Rec_Def));
         end if;

         --  A derived record type must adjust all inherited components. This
         --  action poses the following problem:

         --    procedure Deep_Adjust (Obj : in out Parent_Typ) is
         --    begin
         --       Adjust (Obj);
         --       ...

         --    procedure Deep_Adjust (Obj : in out Derived_Typ) is
         --    begin
         --       Deep_Adjust (Obj._parent);
         --       ...
         --       Adjust (Obj);
         --       ...

         --  Adjusting the derived type will invoke Adjust of the parent and
         --  then that of the derived type. This is undesirable because both
         --  routines may modify shared components. Only the Adjust of the
         --  derived type should be invoked.

         --  To prevent this double adjustment of shared components,
         --  Deep_Adjust uses a flag to control the invocation of Adjust:

         --    procedure Deep_Adjust
         --      (Obj  : in out Some_Type;
         --       Flag : Boolean := True)
         --    is
         --    begin
         --       if Flag then
         --          Adjust (Obj);
         --       end if;
         --       ...

         --  When Deep_Adjust is invokes for field _parent, a value of False is
         --  provided for the flag:

         --    Deep_Adjust (Obj._parent, False);

         if Is_Tagged_Type (Typ) and then Is_Derived_Type (Typ) then
            declare
               Par_Typ  : constant Entity_Id := Parent_Field_Type (Typ);
               Adj_Stmt : Node_Id;
               Call     : Node_Id;

            begin
               if Needs_Finalization (Par_Typ) then
                  Call :=
                    Make_Adjust_Call
                      (Obj_Ref    =>
                         Make_Selected_Component (Loc,
                           Prefix        => Make_Identifier (Loc, Name_V),
                           Selector_Name =>
                             Make_Identifier (Loc, Name_uParent)),
                       Typ        => Par_Typ,
                       For_Parent => True);

                  --  Generate:
                  --    Deep_Adjust (V._parent, False);  --  No_Except_Propagat

                  --    begin                            --  Exceptions OK
                  --       Deep_Adjust (V._parent, False);
                  --    exception
                  --       when Id : others =>
                  --          if not Raised then
                  --             Raised := True;
                  --             Save_Occurrence (E,
                  --               Get_Current_Excep.all.all);
                  --          end if;
                  --    end;

                  if Present (Call) then
                     Adj_Stmt := Call;

                     if Exceptions_OK then
                        Adj_Stmt :=
                          Make_Block_Statement (Loc,
                            Handled_Statement_Sequence =>
                              Make_Handled_Sequence_Of_Statements (Loc,
                                Statements         => New_List (Adj_Stmt),
                                Exception_Handlers => New_List (
                                  Build_Exception_Handler (Finalizer_Data))));
                     end if;

                     Prepend_To (Bod_Stmts, Adj_Stmt);
                  end if;
               end if;
            end;
         end if;

         --  Adjust the object. This action must be performed last after all
         --  components have been adjusted.

         if Is_Controlled (Typ) then
            declare
               Adj_Stmt : Node_Id;
               Proc     : Entity_Id;

            begin
               Proc := Find_Prim_Op (Typ, Name_Adjust);

               --  Generate:
               --    if F then
               --       Adjust (V);  --  No_Exception_Propagation

               --       begin        --  Exception handlers allowed
               --          Adjust (V);
               --       exception
               --          when others =>
               --             if not Raised then
               --                Raised := True;
               --                Save_Occurrence (E,
               --                  Get_Current_Excep.all.all);
               --             end if;
               --       end;
               --    end if;

               if Present (Proc) then
                  Adj_Stmt :=
                    Make_Procedure_Call_Statement (Loc,
                      Name                   => New_Occurrence_Of (Proc, Loc),
                      Parameter_Associations => New_List (
                        Make_Identifier (Loc, Name_V)));

                  if Exceptions_OK then
                     Adj_Stmt :=
                       Make_Block_Statement (Loc,
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements (Loc,
                             Statements         => New_List (Adj_Stmt),
                             Exception_Handlers => New_List (
                               Build_Exception_Handler
                                 (Finalizer_Data))));
                  end if;

                  Append_To (Bod_Stmts,
                    Make_If_Statement (Loc,
                      Condition       => Make_Identifier (Loc, Name_F),
                      Then_Statements => New_List (Adj_Stmt)));
               end if;
            end;
         end if;

         --  At this point either all adjustment statements have been generated
         --  or the type is not controlled.

         if Is_Empty_List (Bod_Stmts) then
            Append_To (Bod_Stmts, Make_Null_Statement (Loc));

            return Bod_Stmts;

         --  Generate:
         --    declare
         --       Abort  : constant Boolean := Triggered_By_Abort;
         --         <or>
         --       Abort  : constant Boolean := False;  --  no abort

         --       E      : Exception_Occurence;
         --       Raised : Boolean := False;

         --    begin
         --       <adjust statements>

         --       if Raised and then not Abort then
         --          Raise_From_Controlled_Operation (E);
         --       end if;
         --    end;

         else
            if Exceptions_OK then
               Append_To (Bod_Stmts,
                 Build_Raise_Statement (Finalizer_Data));
            end if;

            return
              New_List (
                Make_Block_Statement (Loc,
                  Declarations               =>
                    Finalizer_Decls,
                  Handled_Statement_Sequence =>
                    Make_Handled_Sequence_Of_Statements (Loc, Bod_Stmts)));
         end if;
      end Build_Adjust_Statements;

      -------------------------------
      -- Build_Finalize_Statements --
      -------------------------------

      function Build_Finalize_Statements (Typ : Entity_Id) return List_Id is
         Loc             : constant Source_Ptr := Sloc (Typ);
         Typ_Def         : constant Node_Id := Type_Definition (Parent (Typ));
         Bod_Stmts       : List_Id;
         Counter         : Int := 0;
         Finalizer_Data  : Finalization_Exception_Data;
         Finalizer_Decls : List_Id := No_List;
         Rec_Def         : Node_Id;
         Var_Case        : Node_Id;

         Exceptions_OK : constant Boolean :=
                           not Restriction_Active (No_Exception_Propagation);

         function Process_Component_List_For_Finalize
           (Comps : Node_Id) return List_Id;
         --  Build all necessary finalization statements for a single component
         --  list. The statements may include a jump circuitry if flag Is_Local
         --  is enabled.

         -----------------------------------------
         -- Process_Component_List_For_Finalize --
         -----------------------------------------

         function Process_Component_List_For_Finalize
           (Comps : Node_Id) return List_Id
         is
            Alts       : List_Id;
            Counter_Id : Entity_Id;
            Decl       : Node_Id;
            Decl_Id    : Entity_Id;
            Decl_Typ   : Entity_Id;
            Decls      : List_Id;
            Has_POC    : Boolean;
            Jump_Block : Node_Id;
            Label      : Node_Id;
            Label_Id   : Entity_Id;
            Num_Comps  : Int;
            Stmts      : List_Id;

            procedure Process_Component_For_Finalize
              (Decl  : Node_Id;
               Alts  : List_Id;
               Decls : List_Id;
               Stmts : List_Id);
            --  Process the declaration of a single controlled component. If
            --  flag Is_Local is enabled, create the corresponding label and
            --  jump circuitry. Alts is the list of case alternatives, Decls
            --  is the top level declaration list where labels are declared
            --  and Stmts is the list of finalization actions.

            ------------------------------------
            -- Process_Component_For_Finalize --
            ------------------------------------

            procedure Process_Component_For_Finalize
              (Decl  : Node_Id;
               Alts  : List_Id;
               Decls : List_Id;
               Stmts : List_Id)
            is
               Id       : constant Entity_Id := Defining_Identifier (Decl);
               Typ      : constant Entity_Id := Etype (Id);
               Fin_Stmt : Node_Id;

            begin
               if Is_Local then
                  declare
                     Label    : Node_Id;
                     Label_Id : Entity_Id;

                  begin
                     --  Generate:
                     --    LN : label;

                     Label_Id :=
                       Make_Identifier (Loc,
                         Chars => New_External_Name ('L', Num_Comps));
                     Set_Entity (Label_Id,
                       Make_Defining_Identifier (Loc, Chars (Label_Id)));
                     Label := Make_Label (Loc, Label_Id);

                     Append_To (Decls,
                       Make_Implicit_Label_Declaration (Loc,
                         Defining_Identifier => Entity (Label_Id),
                         Label_Construct     => Label));

                     --  Generate:
                     --    when N =>
                     --      goto LN;

                     Append_To (Alts,
                       Make_Case_Statement_Alternative (Loc,
                         Discrete_Choices => New_List (
                           Make_Integer_Literal (Loc, Num_Comps)),

                         Statements => New_List (
                           Make_Goto_Statement (Loc,
                             Name =>
                               New_Occurrence_Of (Entity (Label_Id), Loc)))));

                     --  Generate:
                     --    <<LN>>

                     Append_To (Stmts, Label);

                     --  Decrease the number of components to be processed.
                     --  This action yields a new Label_Id in future calls.

                     Num_Comps := Num_Comps - 1;
                  end;
               end if;

               --  Generate:
               --    [Deep_]Finalize (V.Id);  --  No_Exception_Propagation

               --    begin                    --  Exception handlers allowed
               --       [Deep_]Finalize (V.Id);
               --    exception
               --       when others =>
               --          if not Raised then
               --             Raised := True;
               --             Save_Occurrence (E,
               --               Get_Current_Excep.all.all);
               --          end if;
               --    end;

               Fin_Stmt :=
                 Make_Final_Call
                   (Obj_Ref =>
                      Make_Selected_Component (Loc,
                        Prefix        => Make_Identifier (Loc, Name_V),
                        Selector_Name => Make_Identifier (Loc, Chars (Id))),
                    Typ     => Typ);

               if not Restriction_Active (No_Exception_Propagation) then
                  Fin_Stmt :=
                    Make_Block_Statement (Loc,
                      Handled_Statement_Sequence =>
                        Make_Handled_Sequence_Of_Statements (Loc,
                          Statements         => New_List (Fin_Stmt),
                          Exception_Handlers => New_List (
                            Build_Exception_Handler (Finalizer_Data))));
               end if;

               Append_To (Stmts, Fin_Stmt);
            end Process_Component_For_Finalize;

         --  Start of processing for Process_Component_List_For_Finalize

         begin
            --  Perform an initial check, look for controlled and per-object
            --  constrained components.

            Preprocess_Components (Comps, Num_Comps, Has_POC);

            --  Create a state counter to service the current component list.
            --  This step is performed before the variants are inspected in
            --  order to generate the same state counter names as those from
            --  Build_Initialize_Statements.

            if Num_Comps > 0
              and then Is_Local
            then
               Counter := Counter + 1;

               Counter_Id :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name ('C', Counter));
            end if;

            --  Process the component in the following order:
            --    1) Variants
            --    2) Per-object constrained components
            --    3) Regular components

            --  Start with the variant parts

            Var_Case := Empty;
            if Present (Variant_Part (Comps)) then
               declare
                  Var_Alts : constant List_Id := New_List;
                  Var      : Node_Id;

               begin
                  Var := First_Non_Pragma (Variants (Variant_Part (Comps)));
                  while Present (Var) loop

                     --  Generate:
                     --     when <discrete choices> =>
                     --        <finalize statements>

                     Append_To (Var_Alts,
                       Make_Case_Statement_Alternative (Loc,
                         Discrete_Choices =>
                           New_Copy_List (Discrete_Choices (Var)),
                         Statements =>
                           Process_Component_List_For_Finalize (
                             Component_List (Var))));

                     Next_Non_Pragma (Var);
                  end loop;

                  --  Generate:
                  --     case V.<discriminant> is
                  --        when <discrete choices 1> =>
                  --           <finalize statements 1>
                  --        ...
                  --        when <discrete choices N> =>
                  --           <finalize statements N>
                  --     end case;

                  Var_Case :=
                    Make_Case_Statement (Loc,
                      Expression =>
                        Make_Selected_Component (Loc,
                          Prefix        => Make_Identifier (Loc, Name_V),
                          Selector_Name =>
                            Make_Identifier (Loc,
                              Chars => Chars (Name (Variant_Part (Comps))))),
                      Alternatives => Var_Alts);
               end;
            end if;

            --  The current component list does not have a single controlled
            --  component, however it may contain variants. Return the case
            --  statement for the variants or nothing.

            if Num_Comps = 0 then
               if Present (Var_Case) then
                  return New_List (Var_Case);
               else
                  return New_List (Make_Null_Statement (Loc));
               end if;
            end if;

            --  Prepare all lists

            Alts  := New_List;
            Decls := New_List;
            Stmts := New_List;

            --  Process all per-object constrained components in reverse order

            if Has_POC then
               Decl := Last_Non_Pragma (Component_Items (Comps));
               while Present (Decl) loop
                  Decl_Id  := Defining_Identifier (Decl);
                  Decl_Typ := Etype (Decl_Id);

                  --  Skip _parent

                  if Chars (Decl_Id) /= Name_uParent
                    and then Needs_Finalization (Decl_Typ)
                    and then Has_Access_Constraint (Decl_Id)
                    and then No (Expression (Decl))
                  then
                     Process_Component_For_Finalize (Decl, Alts, Decls, Stmts);
                  end if;

                  Prev_Non_Pragma (Decl);
               end loop;
            end if;

            --  Process the rest of the components in reverse order

            Decl := Last_Non_Pragma (Component_Items (Comps));
            while Present (Decl) loop
               Decl_Id  := Defining_Identifier (Decl);
               Decl_Typ := Etype (Decl_Id);

               --  Skip _parent

               if Chars (Decl_Id) /= Name_uParent
                 and then Needs_Finalization (Decl_Typ)
               then
                  --  Skip per-object constrained components since they were
                  --  handled in the above step.

                  if Has_Access_Constraint (Decl_Id)
                    and then No (Expression (Decl))
                  then
                     null;
                  else
                     Process_Component_For_Finalize (Decl, Alts, Decls, Stmts);
                  end if;
               end if;

               Prev_Non_Pragma (Decl);
            end loop;

            --  Generate:
            --    declare
            --       LN : label;        --  If Is_Local is enabled
            --       ...                    .
            --       L0 : label;            .

            --    begin                     .
            --       case CounterX is       .
            --          when N =>           .
            --             goto LN;         .
            --          ...                 .
            --          when 1 =>           .
            --             goto L1;         .
            --          when others =>      .
            --             goto L0;         .
            --       end case;              .

            --       <<LN>>             --  If Is_Local is enabled
            --          begin
            --             [Deep_]Finalize (V.CompY);
            --          exception
            --             when Id : others =>
            --                if not Raised then
            --                   Raised := True;
            --                   Save_Occurrence (E,
            --                     Get_Current_Excep.all.all);
            --                end if;
            --          end;
            --       ...
            --       <<L0>>  --  If Is_Local is enabled
            --    end;

            if Is_Local then

               --  Add the declaration of default jump location L0, its
               --  corresponding alternative and its place in the statements.

               Label_Id := Make_Identifier (Loc, New_External_Name ('L', 0));
               Set_Entity (Label_Id,
                 Make_Defining_Identifier (Loc, Chars (Label_Id)));
               Label := Make_Label (Loc, Label_Id);

               Append_To (Decls,          --  declaration
                 Make_Implicit_Label_Declaration (Loc,
                   Defining_Identifier => Entity (Label_Id),
                   Label_Construct     => Label));

               Append_To (Alts,           --  alternative
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices => New_List (
                     Make_Others_Choice (Loc)),

                   Statements => New_List (
                     Make_Goto_Statement (Loc,
                       Name => New_Occurrence_Of (Entity (Label_Id), Loc)))));

               Append_To (Stmts, Label);  --  statement

               --  Create the jump block

               Prepend_To (Stmts,
                 Make_Case_Statement (Loc,
                   Expression   => Make_Identifier (Loc, Chars (Counter_Id)),
                   Alternatives => Alts));
            end if;

            Jump_Block :=
              Make_Block_Statement (Loc,
                Declarations               => Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc, Stmts));

            if Present (Var_Case) then
               return New_List (Var_Case, Jump_Block);
            else
               return New_List (Jump_Block);
            end if;
         end Process_Component_List_For_Finalize;

      --  Start of processing for Build_Finalize_Statements

      begin
         Finalizer_Decls := New_List;
         Build_Object_Declarations (Finalizer_Data, Finalizer_Decls, Loc);

         if Nkind (Typ_Def) = N_Derived_Type_Definition then
            Rec_Def := Record_Extension_Part (Typ_Def);
         else
            Rec_Def := Typ_Def;
         end if;

         --  Create a finalization sequence for all record components

         if Present (Component_List (Rec_Def)) then
            Bod_Stmts :=
              Process_Component_List_For_Finalize (Component_List (Rec_Def));
         end if;

         --  A derived record type must finalize all inherited components. This
         --  action poses the following problem:

         --    procedure Deep_Finalize (Obj : in out Parent_Typ) is
         --    begin
         --       Finalize (Obj);
         --       ...

         --    procedure Deep_Finalize (Obj : in out Derived_Typ) is
         --    begin
         --       Deep_Finalize (Obj._parent);
         --       ...
         --       Finalize (Obj);
         --       ...

         --  Finalizing the derived type will invoke Finalize of the parent and
         --  then that of the derived type. This is undesirable because both
         --  routines may modify shared components. Only the Finalize of the
         --  derived type should be invoked.

         --  To prevent this double adjustment of shared components,
         --  Deep_Finalize uses a flag to control the invocation of Finalize:

         --    procedure Deep_Finalize
         --      (Obj  : in out Some_Type;
         --       Flag : Boolean := True)
         --    is
         --    begin
         --       if Flag then
         --          Finalize (Obj);
         --       end if;
         --       ...

         --  When Deep_Finalize is invokes for field _parent, a value of False
         --  is provided for the flag:

         --    Deep_Finalize (Obj._parent, False);

         if Is_Tagged_Type (Typ)
           and then Is_Derived_Type (Typ)
         then
            declare
               Par_Typ  : constant Entity_Id := Parent_Field_Type (Typ);
               Call     : Node_Id;
               Fin_Stmt : Node_Id;

            begin
               if Needs_Finalization (Par_Typ) then
                  Call :=
                    Make_Final_Call
                      (Obj_Ref    =>
                         Make_Selected_Component (Loc,
                           Prefix        => Make_Identifier (Loc, Name_V),
                           Selector_Name =>
                             Make_Identifier (Loc, Name_uParent)),
                       Typ        => Par_Typ,
                       For_Parent => True);

                  --  Generate:
                  --    Deep_Finalize (V._parent, False);  --  No_Except_Propag

                  --    begin                              --  Exceptions OK
                  --       Deep_Finalize (V._parent, False);
                  --    exception
                  --       when Id : others =>
                  --          if not Raised then
                  --             Raised := True;
                  --             Save_Occurrence (E,
                  --               Get_Current_Excep.all.all);
                  --          end if;
                  --    end;

                  if Present (Call) then
                     Fin_Stmt := Call;

                     if Exceptions_OK then
                        Fin_Stmt :=
                          Make_Block_Statement (Loc,
                            Handled_Statement_Sequence =>
                              Make_Handled_Sequence_Of_Statements (Loc,
                                Statements         => New_List (Fin_Stmt),
                                Exception_Handlers => New_List (
                                  Build_Exception_Handler
                                    (Finalizer_Data))));
                     end if;

                     Append_To (Bod_Stmts, Fin_Stmt);
                  end if;
               end if;
            end;
         end if;

         --  Finalize the object. This action must be performed first before
         --  all components have been finalized.

         if Is_Controlled (Typ)
           and then not Is_Local
         then
            declare
               Fin_Stmt : Node_Id;
               Proc     : Entity_Id;

            begin
               Proc := Find_Prim_Op (Typ, Name_Finalize);

               --  Generate:
               --    if F then
               --       Finalize (V);  --  No_Exception_Propagation

               --       begin
               --          Finalize (V);
               --       exception
               --          when others =>
               --             if not Raised then
               --                Raised := True;
               --                Save_Occurrence (E,
               --                  Get_Current_Excep.all.all);
               --             end if;
               --       end;
               --    end if;

               if Present (Proc) then
                  Fin_Stmt :=
                    Make_Procedure_Call_Statement (Loc,
                      Name                   => New_Occurrence_Of (Proc, Loc),
                      Parameter_Associations => New_List (
                        Make_Identifier (Loc, Name_V)));

                  if Exceptions_OK then
                     Fin_Stmt :=
                       Make_Block_Statement (Loc,
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements (Loc,
                             Statements         => New_List (Fin_Stmt),
                             Exception_Handlers => New_List (
                               Build_Exception_Handler
                                 (Finalizer_Data))));
                  end if;

                  Prepend_To (Bod_Stmts,
                    Make_If_Statement (Loc,
                      Condition       => Make_Identifier (Loc, Name_F),
                      Then_Statements => New_List (Fin_Stmt)));
               end if;
            end;
         end if;

         --  At this point either all finalization statements have been
         --  generated or the type is not controlled.

         if No (Bod_Stmts) then
            return New_List (Make_Null_Statement (Loc));

         --  Generate:
         --    declare
         --       Abort  : constant Boolean := Triggered_By_Abort;
         --         <or>
         --       Abort  : constant Boolean := False;  --  no abort

         --       E      : Exception_Occurence;
         --       Raised : Boolean := False;

         --    begin
         --       <finalize statements>

         --       if Raised and then not Abort then
         --          Raise_From_Controlled_Operation (E);
         --       end if;
         --    end;

         else
            if Exceptions_OK then
               Append_To (Bod_Stmts,
                 Build_Raise_Statement (Finalizer_Data));
            end if;

            return
              New_List (
                Make_Block_Statement (Loc,
                  Declarations               =>
                    Finalizer_Decls,
                  Handled_Statement_Sequence =>
                    Make_Handled_Sequence_Of_Statements (Loc, Bod_Stmts)));
         end if;
      end Build_Finalize_Statements;

      -----------------------
      -- Parent_Field_Type --
      -----------------------

      function Parent_Field_Type (Typ : Entity_Id) return Entity_Id is
         Field : Entity_Id;

      begin
         Field := First_Entity (Typ);
         while Present (Field) loop
            if Chars (Field) = Name_uParent then
               return Etype (Field);
            end if;

            Next_Entity (Field);
         end loop;

         --  A derived tagged type should always have a parent field

         raise Program_Error;
      end Parent_Field_Type;

      ---------------------------
      -- Preprocess_Components --
      ---------------------------

      procedure Preprocess_Components
        (Comps     : Node_Id;
         Num_Comps : out Int;
         Has_POC   : out Boolean)
      is
         Decl : Node_Id;
         Id   : Entity_Id;
         Typ  : Entity_Id;

      begin
         Num_Comps := 0;
         Has_POC   := False;

         Decl := First_Non_Pragma (Component_Items (Comps));
         while Present (Decl) loop
            Id  := Defining_Identifier (Decl);
            Typ := Etype (Id);

            --  Skip field _parent

            if Chars (Id) /= Name_uParent
              and then Needs_Finalization (Typ)
            then
               Num_Comps := Num_Comps + 1;

               if Has_Access_Constraint (Id)
                 and then No (Expression (Decl))
               then
                  Has_POC := True;
               end if;
            end if;

            Next_Non_Pragma (Decl);
         end loop;
      end Preprocess_Components;

   --  Start of processing for Make_Deep_Record_Body

   begin
      case Prim is
         when Address_Case =>
            return Make_Finalize_Address_Stmts (Typ);

         when Adjust_Case =>
            return Build_Adjust_Statements (Typ);

         when Finalize_Case =>
            return Build_Finalize_Statements (Typ);

         when Initialize_Case =>
            declare
               Loc : constant Source_Ptr := Sloc (Typ);

            begin
               if Is_Controlled (Typ) then
                  return New_List (
                    Make_Procedure_Call_Statement (Loc,
                      Name                   =>
                        New_Occurrence_Of
                          (Find_Prim_Op (Typ, Name_Of (Prim)), Loc),
                      Parameter_Associations => New_List (
                        Make_Identifier (Loc, Name_V))));
               else
                  return Empty_List;
               end if;
            end;
      end case;
   end Make_Deep_Record_Body;

   ----------------------
   -- Make_Final_Call --
   ----------------------

   function Make_Final_Call
     (Obj_Ref    : Node_Id;
      Typ        : Entity_Id;
      For_Parent : Boolean := False) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Obj_Ref);
      Atyp   : Entity_Id;
      Fin_Id : Entity_Id := Empty;
      Ref    : Node_Id;
      Utyp   : Entity_Id;

   begin
      --  Recover the proper type which contains [Deep_]Finalize

      if Is_Class_Wide_Type (Typ) then
         Utyp := Root_Type (Typ);
         Atyp := Utyp;
         Ref  := Obj_Ref;

      elsif Is_Concurrent_Type (Typ) then
         Utyp := Corresponding_Record_Type (Typ);
         Atyp := Empty;
         Ref  := Convert_Concurrent (Obj_Ref, Typ);

      elsif Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
        and then Is_Concurrent_Type (Full_View (Typ))
      then
         Utyp := Corresponding_Record_Type (Full_View (Typ));
         Atyp := Typ;
         Ref  := Convert_Concurrent (Obj_Ref, Full_View (Typ));

      else
         Utyp := Typ;
         Atyp := Typ;
         Ref  := Obj_Ref;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));
      Set_Assignment_OK (Ref);

      --  Deal with non-tagged derivation of private views. If the parent type
      --  is a protected type, Deep_Finalize is found on the corresponding
      --  record of the ancestor.

      if Is_Untagged_Derivation (Typ) then
         if Is_Protected_Type (Typ) then
            Utyp := Corresponding_Record_Type (Root_Type (Base_Type (Typ)));
         else
            Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));

            if Is_Protected_Type (Utyp) then
               Utyp := Corresponding_Record_Type (Utyp);
            end if;
         end if;

         Ref := Unchecked_Convert_To (Utyp, Ref);
         Set_Assignment_OK (Ref);
      end if;

      --  Deal with derived private types which do not inherit primitives from
      --  their parents. In this case, [Deep_]Finalize can be found in the full
      --  view of the parent type.

      if Is_Tagged_Type (Utyp)
        and then Is_Derived_Type (Utyp)
        and then Is_Empty_Elmt_List (Primitive_Operations (Utyp))
        and then Is_Private_Type (Etype (Utyp))
        and then Present (Full_View (Etype (Utyp)))
      then
         Utyp := Full_View (Etype (Utyp));
         Ref  := Unchecked_Convert_To (Utyp, Ref);
         Set_Assignment_OK (Ref);
      end if;

      --  When dealing with the completion of a private type, use the base type
      --  instead.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Present (Atyp) and then Is_Private_Type (Atyp));

         Utyp := Base_Type (Utyp);
         Ref  := Unchecked_Convert_To (Utyp, Ref);
         Set_Assignment_OK (Ref);
      end if;

      --  Select the appropriate version of Finalize

      if For_Parent then
         if Has_Controlled_Component (Utyp) then
            Fin_Id := Find_Prim_Op (Utyp, TSS_Deep_Finalize);
         end if;

      --  Class-wide types, interfaces and types with controlled components

      elsif Is_Class_Wide_Type (Typ)
        or else Is_Interface (Typ)
        or else Has_Controlled_Component (Utyp)
      then
         if Is_Tagged_Type (Utyp) then
            Fin_Id := Find_Prim_Op (Utyp, TSS_Deep_Finalize);
         else
            Fin_Id := TSS (Utyp, TSS_Deep_Finalize);
         end if;

      --  Derivations from [Limited_]Controlled

      elsif Is_Controlled (Utyp) then
         if Has_Controlled_Component (Utyp) then
            Fin_Id := Find_Prim_Op (Utyp, TSS_Deep_Finalize);
         else
            Fin_Id := Find_Prim_Op (Utyp, Name_Of (Finalize_Case));
         end if;

      --  Tagged types

      elsif Is_Tagged_Type (Utyp) then
         Fin_Id := Find_Prim_Op (Utyp, TSS_Deep_Finalize);

      else
         raise Program_Error;
      end if;

      if Present (Fin_Id) then

         --  When finalizing a class-wide object, do not convert to the root
         --  type in order to produce a dispatching call.

         if Is_Class_Wide_Type (Typ) then
            null;

         --  Ensure that a finalization routine is at least decorated in order
         --  to inspect the object parameter.

         elsif Analyzed (Fin_Id)
           or else Ekind (Fin_Id) = E_Procedure
         then
            --  In certain cases, such as the creation of Stream_Read, the
            --  visible entity of the type is its full view. Since Stream_Read
            --  will have to create an object of type Typ, the local object
            --  will be finalzed by the scope finalizer generated later on. The
            --  object parameter of Deep_Finalize will always use the private
            --  view of the type. To avoid such a clash between a private and a
            --  full view, perform an unchecked conversion of the object
            --  reference to the private view.

            declare
               Formal_Typ : constant Entity_Id :=
                              Etype (First_Formal (Fin_Id));
            begin
               if Is_Private_Type (Formal_Typ)
                 and then Present (Full_View (Formal_Typ))
                 and then Full_View (Formal_Typ) = Utyp
               then
                  Ref := Unchecked_Convert_To (Formal_Typ, Ref);
               end if;
            end;

            Ref := Convert_View (Fin_Id, Ref);
         end if;

         return Make_Call (Loc, Fin_Id, New_Copy_Tree (Ref), For_Parent);
      else
         return Empty;
      end if;
   end Make_Final_Call;

   --------------------------------
   -- Make_Finalize_Address_Body --
   --------------------------------

   procedure Make_Finalize_Address_Body (Typ : Entity_Id) is
      Is_Task : constant Boolean :=
                  Ekind (Typ) = E_Record_Type
                    and then Is_Concurrent_Record_Type (Typ)
                    and then Ekind (Corresponding_Concurrent_Type (Typ)) =
                               E_Task_Type;
      Loc     : constant Source_Ptr := Sloc (Typ);
      Proc_Id : Entity_Id;
      Stmts   : List_Id;

   begin
      --  The corresponding records of task types are not controlled by design.
      --  For the sake of completeness, create an empty Finalize_Address to be
      --  used in task class-wide allocations.

      if Is_Task then
         null;

      --  Nothing to do if the type is not controlled or it already has a
      --  TSS entry for Finalize_Address. Skip class-wide subtypes which do not
      --  come from source. These are usually generated for completeness and
      --  do not need the Finalize_Address primitive.

      elsif not Needs_Finalization (Typ)
        or else Is_Abstract_Type (Typ)
        or else Present (TSS (Typ, TSS_Finalize_Address))
        or else
          (Is_Class_Wide_Type (Typ)
            and then Ekind (Root_Type (Typ)) = E_Record_Subtype
            and then not Comes_From_Source (Root_Type (Typ)))
      then
         return;
      end if;

      Proc_Id :=
        Make_Defining_Identifier (Loc,
          Make_TSS_Name (Typ, TSS_Finalize_Address));

      --  Generate:

      --    procedure <Typ>FD (V : System.Address) is
      --    begin
      --       null;                            --  for tasks

      --       declare                          --  for all other types
      --          type Pnn is access all Typ;
      --          for Pnn'Storage_Size use 0;
      --       begin
      --          [Deep_]Finalize (Pnn (V).all);
      --       end;
      --    end TypFD;

      if Is_Task then
         Stmts := New_List (Make_Null_Statement (Loc));
      else
         Stmts := Make_Finalize_Address_Stmts (Typ);
      end if;

      Discard_Node (
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Proc_Id,

              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_V),
                  Parameter_Type =>
                    New_Occurrence_Of (RTE (RE_Address), Loc)))),

          Declarations => No_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts)));

      Set_TSS (Typ, Proc_Id);
   end Make_Finalize_Address_Body;

   ---------------------------------
   -- Make_Finalize_Address_Stmts --
   ---------------------------------

   function Make_Finalize_Address_Stmts (Typ : Entity_Id) return List_Id is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Ptr_Typ  : constant Entity_Id  := Make_Temporary (Loc, 'P');
      Decls    : List_Id;
      Desg_Typ : Entity_Id;
      Obj_Expr : Node_Id;

   begin
      if Is_Array_Type (Typ) then
         if Is_Constrained (First_Subtype (Typ)) then
            Desg_Typ := First_Subtype (Typ);
         else
            Desg_Typ := Base_Type (Typ);
         end if;

      --  Class-wide types of constrained root types

      elsif Is_Class_Wide_Type (Typ)
        and then Has_Discriminants (Root_Type (Typ))
        and then not
          Is_Empty_Elmt_List (Discriminant_Constraint (Root_Type (Typ)))
      then
         declare
            Parent_Typ : Entity_Id;

         begin
            --  Climb the parent type chain looking for a non-constrained type

            Parent_Typ := Root_Type (Typ);
            while Parent_Typ /= Etype (Parent_Typ)
              and then Has_Discriminants (Parent_Typ)
              and then not
                Is_Empty_Elmt_List (Discriminant_Constraint (Parent_Typ))
            loop
               Parent_Typ := Etype (Parent_Typ);
            end loop;

            --  Handle views created for tagged types with unknown
            --  discriminants.

            if Is_Underlying_Record_View (Parent_Typ) then
               Parent_Typ := Underlying_Record_View (Parent_Typ);
            end if;

            Desg_Typ := Class_Wide_Type (Underlying_Type (Parent_Typ));
         end;

      --  General case

      else
         Desg_Typ := Typ;
      end if;

      --  Generate:
      --    type Ptr_Typ is access all Typ;
      --    for Ptr_Typ'Storage_Size use 0;

      Decls := New_List (
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Ptr_Typ,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication => New_Occurrence_Of (Desg_Typ, Loc))),

        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Occurrence_Of (Ptr_Typ, Loc),
          Chars      => Name_Storage_Size,
          Expression => Make_Integer_Literal (Loc, 0)));

      Obj_Expr := Make_Identifier (Loc, Name_V);

      --  Unconstrained arrays require special processing in order to retrieve
      --  the elements. To achieve this, we have to skip the dope vector which
      --  lays in front of the elements and then use a thin pointer to perform
      --  the address-to-access conversion.

      if Is_Array_Type (Typ)
        and then not Is_Constrained (First_Subtype (Typ))
      then
         declare
            Dope_Id : Entity_Id;

         begin
            --  Ensure that Ptr_Typ a thin pointer, generate:
            --    for Ptr_Typ'Size use System.Address'Size;

            Append_To (Decls,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Occurrence_Of (Ptr_Typ, Loc),
                Chars      => Name_Size,
                Expression =>
                  Make_Integer_Literal (Loc, System_Address_Size)));

            --  Generate:
            --    Dnn : constant Storage_Offset :=
            --            Desg_Typ'Descriptor_Size / Storage_Unit;

            Dope_Id := Make_Temporary (Loc, 'D');

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Dope_Id,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Storage_Offset), Loc),
                Expression          =>
                  Make_Op_Divide (Loc,
                    Left_Opnd  =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (Desg_Typ, Loc),
                        Attribute_Name => Name_Descriptor_Size),
                    Right_Opnd =>
                      Make_Integer_Literal (Loc, System_Storage_Unit))));

            --  Shift the address from the start of the dope vector to the
            --  start of the elements:
            --
            --    V + Dnn
            --
            --  Note that this is done through a wrapper routine since RTSfind
            --  cannot retrieve operations with string names of the form "+".

            Obj_Expr :=
              Make_Function_Call (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_Add_Offset_To_Address), Loc),
                Parameter_Associations => New_List (
                  Obj_Expr,
                  New_Occurrence_Of (Dope_Id, Loc)));
         end;
      end if;

      --  Create the block and the finalization call

      return New_List (
        Make_Block_Statement (Loc,
          Declarations => Decls,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Final_Call (
                  Obj_Ref =>
                    Make_Explicit_Dereference (Loc,
                      Prefix => Unchecked_Convert_To (Ptr_Typ, Obj_Expr)),
                  Typ => Desg_Typ)))));
   end Make_Finalize_Address_Stmts;

   -------------------------------------
   -- Make_Handler_For_Ctrl_Operation --
   -------------------------------------

   --  Generate:

   --    when E : others =>
   --      Raise_From_Controlled_Operation (E);

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
      --  Standard run-time, .NET/JVM targets: add choice parameter E and pass
      --  it to Raise_From_Controlled_Operation so that the original exception
      --  name and message can be recorded in the exception message for
      --  Program_Error.

      if RTE_Available (RE_Raise_From_Controlled_Operation) then
         E_Occ := Make_Defining_Identifier (Loc, Name_E);
         Raise_Node :=
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of
                 (RTE (RE_Raise_From_Controlled_Operation), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (E_Occ, Loc)));

      --  Restricted run-time: exception messages are not supported

      else
         E_Occ := Empty;
         Raise_Node :=
           Make_Raise_Program_Error (Loc,
             Reason => PE_Finalize_Raised_Exception);
      end if;

      return
        Make_Implicit_Exception_Handler (Loc,
          Exception_Choices => New_List (Make_Others_Choice (Loc)),
          Choice_Parameter  => E_Occ,
          Statements        => New_List (Raise_Node));
   end Make_Handler_For_Ctrl_Operation;

   --------------------
   -- Make_Init_Call --
   --------------------

   function Make_Init_Call
     (Obj_Ref : Node_Id;
      Typ     : Entity_Id) return Node_Id
   is
      Loc     : constant Source_Ptr := Sloc (Obj_Ref);
      Is_Conc : Boolean;
      Proc    : Entity_Id;
      Ref     : Node_Id;
      Utyp    : Entity_Id;

   begin
      --  Deal with the type and object reference. Depending on the context, an
      --  object reference may need several conversions.

      if Is_Concurrent_Type (Typ) then
         Is_Conc := True;
         Utyp    := Corresponding_Record_Type (Typ);
         Ref     := Convert_Concurrent (Obj_Ref, Typ);

      elsif Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
        and then Is_Concurrent_Type (Underlying_Type (Typ))
      then
         Is_Conc := True;
         Utyp    := Corresponding_Record_Type (Underlying_Type (Typ));
         Ref     := Convert_Concurrent (Obj_Ref, Underlying_Type (Typ));

      else
         Is_Conc := False;
         Utyp    := Typ;
         Ref     := Obj_Ref;
      end if;

      Set_Assignment_OK (Ref);

      Utyp := Underlying_Type (Base_Type (Utyp));

      --  Deal with non-tagged derivation of private views

      if Is_Untagged_Derivation (Typ)
        and then not Is_Conc
      then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Ref  := Unchecked_Convert_To (Utyp, Ref);

         --  The following is to prevent problems with UC see 1.156 RH ???

         Set_Assignment_OK (Ref);
      end if;

      --  If the underlying_type is a subtype, then we are dealing with the
      --  completion of a private type. We need to access the base type and
      --  generate a conversion to it.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));
         Utyp := Base_Type (Utyp);
         Ref  := Unchecked_Convert_To (Utyp, Ref);
      end if;

      --  Select the appropriate version of initialize

      if Has_Controlled_Component (Utyp) then
         Proc := TSS (Utyp, Deep_Name_Of (Initialize_Case));
      else
         Proc := Find_Prim_Op (Utyp, Name_Of (Initialize_Case));
         Check_Visibly_Controlled (Initialize_Case, Typ, Proc, Ref);
      end if;

      --  The object reference may need another conversion depending on the
      --  type of the formal and that of the actual.

      Ref := Convert_View (Proc, Ref);

      --  Generate:
      --    [Deep_]Initialize (Ref);

      return
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (Proc, Loc),
          Parameter_Associations => New_List (Ref));
   end Make_Init_Call;

   ------------------------------
   -- Make_Local_Deep_Finalize --
   ------------------------------

   function Make_Local_Deep_Finalize
     (Typ : Entity_Id;
      Nam : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);
      Formals : List_Id;

   begin
      Formals := New_List (

         --  V : in out Typ

        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
          In_Present          => True,
          Out_Present         => True,
          Parameter_Type      => New_Occurrence_Of (Typ, Loc)),

         --  F : Boolean := True

        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_F),
          Parameter_Type      => New_Occurrence_Of (Standard_Boolean, Loc),
          Expression          => New_Occurrence_Of (Standard_True, Loc)));

      --  Add the necessary number of counters to represent the initialization
      --  state of an object.

      return
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Nam,
              Parameter_Specifications => Formals),

          Declarations => No_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Make_Deep_Record_Body (Finalize_Case, Typ, True)));
   end Make_Local_Deep_Finalize;

   ------------------------------------
   -- Make_Set_Finalize_Address_Call --
   ------------------------------------

   function Make_Set_Finalize_Address_Call
     (Loc     : Source_Ptr;
      Typ     : Entity_Id;
      Ptr_Typ : Entity_Id) return Node_Id
   is
      Desig_Typ   : constant Entity_Id :=
                      Available_View (Designated_Type (Ptr_Typ));
      Fin_Mas_Id  : constant Entity_Id := Finalization_Master (Ptr_Typ);
      Fin_Mas_Ref : Node_Id;
      Utyp        : Entity_Id;

   begin
      --  If the context is a class-wide allocator, we use the class-wide type
      --  to obtain the proper Finalize_Address routine.

      if Is_Class_Wide_Type (Desig_Typ) then
         Utyp := Desig_Typ;

      else
         Utyp := Typ;

         if Is_Private_Type (Utyp) and then Present (Full_View (Utyp)) then
            Utyp := Full_View (Utyp);
         end if;

         if Is_Concurrent_Type (Utyp) then
            Utyp := Corresponding_Record_Type (Utyp);
         end if;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));

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

            if Is_Protected_Type (Utyp) then
               Utyp := Corresponding_Record_Type (Utyp);
            end if;
         end if;
      end if;

      --  If the underlying_type is a subtype, we are dealing with the
      --  completion of a private type. We need to access the base type and
      --  generate a conversion to it.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));

         Utyp := Base_Type (Utyp);
      end if;

      Fin_Mas_Ref := New_Occurrence_Of (Fin_Mas_Id, Loc);

      --  If the call is from a build-in-place function, the Master parameter
      --  is actually a pointer. Dereference it for the call.

      if Is_Access_Type (Etype (Fin_Mas_Id)) then
         Fin_Mas_Ref := Make_Explicit_Dereference (Loc, Fin_Mas_Ref);
      end if;

      --  Generate:
      --    Set_Finalize_Address (<Ptr_Typ>FM, <Utyp>FD'Unrestricted_Access);

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Set_Finalize_Address), Loc),
          Parameter_Associations => New_List (
            Fin_Mas_Ref,
            Make_Attribute_Reference (Loc,
              Prefix         =>
                New_Occurrence_Of (TSS (Utyp, TSS_Finalize_Address), Loc),
              Attribute_Name => Name_Unrestricted_Access)));
   end Make_Set_Finalize_Address_Call;

   --------------------------
   -- Make_Transient_Block --
   --------------------------

   function Make_Transient_Block
     (Loc    : Source_Ptr;
      Action : Node_Id;
      Par    : Node_Id) return Node_Id
   is
      Decls  : constant List_Id := New_List;
      Instrs : constant List_Id := New_List (Action);
      Block  : Node_Id;
      Insert : Node_Id;

   begin
      --  Case where only secondary stack use is involved

      if VM_Target = No_VM
        and then Uses_Sec_Stack (Current_Scope)
        and then Nkind (Action) /= N_Simple_Return_Statement
        and then Nkind (Par) /= N_Exception_Handler
      then
         declare
            S : Entity_Id;

         begin
            S := Scope (Current_Scope);
            loop
               --  At the outer level, no need to release the sec stack

               if S = Standard_Standard then
                  Set_Uses_Sec_Stack (Current_Scope, False);
                  exit;

               --  In a function, only release the sec stack if the function
               --  does not return on the sec stack otherwise the result may
               --  be lost. The caller is responsible for releasing.

               elsif Ekind (S) = E_Function then
                  Set_Uses_Sec_Stack (Current_Scope, False);

                  if not Requires_Transient_Scope (Etype (S)) then
                     Set_Uses_Sec_Stack (S, True);
                     Check_Restriction (No_Secondary_Stack, Action);
                  end if;

                  exit;

               --  In a loop or entry we should install a block encompassing
               --  all the construct. For now just release right away.

               elsif Ekind_In (S, E_Entry, E_Loop) then
                  exit;

               --  In a procedure or a block, we release on exit of the
               --  procedure or block. ??? memory leak can be created by
               --  recursive calls.

               elsif Ekind_In (S, E_Block, E_Procedure) then
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

      --  Create the transient block. Set the parent now since the block itself
      --  is not part of the tree.

      Block :=
        Make_Block_Statement (Loc,
          Identifier                 => New_Occurrence_Of (Current_Scope, Loc),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Instrs),
          Has_Created_Identifier     => True);
      Set_Parent (Block, Par);

      --  Insert actions stuck in the transient scopes as well as all freezing
      --  nodes needed by those actions.

      Insert_Actions_In_Scope_Around (Action);

      Insert := Prev (Action);
      if Present (Insert) then
         Freeze_All (First_Entity (Current_Scope), Insert);
      end if;

      --  When the transient scope was established, we pushed the entry for the
      --  transient scope onto the scope stack, so that the scope was active
      --  for the installation of finalizable entities etc. Now we must remove
      --  this entry, since we have constructed a proper block.

      Pop_Scope;

      return Block;
   end Make_Transient_Block;

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
   --  initialization they will be attached to the proper finalization list.
   --  For instance, the following declaration :

   --        X : Typ := F (G (A), G (B));

   --  (where G(A) and G(B) return controlled values, expanded as _v1 and _v2)
   --  is expanded into :

   --    X : Typ := [ complex Expression-Action ];
   --    [Deep_]Finalize (_v1);
   --    [Deep_]Finalize (_v2);

   procedure Wrap_Transient_Declaration (N : Node_Id) is
      Encl_S  : Entity_Id;
      S       : Entity_Id;
      Uses_SS : Boolean;

   begin
      S := Current_Scope;
      Encl_S := Scope (S);

      --  Insert Actions kept in the Scope stack

      Insert_Actions_In_Scope_Around (N);

      --  If the declaration is consuming some secondary stack, mark the
      --  enclosing scope appropriately.

      Uses_SS := Uses_Sec_Stack (S);
      Pop_Scope;

      --  Put the local entities back in the enclosing scope, and set the
      --  Is_Public flag appropriately.

      Transfer_Entities (S, Encl_S);

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

   procedure Wrap_Transient_Expression (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Expr : Node_Id             := Relocate_Node (N);
      Temp : constant Entity_Id  := Make_Temporary (Loc, 'E', N);
      Typ  : constant Entity_Id  := Etype (N);

   begin
      --  Generate:

      --    Temp : Typ;
      --    declare
      --       M : constant Mark_Id := SS_Mark;
      --       procedure Finalizer is ...  (See Build_Finalizer)
      --
      --    begin
      --       Temp := <Expr>;                           --  general case
      --       Temp := (if <Expr> then True else False); --  boolean case
      --
      --    at end
      --       Finalizer;
      --    end;

      --  A special case is made for Boolean expressions so that the back-end
      --  knows to generate a conditional branch instruction, if running with
      --  -fpreserve-control-flow. This ensures that a control flow change
      --  signalling the decision outcome occurs before the cleanup actions.

      if Opt.Suppress_Control_Flow_Optimizations
        and then Is_Boolean_Type (Typ)
      then
         Expr :=
           Make_If_Expression (Loc,
             Expressions => New_List (
               Expr,
               New_Occurrence_Of (Standard_True, Loc),
               New_Occurrence_Of (Standard_False, Loc)));
      end if;

      Insert_Actions (N, New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Temp,
          Object_Definition   => New_Occurrence_Of (Typ, Loc)),

        Make_Transient_Block (Loc,
          Action =>
            Make_Assignment_Statement (Loc,
              Name       => New_Occurrence_Of (Temp, Loc),
              Expression => Expr),
          Par    => Parent (N))));

      Rewrite (N, New_Occurrence_Of (Temp, Loc));
      Analyze_And_Resolve (N, Typ);
   end Wrap_Transient_Expression;

   ------------------------------
   -- Wrap_Transient_Statement --
   ------------------------------

   procedure Wrap_Transient_Statement (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      New_Stmt : constant Node_Id    := Relocate_Node (N);

   begin
      --  Generate:
      --    declare
      --       M : constant Mark_Id := SS_Mark;
      --       procedure Finalizer is ...  (See Build_Finalizer)
      --
      --    begin
      --       <New_Stmt>;
      --
      --    at end
      --       Finalizer;
      --    end;

      Rewrite (N,
        Make_Transient_Block (Loc,
          Action => New_Stmt,
          Par    => Parent (N)));

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
