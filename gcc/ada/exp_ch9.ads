------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 9                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

--  Expand routines for chapter 9 constructs

with Types; use Types;

package Exp_Ch9 is

   procedure Add_Discriminal_Declarations
     (Decls : List_Id;
      Typ   : Entity_Id;
      Name  : Name_Id;
      Loc   : Source_Ptr);
   --  This routine is used to add discriminal declarations to task and
   --  protected operation bodies. The discriminants are available by normal
   --  selection from the concurrent object (whose name is passed as the third
   --  parameter). Discriminant references inside the body have already
   --  been replaced by references to the corresponding discriminals. The
   --  declarations constructed by this procedure hook the references up with
   --  the objects:
   --
   --    discriminal_name : discr_type renames name.discriminant_name;
   --
   --  Obviously we could have expanded the discriminant references in the
   --  first place to be the appropriate selection, but this turns out to
   --  be hard to do because it would introduce difference in handling of
   --  discriminant references depending on their location.

   procedure Add_Private_Declarations
     (Decls : List_Id;
      Typ   : Entity_Id;
      Name  : Name_Id;
      Loc : Source_Ptr);
   --  This routine is used to add private declarations to protected bodies.
   --  These are analogous to the discriminal declarations added to tasks
   --  and protected operations, and consist of a renaming of each private
   --  object to a selection from the concurrent object passed as an extra
   --  parameter to each such operation:
   --    private_name : private_type renames name.private_name;
   --  As with discriminals, private references inside the protected
   --  subprogram bodies have already been replaced by references to the
   --  corresponding privals.

   procedure Build_Activation_Chain_Entity (N : Node_Id);
   --  Given a declaration N of an object that is a task, or contains tasks
   --  (other than allocators to tasks) this routine ensures that an activation
   --  chain has been declared in the appropriate scope, building the required
   --  declaration for the chain variable if not. The name of this variable
   --  is always _Chain and it is accessed by name. This procedure also adds
   --  an appropriate call to Activate_Tasks to activate the tasks for this
   --  activation chain. It does not however deal with the call needed in the
   --  case of allocators to Expunge_Unactivated_Tasks, this is separately
   --  handled in the Expand_Task_Allocator routine.

   function Build_Call_With_Task (N : Node_Id; E : Entity_Id) return Node_Id;
   --  N is a node representing the name of a task or an access to a task.
   --  The value returned is a call to the function whose name is the entity
   --  E (typically a runtime routine entity obtained using RTE) with the
   --  Task_Id of the associated task as the parameter. The caller is
   --  responsible for analyzing and resolving the resulting tree.

   procedure Build_Master_Entity (E : Entity_Id);
   --  Given an entity E for the declaration of an object containing tasks
   --  or of a type declaration for an allocator whose designated type is a
   --  task or contains tasks, this routine marks the appropriate enclosing
   --  context as a master, and also declares a variable called _Master in
   --  the current declarative part which captures the value of Current_Master
   --  (if not already built by a prior call). We build this object (instead
   --  of just calling Current_Master) for two reasons. First it is clearly
   --  more efficient to call Current_Master only once for a bunch of tasks
   --  in the same declarative part, and second it makes things easier in
   --  generating the initialization routines, since they can just reference
   --  the object _Master by name, and they will get the proper Current_Master
   --  value at the outer level, and copy in the parameter value for the outer
   --  initialization call if the call is for a nested component). Note that
   --  in the case of nested packages, we only really need to make one such
   --  object at the outer level, but it is much easier to generate one per
   --  declarative part.

   function Build_Protected_Sub_Specification
     (N           : Node_Id;
      Prottyp     : Entity_Id;
      Unprotected : Boolean := False)
      return        Node_Id;
   --  Build specification for protected subprogram. This is called when
   --  expanding a protected type, and also when expanding the declaration for
   --  an Access_To_Protected_Subprogram type. In the latter case, Prottyp is
   --  empty, and the first parameter of the signature of the protected op is
   --  of type System.Address.

   procedure Build_Protected_Subprogram_Call
     (N        : Node_Id;
      Name     : Node_Id;
      Rec      : Node_Id;
      External : Boolean := True);
   --  The node N is a subprogram or entry call to a protected subprogram.
   --  This procedure rewrites this call with the appropriate expansion.
   --  Name is the subprogram, and Rec is the record corresponding to the
   --  protected object. External is False if the call is to another
   --  protected subprogram within the same object.

   procedure Build_Task_Activation_Call (N : Node_Id);
   --  This procedure is called for constructs that can be task activators
   --  i.e. task bodies, subprogram bodies, package bodies and blocks. If
   --  the construct is a task activator (as indicated by the non-empty
   --  setting of Activation_Chain_Entity, either in the construct, or, in
   --  the case of a package body, in its associated package spec), then
   --  a call to Activate_Tasks with this entity as the single parameter
   --  is inserted at the start of the statements of the activator.

   procedure Build_Task_Allocate_Block
     (Actions : List_Id;
      N       : Node_Id;
      Args    : List_Id);
   --  This routine is used in the case of allocators where the designated
   --  type is a task or contains tasks. In this case, the normal initialize
   --  call is replaced by:
   --
   --    blockname : label;
   --    blockname : declare
   --       _Chain  : Activation_Chain;
   --
   --       procedure _Expunge is
   --       begin
   --         Expunge_Unactivated_Tasks (_Chain);
   --       end;
   --
   --    begin
   --       Init (Args);
   --       Activate_Tasks (_Chain);
   --    at end
   --       _Expunge;
   --    end;
   --
   --  to get the task or tasks created and initialized. The expunge call
   --  ensures that any tasks that get created but not activated due to an
   --  exception are properly expunged (it has no effect in the normal case)
   --  The argument N is the allocator, and Args is the list of arguments
   --  for the initialization call, constructed by the caller, which uses
   --  the Master_Id of the access type as the _Master parameter, and _Chain
   --  (defined above) as the _Chain parameter.

   procedure Build_Task_Allocate_Block_With_Init_Stmts
     (Actions    : List_Id;
      N          : Node_Id;
      Init_Stmts : List_Id);
   --  Ada 2005 (AI-287): Similar to previous routine, but used to expand
   --  allocated aggregates with default initialized components. Init_Stmts
   --  contains the list of statements required to initialize the allocated
   --  aggregate. It replaces the call to Init (Args) done by
   --  Build_Task_Allocate_Block.

   function Concurrent_Ref (N : Node_Id) return Node_Id;
   --  Given the name of a concurrent object (task or protected object), or
   --  the name of an access to a concurrent object, this function returns an
   --  expression referencing the associated Task_Id or Protection object,
   --  respectively. Note that a special case is when the name is a reference
   --  to a task type name. This can only happen within a task body, and the
   --  meaning is to get the Task_Id for the currently executing task.

   function Convert_Concurrent
     (N    : Node_Id;
      Typ  : Entity_Id)
      return Node_Id;
   --  N is an expression of type Typ. If the type is not a concurrent
   --  type then it is returned unchanged. If it is a task or protected
   --  reference, Convert_Concurrent creates an unchecked conversion node
   --  from this expression to the corresponding concurrent record type
   --  value. We need this in any situation where the concurrent type is
   --  used, because the actual concurrent object is an object of the
   --  corresponding concurrent type, and manipulations on the concurrent
   --  object actually manipulate the corresponding object of the record
   --  type.

   function Entry_Index_Expression
     (Sloc  : Source_Ptr;
      Ent   : Entity_Id;
      Index : Node_Id;
      Ttyp  : Entity_Id)
      return  Node_Id;
   --  Returns an expression to compute a task entry index given the name
   --  of the entry or entry family. For the case of a task entry family,
   --  the Index parameter contains the expression for the subscript.
   --  Ttyp is the task type.

   procedure Establish_Task_Master (N : Node_Id);
   --  Given a subprogram body, or a block statement, or a task body, this
   --  proccedure makes the necessary transformations required of a task
   --  master (add Enter_Master call at start, and establish a cleanup
   --  routine to make sure Complete_Master is called on exit).

   procedure Expand_Access_Protected_Subprogram_Type (N : Node_Id);
   --  Build Equivalent_Type for an Access_to_protected_Subprogram.

   procedure Expand_Accept_Declarations (N : Node_Id; Ent : Entity_Id);
   --  Expand declarations required for accept statement. See bodies of
   --  both Expand_Accept_Declarations and Expand_N_Accept_Statement for
   --  full details of the nature and use of these declarations, which
   --  are inserted immediately before the accept node N. The second
   --  argument is the entity for the corresponding entry.

   procedure Expand_Entry_Barrier (N : Node_Id; Ent : Entity_Id);
   --  Expand the entry barrier into a function. This is called directly
   --  from Analyze_Entry_Body so that the discriminals and privals of the
   --  barrier can be attached to the function declaration list, and a new
   --  set prepared for the entry body procedure, bedore the entry body
   --  statement sequence can be expanded. The resulting function is analyzed
   --  now, within the context of the protected object, to resolve calls to
   --  other protected functions.

   procedure Expand_Entry_Body_Declarations (N : Node_Id);
   --  Expand declarations required for the expansion of the
   --  statements of the body.

   procedure Expand_N_Abort_Statement            (N : Node_Id);
   procedure Expand_N_Accept_Statement           (N : Node_Id);
   procedure Expand_N_Asynchronous_Select        (N : Node_Id);
   procedure Expand_N_Conditional_Entry_Call     (N : Node_Id);
   procedure Expand_N_Delay_Relative_Statement   (N : Node_Id);
   procedure Expand_N_Delay_Until_Statement      (N : Node_Id);
   procedure Expand_N_Entry_Body                 (N : Node_Id);
   procedure Expand_N_Entry_Call_Statement       (N : Node_Id);
   procedure Expand_N_Entry_Declaration          (N : Node_Id);
   procedure Expand_N_Protected_Body             (N : Node_Id);

   procedure Expand_N_Protected_Type_Declaration (N : Node_Id);
   --  Expands protected type declarations. This results, among
   --  other things, in the declaration of a record type for the
   --  representation of protected objects and (if there are entries)
   --  in an entry service procedure. The Protection value used by
   --  the GNARL to control the object will always be the first
   --  field of the record, and the entry service procedure spec
   --  (if it exists) will always immediately follow the record
   --  declaration. This allows these two nodes to be found from
   --  the type using Corresponding_Record, without benefit of
   --  of further attributes.

   procedure Expand_N_Requeue_Statement          (N : Node_Id);
   procedure Expand_N_Selective_Accept           (N : Node_Id);
   procedure Expand_N_Single_Task_Declaration    (N : Node_Id);
   procedure Expand_N_Task_Body                  (N : Node_Id);
   procedure Expand_N_Task_Type_Declaration      (N : Node_Id);
   procedure Expand_N_Timed_Entry_Call           (N : Node_Id);

   procedure Expand_Protected_Body_Declarations
     (N       : Node_Id;
      Spec_Id : Entity_Id);
   --  Expand declarations required for a protected body. See bodies of
   --  both Expand_Protected_Body_Declarations and Expand_N_Protected_Body
   --  for full details of the nature and use of these declarations.
   --  The second argument is the entity for the corresponding
   --  protected type declaration.

   function External_Subprogram (E : Entity_Id) return Entity_Id;
   --  return the external version of a protected operation, which locks
   --  the object before invoking the internal protected subprogram body.

   function First_Protected_Operation (D : List_Id) return Node_Id;
   --  Given the declarations list for a protected body, find the
   --  first protected operation body.

   function Make_Task_Create_Call (Task_Rec : Entity_Id) return Node_Id;
   --  Given the entity of the record type created for a task type, build
   --  the call to Create_Task

   function Make_Initialize_Protection
     (Protect_Rec : Entity_Id)
      return        List_Id;
   --  Given the entity of the record type created for a protected type, build
   --  a list of statements needed for proper initialization of the object.

   function Next_Protected_Operation (N : Node_Id) return Node_Id;
   --  Given a protected operation node (a subprogram or entry body),
   --  find the following node in the declarations list.

   procedure Set_Discriminals (Dec : Node_Id);
   --  Replace discriminals in a protected type for use by the
   --  next protected operation on the type. Each operation needs a
   --  new set of discirminals, since it needs a unique renaming of
   --  the discriminant fields in the record used to implement the
   --  protected type.

   procedure Set_Privals
      (Dec : Node_Id;
       Op  : Node_Id;
       Loc : Source_Ptr);
   --  Associates a new set of privals (placeholders for later access to
   --  private components of protected objects) with the private object
   --  declarations of a protected object. These will be used to expand
   --  the references to private objects in the next protected
   --  subprogram or entry body to be expanded.

end Exp_Ch9;
