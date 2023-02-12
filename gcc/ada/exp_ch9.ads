------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 9                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  Expand routines for chapter 9 constructs

with Types; use Types;

package Exp_Ch9 is

   type Subprogram_Protection_Mode is
     (Dispatching_Mode,
      Protected_Mode,
      Unprotected_Mode);
   --  This type is used to distinguish the different protection modes of a
   --  protected subprogram.

   procedure Build_Activation_Chain_Entity (N : Node_Id);
   --  Given a declaration N of an object that is a task, or contains tasks
   --  (other than allocators to tasks) this routine ensures that an activation
   --  chain has been declared in the appropriate scope, building the required
   --  declaration for the chain variable if not. The name of this variable
   --  is always _Chain and it is accessed by name.

   function Build_Call_With_Task (N : Node_Id; E : Entity_Id) return Node_Id;
   --  N is a node representing the name of a task or an access to a task.
   --  The value returned is a call to the function whose name is the entity
   --  E (typically a runtime routine entity obtained using RTE) with the
   --  Task_Id of the associated task as the parameter. The caller is
   --  responsible for analyzing and resolving the resulting tree.

   procedure Build_Class_Wide_Master (Typ : Entity_Id);
   --  Given an access-to-limited class-wide type or an access-to-limited
   --  interface, ensure that the designated type has a _master and generate
   --  a renaming of the said master to service the access type.

   function Build_Master_Declaration (Loc : Source_Ptr) return Node_Id;
   --  For targets supporting tasks, generate:
   --      _Master : constant Integer := Current_Master.all;
   --  For targets where tasks or tasking hierarchies are prohibited, generate:
   --      _Master : constant Master_Id := 3;

   procedure Build_Master_Entity (Obj_Or_Typ : Entity_Id);
   --  Given the name of an object or a type which is either a task, contains
   --  tasks or designates tasks, create a _master in the appropriate scope
   --  which captures the value of Current_Master. Mark the nearest enclosing
   --  body or block as being a task master.

   procedure Build_Master_Renaming
     (Ptr_Typ : Entity_Id;
      Ins_Nod : Node_Id := Empty);
   --  Given an access type Ptr_Typ whose designated type is either a task or
   --  contains tasks, create a renaming of the form:
   --
   --     <Ptr_Typ>M : Master_Id renames _Master;
   --
   --  where _master denotes the task master of the enclosing context. Ins_Nod
   --  is used to provide a specific insertion node for the renaming.

   function Build_Protected_Sub_Specification
     (N        : Node_Id;
      Prot_Typ : Entity_Id;
      Mode     : Subprogram_Protection_Mode) return Node_Id;
   --  Build the specification for protected subprogram. This is called when
   --  expanding a protected type, and also when expanding the declaration for
   --  an Access_To_Protected_Subprogram type. In the latter case, Prot_Typ is
   --  empty, and the first parameter of the signature of the protected op is
   --  of type System.Address.

   procedure Build_Protected_Subprogram_Call
     (N        : Node_Id;
      Name     : Node_Id;
      Rec      : Node_Id;
      External : Boolean := True);
   --  The node N is a subprogram or entry call to a protected subprogram. This
   --  procedure rewrites this call with the appropriate expansion. Name is the
   --  subprogram, and Rec is the record corresponding to the protected object.
   --  External is False if the call is to another protected subprogram within
   --  the same object.

   procedure Build_Protected_Subprogram_Call_Cleanup
     (Op_Spec   : Node_Id;
      Conc_Typ  : Node_Id;
      Loc       : Source_Ptr;
      Stmts     : List_Id);
   --  Append to Stmts the cleanups after a call to a protected subprogram
   --  whose specification is Op_Spec. Conc_Typ is the concurrent type and Loc
   --  the sloc for appended statements. The cleanup will either unlock the
   --  protected object or serve pending entries.

   procedure Build_Task_Activation_Call (N : Node_Id);
   --  This procedure is called for constructs that can be task activators,
   --  i.e. task bodies, subprogram bodies, package bodies and blocks. If the
   --  construct is a task activator (as indicated by the non-empty setting of
   --  Activation_Chain_Entity, either in the construct, or, in the case of a
   --  package body, in its associated package spec), then a call to
   --  Activate_Tasks with this entity as the single parameter is inserted at
   --  the start of the statements of the activator.

   procedure Build_Task_Allocate_Block
     (Actions : List_Id;
      N       : Node_Id;
      Args    : List_Id);
   --  This routine is used in the case of allocators where the designated type
   --  is a task or contains tasks. In this case, the normal initialize call
   --  is replaced by:
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
   --  exception are properly expunged (it has no effect in the normal case).
   --  The argument N is the allocator, and Args is the list of arguments for
   --  the initialization call, constructed by the caller, which uses the
   --  Master_Id of the access type as the _Master parameter, and _Chain
   --  (defined above) as the _Chain parameter.

   procedure Build_Task_Allocate_Block_With_Init_Stmts
     (Actions    : List_Id;
      N          : Node_Id;
      Init_Stmts : List_Id);
   --  Ada 2005 (AI-287): Similar to previous routine, but used to expand
   --  allocated aggregates with default initialized components. Init_Stmts
   --  contains the list of statements required to initialize the allocated
   --  aggregate. It replaces the call to Init (Args) done by
   --  Build_Task_Allocate_Block. Also used to expand allocators containing
   --  build-in-place function calls.

   function Build_Wrapper_Spec
     (Subp_Id : Entity_Id;
      Obj_Typ : Entity_Id;
      Formals : List_Id) return Node_Id;
   --  Ada 2005 (AI-345): Build the specification of a primitive operation
   --  associated with a protected or task type. This is required to implement
   --  dispatching calls through interfaces. Subp_Id is the primitive to be
   --  wrapped, Obj_Typ is the type of the newly added formal parameter to
   --  handle object notation, Formals are the original entry formals that
   --  will be explicitly replicated.

   function Concurrent_Ref (N : Node_Id) return Node_Id;
   --  Given the name of a concurrent object (task or protected object), or
   --  the name of an access to a concurrent object, this function returns an
   --  expression referencing the associated Task_Id or Protection object,
   --  respectively. Note that a special case is when the name is a reference
   --  to a task type name. This can only happen within a task body, and the
   --  meaning is to get the Task_Id for the currently executing task.

   function Convert_Concurrent
     (N   : Node_Id;
      Typ : Entity_Id) return Node_Id;
   --  N is an expression of type Typ. If the type is not a concurrent type
   --  then it is returned unchanged. If it is a task or protected reference,
   --  Convert_Concurrent creates an unchecked conversion node from this
   --  expression to the corresponding concurrent record type value. We need
   --  this in any situation where the concurrent type is used, because the
   --  actual concurrent object is an object of the corresponding concurrent
   --  type, and manipulations on the concurrent object actually manipulate the
   --  corresponding object of the record type.

   function Entry_Index_Expression
     (Sloc  : Source_Ptr;
      Ent   : Entity_Id;
      Index : Node_Id;
      Ttyp  : Entity_Id)
      return  Node_Id;
   --  Returns an expression to compute a task entry index given the name of
   --  the entry or entry family. For the case of a task entry family, the
   --  Index parameter contains the expression for the subscript. Ttyp is the
   --  task type.

   procedure Establish_Task_Master (N : Node_Id);
   --  Given a subprogram body, or a block statement, or a task body, this
   --  procedure makes the necessary transformations required of a task master
   --  (add Enter_Master call at start, and establish a cleanup routine to make
   --  sure Complete_Master is called on exit).

   procedure Expand_Access_Protected_Subprogram_Type (N : Node_Id);
   --  Build Equivalent_Type for an Access_To_Protected_Subprogram.
   --  Equivalent_Type is a record type with two components: a pointer to the
   --  protected object, and a pointer to the operation itself.

   procedure Expand_Accept_Declarations (N : Node_Id; Ent : Entity_Id);
   --  Expand declarations required for accept statement. See bodies of both
   --  Expand_Accept_Declarations and Expand_N_Accept_Statement for full
   --  details of the nature and use of these declarations, which are inserted
   --  immediately before the accept node N. The second argument is the entity
   --  for the corresponding entry.

   procedure Expand_Entry_Barrier (N : Node_Id; Ent : Entity_Id);
   --  Expand the entry barrier into a function. This is called directly
   --  from Analyze_Entry_Body so that the discriminals and privals of the
   --  barrier can be attached to the function declaration list, and a new
   --  set prepared for the entry body procedure, before the entry body
   --  statement sequence can be expanded. The resulting function is analyzed
   --  now, within the context of the protected object, to resolve calls to
   --  other protected functions.

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
   --  Expands protected type declarations. This results, among other things,
   --  in the declaration of a record type for the representation of protected
   --  objects and (if there are entries) in an entry service procedure. The
   --  Protection value used by the GNARL to control the object will always be
   --  the first field of the record, and the entry service procedure spec (if
   --  it exists) will always immediately follow the record declaration. This
   --  allows these two nodes to be found from the type, without benefit of
   --  further attributes, using Corresponding_Record.

   procedure Expand_N_Requeue_Statement            (N : Node_Id);
   procedure Expand_N_Selective_Accept             (N : Node_Id);
   procedure Expand_N_Single_Protected_Declaration (N : Node_Id);
   procedure Expand_N_Single_Task_Declaration      (N : Node_Id);
   procedure Expand_N_Task_Body                    (N : Node_Id);
   procedure Expand_N_Task_Type_Declaration        (N : Node_Id);
   procedure Expand_N_Timed_Entry_Call             (N : Node_Id);

   procedure Expand_Protected_Body_Declarations
     (N       : Node_Id;
      Spec_Id : Entity_Id);
   --  Expand declarations required for a protected body. See bodies of both
   --  Expand_Protected_Body_Declarations and Expand_N_Protected_Body for full
   --  details of the nature and use of these declarations. The second argument
   --  is the entity for the corresponding protected type declaration.

   function External_Subprogram (E : Entity_Id) return Entity_Id;
   --  Return the external version of a protected operation, which locks
   --  the object before invoking the internal protected subprogram body.

   function Find_Master_Scope (E : Entity_Id) return Entity_Id;
   --  When a type includes tasks, a master entity is created in the scope, to
   --  be used by the runtime during activation. In general the master is the
   --  immediate scope in which the type is declared, but in Ada 2005, in the
   --  presence of synchronized classwide interfaces, the immediate scope of
   --  an anonymous access type may be a transient scope, which has no run-time
   --  presence. In this case, the scope of the master is the innermost scope
   --  that comes from source.

   function First_Protected_Operation (D : List_Id) return Node_Id;
   --  Given the declarations list for a protected body, find the
   --  first protected operation body.

   procedure Install_Private_Data_Declarations
     (Loc      : Source_Ptr;
      Spec_Id  : Entity_Id;
      Conc_Typ : Entity_Id;
      Body_Nod : Node_Id;
      Decls    : List_Id;
      Barrier  : Boolean := False;
      Family   : Boolean := False);
   --  This routines generates several types, objects and object renamings used
   --  in the handling of discriminants and private components of protected and
   --  task types. It also generates the entry index for entry families. Formal
   --  Spec_Id denotes an entry, entry family or a subprogram, Conc_Typ is the
   --  concurrent type where Spec_Id resides, Body_Nod is the corresponding
   --  body of Spec_Id, Decls are the declarations of the subprogram or entry.
   --  Flag Barrier denotes whether the context is an entry barrier function.
   --  Flag Family is used in conjunction with Barrier to denote a barrier for
   --  an entry family.
   --
   --  The generated types, entities and renamings are:
   --
   --  * If flag Barrier is set or Spec_Id denotes a protected entry or an
   --    entry family, generate:
   --
   --      type prot_typVP is access prot_typV;
   --      _object : prot_typVP := prot_typV (_O);
   --
   --    where prot_typV is the corresponding record of a protected type and
   --    _O is a formal parameter representing the concurrent object of either
   --    the barrier function or the entry (family).
   --
   --  * If Conc_Typ is a protected type, create a renaming for the Protection
   --    field _object:
   --
   --      conc_typR : protection_typ renames _object._object;
   --
   --  * If Conc_Typ has discriminants, create renamings of the form:
   --
   --      discr_nameD : discr_typ renames _object.discr_name;
   --        or
   --      discr_nameD : discr_typ renames _task.discr_name;
   --
   --  * If Conc_Typ denotes a protected type and has private components,
   --    generate renamings of the form:
   --
   --      comp_name : comp_typ renames _object.comp_name;
   --
   --  * Finally, is flag Barrier and Family are set or Spec_Id denotes an
   --    entry family, generate the entry index constant:
   --
   --      subtype Jnn is <Type of Index> range Low .. High;
   --      J : constant Jnn :=
   --            Jnn'Val (_E - <Index expression> + Jnn'Pos (Jnn'First));
   --
   --  All the above declarations are inserted in the order shown to the front
   --  of Decls.

   function Make_Task_Create_Call (Task_Rec : Entity_Id) return Node_Id;
   --  Given the entity of the record type created for a task type, build
   --  the call to Create_Task

   function Make_Initialize_Protection
     (Protect_Rec : Entity_Id) return List_Id;
   --  Given the entity of the record type created for a protected type, build
   --  a list of statements needed for proper initialization of the object.

   function Next_Protected_Operation (N : Node_Id) return Node_Id;
   --  Given a protected operation node (a subprogram or entry body), find the
   --  following node in the declarations list.

   procedure Set_Discriminals (Dec : Node_Id);
   --  Replace discriminals in a protected type for use by the next protected
   --  operation on the type. Each operation needs a new set of discriminals,
   --  since it needs a unique renaming of the discriminant fields in the
   --  record used to implement the protected type.

end Exp_Ch9;
