------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 7                               --
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

with Namet; use Namet;
with Types; use Types;

package Exp_Ch7 is

   procedure Expand_N_Package_Body        (N : Node_Id);
   procedure Expand_N_Package_Declaration (N : Node_Id);

   -----------------------------
   -- Finalization Management --
   -----------------------------

   procedure Build_Anonymous_Master (Ptr_Typ : Entity_Id);
   --  Build a finalization master for an anonymous access-to-controlled type
   --  denoted by Ptr_Typ. The master is inserted in the declarations of the
   --  current unit.

   procedure Build_Controlling_Procs (Typ : Entity_Id);
   --  Typ is a record, and array type having controlled components.
   --  Create the procedures Deep_Initialize, Deep_Adjust and Deep_Finalize
   --  that take care of finalization management at run-time.

   --  Support of exceptions from user finalization procedures

   --  There is a specific mechanism to handle these exceptions, continue
   --  finalization and then raise PE. This mechanism is used by this package
   --  but also by exp_intr for Ada.Unchecked_Deallocation.

   --  There are 3 subprograms to use this mechanism, and the type
   --  Finalization_Exception_Data carries internal data between these
   --  subprograms:
   --
   --    1. Build_Object_Declaration: create the variables for the next two
   --       subprograms.
   --    2. Build_Exception_Handler: create the exception handler for a call
   --       to a user finalization procedure.
   --    3. Build_Raise_Stmt: create code to potentially raise a PE exception
   --       if an exception was raise in a user finalization procedure.

   type Finalization_Exception_Data is record
      Loc : Source_Ptr;
      --  Sloc for the added nodes

      Abort_Id : Entity_Id;
      --  Boolean variable set to true if the finalization was triggered by
      --  an abort.

      E_Id : Entity_Id;
      --  Variable containing the exception occurrence raised by user code

      Raised_Id : Entity_Id;
      --  Boolean variable set to true if an exception was raised in user code
   end record;

   function Build_Exception_Handler
     (Data        : Finalization_Exception_Data;
      For_Library : Boolean := False) return Node_Id;
   --  Subsidiary to Build_Finalizer, Make_Deep_Array_Body and Make_Deep_Record
   --  _Body. Create an exception handler of the following form:
   --
   --    when others =>
   --       if not Raised_Id then
   --          Raised_Id := True;
   --          Save_Occurrence (E_Id, Get_Current_Excep.all.all);
   --       end if;
   --
   --  If flag For_Library is set (and not in restricted profile):
   --
   --    when others =>
   --       if not Raised_Id then
   --          Raised_Id := True;
   --          Save_Library_Occurrence (Get_Current_Excep.all);
   --       end if;
   --
   --  E_Id denotes the defining identifier of a local exception occurrence.
   --  Raised_Id is the entity of a local boolean flag. Flag For_Library is
   --  used when operating at the library level, when enabled the current
   --  exception will be saved to a global location.

   procedure Build_Finalization_Master
     (Typ            : Entity_Id;
      For_Lib_Level  : Boolean   := False;
      For_Private    : Boolean   := False;
      Context_Scope  : Entity_Id := Empty;
      Insertion_Node : Node_Id   := Empty);
   --  Build a finalization master for an access type. The designated type may
   --  not necessarily be controlled or need finalization actions depending on
   --  the context. Flag For_Lib_Level must be set when creating a master for a
   --  build-in-place function call access result type. Flag For_Private must
   --  be set when the designated type contains a private component. Parameters
   --  Context_Scope and Insertion_Node must be used in conjunction with flag
   --  For_Private. Context_Scope is the scope of the context where the
   --  finalization master must be analyzed. Insertion_Node is the insertion
   --  point before which the master is to be inserted.

   procedure Build_Late_Proc (Typ : Entity_Id; Nam : Name_Id);
   --  Build one controlling procedure when a late body overrides one of the
   --  controlling operations.

   procedure Build_Object_Declarations
     (Data        : out Finalization_Exception_Data;
      Decls       : List_Id;
      Loc         : Source_Ptr;
      For_Package : Boolean := False);
   --  Subsidiary to Make_Deep_Array_Body and Make_Deep_Record_Body. Create the
   --  list List containing the object declarations of boolean flag Abort_Id,
   --  the exception occurrence E_Id and boolean flag Raised_Id.
   --
   --    Abort_Id  : constant Boolean :=
   --                  Exception_Identity (Get_Current_Excep.all) =
   --                    Standard'Abort_Signal'Identity;
   --      <or>
   --    Abort_Id  : constant Boolean := False;  --  no abort or For_Package
   --
   --    E_Id      : Exception_Occurrence;
   --    Raised_Id : Boolean := False;

   function Build_Raise_Statement
     (Data : Finalization_Exception_Data) return Node_Id;
   --  Subsidiary to routines Build_Finalizer, Make_Deep_Array_Body and Make_
   --  Deep_Record_Body. Generate the following conditional raise statement:
   --
   --    if Raised_Id and then not Abort_Id then
   --       Raise_From_Controlled_Operation (E_Id);
   --    end if;
   --
   --  Abort_Id is a local boolean flag which is set when the finalization was
   --  triggered by an abort, E_Id denotes the defining identifier of a local
   --  exception occurrence, Raised_Id is the entity of a local boolean flag.

   function Make_Adjust_Call
     (Obj_Ref   : Node_Id;
      Typ       : Entity_Id;
      Skip_Self : Boolean := False) return Node_Id;
   --  Create a call to either Adjust or Deep_Adjust depending on the structure
   --  of type Typ. Obj_Ref is an expression with no side effects (not required
   --  to have been previously analyzed) that references the object to be
   --  adjusted. Typ is the expected type of Obj_Ref. When Skip_Self is set,
   --  only the components (if any) are adjusted. Return Empty if Adjust or
   --  Deep_Adjust is not available, possibly due to previous errors.

   function Make_Final_Call
     (Obj_Ref   : Node_Id;
      Typ       : Entity_Id;
      Skip_Self : Boolean := False) return Node_Id;
   --  Create a call to either Finalize or Deep_Finalize, depending on the
   --  structure of type Typ. Obj_Ref is an expression (with no side effects
   --  and is not required to have been previously analyzed) that references
   --  the object to be finalized. Typ is the expected type of Obj_Ref. When
   --  Skip_Self is set, only the components (if any) are finalized. Return
   --  Empty if Finalize or Deep_Finalize is not available, possibly due to
   --  previous errors.

   procedure Make_Finalize_Address_Body (Typ : Entity_Id);
   --  Create the body of TSS routine Finalize_Address if Typ is controlled and
   --  does not have a TSS entry for Finalize_Address. The procedure converts
   --  an address into a pointer and subsequently calls Deep_Finalize on the
   --  dereference.

   function Make_Init_Call
     (Obj_Ref : Node_Id;
      Typ     : Entity_Id) return Node_Id;
   --  Create a call to either Initialize or Deep_Initialize, depending on the
   --  structure of type Typ. Obj_Ref is an expression with no side effects
   --  (not required to have been previously analyzed) that references the
   --  object to be initialized. Typ is the expected type of Obj_Ref. Return
   --  Empty if Initialize or Deep_Initialize is not available, possibly due to
   --  previous errors.

   function Make_Handler_For_Ctrl_Operation (Loc : Source_Ptr) return Node_Id;
   --  Generate an implicit exception handler with an 'others' choice,
   --  converting any occurrence to a raise of Program_Error.

   function Make_Local_Deep_Finalize
     (Typ : Entity_Id;
      Nam : Entity_Id) return Node_Id;
   --  Create a special version of Deep_Finalize with identifier Nam. The
   --  routine has state information and can perform partial finalization.

   function Make_Set_Finalize_Address_Call
     (Loc     : Source_Ptr;
      Ptr_Typ : Entity_Id) return Node_Id;
   --  Associate the Finalize_Address primitive of the designated type with the
   --  finalization master of access type Ptr_Typ. The returned call is:
   --
   --    Set_Finalize_Address
   --      (<Ptr_Typ>FM, <Desig_Typ>FD'Unrestricted_Access);

   --------------------------------------------
   -- Task and Protected Object finalization --
   --------------------------------------------

   function Cleanup_Array
     (N   : Node_Id;
      Obj : Node_Id;
      Typ : Entity_Id) return List_Id;
   --  Generate loops to finalize any tasks or simple protected objects that
   --  are subcomponents of an array.

   function Cleanup_Protected_Object
     (N   : Node_Id;
      Ref : Node_Id) return Node_Id;
   --  Generate code to finalize a protected object without entries

   function Cleanup_Record
     (N   : Node_Id;
      Obj : Node_Id;
      Typ : Entity_Id) return List_Id;
   --  For each subcomponent of a record that contains tasks or simple
   --  protected objects, generate the appropriate finalization call.

   function Cleanup_Task
     (N   : Node_Id;
      Ref : Node_Id) return Node_Id;
   --  Generate code to finalize a task

   function Has_Simple_Protected_Object (T : Entity_Id) return Boolean;
   --  Check whether composite type contains a simple protected component

   function Is_Simple_Protected_Type (T : Entity_Id) return Boolean;
   --  Determine whether T denotes a protected type without entries whose
   --  _object field is of type System.Tasking.Protected_Objects.Protection.
   --  Something wrong here, implementation was changed to test Lock_Free
   --  but this spec does not mention that ???

   --------------------------------
   -- Transient Scope Management --
   --------------------------------

   procedure Expand_Cleanup_Actions (N : Node_Id);
   --  Expand the necessary stuff into a scope to enable finalization of local
   --  objects and deallocation of transient data when exiting the scope. N is
   --  one of N_Block_Statement, N_Subprogram_Body, N_Task_Body, N_Entry_Body,
   --  or N_Extended_Return_Statement.

   procedure Establish_Transient_Scope
     (N                : Node_Id;
      Manage_Sec_Stack : Boolean);
   --  Push a new transient scope on the scope stack. N is the node which must
   --  be serviced by the transient scope. Set Manage_Sec_Stack when the scope
   --  must mark and release the secondary stack.

   function Node_To_Be_Wrapped return Node_Id;
   --  Return the node to be wrapped if the current scope is transient

   procedure Store_Before_Actions_In_Scope (L : List_Id);
   --  Append the list L of actions to the end of the before-actions store in
   --  the top of the scope stack (also analyzes these actions).

   procedure Store_After_Actions_In_Scope (L : List_Id);
   --  Prepend the list L of actions to the beginning of the after-actions
   --  stored in the top of the scope stack (also analyzes these actions).
   --
   --  Note that we are prepending here rather than appending. This means that
   --  if several calls are made to this procedure for the same scope, the
   --  actions will be executed in reverse order of the calls (actions for the
   --  last call executed first). Within the list L for a single call, the
   --  actions are executed in the order in which they appear in this list.

   procedure Store_Cleanup_Actions_In_Scope (L : List_Id);
   --  Prepend the list L of actions to the beginning of the cleanup-actions
   --  store in the top of the scope stack.

   procedure Wrap_Transient_Declaration (N : Node_Id);
   --  N is an object declaration. Expand the finalization calls after the
   --  declaration and make the outer scope being the transient one.

   procedure Wrap_Transient_Expression (N : Node_Id);
   --  N is a sub-expression. Expand a transient block around an expression

   procedure Wrap_Transient_Statement (N : Node_Id);
   --  N is a statement. Expand a transient block around an instruction

end Exp_Ch7;
