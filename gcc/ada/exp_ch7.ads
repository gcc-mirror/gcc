------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 7                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

with Types; use Types;

package Exp_Ch7 is

   procedure Expand_N_Package_Body        (N : Node_Id);
   procedure Expand_N_Package_Declaration (N : Node_Id);

   ------------------------------
   --  Finalization Management --
   ------------------------------

   function In_Finalization_Root (E : Entity_Id) return Boolean;
   --  True if current scope is in package System.Finalization_Root. Used
   --  to avoid certain expansions that would involve circularity in the
   --  Rtsfind mechanism.

   procedure Build_Final_List (N : Node_Id; Typ : Entity_Id);
   --  Build finalization list for anonymous access types, and for access
   --  types that are frozen before their designated types are known to
   --  be controlled.

   procedure Build_Controlling_Procs (Typ : Entity_Id);
   --  Typ is a record, and array type having controlled components.
   --  Create the procedures Deep_Initialize, Deep_Adjust and Deep_Finalize
   --  that take care of finalization management at run-time.

   function Controller_Component (Typ : Entity_Id) return Entity_Id;
   --  Returns the entity of the component whose name is 'Name_uController'

   function Controlled_Type (T : Entity_Id) return Boolean;
   --  True if T potentially needs finalization actions

   function Find_Final_List
     (E    : Entity_Id;
      Ref  : Node_Id := Empty)
      return Node_Id;
      --  E is an entity representing a controlled object, a controlled type
      --  or a scope. If Ref is not empty, it is a reference to a controlled
      --  record, the closest Final list is in the controller component of
      --  the record containing Ref otherwise this function returns a
      --  reference to the final list attached to the closest dynamic scope
      --  (that can be E itself) creating this final list if necessary.

   function Has_New_Controlled_Component (E : Entity_Id) return Boolean;
   --  E is a type entity. Give the same resul as Has_Controlled_Component
   --  except for tagged extensions where the result is True only if the
   --  latest extension contains a controlled component.

   function Make_Attach_Call
     (Obj_Ref      : Node_Id;
      Flist_Ref    : Node_Id;
      With_Attach  : Node_Id)
      return         Node_Id;
   --  Attach the referenced object to the referenced Final Chain
   --  'Flist_Ref' With_Attach is an expression of type Short_Short_Integer
   --  which can be either '0' to signify no attachment, '1' for
   --  attachement to a simply linked list or '2' for attachement to a
   --  doubly linked list.

   function Make_Init_Call
     (Ref          : Node_Id;
      Typ          : Entity_Id;
      Flist_Ref    : Node_Id;
      With_Attach  : Node_Id)
      return         List_Id;
   --  Ref is an expression (with no-side effect and is not required to
   --  have been previously analyzed) that references the object to be
   --  initialized. Typ is the expected type of Ref, which is a controlled
   --  type (Is_Controlled) or a type with controlled components
   --  (Has_Controlled). 'Dynamic_Case' controls the way the object is
   --  attached which is different whether the object is dynamically
   --  allocated or not.
   --
   --  This function will generate the appropriate calls to make
   --  sure that the objects referenced by Ref are initialized. The
   --  generate code is quite different depending on the fact the type
   --  IS_Controlled or HAS_Controlled but this is not the problem of the
   --  caller, the details are in the body.

   function Make_Adjust_Call
     (Ref          : Node_Id;
      Typ          : Entity_Id;
      Flist_Ref    : Node_Id;
      With_Attach  : Node_Id)
      return         List_Id;
   --  Ref is an expression (with no-side effect and is not required to
   --  have been previously analyzed) that references the object to be
   --  adjusted. Typ is the expected type of Ref, which is a controlled
   --  type (Is_Controlled) or a type with controlled components
   --  (Has_Controlled).
   --
   --  This function will generate the appropriate calls to make
   --  sure that the objects referenced by Ref are adjusted. The generated
   --  code is quite different depending on the fact the type IS_Controlled
   --  or HAS_Controlled but this is not the problem of the caller, the
   --  details are in the body. If the parameter With_Attach is set to
   --  True, the finalizable objects involved are attached to the proper
   --  finalization chain. The objects must be attached when the adjust
   --  takes place after an initialization expression but not when it takes
   --  place after a regular assignment.
   --
   --  The description of With_Attach is completely obsolete ???

   function Make_Final_Call
     (Ref         : Node_Id;
      Typ         : Entity_Id;
      With_Detach : Node_Id)
      return        List_Id;
   --  Ref is an expression (with no-side effect and is not required to
   --  have been previously analyzed) that references the object
   --  to be Finalized. Typ is the expected type of Ref, which is a
   --  controlled type (Is_Controlled) or a type with controlled
   --  components (Has_Controlled).
   --
   --  This function will generate the appropriate calls to make
   --  sure that the objects referenced by Ref are finalized. The generated
   --  code is quite different depending on the fact the type IS_Controlled
   --  or HAS_Controlled but this is not the problem of the caller, the
   --  details are in the body. If the parameter With_Detach is set to
   --  True, the finalizable objects involved are detached from the proper
   --  finalization chain. The objects must be detached when finalizing an
   --  unchecked deallocated object but not when finalizing the target of
   --  an assignment, it is not necessary either on scope exit.

   procedure Expand_Ctrl_Function_Call (N : Node_Id);
   --  Expand a call to a function returning a controlled value. That is to
   --  say attach the result of the call to the current finalization list,
   --  which is the one of the transient scope created for such constructs.

   --------------------------------
   -- Transient Scope Management --
   --------------------------------

   procedure Expand_Cleanup_Actions (N : Node_Id);
   --  Expand the necessary stuff into a scope to enable finalization of local
   --  objects and deallocation of transient data when exiting the scope. N is
   --  a "scope node" that is to say one of the following: N_Block_Statement,
   --  N_Subprogram_Body, N_Task_Body, N_Entry_Body.

   procedure Establish_Transient_Scope (N : Node_Id; Sec_Stack : Boolean);
   --  Push a new transient scope on the scope stack. N is the node responsible
   --  for the need of a transient scope. If Sec_Stack is True then the
   --  secondary stack is brought in, otherwise it isn't.

   function Node_To_Be_Wrapped return Node_Id;
   --  return the node to be wrapped if the current scope is transient.

   procedure Store_Before_Actions_In_Scope (L : List_Id);
   --  Append the list L of actions to the end of the before-actions store
   --  in the top of the scope stack

   procedure Store_After_Actions_In_Scope (L : List_Id);
   --  Append the list L of actions to the beginning of the after-actions
   --  store in the top of the scope stack

   procedure Wrap_Transient_Declaration (N : Node_Id);
   --  N is an object declaration. Expand the finalization calls after the
   --  declaration and make the outer scope beeing the transient one.

   procedure Wrap_Transient_Expression (N : Node_Id);
   --  N is a sub-expression. Expand a transient block around an expression

   procedure Wrap_Transient_Statement (N : Node_Id);
   --  N is a statement. Expand a transient block around an instruction

end Exp_Ch7;
