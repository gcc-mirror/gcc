------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
--                                                                          --
--                                 S p e c                                  --
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

--  Expand routines for chapter 6 constructs

with Types; use Types;

package Exp_Ch6 is

   procedure Expand_N_Extended_Return_Statement (N : Node_Id);
   procedure Expand_N_Function_Call             (N : Node_Id);
   procedure Expand_N_Procedure_Call_Statement  (N : Node_Id);
   procedure Expand_N_Simple_Return_Statement   (N : Node_Id);
   procedure Expand_N_Subprogram_Body           (N : Node_Id);
   procedure Expand_N_Subprogram_Body_Stub      (N : Node_Id);
   procedure Expand_N_Subprogram_Declaration    (N : Node_Id);

   procedure Expand_Call (N : Node_Id);
   --  This procedure contains common processing for Expand_N_Function_Call,
   --  Expand_N_Procedure_Statement, and Expand_N_Entry_Call.

   procedure Freeze_Subprogram (N : Node_Id);
   --  generate the appropriate expansions related to Subprogram freeze
   --  nodes (e.g. the filling of the corresponding Dispatch Table for
   --  Primitive Operations)

   --  The following type defines the various forms of allocation used for the
   --  results of build-in-place function calls.

   type BIP_Allocation_Form is
     (Unspecified,
      Caller_Allocation,
      Secondary_Stack,
      Global_Heap,
      User_Storage_Pool);

   type BIP_Formal_Kind is
   --  Ada 2005 (AI-318-02): This type defines the kinds of implicit extra
   --  formals created for build-in-place functions. The order of these
   --  enumeration literals matches the order in which the formals are
   --  declared. See Sem_Ch6.Create_Extra_Formals.

     (BIP_Alloc_Form,
      --  Present if result subtype is unconstrained or tagged. Indicates
      --  whether the return object is allocated by the caller or callee, and
      --  if the callee, whether to use the secondary stack or the heap. See
      --  Create_Extra_Formals.

      BIP_Storage_Pool,
      --  Present if result subtype is unconstrained or tagged. If
      --  BIP_Alloc_Form = User_Storage_Pool, this is a pointer to the pool
      --  (of type access to Root_Storage_Pool'Class). Otherwise null.

      BIP_Finalization_Master,
      --  Present if result type needs finalization. Pointer to caller's
      --  finalization master.

      BIP_Task_Master,
      --  Present if result type contains tasks. Master associated with
      --  calling context.

      BIP_Activation_Chain,
      --  Present if result type contains tasks. Caller's activation chain

      BIP_Object_Access);
      --  Present for all build-in-place functions. Address at which to place
      --  the return object, or null if BIP_Alloc_Form indicates allocated by
      --  callee.
      --
      --  ??? We might also need to be able to pass in a constrained flag.

   procedure Add_Extra_Actual_To_Call
     (Subprogram_Call : Node_Id;
      Extra_Formal    : Entity_Id;
      Extra_Actual    : Node_Id);
   --  Adds Extra_Actual as a named parameter association for the formal
   --  Extra_Formal in Subprogram_Call.

   function BIP_Formal_Suffix (Kind : BIP_Formal_Kind) return String;
   --  Ada 2005 (AI-318-02): Returns a string to be used as the suffix of names
   --  for build-in-place formal parameters of the given kind.

   function Build_In_Place_Formal
     (Func : Entity_Id;
      Kind : BIP_Formal_Kind) return Entity_Id;
   --  Ada 2005 (AI-318-02): Locates and returns the entity for the implicit
   --  build-in-place formal parameter of the given kind associated with the
   --  function Func, and returns its Entity_Id. It is a bug if not found; the
   --  caller should ensure this is called only when the extra formal exists.

   function Build_Procedure_Body_Form
     (Func_Id : Entity_Id; Func_Body : Node_Id) return Node_Id;
   --  Create a procedure body which emulates the behavior of function Func_Id.
   --  Func_Body is the root of the body of the function before its analysis.
   --  The returned node is the root of the procedure body which will replace
   --  the original function body, which is not needed for the C program.

   function Is_Build_In_Place_Function (E : Entity_Id) return Boolean;
   --  Ada 2005 (AI-318-02): Returns True if E denotes a function, generic
   --  function, or access-to-function type whose result must be built in
   --  place; otherwise returns False. For Ada 2005, this is currently
   --  restricted to the set of functions whose result subtype is an inherently
   --  limited type. In Ada 95, this must be False for inherently limited
   --  result types (but currently returns False for all Ada 95 functions).
   --  Eventually we plan to support build-in-place for nonlimited types.
   --  Build-in-place is usually more efficient for large things, and less
   --  efficient for small things. However, we never use build-in-place if the
   --  convention is other than Ada, because that would disturb mixed-language
   --  programs. Note that for the non-inherently-limited cases, we must make
   --  the same decision for Ada 95 and 2005, so that mixed-dialect programs
   --  will work.

   function Is_Build_In_Place_Function_Call (N : Node_Id) return Boolean;
   --  Ada 2005 (AI-318-02): Returns True if N denotes a call to a function
   --  that requires handling as a build-in-place call or is a qualified
   --  expression applied to such a call; otherwise returns False.

   function Is_Null_Procedure (Subp : Entity_Id) return Boolean;
   --  Predicate to recognize stubbed procedures and null procedures, which
   --  can be inlined unconditionally in all cases.

   procedure Make_Build_In_Place_Call_In_Allocator
     (Allocator     : Node_Id;
      Function_Call : Node_Id);
   --  Ada 2005 (AI-318-02): Handle a call to a build-in-place function that
   --  occurs as the expression initializing an allocator, by passing access
   --  to the allocated object as an additional parameter of the function call.
   --  A new access object is declared that is initialized to the result of the
   --  allocator, passed to the function, and the allocator is rewritten to
   --  refer to that access object. Function_Call must denote either an
   --  N_Function_Call node for which Is_Build_In_Place_Call is True, or else
   --  an N_Qualified_Expression node applied to such a function call.

   procedure Make_Build_In_Place_Call_In_Anonymous_Context
     (Function_Call : Node_Id);
   --  Ada 2005 (AI-318-02): Handle a call to a build-in-place function that
   --  occurs in a context that does not provide a separate object. A temporary
   --  object is created to act as the return object and an access to the
   --  temporary is passed as an additional parameter of the call. This occurs
   --  in contexts such as subprogram call actuals and object renamings.
   --  Function_Call must denote either an N_Function_Call node for which
   --  Is_Build_In_Place_Call is True, or else an N_Qualified_Expression node
   --  applied to such a function call.

   procedure Make_Build_In_Place_Call_In_Assignment
     (Assign        : Node_Id;
      Function_Call : Node_Id);
   --  Ada 2005 (AI-318-02): Handle a call to a build-in-place function that
   --  occurs as the right-hand side of an assignment statement by passing
   --  access to the left-hand side as an additional parameter of the function
   --  call. Assign must denote a N_Assignment_Statement. Function_Call must
   --  denote either an N_Function_Call node for which Is_Build_In_Place_Call
   --  is True, or an N_Qualified_Expression node applied to such a function
   --  call.

   procedure Make_Build_In_Place_Call_In_Object_Declaration
     (Obj_Decl      : Node_Id;
      Function_Call : Node_Id);
   --  Ada 2005 (AI-318-02): Handle a call to a build-in-place function that
   --  occurs as the expression initializing an object declaration by
   --  passing access to the declared object as an additional parameter of the
   --  function call. Function_Call must denote either an N_Function_Call node
   --  for which Is_Build_In_Place_Call is True, or an N_Qualified_Expression
   --  node applied to such a function call.

   procedure Make_CPP_Constructor_Call_In_Allocator
     (Allocator     : Node_Id;
      Function_Call : Node_Id);
   --  Handle a call to a CPP constructor that occurs as the expression that
   --  initializes an allocator, by passing access to the allocated object as
   --  an additional parameter of the constructor call. A new access object is
   --  declared that is initialized to the result of the allocator, passed to
   --  the constructor, and the allocator is rewritten to refer to that access
   --  object. Function_Call must denote a call to a CPP_Constructor function.

   function Needs_BIP_Alloc_Form (Func_Id : Entity_Id) return Boolean;
   --  Ada 2005 (AI-318-02): Return True if the function needs an implicit
   --  BIP_Alloc_Form parameter (see type BIP_Formal_Kind).

   function Needs_BIP_Finalization_Master (Func_Id : Entity_Id) return Boolean;
   --  Ada 2005 (AI-318-02): Return True if the result subtype of function
   --  Func_Id might need finalization actions. This includes build-in-place
   --  functions with tagged result types, since they can be invoked via
   --  dispatching calls, and descendant types may require finalization.

   function Needs_Result_Accessibility_Level
     (Func_Id : Entity_Id) return Boolean;
   --  Ada 2012 (AI05-0234): Return True if the function needs an implicit
   --  parameter to identify the accessibility level of the function result
   --  "determined by the point of call".

end Exp_Ch6;
