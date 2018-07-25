------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

--  Package containing utility procedures used throughout the expander

with Exp_Tss; use Exp_Tss;
with Namet;   use Namet;
with Rtsfind; use Rtsfind;
with Sinfo;   use Sinfo;
with Types;   use Types;
with Uintp;   use Uintp;

package Exp_Util is

   -----------------------------------------------
   -- Handling of Actions Associated with Nodes --
   -----------------------------------------------

   --  The evaluation of certain expression nodes involves the elaboration
   --  of associated types and other declarations, and the execution of
   --  statement sequences. Expansion routines generating such actions must
   --  find an appropriate place in the tree to hang the actions so that
   --  they will be evaluated at the appropriate point.

   --  Some cases are simple:

   --    For an expression occurring in a simple statement that is in a list
   --    of statements, the actions are simply inserted into the list before
   --    the associated statement.

   --    For an expression occurring in a declaration (declarations always
   --    appear in lists), the actions are similarly inserted into the list
   --    just before the associated declaration. ???Declarations do not always
   --    appear in lists; in particular, a library unit declaration does not
   --    appear in a list, and Insert_Action will crash in that case.

   --  The following special cases arise:

   --    For actions associated with the right operand of a short circuit
   --    form, the actions are first stored in the short circuit form node
   --    in the Actions field. The expansion of these forms subsequently
   --    expands the short circuit forms into if statements which can then
   --    be moved as described above.

   --    For actions appearing in the Condition expression of a while loop,
   --    or an elsif clause, the actions are similarly temporarily stored in
   --    in the node (N_Elsif_Part or N_Iteration_Scheme) associated with
   --    the expression using the Condition_Actions field. Subsequently, the
   --    expansion of these nodes rewrites the control structures involved to
   --    reposition the actions in normal statement sequence.

   --    For actions appearing in the then or else expression of a conditional
   --    expression, these actions are similarly placed in the node, using the
   --    Then_Actions or Else_Actions field as appropriate. Once again the
   --    expansion of the N_If_Expression node rewrites the node so that the
   --    actions can be positioned normally.

   --    For actions coming from expansion of the expression in an expression
   --    with actions node, the action is appended to the list of actions.

   --  Basically what we do is to climb up to the tree looking for the
   --  proper insertion point, as described by one of the above cases,
   --  and then insert the appropriate action or actions.

   --  Note if more than one insert call is made specifying the same
   --  Assoc_Node, then the actions are elaborated in the order of the
   --  calls, and this guarantee is preserved for the special cases above.

   procedure Insert_Action
     (Assoc_Node : Node_Id;
      Ins_Action : Node_Id);
   --  Insert the action Ins_Action at the appropriate point as described
   --  above. The action is analyzed using the default checks after it is
   --  inserted. Assoc_Node is the node with which the action is associated.

   procedure Insert_Action
     (Assoc_Node : Node_Id;
      Ins_Action : Node_Id;
      Suppress   : Check_Id);
   --  Insert the action Ins_Action at the appropriate point as described
   --  above. The action is analyzed using the default checks as modified
   --  by the given Suppress argument after it is inserted. Assoc_Node is
   --  the node with which the action is associated.

   procedure Insert_Actions
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id);
   --  Insert the list of action Ins_Actions at the appropriate point as
   --  described above. The actions are analyzed using the default checks
   --  after they are inserted. Assoc_Node is the node with which the actions
   --  are associated. Ins_Actions may be No_List, in which case the call has
   --  no effect.

   procedure Insert_Actions
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id;
      Suppress    : Check_Id);
   --  Insert the list of action Ins_Actions at the appropriate point as
   --  described above. The actions are analyzed using the default checks
   --  as modified by the given Suppress argument after they are inserted.
   --  Assoc_Node is the node with which the actions are associated.
   --  Ins_Actions may be No_List, in which case the call has no effect.

   procedure Insert_Action_After
     (Assoc_Node : Node_Id;
      Ins_Action : Node_Id);
   --  Assoc_Node must be a node in a list. Same as Insert_Action but the
   --  action will be inserted after N in a manner that is compatible with
   --  the transient scope mechanism.
   --
   --  Note: If several successive calls to Insert_Action_After are made for
   --  the same node, they will each in turn be inserted just after the node.
   --  This means they will end up being executed in reverse order. Use the
   --  call to Insert_Actions_After to insert a list of actions to be executed
   --  in the sequence in which they are given in the list.

   procedure Insert_Actions_After
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id);
   --  Assoc_Node must be a node in a list. Same as Insert_Actions but
   --  actions will be inserted after N in a manner that is compatible with
   --  the transient scope mechanism. This procedure must be used instead
   --  of Insert_List_After if Assoc_Node may be in a transient scope.
   --
   --  Implementation limitation: Assoc_Node must be a statement. We can
   --  generalize to expressions if there is a need but this is tricky to
   --  implement because of short-circuits (among other things).???

   procedure Insert_Declaration (N : Node_Id; Decl : Node_Id);
   --  N must be a subexpression (Nkind in N_Subexpr). This is similar to
   --  Insert_Action (N, Decl), but inserts Decl outside the expression in
   --  which N appears. This is called Insert_Declaration because the intended
   --  use is for declarations that have no associated code. We can't go
   --  moving other kinds of things out of the current expression, since they
   --  could be executed conditionally (e.g. right operand of short circuit,
   --  or THEN/ELSE of if expression). This is currently used only in
   --  Modify_Tree_For_C mode, where it is needed because in C we have no
   --  way of having declarations within an expression (a really annoying
   --  limitation).

   procedure Insert_Library_Level_Action (N : Node_Id);
   --  This procedure inserts and analyzes the node N as an action at the
   --  library level for the current unit (i.e. it is attached to the
   --  Actions field of the N_Compilation_Aux node for the main unit).

   procedure Insert_Library_Level_Actions (L : List_Id);
   --  Similar, but inserts a list of actions

   -----------------------
   -- Other Subprograms --
   -----------------------

   procedure Activate_Atomic_Synchronization (N : Node_Id);
   --  N is a node for which atomic synchronization may be required (it is
   --  either an identifier, expanded name, or selected/indexed component or
   --  an explicit dereference). The caller has checked the basic conditions
   --  (atomic variable appearing and Atomic_Sync not disabled). This function
   --  checks if atomic synchronization is required and if so sets the flag
   --  and if appropriate generates a warning (in -gnatw.n mode).

   procedure Adjust_Condition (N : Node_Id);
   --  The node N is an expression whose root-type is Boolean, and which
   --  represents a boolean value used as a condition (i.e. a True/False
   --  value). This routine handles the case of C and Fortran convention
   --  boolean types, which have zero/non-zero semantics rather than the normal
   --  0/1 semantics, and also the case of an enumeration rep clause that
   --  specifies a non-standard representation. On return, node N always has
   --  the type Standard.Boolean, with a value that is a standard Boolean
   --  values of 0/1 for False/True. This procedure is used in two situations.
   --  First, the processing for a condition field always calls
   --  Adjust_Condition, so that the boolean value presented to the backend is
   --  a standard value. Second, for the code for boolean operations such as
   --  AND, Adjust_Condition is called on both operands, and then the operation
   --  is done in the domain of Standard_Boolean, then Adjust_Result_Type is
   --  called on the result to possibly reset the original type. This procedure
   --  also takes care of validity checking if Validity_Checks = Tests.

   procedure Adjust_Result_Type (N : Node_Id; T : Entity_Id);
   --  The processing of boolean operations like AND uses the procedure
   --  Adjust_Condition so that it can operate on Standard.Boolean, which is
   --  the only boolean type on which the backend needs to be able to implement
   --  such operators. This means that the result is also of type
   --  Standard.Boolean. In general the type must be reset back to the original
   --  type to get proper semantics, and that is the purpose of this procedure.
   --  N is the node (of type Standard.Boolean), and T is the desired type. As
   --  an optimization, this procedure leaves the type as Standard.Boolean in
   --  contexts where this is permissible (in particular for Condition fields,
   --  and for operands of other logical operations higher up the tree). The
   --  call to this procedure is completely ignored if the argument N is not of
   --  type Boolean.

   procedure Append_Freeze_Action (T : Entity_Id; N : Node_Id);
   --  Add a new freeze action for the given type. The freeze action is
   --  attached to the freeze node for the type. Actions will be elaborated in
   --  the order in which they are added. Note that the added node is not
   --  analyzed. The analyze call is found in Exp_Ch13.Expand_N_Freeze_Entity.

   procedure Append_Freeze_Actions (T : Entity_Id; L : List_Id);
   --  Adds the given list of freeze actions (declarations or statements) for
   --  the given type. The freeze actions are attached to the freeze node for
   --  the type. Actions will be elaborated in the order in which they are
   --  added, and the actions within the list will be elaborated in list order.
   --  Note that the added nodes are not analyzed. The analyze call is found in
   --  Exp_Ch13.Expand_N_Freeze_Entity.

   procedure Build_Allocate_Deallocate_Proc
     (N           : Node_Id;
      Is_Allocate : Boolean);
   --  Create a custom Allocate/Deallocate to be associated with an allocation
   --  or deallocation:
   --
   --    1) controlled objects
   --    2) class-wide objects
   --    3) any kind of object on a subpool
   --
   --  N must be an allocator or the declaration of a temporary variable which
   --  represents the expression of the original allocator node, otherwise N
   --  must be a free statement. If flag Is_Allocate is set, the generated
   --  routine is allocate, deallocate otherwise.

   function Build_Abort_Undefer_Block
     (Loc     : Source_Ptr;
      Stmts   : List_Id;
      Context : Node_Id) return Node_Id;
   --  Wrap statements Stmts in a block where the AT END handler contains a
   --  call to Abort_Undefer_Direct. Context is the node which prompted the
   --  inlining of the abort undefer routine. Note that this routine does
   --  not install a call to Abort_Defer.

   procedure Build_Class_Wide_Expression
     (Prag          : Node_Id;
      Subp          : Entity_Id;
      Par_Subp      : Entity_Id;
      Adjust_Sloc   : Boolean;
      Needs_Wrapper : out Boolean);
   --  Build the expression for an inherited class-wide condition. Prag is
   --  the pragma constructed from the corresponding aspect of the parent
   --  subprogram, and Subp is the overriding operation, and Par_Subp is
   --  the overridden operation that has the condition. Adjust_Sloc is True
   --  when the sloc of nodes traversed should be adjusted for the inherited
   --  pragma. The routine is also called to check whether an inherited
   --  operation that is not overridden but has inherited conditions needs
   --  a wrapper, because the inherited condition includes calls to other
   --  primitives that have been overridden. In that case the first argument
   --  is the expression of the original class-wide aspect. In SPARK_Mode, such
   --  operation which are just inherited but have modified pre/postconditions
   --  are illegal.
   --  If there are calls to overridden operations in the condition, and the
   --  pragma applies to an inherited operation, a wrapper must be built for
   --  it to capture the new inherited condition. The flag Needs_Wrapper is
   --  set in that case so that the wrapper can be built, when the controlling
   --  type is frozen.

   function Build_DIC_Call
     (Loc    : Source_Ptr;
      Obj_Id : Entity_Id;
      Typ    : Entity_Id) return Node_Id;
   --  Build a call to the DIC procedure of type Typ with Obj_Id as the actual
   --  parameter.

   procedure Build_DIC_Procedure_Body
     (Typ        : Entity_Id;
      For_Freeze : Boolean := False);
   --  Create the body of the procedure which verifies the assertion expression
   --  of pragma Default_Initial_Condition at run time. Flag For_Freeze should
   --  be set when the body is constructed as part of the freezing actions for
   --  Typ.

   procedure Build_DIC_Procedure_Declaration (Typ : Entity_Id);
   --  Create the declaration of the procedure which verifies the assertion
   --  expression of pragma Default_Initial_Condition at run time.

   procedure Build_Invariant_Procedure_Body
     (Typ               : Entity_Id;
      Partial_Invariant : Boolean := False);
   --  Create the body of the procedure which verifies the invariants of type
   --  Typ at runtime. Flag Partial_Invariant should be set when Typ denotes a
   --  private type, otherwise it is assumed that Typ denotes the full view of
   --  a private type.

   procedure Build_Invariant_Procedure_Declaration
     (Typ               : Entity_Id;
      Partial_Invariant : Boolean := False);
   --  Create the declaration of the procedure which verifies the invariants of
   --  type Typ at runtime. Flag Partial_Invariant should be set when building
   --  the invariant procedure for a private type.

   procedure Build_Procedure_Form (N : Node_Id);
   --  Create a procedure declaration which emulates the behavior of a function
   --  that returns an array type, for C-compatible generation.

   function Build_Runtime_Call (Loc : Source_Ptr; RE : RE_Id) return Node_Id;
   --  Build an N_Procedure_Call_Statement calling the given runtime entity.
   --  The call has no parameters. The first argument provides the location
   --  information for the tree and for error messages. The call node is not
   --  analyzed on return, the caller is responsible for analyzing it.

   function Build_SS_Mark_Call
     (Loc  : Source_Ptr;
      Mark : Entity_Id) return Node_Id;
   --  Build a call to routine System.Secondary_Stack.Mark. Mark denotes the
   --  entity of the secondary stack mark.

   function Build_SS_Release_Call
     (Loc  : Source_Ptr;
      Mark : Entity_Id) return Node_Id;
   --  Build a call to routine System.Secondary_Stack.Release. Mark denotes the
   --  entity of the secondary stack mark.

   function Build_Task_Image_Decls
     (Loc          : Source_Ptr;
      Id_Ref       : Node_Id;
      A_Type       : Entity_Id;
      In_Init_Proc : Boolean := False) return List_Id;
   --  Build declaration for a variable that holds an identifying string to be
   --  used as a task name. Id_Ref is an identifier if the task is a variable,
   --  and a selected or indexed component if the task is component of an
   --  object. If it is an indexed component, A_Type is the corresponding array
   --  type. Its index types are used to build the string as an image of the
   --  index values. For composite types, the result includes two declarations:
   --  one for a generated function that computes the image without using
   --  concatenation, and one for the variable that holds the result.
   --
   --  If In_Init_Proc is true, the call is part of the initialization of
   --  a component of a composite type, and the enclosing initialization
   --  procedure must be flagged as using the secondary stack. If In_Init_Proc
   --  is false, the call is for a stand-alone object, and the generated
   --  function itself must do its own cleanups.

   procedure Build_Transient_Object_Statements
     (Obj_Decl     : Node_Id;
      Fin_Call     : out Node_Id;
      Hook_Assign  : out Node_Id;
      Hook_Clear   : out Node_Id;
      Hook_Decl    : out Node_Id;
      Ptr_Decl     : out Node_Id;
      Finalize_Obj : Boolean := True);
   --  Subsidiary to the processing of transient objects in transient scopes,
   --  if expressions, case expressions, expression_with_action nodes, array
   --  aggregates, and record aggregates. Obj_Decl denotes the declaration of
   --  the transient object. Generate the following nodes:
   --
   --    * Fin_Call - the call to [Deep_]Finalize which cleans up the transient
   --    object if flag Finalize_Obj is set to True, or finalizes the hook when
   --    the flag is False.
   --
   --    * Hook_Assign - the assignment statement which captures a reference to
   --    the transient object in the hook.
   --
   --    * Hook_Clear - the assignment statement which resets the hook to null
   --
   --    * Hook_Decl - the declaration of the hook object
   --
   --    * Ptr_Decl - the full type declaration of the hook type
   --
   --  These nodes are inserted in specific places depending on the context by
   --  the various Process_Transient_xxx routines.

   procedure Check_Float_Op_Overflow (N : Node_Id);
   --  Called where we could have a floating-point binary operator where we
   --  must check for infinities if we are operating in Check_Float_Overflow
   --  mode. Note that we don't need to worry about unary operator cases,
   --  since for floating-point, abs, unary "-", and unary "+" can never
   --  case overflow.

   function Component_May_Be_Bit_Aligned (Comp : Entity_Id) return Boolean;
   --  This function is in charge of detecting record components that may
   --  cause trouble in the back end if an attempt is made to assign the
   --  component. The back end can handle such assignments with no problem if
   --  the components involved are small (64-bits or less) records or scalar
   --  items (including bit-packed arrays represented with modular types) or
   --  are both aligned on a byte boundary (starting on a byte boundary, and
   --  occupying an integral number of bytes).
   --
   --  However, problems arise for records larger than 64 bits, or for arrays
   --  (other than bit-packed arrays represented with a modular type) if the
   --  component starts on a non-byte boundary, or does not occupy an integral
   --  number of bytes (i.e. there are some bits possibly shared with fields
   --  at the start or beginning of the component). The back end cannot handle
   --  loading and storing such components in a single operation.
   --
   --  This function is used to detect the troublesome situation. it is
   --  conservative in the sense that it produces True unless it knows for
   --  sure that the component is safe (as outlined in the first paragraph
   --  above). The code generation for record and array assignment checks for
   --  trouble using this function, and if so the assignment is generated
   --  component-wise, which the back end is required to handle correctly.
   --
   --  Note that in GNAT 3, the back end will reject such components anyway,
   --  so the hard work in checking for this case is wasted in GNAT 3, but
   --  it is harmless, so it is easier to do it in all cases, rather than
   --  conditionalize it in GNAT 5 or beyond.

   function Containing_Package_With_Ext_Axioms
     (E : Entity_Id) return Entity_Id;
   --  Returns the package entity with an external axiomatization containing E,
   --  if any, or Empty if none.

   procedure Convert_To_Actual_Subtype (Exp : Node_Id);
   --  The Etype of an expression is the nominal type of the expression,
   --  not the actual subtype. Often these are the same, but not always.
   --  For example, a reference to a formal of unconstrained type has the
   --  unconstrained type as its Etype, but the actual subtype is obtained by
   --  applying the actual bounds. This routine is given an expression, Exp,
   --  and (if necessary), replaces it using Rewrite, with a conversion to
   --  the actual subtype, building the actual subtype if necessary. If the
   --  expression is already of the requested type, then it is unchanged.

   function Corresponding_Runtime_Package (Typ : Entity_Id) return RTU_Id;
   --  Return the id of the runtime package that will provide support for
   --  concurrent type Typ. Currently only protected types are supported,
   --  and the returned value is one of the following:
   --    System_Tasking_Protected_Objects
   --    System_Tasking_Protected_Objects_Entries
   --    System_Tasking_Protected_Objects_Single_Entry

   function Current_Sem_Unit_Declarations return List_Id;
   --  Return the place where it is fine to insert declarations for the
   --  current semantic unit. If the unit is a package body, return the
   --  visible declarations of the corresponding spec. For RCI stubs, this
   --  is necessary because the point at which they are generated may not
   --  be the earliest point at which they are used.

   function Duplicate_Subexpr
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Renaming_Req : Boolean := False) return Node_Id;
   --  Given the node for a subexpression, this function makes a logical copy
   --  of the subexpression, and returns it. This is intended for use when the
   --  expansion of an expression needs to repeat part of it. For example,
   --  replacing a**2 by a*a requires two references to a which may be a
   --  complex subexpression. Duplicate_Subexpr guarantees not to duplicate
   --  side effects. If necessary, it generates actions to save the expression
   --  value in a temporary, inserting these actions into the tree using
   --  Insert_Actions with Exp as the insertion location. The original
   --  expression and the returned result then become references to this saved
   --  value. Exp must be analyzed on entry. On return, Exp is analyzed, but
   --  the caller is responsible for analyzing the returned copy after it is
   --  attached to the tree.
   --
   --  The Name_Req flag is set to ensure that the result is suitable for use
   --  in a context requiring a name (for example, the prefix of an attribute
   --  reference) (can't this just be a qualification in Ada 2012???).
   --
   --  The Renaming_Req flag is set to produce an object renaming declaration
   --  rather than an object declaration. This is valid only if the expression
   --  Exp designates a renamable object. This is used for example in the case
   --  of an unchecked deallocation, to make sure the object gets set to null.
   --
   --  Note that if there are any run time checks in Exp, these same checks
   --  will be duplicated in the returned duplicated expression. The two
   --  following functions allow this behavior to be modified.

   function Duplicate_Subexpr_No_Checks
     (Exp           : Node_Id;
      Name_Req      : Boolean   := False;
      Renaming_Req  : Boolean   := False;
      Related_Id    : Entity_Id := Empty;
      Is_Low_Bound  : Boolean   := False;
      Is_High_Bound : Boolean   := False) return Node_Id;
   --  Identical in effect to Duplicate_Subexpr, except that Remove_Checks is
   --  called on the result, so that the duplicated expression does not include
   --  checks. This is appropriate for use when Exp, the original expression is
   --  unconditionally elaborated before the duplicated expression, so that
   --  there is no need to repeat any checks.
   --
   --  Related_Id denotes the entity of the context where Expr appears. Flags
   --  Is_Low_Bound and Is_High_Bound specify whether the expression to check
   --  is the low or the high bound of a range. These three optional arguments
   --  signal Remove_Side_Effects to create an external symbol of the form
   --  Chars (Related_Id)_FIRST/_LAST. For suggested use of these parameters
   --  see the warning in the body of Sem_Ch3.Process_Range_Expr_In_Decl.

   function Duplicate_Subexpr_Move_Checks
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Renaming_Req : Boolean := False) return Node_Id;
   --  Identical in effect to Duplicate_Subexpr, except that Remove_Checks is
   --  called on Exp after the duplication is complete, so that the original
   --  expression does not include checks. In this case the result returned
   --  (the duplicated expression) will retain the original checks. This is
   --  appropriate for use when the duplicated expression is sure to be
   --  elaborated before the original expression Exp, so that there is no need
   --  to repeat the checks.

   function Enclosing_Init_Proc return Entity_Id;
   --  Obtain the entity of the type initialization procedure which encloses
   --  the current scope. Return Empty if no such procedure exists.

   procedure Ensure_Defined (Typ : Entity_Id; N : Node_Id);
   --  This procedure ensures that type referenced by Typ is defined. For the
   --  case of a type other than an Itype, nothing needs to be done, since
   --  all such types have declaration nodes. For Itypes, an N_Itype_Reference
   --  node is generated and inserted as an action on node N. This is typically
   --  used to ensure that an Itype is properly defined outside a conditional
   --  construct when it is referenced in more than one branch.

   function Entry_Names_OK return Boolean;
   --  Determine whether it is appropriate to dynamically allocate strings
   --  which represent entry [family member] names. These strings are created
   --  by the compiler and used by GDB.

   procedure Evaluate_Name (Nam : Node_Id);
   --  Remove all side effects from a name which appears as part of an object
   --  renaming declaration. Similarly to Force_Evaluation, it removes the
   --  side effects and captures the values of the variables, except for the
   --  variable being renamed. Hence this differs from Force_Evaluation and
   --  Remove_Side_Effects (but it calls Force_Evaluation on subexpressions
   --  whose value needs to be fixed).

   procedure Evolve_And_Then (Cond : in out Node_Id; Cond1 : Node_Id);
   --  Rewrites Cond with the expression: Cond and then Cond1. If Cond is
   --  Empty, then simply returns Cond1 (this allows the use of Empty to
   --  initialize a series of checks evolved by this routine, with a final
   --  result of Empty indicating that no checks were required). The Sloc field
   --  of the constructed N_And_Then node is copied from Cond1.

   procedure Evolve_Or_Else (Cond : in out Node_Id; Cond1 : Node_Id);
   --  Rewrites Cond with the expression: Cond or else Cond1. If Cond is Empty,
   --  then simply returns Cond1 (this allows the use of Empty to initialize a
   --  series of checks evolved by this routine, with a final result of Empty
   --  indicating that no checks were required). The Sloc field of the
   --  constructed N_Or_Else node is copied from Cond1.

   function Exceptions_In_Finalization_OK return Boolean;
   --  Determine whether the finalization machinery can safely add exception
   --  handlers and recovery circuitry.

   procedure Expand_Static_Predicates_In_Choices (N : Node_Id);
   --  N is either a case alternative or a variant. The Discrete_Choices field
   --  of N points to a list of choices. If any of these choices is the name
   --  of a (statically) predicated subtype, then it is rewritten as the series
   --  of choices that correspond to the values allowed for the subtype.

   procedure Expand_Subtype_From_Expr
     (N             : Node_Id;
      Unc_Type      : Entity_Id;
      Subtype_Indic : Node_Id;
      Exp           : Node_Id;
      Related_Id    : Entity_Id := Empty);
   --  Build a constrained subtype from the initial value in object
   --  declarations and/or allocations when the type is indefinite (including
   --  class-wide). Set Related_Id to request an external name for the subtype
   --  rather than an internal temporary.

   function Expression_Contains_Primitives_Calls_Of
     (Expr : Node_Id;
      Typ  : Entity_Id) return Boolean;
   --  Return True if the expression Expr contains a nondispatching call to a
   --  function which is a primitive of the tagged type Typ.

   function Finalize_Address (Typ : Entity_Id) return Entity_Id;
   --  Locate TSS primitive Finalize_Address in type Typ. Return Empty if the
   --  subprogram is not available.

   function Find_Interface_ADT
     (T     : Entity_Id;
      Iface : Entity_Id) return Elmt_Id;
   --  Ada 2005 (AI-251): Given a type T implementing the interface Iface,
   --  return the element of Access_Disp_Table containing the tag of the
   --  interface.

   function Find_Interface_Tag
     (T     : Entity_Id;
      Iface : Entity_Id) return Entity_Id;
   --  Ada 2005 (AI-251): Given a type T implementing the interface Iface,
   --  return the record component containing the tag of Iface.

   function Find_Prim_Op (T : Entity_Id; Name : Name_Id) return Entity_Id;
   --  Find the first primitive operation of a tagged type T with name Name.
   --  This function allows the use of a primitive operation which is not
   --  directly visible. If T is a class wide type, then the reference is to an
   --  operation of the corresponding root type. It is an error if no primitive
   --  operation with the given name is found.

   function Find_Prim_Op
     (T    : Entity_Id;
      Name : TSS_Name_Type) return Entity_Id;
   --  Same as Find_Prim_Op above, except we're searching for an op that has
   --  the form indicated by Name (i.e. is a type support subprogram with the
   --  indicated suffix).

   function Find_Optional_Prim_Op
     (T : Entity_Id; Name : Name_Id) return Entity_Id;
   function Find_Optional_Prim_Op
     (T    : Entity_Id;
      Name : TSS_Name_Type) return Entity_Id;
   --  Same as Find_Prim_Op, except returns Empty if not found

   function Find_Protection_Object (Scop : Entity_Id) return Entity_Id;
   --  Traverse the scope stack starting from Scop and look for an entry, entry
   --  family, or a subprogram that has a Protection_Object and return it. Must
   --  always return a value since the context in which this routine is invoked
   --  should always have a protection object.

   function Find_Protection_Type (Conc_Typ : Entity_Id) return Entity_Id;
   --  Given a protected type or its corresponding record, find the type of
   --  field _object.

   function Find_Hook_Context (N : Node_Id) return Node_Id;
   --  Determine a suitable node on which to attach actions related to N that
   --  need to be elaborated unconditionally. In general this is the topmost
   --  expression of which N is a subexpression, which in turn may or may not
   --  be evaluated, for example if N is the right operand of a short circuit
   --  operator.

   function Following_Address_Clause (D : Node_Id) return Node_Id;
   --  D is the node for an object declaration. This function searches the
   --  current declarative part to look for an address clause for the object
   --  being declared, and returns the clause if one is found, returns
   --  Empty otherwise.
   --
   --  Note: this function can be costly and must be invoked with special care.
   --  Possibly we could introduce a flag at parse time indicating the presence
   --  of an address clause to speed this up???
   --
   --  Note: currently this function does not scan the private part, that seems
   --  like a potential bug ???

   type Force_Evaluation_Mode is (Relaxed, Strict);

   procedure Force_Evaluation
     (Exp           : Node_Id;
      Name_Req      : Boolean   := False;
      Related_Id    : Entity_Id := Empty;
      Is_Low_Bound  : Boolean   := False;
      Is_High_Bound : Boolean   := False;
      Mode          : Force_Evaluation_Mode := Relaxed);
   --  Force the evaluation of the expression right away. Similar behavior
   --  to Remove_Side_Effects when Variable_Ref is set to TRUE. That is to
   --  say, it removes the side effects and captures the values of the
   --  variables. Remove_Side_Effects guarantees that multiple evaluations
   --  of the same expression won't generate multiple side effects, whereas
   --  Force_Evaluation further guarantees that all evaluations will yield
   --  the same result. If Mode is Relaxed then calls to this subprogram have
   --  no effect if Exp is side-effect free; if Mode is Strict and Exp is not
   --  a static expression then no side-effect check is performed on Exp and
   --  temporaries are unconditionally generated.
   --
   --  Related_Id denotes the entity of the context where Expr appears. Flags
   --  Is_Low_Bound and Is_High_Bound specify whether the expression to check
   --  is the low or the high bound of a range. These three optional arguments
   --  signal Remove_Side_Effects to create an external symbol of the form
   --  Chars (Related_Id)_FIRST/_LAST. If Related_Id is set, then exactly one
   --  of the Is_xxx_Bound flags must be set. For use of these parameters see
   --  the warning in the body of Sem_Ch3.Process_Range_Expr_In_Decl.

   function Fully_Qualified_Name_String
     (E          : Entity_Id;
      Append_NUL : Boolean := True) return String_Id;
   --  Generates the string literal corresponding to the fully qualified name
   --  of entity E, in all upper case, with an ASCII.NUL appended at the end
   --  of the name if Append_NUL is True.

   procedure Generate_Poll_Call (N : Node_Id);
   --  If polling is active, then a call to the Poll routine is built,
   --  and then inserted before the given node N and analyzed.

   procedure Get_Current_Value_Condition
     (Var : Node_Id;
      Op  : out Node_Kind;
      Val : out Node_Id);
   --  This routine processes the Current_Value field of the variable Var. If
   --  the Current_Value field is null or if it represents a known value, then
   --  on return Cond is set to N_Empty, and Val is set to Empty.
   --
   --  The other case is when Current_Value points to an N_If_Statement or an
   --  N_Elsif_Part or a N_Iteration_Scheme node (see description in Einfo for
   --  exact details). In this case, Get_Current_Condition digs out the
   --  condition, and then checks if the condition is known false, known true,
   --  or not known at all. In the first two cases, Get_Current_Condition will
   --  return with Op set to the appropriate conditional operator (inverted if
   --  the condition is known false), and Val set to the constant value. If the
   --  condition is not known, then Op and Val are set for the empty case
   --  (N_Empty and Empty).
   --
   --  The check for whether the condition is true/false unknown depends
   --  on the case:
   --
   --     For an IF, the condition is known true in the THEN part, known false
   --     in any ELSIF or ELSE part, and not known outside the IF statement in
   --     question.
   --
   --     For an ELSIF, the condition is known true in the ELSIF part, known
   --     FALSE in any subsequent ELSIF, or ELSE part, and not known before the
   --     ELSIF, or after the end of the IF statement.
   --
   --  The caller can use this result to determine the value (for the case of
   --  N_Op_Eq), or to determine the result of some other test in other cases
   --  (e.g. no access check required if N_Op_Ne Null).

   function Get_Stream_Size (E : Entity_Id) return Uint;
   --  Return the stream size value of the subtype E

   function Has_Access_Constraint (E : Entity_Id) return Boolean;
   --  Given object or type E, determine if a discriminant is of an access type

   function Has_Annotate_Pragma_For_External_Axiomatization
     (E : Entity_Id) return Boolean;
   --  Returns whether E is a package entity, for which the initial list of
   --  pragmas at the start of the package declaration contains
   --    pragma Annotate (GNATprove, External_Axiomatization);

   function Homonym_Number (Subp : Entity_Id) return Nat;
   --  Here subp is the entity for a subprogram. This routine returns the
   --  homonym number used to disambiguate overloaded subprograms in the same
   --  scope (the number is used as part of constructed names to make sure that
   --  they are unique). The number is the ordinal position on the Homonym
   --  chain, counting only entries in the current scope. If an entity is not
   --  overloaded, the returned number will be one.

   function Inside_Init_Proc return Boolean;
   --  Returns True if current scope is within an init proc

   function In_Library_Level_Package_Body (Id : Entity_Id) return Boolean;
   --  Given an arbitrary entity, determine whether it appears at the library
   --  level of a package body.

   function In_Unconditional_Context (Node : Node_Id) return Boolean;
   --  Node is the node for a statement or a component of a statement. This
   --  function determines if the statement appears in a context that is
   --  unconditionally executed, i.e. it is not within a loop or a conditional
   --  or a case statement etc.

   function Is_All_Null_Statements (L : List_Id) return Boolean;
   --  Return True if all the items of the list are N_Null_Statement nodes.
   --  False otherwise. True for an empty list. It is an error to call this
   --  routine with No_List as the argument.

   function Is_Displacement_Of_Object_Or_Function_Result
     (Obj_Id : Entity_Id) return Boolean;
   --  Determine whether Obj_Id is a source entity that has been initialized by
   --  either a controlled function call or the assignment of another source
   --  object. In both cases the initialization expression is rewritten as a
   --  class-wide conversion of Ada.Tags.Displace.

   function Is_Finalizable_Transient
     (Decl     : Node_Id;
      Rel_Node : Node_Id) return Boolean;
   --  Determine whether declaration Decl denotes a controlled transient which
   --  should be finalized. Rel_Node is the related context. Even though some
   --  transients are controlled, they may act as renamings of other objects or
   --  function calls.

   function Is_Fully_Repped_Tagged_Type (T : Entity_Id) return Boolean;
   --  Tests given type T, and returns True if T is a non-discriminated tagged
   --  type which has a record representation clause that specifies the layout
   --  of all the components, including recursively components in all parent
   --  types. We exclude discriminated types for convenience, it is extremely
   --  unlikely that the special processing associated with the use of this
   --  routine is useful for the case of a discriminated type, and testing for
   --  component overlap would be a pain.

   function Is_Library_Level_Tagged_Type (Typ : Entity_Id) return Boolean;
   --  Return True if Typ is a library level tagged type. Currently we use
   --  this information to build statically allocated dispatch tables.

   function Is_Non_BIP_Func_Call (Expr : Node_Id) return Boolean;
   --  Determine whether node Expr denotes a non build-in-place function call

   function Is_Possibly_Unaligned_Object (N : Node_Id) return Boolean;
   --  Node N is an object reference. This function returns True if it is
   --  possible that the object may not be aligned according to the normal
   --  default alignment requirement for its type (e.g. if it appears in a
   --  packed record, or as part of a component that has a component clause.)

   function Is_Possibly_Unaligned_Slice (N : Node_Id) return Boolean;
   --  Determine whether the node P is a slice of an array where the slice
   --  result may cause alignment problems because it has an alignment that
   --  is not compatible with the type. Return True if so.

   function Is_Ref_To_Bit_Packed_Array (N : Node_Id) return Boolean;
   --  Determine whether the node P is a reference to a bit packed array, i.e.
   --  whether the designated object is a component of a bit packed array, or a
   --  subcomponent of such a component. If so, then all subscripts in P are
   --  evaluated with a call to Force_Evaluation, and True is returned.
   --  Otherwise False is returned, and P is not affected.

   function Is_Ref_To_Bit_Packed_Slice (N : Node_Id) return Boolean;
   --  Determine whether the node P is a reference to a bit packed slice, i.e.
   --  whether the designated object is bit packed slice or a component of a
   --  bit packed slice. Return True if so.

   function Is_Related_To_Func_Return (Id : Entity_Id) return Boolean;
   --  Determine whether object Id is related to an expanded return statement.
   --  The case concerned is "return Id.all;".

   function Is_Renamed_Object (N : Node_Id) return Boolean;
   --  Returns True if the node N is a renamed object. An expression is
   --  considered to be a renamed object if either it is the Name of an object
   --  renaming declaration, or is the prefix of a name which is a renamed
   --  object. For example, in:
   --
   --     x : r renames a (1 .. 2) (1);
   --
   --  We consider that a (1 .. 2) is a renamed object since it is the prefix
   --  of the name in the renaming declaration.

   function Is_Secondary_Stack_BIP_Func_Call (Expr : Node_Id) return Boolean;
   --  Determine whether Expr denotes a build-in-place function which returns
   --  its result on the secondary stack.

   function Is_Tag_To_Class_Wide_Conversion
     (Obj_Id : Entity_Id) return Boolean;
   --  Determine whether object Obj_Id is the result of a tag-to-class-wide
   --  type conversion.

   function Is_Untagged_Derivation (T : Entity_Id) return Boolean;
   --  Returns true if type T is not tagged and is a derived type,
   --  or is a private type whose completion is such a type.

   function Is_Untagged_Private_Derivation
     (Priv_Typ : Entity_Id;
      Full_Typ : Entity_Id) return Boolean;
   --  Determine whether private type Priv_Typ and its full view Full_Typ
   --  represent an untagged derivation from a private parent.

   function Is_Volatile_Reference (N : Node_Id) return Boolean;
   --  Checks if the node N represents a volatile reference, which can be
   --  either a direct reference to a variable treated as volatile, or an
   --  indexed/selected component where the prefix is treated as volatile,
   --  or has Volatile_Components set. A slice of a volatile variable is
   --  also volatile.

   procedure Kill_Dead_Code (N : Node_Id; Warn : Boolean := False);
   --  N represents a node for a section of code that is known to be dead. Any
   --  exception handler references and warning messages relating to this code
   --  are removed. If Warn is True, a warning will be output at the start of N
   --  indicating the deletion of the code. Note that the tree for the deleted
   --  code is left intact so that e.g. cross-reference data is still valid.

   procedure Kill_Dead_Code (L : List_Id; Warn : Boolean := False);
   --  Like the above procedure, but applies to every element in the given
   --  list. If Warn is True, a warning will be output at the start of N
   --  indicating the deletion of the code.

   function Known_Non_Negative (Opnd : Node_Id) return Boolean;
   --  Given a node for a subexpression, determines if it represents a value
   --  that cannot possibly be negative, and if so returns True. A value of
   --  False means that it is not known if the value is positive or negative.

   function Make_Invariant_Call (Expr : Node_Id) return Node_Id;
   --  Generate a call to the Invariant_Procedure associated with the type of
   --  expression Expr. Expr is passed as an actual parameter in the call.

   function Make_Predicate_Call
     (Typ  : Entity_Id;
      Expr : Node_Id;
      Mem  : Boolean := False) return Node_Id;
   --  Typ is a type with Predicate_Function set. This routine builds a call to
   --  this function passing Expr as the argument, and returns it unanalyzed.
   --  If Mem is set True, this is the special call for the membership case,
   --  and the function called is the Predicate_Function_M if present.

   function Make_Predicate_Check
     (Typ  : Entity_Id;
      Expr : Node_Id) return Node_Id;
   --  Typ is a type with Predicate_Function set. This routine builds a Check
   --  pragma whose first argument is Predicate, and the second argument is
   --  a call to the predicate function of Typ with Expr as the argument. If
   --  Predicate_Check is suppressed then a null statement is returned instead.

   function Make_Subtype_From_Expr
     (E          : Node_Id;
      Unc_Typ    : Entity_Id;
      Related_Id : Entity_Id := Empty) return Node_Id;
   --  Returns a subtype indication corresponding to the actual type of an
   --  expression E. Unc_Typ is an unconstrained array or record, or a class-
   --  wide type. Set Related_Id to request an external name for the subtype
   --  rather than an internal temporary.

   procedure Map_Types (Parent_Type : Entity_Id; Derived_Type : Entity_Id);
   --  Establish the following mapping between the attributes of tagged parent
   --  type Parent_Type and tagged derived type Derived_Type.
   --
   --    * Map each discriminant of Parent_Type to ether the corresponding
   --      discriminant of Derived_Type or come constraint.

   --    * Map each primitive operation of Parent_Type to the corresponding
   --      primitive of Derived_Type.
   --
   --  The mapping Parent_Type -> Derived_Type is also added to the table in
   --  order to prevent subsequent attempts of the same mapping.

   function Matching_Standard_Type (Typ : Entity_Id) return Entity_Id;
   --  Given a scalar subtype Typ, returns a matching type in standard that
   --  has the same object size value. For example, a 16 bit signed type will
   --  typically return Standard_Short_Integer. For fixed-point types, this
   --  will return integer types of the corresponding size.

   function May_Generate_Large_Temp (Typ : Entity_Id) return Boolean;
   --  Determines if the given type, Typ, may require a large temporary of the
   --  kind that causes back-end trouble if stack checking is enabled. The
   --  result is True only the size of the type is known at compile time and
   --  large, where large is defined heuristically by the body of this routine.
   --  The purpose of this routine is to help avoid generating troublesome
   --  temporaries that interfere with stack checking mechanism. Note that the
   --  caller has to check whether stack checking is actually enabled in order
   --  to guide the expansion (typically of a function call).

   function Needs_Conditional_Null_Excluding_Check
     (Typ : Entity_Id) return Boolean;
   --  Check if a type meets certain properties that require it to have a
   --  conditional null-excluding check within its Init_Proc.

   function Needs_Constant_Address
     (Decl : Node_Id;
      Typ  : Entity_Id) return Boolean;
   --  Check whether the expression in an address clause is restricted to
   --  consist of constants, when the object has a nontrivial initialization
   --  or is controlled.

   function Needs_Finalization (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ is controlled and this requires finalization
   --  actions.

   function Non_Limited_Designated_Type (T : Entity_Id) return Entity_Id;
   --  An anonymous access type may designate a limited view. Check whether
   --  non-limited view is available during expansion, to examine components
   --  or other characteristics of the full type.

   function OK_To_Do_Constant_Replacement (E : Entity_Id) return Boolean;
   --  This function is used when testing whether or not to replace a reference
   --  to entity E by a known constant value. Such replacement must be done
   --  only in a scope known to be safe for such replacements. In particular,
   --  if we are within a subprogram and the entity E is declared outside the
   --  subprogram then we cannot do the replacement, since we do not attempt to
   --  trace subprogram call flow. It is also unsafe to replace statically
   --  allocated values (since they can be modified outside the scope), and we
   --  also inhibit replacement of Volatile or aliased objects since their
   --  address might be captured in a way we do not detect. A value of True is
   --  returned only if the replacement is safe.

   function Possible_Bit_Aligned_Component (N : Node_Id) return Boolean;
   --  This function is used during processing the assignment of a record or
   --  indexed component. The argument N is either the left hand or right hand
   --  side of an assignment, and this function determines if there is a record
   --  component reference where the record may be bit aligned in a manner that
   --  causes trouble for the back end (see Component_May_Be_Bit_Aligned for
   --  further details).

   function Power_Of_Two (N : Node_Id) return Nat;
   --  Determines if N is a known at compile time value which  is of the form
   --  2**K, where K is in the range 1 .. M, where the Esize of N is 2**(M+1).
   --  If so, returns the value K, otherwise returns zero. The caller checks
   --  that N is of an integer type.

   procedure Process_Statements_For_Controlled_Objects (N : Node_Id);
   --  N is a node which contains a non-handled statement list. Inspect the
   --  statements looking for declarations of controlled objects. If at least
   --  one such object is found, wrap the statement list in a block.

   function Remove_Init_Call
     (Var        : Entity_Id;
      Rep_Clause : Node_Id) return Node_Id;
   --  Look for init_proc call or aggregate initialization statements for
   --  variable Var, either among declarations between that of Var and a
   --  subsequent Rep_Clause applying to Var, or in the list of freeze actions
   --  associated with Var, and if found, remove and return that call node.

   procedure Remove_Side_Effects
     (Exp                : Node_Id;
      Name_Req           : Boolean   := False;
      Renaming_Req       : Boolean   := False;
      Variable_Ref       : Boolean   := False;
      Related_Id         : Entity_Id := Empty;
      Is_Low_Bound       : Boolean   := False;
      Is_High_Bound      : Boolean   := False;
      Check_Side_Effects : Boolean   := True);
   --  Given the node for a subexpression, this function replaces the node if
   --  necessary by an equivalent subexpression that is guaranteed to be side
   --  effect free. This is done by extracting any actions that could cause
   --  side effects, and inserting them using Insert_Actions into the tree
   --  to which Exp is attached. Exp must be analyzed and resolved before the
   --  call and is analyzed and resolved on return. Name_Req may only be set to
   --  True if Exp has the form of a name, and the effect is to guarantee that
   --  any replacement maintains the form of name. If Renaming_Req is set to
   --  True, the routine produces an object renaming reclaration capturing the
   --  expression. If Variable_Ref is set to True, a variable is considered as
   --  side effect (used in implementing Force_Evaluation). Note: after call to
   --  Remove_Side_Effects, it is safe to call New_Copy_Tree to obtain a copy
   --  of the resulting expression. If Check_Side_Effects is set to True then
   --  no action is performed if Exp is known to be side-effect free.
   --
   --  Related_Id denotes the entity of the context where Expr appears. Flags
   --  Is_Low_Bound and Is_High_Bound specify whether the expression to check
   --  is the low or the high bound of a range. These three optional arguments
   --  signal Remove_Side_Effects to create an external symbol of the form
   --  Chars (Related_Id)_FIRST/_LAST. If Related_Id is set, then exactly one
   --  of the Is_xxx_Bound flags must be set. For use of these parameters see
   --  the warning in the body of Sem_Ch3.Process_Range_Expr_In_Decl.
   --
   --  The side effects are captured using one of the following methods:
   --
   --    1) a constant initialized with the value of the subexpression
   --    2) a renaming of the subexpression
   --    3) a reference to the subexpression
   --
   --  For elementary types, methods 1) and 2) are used; for composite types,
   --  methods 2) and 3) are used. The renaming (method 2) is used only when
   --  the subexpression denotes a name, so that it can be elaborated by gigi
   --  without evaluating the subexpression.
   --
   --  Historical note: the reference (method 3) used to be the common fallback
   --  method but it gives rise to aliasing issues if the subexpression denotes
   --  a name that is not aliased, since it is equivalent to taking the address
   --  in this case. The renaming (method 2) used to be applied to any objects
   --  in the RM sense, that is to say to the cases where a renaming is legal
   --  in Ada. But for some of these cases, most notably functions calls, the
   --  renaming cannot be elaborated without evaluating the subexpression, so
   --  gigi would resort to method 1) or 3) under the hood for them.

   procedure Replace_References
     (Expr      : Node_Id;
      Par_Typ   : Entity_Id;
      Deriv_Typ : Entity_Id;
      Par_Obj   : Entity_Id := Empty;
      Deriv_Obj : Entity_Id := Empty);
   --  Expr denotes an arbitrary expression. Par_Typ is a tagged parent type
   --  in a type hierarchy. Deriv_Typ is a tagged type derived from Par_Typ
   --  with optional ancestors in between. Par_Obj is a formal parameter
   --  which emulates the current instance of Par_Typ. Deriv_Obj is a formal
   --  parameter which emulates the current instance of Deriv_Typ. Perform the
   --  following substitutions in Expr:
   --
   --    * Replace a reference to Par_Obj with a reference to Deriv_Obj
   --
   --    * Replace a reference to a discriminant of Par_Typ with a suitable
   --      value from the point of view of Deriv_Typ.
   --
   --    * Replace a call to an overridden primitive of Par_Typ with a call to
   --      an overriding primitive of Deriv_Typ.
   --
   --    * Replace a call to an inherited primitive of Par_Type with a call to
   --      the internally-generated inherited primitive of Deriv_Typ.

   procedure Replace_Type_References
     (Expr   : Node_Id;
      Typ    : Entity_Id;
      Obj_Id : Entity_Id);
   --  Substitute all references of the current instance of type Typ with
   --  references to formal parameter Obj_Id within expression Expr.

   function Represented_As_Scalar (T : Entity_Id) return Boolean;
   --  Returns True iff the implementation of this type in code generation
   --  terms is scalar. This is true for scalars in the Ada sense, and for
   --  packed arrays which are represented by a scalar (modular) type.

   function Requires_Cleanup_Actions
     (N         : Node_Id;
      Lib_Level : Boolean) return Boolean;
   --  Given a node N, determine whether its declarative and/or statement list
   --  contains one of the following:
   --
   --    1) controlled objects
   --    2) library-level tagged types
   --
   --  These cases require special actions on scope exit. The flag Lib_Level
   --  is set True if the construct is at library level, and False otherwise.

   function Safe_Unchecked_Type_Conversion (Exp : Node_Id) return Boolean;
   --  Given the node for an N_Unchecked_Type_Conversion, return True if this
   --  is an unchecked conversion that Gigi can handle directly. Otherwise
   --  return False if it is one for which the front end must provide a
   --  temporary. Note that the node need not be analyzed, and thus the Etype
   --  field may not be set, but in that case it must be the case that the
   --  Subtype_Mark field of the node is set/analyzed.

   procedure Set_Current_Value_Condition (Cnode : Node_Id);
   --  Cnode is N_If_Statement, N_Elsif_Part, or N_Iteration_Scheme (the latter
   --  when a WHILE condition is present). This call checks whether Condition
   --  (Cnode) has embedded expressions of a form that should result in setting
   --  the Current_Value field of one or more entities, and if so sets these
   --  fields to point to Cnode.

   procedure Set_Elaboration_Flag (N : Node_Id; Spec_Id : Entity_Id);
   --  N is the node for a subprogram or generic body, and Spec_Id is the
   --  entity for the corresponding spec. If an elaboration entity is defined,
   --  then this procedure generates an assignment statement to set it True,
   --  immediately after the body is elaborated. However, no assignment is
   --  generated in the case of library level procedures, since the setting of
   --  the flag in this case is generated in the binder. We do that so that we
   --  can detect cases where this is the only elaboration action that is
   --  required.

   procedure Set_Renamed_Subprogram (N : Node_Id; E : Entity_Id);
   --  N is an node which is an entity name that represents the name of a
   --  renamed subprogram. The node is rewritten to be an identifier that
   --  refers directly to the renamed subprogram, given by entity E.

   function Side_Effect_Free
     (N            : Node_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False) return Boolean;
   --  Determines if the tree N represents an expression that is known not
   --  to have side effects. If this function returns True, then for example
   --  a call to Remove_Side_Effects has no effect.
   --
   --  Name_Req controls the handling of volatile variable references. If
   --  Name_Req is False (the normal case), then volatile references are
   --  considered to be side effects. If Name_Req is True, then volatility
   --  of variables is ignored.
   --
   --  If Variable_Ref is True, then all variable references are considered to
   --  be side effects (regardless of volatility or the setting of Name_Req).

   function Side_Effect_Free
     (L            : List_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False) return Boolean;
   --  Determines if all elements of the list L are side-effect free. Name_Req
   --  and Variable_Ref are as described above.

   procedure Silly_Boolean_Array_Not_Test (N : Node_Id; T : Entity_Id);
   --  N is the node for a boolean array NOT operation, and T is the type of
   --  the array. This routine deals with the silly case where the subtype of
   --  the boolean array is False..False or True..True, where it is required
   --  that a Constraint_Error exception be raised (RM 4.5.6(6)).

   procedure Silly_Boolean_Array_Xor_Test (N : Node_Id; T : Entity_Id);
   --  N is the node for a boolean array XOR operation, and T is the type of
   --  the array. This routine deals with the silly case where the subtype of
   --  the boolean array is True..True, where a raise of a Constraint_Error
   --  exception is required (RM 4.5.6(6)).

   function Target_Has_Fixed_Ops
     (Left_Typ   : Entity_Id;
      Right_Typ  : Entity_Id;
      Result_Typ : Entity_Id) return Boolean;
   --  Returns True if and only if the target machine has direct support
   --  for fixed-by-fixed multiplications and divisions for the given
   --  operand and result types. This is called in package Exp_Fixd to
   --  determine whether to expand such operations.

   function Type_May_Have_Bit_Aligned_Components
     (Typ : Entity_Id) return Boolean;
   --  Determines if Typ is a composite type that has within it (looking down
   --  recursively at any subcomponents), a record type which has component
   --  that may be bit aligned (see Possible_Bit_Aligned_Component). The result
   --  is conservative, in that a result of False is decisive. A result of True
   --  means that such a component may or may not be present.

   procedure Update_Primitives_Mapping
     (Inher_Id : Entity_Id;
      Subp_Id  : Entity_Id);
   --  Map primitive operations of the parent type to the corresponding
   --  operations of the descendant. Note that the descendant type may not be
   --  frozen yet, so we cannot use the dispatch table directly. This is called
   --  when elaborating a contract for a subprogram, and when freezing a type
   --  extension to verify legality rules on inherited conditions.

   function Within_Case_Or_If_Expression (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N is within a case or an if expression

   function Within_Internal_Subprogram return Boolean;
   --  Indicates that some expansion is taking place within the body of a
   --  predefined primitive operation. Some expansion activity (e.g. predicate
   --  checks) is disabled in such. Because we want to detect invalid uses
   --  of function calls within predicates (which lead to infinite recursion)
   --  predicate functions themselves are not considered internal here.

private
   pragma Inline (Duplicate_Subexpr);
   pragma Inline (Force_Evaluation);
   pragma Inline (Is_Library_Level_Tagged_Type);
end Exp_Util;
