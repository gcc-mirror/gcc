------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  Package containing utility procedures used throughout the expander

with Snames;  use Snames;
with Rtsfind; use Rtsfind;
with Types;   use Types;

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
   --    just before the associated declaration.

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
   --    expansion of the N_Conditional_Expression node rewrites the node so
   --    that the actions can be normally positioned.

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
   --  implement because of short-ciruits (among other things).???

   procedure Insert_Library_Level_Action (N : Node_Id);
   --  This procedure inserts and analyzes the node N as an action at the
   --  library level for the current unit (i.e. it is attached to the
   --  Actions field of the N_Compilation_Aux node for the main unit).

   procedure Insert_Library_Level_Actions (L : List_Id);
   --  Similar, but inserts a list of actions.

   -----------------------
   -- Other Subprograms --
   -----------------------

   procedure Adjust_Condition (N : Node_Id);
   --  The node N is an expression whose root-type is Boolean, and which
   --  represents a boolean value used as a condition (i.e. a True/False
   --  value). This routine handles the case of C and Fortran convention
   --  boolean types, which have zero/non-zero semantics rather than the
   --  normal 0/1 semantics, and also the case of an enumeration rep
   --  clause that specifies a non-standard representation. On return,
   --  node N always has the type Standard.Boolean, with a value that
   --  is a standard Boolean values of 0/1 for False/True. This procedure
   --  is used in two situations. First, the processing for a condition
   --  field always calls Adjust_Condition, so that the boolean value
   --  presented to the backend is a standard value. Second, for the
   --  code for boolean operations such as AND, Adjust_Condition is
   --  called on both operands, and then the operation is done in the
   --  domain of Standard_Boolean, then Adjust_Result_Type is called
   --  on the result to possibly reset the original type. This procedure
   --  also takes care of validity checking if Validity_Checks = Tests.

   procedure Adjust_Result_Type (N : Node_Id; T : Entity_Id);
   --  The processing of boolean operations like AND uses the procedure
   --  Adjust_Condition so that it can operate on Standard.Boolean, which
   --  is the only boolean type on which the backend needs to be able to
   --  implement such operators. This means that the result is also of
   --  type Standard.Boolean. In general the type must be reset back to
   --  the original type to get proper semantics, and that is the purpose
   --  of this procedure. N is the node (of type Standard.Boolean), and
   --  T is the desired type. As an optimization, this procedure leaves
   --  the type as Standard.Boolean in contexts where this is permissible
   --  (in particular for Condition fields, and for operands of other
   --  logical operations higher up the tree). The call to this procedure
   --  is completely ignored if the argument N is not of type Boolean.

   procedure Append_Freeze_Action (T : Entity_Id; N : Node_Id);
   --  Add a new freeze action for the given type. The freeze action is
   --  attached to the freeze node for the type. Actions will be elaborated
   --  in the order in which they are added. Note that the added node is not
   --  analyzed. The analyze call is found in Sem_Ch13.Expand_N_Freeze_Entity.

   procedure Append_Freeze_Actions (T : Entity_Id; L : List_Id);
   --  Adds the given list of freeze actions (declarations or statements)
   --  for the given type. The freeze actions are attached to the freeze
   --  node for the type. Actions will be elaborated in the order in which
   --  they are added, and the actions within the list will be elaborated in
   --  list order. Note that the added nodes are not analyzed. The analyze
   --  call is found in Sem_Ch13.Expand_N_Freeze_Entity.

   function Build_Runtime_Call (Loc : Source_Ptr; RE : RE_Id) return Node_Id;
   --  Build an N_Procedure_Call_Statement calling the given runtime entity.
   --  The call has no parameters. The first argument provides the location
   --  information for the tree and for error messages. The call node is not
   --  analyzed on return, the caller is responsible for analyzing it.

   function Build_Task_Image_Decls
     (Loc    : Source_Ptr;
      Id_Ref : Node_Id;
      A_Type : Entity_Id)
      return   List_Id;
   --  Build declaration for a variable that holds an identifying string
   --  to be used as a task name. Id_Ref is an identifier if the task is
   --  a variable, and a selected or indexed component if the task is a
   --  component of an object. If it is an indexed component, A_Type is
   --  the corresponding array type. Its index types are used to build the
   --  string as an image of the index values. For composite types, the
   --  result includes two declarations: one for a generated function that
   --  computes the image without using concatenation, and one for the
   --  variable that holds the result.

   procedure Convert_To_Actual_Subtype (Exp : Node_Id);
   --  The Etype of an expression is the nominal type of the expression,
   --  not the actual subtype. Often these are the same, but not always.
   --  For example, a reference to a formal of unconstrained type has the
   --  unconstrained type as its Etype, but the actual subtype is obtained
   --  by applying the actual bounds. This routine is given an expression,
   --  Exp, and (if necessary), replaces it using Rewrite, with a conversion
   --  to the actual subtype, building the actual subtype if necessary. If
   --  the expression is already of the requested type, then it is unchanged.

   function Current_Sem_Unit_Declarations return List_Id;
   --  Return the a place where it is fine to insert declarations for the
   --  current semantic unit. If the unit is a package body, return the
   --  visible declarations of the corresponding spec. For RCI stubs, this
   --  is necessary because the point at which they are generated may not
   --  be the earliest point at which they are used.

   function Duplicate_Subexpr
     (Exp      : Node_Id;
      Name_Req : Boolean := False)
      return     Node_Id;
   --  Given the node for a subexpression, this function makes a logical
   --  copy of the subexpression, and returns it. This is intended for use
   --  when the expansion of an expression needs to repeat part of it. For
   --  example, replacing a**2 by a*a requires two references to a which
   --  may be a complex subexpression. Duplicate_Subexpression guarantees
   --  not to duplicate side effects. If necessary, it generates actions
   --  to save the expression value in a temporary, inserting these actions
   --  into the tree using Insert_Actions with Exp as the insertion location.
   --  The original expression and the returned result then become references
   --  to this saved value. Exp must be analyzed on entry. On return, Exp
   --  is analyzed, but the caller is responsible for analyzing the returned
   --  copy after it is attached to the tree. The Name_Req flag is set to
   --  ensure that the result is suitable for use in a context requiring a
   --  name (e.g. the prefix of an attribute reference).

   procedure Ensure_Defined (Typ : Entity_Id; N : Node_Id);
   --  This procedure ensures that type referenced by Typ is defined. For the
   --  case of a type other than an Itype, nothing needs to be done, since
   --  all such types have declaration nodes. For Itypes, an N_Itype_Reference
   --  node is generated and inserted at the given node N. This is typically
   --  used to ensure that an Itype is properly defined outside a conditional
   --  construct when it is referenced in more than one branch.

   procedure Evolve_And_Then (Cond : in out Node_Id; Cond1 : Node_Id);
   --  Rewrites Cond with the expression: Cond and then Cond1. If Cond is
   --  Empty, then simply returns Cond1 (this allows the use of Empty to
   --  initialize a series of checks evolved by this routine, with a final
   --  result of Empty indicating that no checks were required). The Sloc
   --  field of the constructed N_And_Then node is copied from Cond1.

   procedure Evolve_Or_Else (Cond : in out Node_Id; Cond1 : Node_Id);
   --  Rewrites Cond with the expression: Cond or else Cond1. If Cond is
   --  Empty, then simply returns Cond1 (this allows the use of Empty to
   --  initialize a series of checks evolved by this routine, with a final
   --  result of Empty indicating that no checks were required). The Sloc
   --  field of the constructed N_And_Then node is copied from Cond1.

   procedure Expand_Subtype_From_Expr
     (N             : Node_Id;
      Unc_Type      : Entity_Id;
      Subtype_Indic : Node_Id;
      Exp           : Node_Id);
   --  Build a constrained subtype from the initial value in object
   --  declarations and/or allocations when the type is indefinite (including
   --  class-wide).

   function Find_Prim_Op (T : Entity_Id; Name : Name_Id) return Entity_Id;
   --  Find the first primitive operation of type T whose name is 'Name'.
   --  this function allows the use of a primitive operation which is not
   --  directly visible

   procedure Force_Evaluation
     (Exp      : Node_Id;
      Name_Req : Boolean := False);
   --  Force the evaluation of the expression right away. Similar behavior
   --  to Remove_Side_Effects when Variable_Ref is set to TRUE. That is to
   --  say, it removes the side-effects and capture the values of the
   --  variables. Remove_Side_effects guarantees that multiple evaluations
   --  of the same expression won't generate multiple side effects, whereas
   --  Force_Evaluation further guarantees that all evaluations will yield
   --  the same result.

   procedure Generate_Poll_Call (N : Node_Id);
   --  If polling is active, then a call to the Poll routine is built,
   --  and then inserted before the given node N and analyzed.

   function Homonym_Number (Subp : Entity_Id) return Nat;
   --  Here subp is the entity for a subprogram. This routine returns the
   --  homonym number used to disambiguate overloaded subprograms in the
   --  same scope (the number is used as part of constructed names to make
   --  sure that they are unique). The number is the ordinal position on
   --  the Homonym chain, counting only entries in the curren scope. If
   --  an entity is not overloaded, the returned number will be one.

   function Inside_Init_Proc return Boolean;
   --  Returns True if current scope is within an Init_Proc

   function In_Unconditional_Context (Node : Node_Id) return Boolean;
   --  Node is the node for a statement or a component of a statement.
   --  This function deteermines if the statement appears in a context
   --  that is unconditionally executed, i.e. it is not within a loop
   --  or a conditional or a case statement etc.

   function Is_Ref_To_Bit_Packed_Array (P : Node_Id) return Boolean;
   --  Determine whether the node P is a reference to a bit packed
   --  array, i.e. whether the designated object is a component of
   --  a bit packed array, or a subcomponent of such a component.
   --  If so, then all subscripts in P are evaluated with a call
   --  to Force_Evaluation, and True is returned. Otherwise False
   --  is returned, and P is not affected.

   function Is_Ref_To_Bit_Packed_Slice (P : Node_Id) return Boolean;
   --  Determine whether the node P is a reference to a bit packed
   --  slice, i.e. whether the designated object is bit packed slice
   --  or a component of a bit packed slice. Return True if so.

   function Is_Renamed_Object (N : Node_Id) return Boolean;
   --  Returns True if the node N is a renamed object. An expression
   --  is considered to be a renamed object if either it is the Name
   --  of an object renaming declaration, or is the prefix of a name
   --  which is a renamed object. For example, in:
   --
   --     x : r renames a (1 .. 2) (1);
   --
   --  We consider that a (1 .. 2) is a renamed object since it is the
   --  prefix of the name in the renaming declaration.

   function Is_Untagged_Derivation (T : Entity_Id) return Boolean;
   --  Returns true if type T is not tagged and is a derived type,
   --  or is a private type whose completion is such a type.

   procedure Kill_Dead_Code (N : Node_Id);
   --  N represents a node for a section of code that is known to be
   --  dead. The node is deleted, and any exception handler references
   --  and warning messages relating to this code are removed.

   procedure Kill_Dead_Code (L : List_Id);
   --  Like the above procedure, but applies to every element in the given
   --  list. Each of the entries is removed from the list before killing it.

   function Known_Non_Negative (Opnd : Node_Id) return Boolean;
   --  Given a node for a subexpression, determines if it represents a value
   --  that cannot possibly be negative, and if so returns True. A value of
   --  False means that it is not known if the value is positive or negative.

   function Make_Subtype_From_Expr
     (E       : Node_Id;
      Unc_Typ : Entity_Id)
      return    Node_Id;
   --  Returns a subtype indication corresponding to the actual type of an
   --  expression E. Unc_Typ is an unconstrained array or record, or
   --  a classwide type.

   function May_Generate_Large_Temp (Typ : Entity_Id) return Boolean;
   --  Determines if the given type, Typ, may require a large temporary
   --  of the type that causes trouble if stack checking is enabled. The
   --  result is True only if stack checking is enabled and the size of
   --  the type is known at compile time and large, where large is defined
   --  hueristically by the body of this routine. The purpose of this
   --  routine is to help avoid generating troublesome temporaries that
   --  intefere with the stack checking mechanism.

   procedure Remove_Side_Effects
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False);
   --  Given the node for a subexpression, this function replaces the node
   --  if necessary by an equivalent subexpression that is guaranteed to be
   --  side effect free. This is done by extracting any actions that could
   --  cause side effects, and inserting them using Insert_Actions into the
   --  tree to which Exp is attached. Exp must be analayzed and resolved
   --  before the call and is analyzed and resolved on return. The Name_Req
   --  may only be set to True if Exp has the form of a name, and the
   --  effect is to guarantee that any replacement maintains the form of a
   --  name. If Variable_Ref is set to TRUE, a variable is considered as a
   --  side effect (used in implementing Force_Evaluation). Note: after a
   --  call to Remove_Side_Effects, it is safe to use a call to
   --  New_Copy_Tree to obtain a copy of the resulting expression.

   function Safe_Unchecked_Type_Conversion (Exp : Node_Id) return Boolean;
   --  Given the node for an N_Unchecked_Type_Conversion, return True
   --  if this is an unchecked conversion that Gigi can handle directly.
   --  Otherwise return False if it is one for which the front end must
   --  provide a temporary. Note that the node need not be analyzed, and
   --  thus the Etype field may not be set, but in that case it must be
   --  the case that the Subtype_Mark field of the node is set/analyzed.

   procedure Set_Elaboration_Flag (N : Node_Id; Spec_Id : Entity_Id);
   --  N is the node for a subprogram or generic body, and Spec_Id
   --  is the entity for the corresponding spec. If an elaboration
   --  entity is defined, then this procedure generates an assignment
   --  statement to set it True, immediately after the body is elaborated.
   --  However, no assignment is generated in the case of library level
   --  procedures, since the setting of the flag in this case is generated
   --  in the binder. We do that so that we can detect cases where this is
   --  the only elaboration action that is required.

   procedure Wrap_Cleanup_Procedure (N : Node_Id);
   --  Given an N_Subprogram_Body node, this procedure adds an Abort_Defer
   --  call at the start of the statement sequence, and an Abort_Undefer call
   --  at the end of the statement sequence. All cleanup routines (i.e. those
   --  that are called from "at end" handlers) must defer abort on entry and
   --  undefer abort on exit. Note that it is assumed that the code for the
   --  procedure does not contain any return statements which would allow the
   --  flow of control to escape doing the undefer call.

private
   pragma Inline (Force_Evaluation);
   pragma Inline (Duplicate_Subexpr);

end Exp_Util;
