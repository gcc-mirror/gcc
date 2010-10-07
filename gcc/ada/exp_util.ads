------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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
   --  implement because of short-circuits (among other things).???

   procedure Insert_Library_Level_Action (N : Node_Id);
   --  This procedure inserts and analyzes the node N as an action at the
   --  library level for the current unit (i.e. it is attached to the
   --  Actions field of the N_Compilation_Aux node for the main unit).

   procedure Insert_Library_Level_Actions (L : List_Id);
   --  Similar, but inserts a list of actions

   -----------------------
   -- Other Subprograms --
   -----------------------

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

   function Build_Runtime_Call (Loc : Source_Ptr; RE : RE_Id) return Node_Id;
   --  Build an N_Procedure_Call_Statement calling the given runtime entity.
   --  The call has no parameters. The first argument provides the location
   --  information for the tree and for error messages. The call node is not
   --  analyzed on return, the caller is responsible for analyzing it.

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
     (Exp      : Node_Id;
      Name_Req : Boolean := False) return Node_Id;
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
   --  attached to the tree. The Name_Req flag is set to ensure that the result
   --  is suitable for use in a context requiring name (e.g. the prefix of an
   --  attribute reference).
   --
   --  Note that if there are any run time checks in Exp, these same checks
   --  will be duplicated in the returned duplicated expression. The two
   --  following functions allow this behavior to be modified.

   function Duplicate_Subexpr_No_Checks
     (Exp      : Node_Id;
      Name_Req : Boolean := False) return Node_Id;
   --  Identical in effect to Duplicate_Subexpr, except that Remove_Checks
   --  is called on the result, so that the duplicated expression does not
   --  include checks. This is appropriate for use when Exp, the original
   --  expression is unconditionally elaborated before the duplicated
   --  expression, so that there is no need to repeat any checks.

   function Duplicate_Subexpr_Move_Checks
     (Exp      : Node_Id;
      Name_Req : Boolean := False) return Node_Id;
   --  Identical in effect to Duplicate_Subexpr, except that Remove_Checks is
   --  called on Exp after the duplication is complete, so that the original
   --  expression does not include checks. In this case the result returned
   --  (the duplicated expression) will retain the original checks. This is
   --  appropriate for use when the duplicated expression is sure to be
   --  elaborated before the original expression Exp, so that there is no need
   --  to repeat the checks.

   procedure Ensure_Defined (Typ : Entity_Id; N : Node_Id);
   --  This procedure ensures that type referenced by Typ is defined. For the
   --  case of a type other than an Itype, nothing needs to be done, since
   --  all such types have declaration nodes. For Itypes, an N_Itype_Reference
   --  node is generated and inserted at the given node N. This is typically
   --  used to ensure that an Itype is properly defined outside a conditional
   --  construct when it is referenced in more than one branch.

   function Entry_Names_OK return Boolean;
   --  Determine whether it is appropriate to dynamically allocate strings
   --  which represent entry [family member] names. These strings are created
   --  by the compiler and used by GDB.

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

   procedure Expand_Subtype_From_Expr
     (N             : Node_Id;
      Unc_Type      : Entity_Id;
      Subtype_Indic : Node_Id;
      Exp           : Node_Id);
   --  Build a constrained subtype from the initial value in object
   --  declarations and/or allocations when the type is indefinite (including
   --  class-wide).

   function Find_Init_Call
     (Var        : Entity_Id;
      Rep_Clause : Node_Id) return Node_Id;
   --  Look for init_proc call for variable Var, either among declarations
   --  between that of Var and a subsequent Rep_Clause applying to Var, or
   --  in the list of freeze actions associated with Var, and if found, return
   --  that call node.

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
   --  Find the first primitive operation of type T whose name is 'Name'.
   --  This function allows the use of a primitive operation which is not
   --  directly visible. If T is a class wide type, then the reference is
   --  to an operation of the corresponding root type. Raises Program_Error
   --  exception if no primitive operation is found. This is normally an
   --  internal error, but in some cases is an expected consequence of
   --  illegalities elsewhere.

   function Find_Prim_Op
     (T    : Entity_Id;
      Name : TSS_Name_Type) return Entity_Id;
   --  Find the first primitive operation of type T whose name has the form
   --  indicated by the name parameter (i.e. is a type support subprogram
   --  with the indicated suffix). This function allows use of a primitive
   --  operation which is not directly visible. If T is a class wide type,
   --  then the reference is to an operation of the corresponding root type.
   --  Raises Program_Error exception if no primitive operation is found.
   --  This is normally an internal error, but in some cases is an expected
   --  consequence of illegalities elsewhere.

   function Find_Protection_Object (Scop : Entity_Id) return Entity_Id;
   --  Traverse the scope stack starting from Scop and look for an entry,
   --  entry family, or a subprogram that has a Protection_Object and return
   --  it. Raises Program_Error if no such entity is found since the context
   --  in which this routine is invoked should always have a protection
   --  object.

   procedure Force_Evaluation
     (Exp      : Node_Id;
      Name_Req : Boolean := False);
   --  Force the evaluation of the expression right away. Similar behavior
   --  to Remove_Side_Effects when Variable_Ref is set to TRUE. That is to
   --  say, it removes the side-effects and captures the values of the
   --  variables. Remove_Side_Effects guarantees that multiple evaluations
   --  of the same expression won't generate multiple side effects, whereas
   --  Force_Evaluation further guarantees that all evaluations will yield
   --  the same result.

   function Fully_Qualified_Name_String (E : Entity_Id) return String_Id;
   --  Generates the string literal corresponding to the fully qualified name
   --  of entity E with an ASCII.NUL appended at the end of the name.

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

   function Has_Controlled_Coextensions (Typ : Entity_Id) return Boolean;
   --  Determine whether a record type has anonymous access discriminants with
   --  a controlled designated type.

   function Has_Following_Address_Clause (D : Node_Id) return Boolean;
   --  D is the node for an object declaration. This function searches the
   --  current declarative part to look for an address clause for the object
   --  being declared, and returns True if one is found.

   function Homonym_Number (Subp : Entity_Id) return Nat;
   --  Here subp is the entity for a subprogram. This routine returns the
   --  homonym number used to disambiguate overloaded subprograms in the same
   --  scope (the number is used as part of constructed names to make sure that
   --  they are unique). The number is the ordinal position on the Homonym
   --  chain, counting only entries in the current scope. If an entity is not
   --  overloaded, the returned number will be one.

   function Inside_Init_Proc return Boolean;
   --  Returns True if current scope is within an init proc

   function In_Unconditional_Context (Node : Node_Id) return Boolean;
   --  Node is the node for a statement or a component of a statement. This
   --  function determines if the statement appears in a context that is
   --  unconditionally executed, i.e. it is not within a loop or a conditional
   --  or a case statement etc.

   function Is_All_Null_Statements (L : List_Id) return Boolean;
   --  Return True if all the items of the list are N_Null_Statement nodes.
   --  False otherwise. True for an empty list. It is an error to call this
   --  routine with No_List as the argument.

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

   function Is_Possibly_Unaligned_Slice (N : Node_Id) return Boolean;
   --  Determine whether the node P is a slice of an array where the slice
   --  result may cause alignment problems because it has an alignment that
   --  is not compatible with the type. Return True if so.

   function Is_Possibly_Unaligned_Object (N : Node_Id) return Boolean;
   --  Node N is an object reference. This function returns True if it is
   --  possible that the object may not be aligned according to the normal
   --  default alignment requirement for its type (e.g. if it appears in a
   --  packed record, or as part of a component that has a component clause.)

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

   function Is_Untagged_Derivation (T : Entity_Id) return Boolean;
   --  Returns true if type T is not tagged and is a derived type,
   --  or is a private type whose completion is such a type.

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

   function Known_Non_Null (N : Node_Id) return Boolean;
   --  Given a node N for a subexpression of an access type, determines if
   --  this subexpression yields a value that is known at compile time to
   --  be non-null and returns True if so. Returns False otherwise. It is
   --  an error to call this function if N is not of an access type.

   function Known_Null (N : Node_Id) return Boolean;
   --  Given a node N for a subexpression of an access type, determines if this
   --  subexpression yields a value that is known at compile time to be null
   --  and returns True if so. Returns False otherwise. It is an error to call
   --  this function if N is not of an access type.

   function Make_Subtype_From_Expr
     (E       : Node_Id;
      Unc_Typ : Entity_Id) return Node_Id;
   --  Returns a subtype indication corresponding to the actual type of an
   --  expression E. Unc_Typ is an unconstrained array or record, or
   --  a classwide type.

   function May_Generate_Large_Temp (Typ : Entity_Id) return Boolean;
   --  Determines if the given type, Typ, may require a large temporary of the
   --  kind that causes back-end trouble if stack checking is enabled. The
   --  result is True only the size of the type is known at compile time and
   --  large, where large is defined heuristically by the body of this routine.
   --  The purpose of this routine is to help avoid generating troublesome
   --  temporaries that interfere with stack checking mechanism. Note that the
   --  caller has to check whether stack checking is actually enabled in order
   --  to guide the expansion (typically of a function call).

   function Needs_Constant_Address
     (Decl : Node_Id;
      Typ  : Entity_Id) return Boolean;
   --  Check whether the expression in an address clause is restricted to
   --  consist of constants, when the object has a non-trivial initialization
   --  or is controlled.

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

   procedure Remove_Side_Effects
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False);
   --  Given the node for a subexpression, this function replaces the node if
   --  necessary by an equivalent subexpression that is guaranteed to be side
   --  effect free. This is done by extracting any actions that could cause
   --  side effects, and inserting them using Insert_Actions into the tree to
   --  which Exp is attached. Exp must be analyzed and resolved before the call
   --  and is analyzed and resolved on return. The Name_Req may only be set to
   --  True if Exp has the form of a name, and the effect is to guarantee that
   --  any replacement maintains the form of name. If Variable_Ref is set to
   --  TRUE, a variable is considered as side effect (used in implementing
   --  Force_Evaluation). Note: after call to Remove_Side_Effects, it is safe
   --  to call New_Copy_Tree to obtain a copy of the resulting expression.

   function Represented_As_Scalar (T : Entity_Id) return Boolean;
   --  Returns True iff the implementation of this type in code generation
   --  terms is scalar. This is true for scalars in the Ada sense, and for
   --  packed arrays which are represented by a scalar (modular) type.

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

   procedure Wrap_Cleanup_Procedure (N : Node_Id);
   --  Given an N_Subprogram_Body node, this procedure adds an Abort_Defer call
   --  at the start of the statement sequence, and an Abort_Undefer call at the
   --  end of the statement sequence. All cleanup routines (i.e. those that are
   --  called from "at end" handlers) must defer abort on entry and undefer
   --  abort on exit. Note that it is assumed that the code for the procedure
   --  does not contain any return statements which would allow the flow of
   --  control to escape doing the undefer call.

private
   pragma Inline (Duplicate_Subexpr);
   pragma Inline (Force_Evaluation);
   pragma Inline (Is_Library_Level_Tagged_Type);
end Exp_Util;
