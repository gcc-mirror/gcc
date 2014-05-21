------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
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

--  Package containing utility procedures used throughout the semantics

with Einfo;   use Einfo;
with Exp_Tss; use Exp_Tss;
with Namet;   use Namet;
with Snames;  use Snames;
with Types;   use Types;
with Uintp;   use Uintp;
with Urealp;  use Urealp;

package Sem_Util is

   function Abstract_Interface_List (Typ : Entity_Id) return List_Id;
   --  Given a type that implements interfaces look for its associated
   --  definition node and return its list of interfaces.

   procedure Add_Access_Type_To_Process (E : Entity_Id; A : Entity_Id);
   --  Add A to the list of access types to process when expanding the
   --  freeze node of E.

   procedure Add_Block_Identifier (N : Node_Id; Id : out Entity_Id);
   --  Given a block statement N, generate an internal E_Block label and make
   --  it the identifier of the block. Id denotes the generated entity. If the
   --  block already has an identifier, Id returns the entity of its label.

   procedure Add_Contract_Item (Prag : Node_Id; Id : Entity_Id);
   --  Add pragma Prag to the contract of an entry, a package [body], a
   --  subprogram [body] or variable denoted by Id. The following are valid
   --  pragmas:
   --    Abstract_States
   --    Async_Readers
   --    Async_Writers
   --    Contract_Cases
   --    Depends
   --    Effective_Reads
   --    Effective_Writes
   --    Global
   --    Initial_Condition
   --    Initializes
   --    Part_Of
   --    Postcondition
   --    Precondition
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Refined_States
   --    Test_Case

   procedure Add_Global_Declaration (N : Node_Id);
   --  These procedures adds a declaration N at the library level, to be
   --  elaborated before any other code in the unit. It is used for example
   --  for the entity that marks whether a unit has been elaborated. The
   --  declaration is added to the Declarations list of the Aux_Decls_Node
   --  for the current unit. The declarations are added in the current scope,
   --  so the caller should push a new scope as required before the call.

   function Address_Integer_Convert_OK (T1, T2 : Entity_Id) return Boolean;
   --  Given two types, returns True if we are in Allow_Integer_Address mode
   --  and one of the types is (a descendent of) System.Address (and this type
   --  is private), and the other type is any integer type.

   function Addressable (V : Uint) return Boolean;
   function Addressable (V : Int)  return Boolean;
   pragma Inline (Addressable);
   --  Returns True if the value of V is the word size of an addressable
   --  factor of the word size (typically 8, 16, 32 or 64).

   function Alignment_In_Bits (E : Entity_Id) return Uint;
   --  If the alignment of the type or object E is currently known to the
   --  compiler, then this function returns the alignment value in bits.
   --  Otherwise Uint_0 is returned, indicating that the alignment of the
   --  entity is not yet known to the compiler.

   procedure Append_Inherited_Subprogram (S : Entity_Id);
   --  If the parent of the operation is declared in the visible part of
   --  the current scope, the inherited operation is visible even though the
   --  derived type that inherits the operation may be completed in the private
   --  part of the current package.

   procedure Apply_Compile_Time_Constraint_Error
     (N      : Node_Id;
      Msg    : String;
      Reason : RT_Exception_Code;
      Ent    : Entity_Id  := Empty;
      Typ    : Entity_Id  := Empty;
      Loc    : Source_Ptr := No_Location;
      Rep    : Boolean    := True;
      Warn   : Boolean    := False);
   --  N is a subexpression which will raise constraint error when evaluated
   --  at runtime. Msg is a message that explains the reason for raising the
   --  exception. The last character is ? if the message is always a warning,
   --  even in Ada 95, and is not a ? if the message represents an illegality
   --  (because of violation of static expression rules) in Ada 95 (but not
   --  in Ada 83). Typically this routine posts all messages at the Sloc of
   --  node N. However, if Loc /= No_Location, Loc is the Sloc used to output
   --  the message. After posting the appropriate message, and if the flag
   --  Rep is set, this routine replaces the expression with an appropriate
   --  N_Raise_Constraint_Error node using the given Reason code. This node
   --  is then marked as being static if the original node is static, but
   --  sets the flag Raises_Constraint_Error, preventing further evaluation.
   --  The error message may contain a } or & insertion character. This
   --  normally references Etype (N), unless the Ent argument is given
   --  explicitly, in which case it is used instead. The type of the raise
   --  node that is built is normally Etype (N), but if the Typ parameter
   --  is present, this is used instead. Warn is normally False. If it is
   --  True then the message is treated as a warning even though it does
   --  not end with a ? (this is used when the caller wants to parameterize
   --  whether an error or warning is given).

   function Async_Readers_Enabled (Id : Entity_Id) return Boolean;
   --  Given the entity of an abstract state or a variable, determine whether
   --  Id is subject to external property Async_Readers and if it is, the
   --  related expression evaluates to True.

   function Async_Writers_Enabled (Id : Entity_Id) return Boolean;
   --  Given the entity of an abstract state or a variable, determine whether
   --  Id is subject to external property Async_Writers and if it is, the
   --  related expression evaluates to True.

   function Available_Full_View_Of_Component (T : Entity_Id) return Boolean;
   --  If at the point of declaration an array type has a private or limited
   --  component, several array operations are not avaiable on the type, and
   --  the array type is flagged accordingly. If in the immediate scope of
   --  the array type the component becomes non-private or non-limited, these
   --  operations become avaiable. This can happen if the scopes of both types
   --  are open, and the scope of the array is not outside the scope of the
   --  component.

   procedure Bad_Attribute
     (N    : Node_Id;
      Nam  : Name_Id;
      Warn : Boolean := False);
   --  Called when node N is expected to contain a valid attribute name, and
   --  Nam is found instead. If Warn is set True this is a warning, else this
   --  is an error.

   procedure Bad_Predicated_Subtype_Use
     (Msg            : String;
      N              : Node_Id;
      Typ            : Entity_Id;
      Suggest_Static : Boolean := False);
   --  This is called when Typ, a predicated subtype, is used in a context
   --  which does not allow the use of a predicated subtype. Msg is passed to
   --  Error_Msg_FE to output an appropriate message using N as the location,
   --  and Typ as the entity. The caller must set up any insertions other than
   --  the & for the type itself. Note that if Typ is a generic actual type,
   --  then the message will be output as a warning, and a raise Program_Error
   --  is inserted using Insert_Action with node N as the insertion point. Node
   --  N also supplies the source location for construction of the raise node.
   --  If Typ does not have any predicates, the call has no effect. Set flag
   --  Suggest_Static when the context warrants an advice on how to avoid the
   --  use error.

   function Bad_Unordered_Enumeration_Reference
     (N : Node_Id;
      T : Entity_Id) return Boolean;
   --  Node N contains a potentially dubious reference to type T, either an
   --  explicit comparison, or an explicit range. This function returns True
   --  if the type T is an enumeration type for which No pragma Order has been
   --  given, and the reference N is not in the same extended source unit as
   --  the declaration of T.

   function Build_Actual_Subtype
     (T : Entity_Id;
      N : Node_Or_Entity_Id) return Node_Id;
   --  Build an anonymous subtype for an entity or expression, using the
   --  bounds of the entity or the discriminants of the enclosing record.
   --  T is the type for which the actual subtype is required, and N is either
   --  a defining identifier, or any subexpression.

   function Build_Actual_Subtype_Of_Component
     (T : Entity_Id;
      N : Node_Id) return Node_Id;
   --  Determine whether a selected component has a type that depends on
   --  discriminants, and build actual subtype for it if so.

   function Build_Default_Subtype
     (T : Entity_Id;
      N : Node_Id) return Entity_Id;
   --  If T is an unconstrained type with defaulted discriminants, build a
   --  subtype constrained by the default values, insert the subtype
   --  declaration in the tree before N, and return the entity of that
   --  subtype. Otherwise, simply return T.

   function Build_Discriminal_Subtype_Of_Component
     (T : Entity_Id) return Node_Id;
   --  Determine whether a record component has a type that depends on
   --  discriminants, and build actual subtype for it if so.

   procedure Build_Elaboration_Entity (N : Node_Id; Spec_Id : Entity_Id);
   --  Given a compilation unit node N, allocate an elaboration counter for
   --  the compilation unit, and install it in the Elaboration_Entity field
   --  of Spec_Id, the entity for the compilation unit.

   procedure Build_Explicit_Dereference
     (Expr : Node_Id;
      Disc : Entity_Id);
   --  AI05-139: Names with implicit dereference. If the expression N is a
   --  reference type and the context imposes the corresponding designated
   --  type, convert N into N.Disc.all. Such expressions are always over-
   --  loaded with both interpretations, and the dereference interpretation
   --  carries the name of the reference discriminant.

   function Cannot_Raise_Constraint_Error (Expr : Node_Id) return Boolean;
   --  Returns True if the expression cannot possibly raise Constraint_Error.
   --  The response is conservative in the sense that a result of False does
   --  not necessarily mean that CE could be raised, but a response of True
   --  means that for sure CE cannot be raised.

   procedure Check_Dynamically_Tagged_Expression
     (Expr        : Node_Id;
      Typ         : Entity_Id;
      Related_Nod : Node_Id);
   --  Check wrong use of dynamically tagged expression

   procedure Check_Expression_Against_Static_Predicate
     (Expr : Node_Id;
      Typ  : Entity_Id);
   --  Determine whether an arbitrary expression satisfies the static predicate
   --  of a type. The routine does nothing if Expr is not known at compile time
   --  or Typ lacks a static predicate, otherwise it may emit a warning if the
   --  expression is prohibited by the predicate.

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id);
   --  Verify that the full declaration of type T has been seen. If not, place
   --  error message on node N. Used in object declarations, type conversions
   --  and qualified expressions.

   procedure Check_Function_Writable_Actuals (N : Node_Id);
   --  (Ada 2012): If the construct N has two or more direct constituents that
   --  are names or expressions whose evaluation may occur in an arbitrary
   --  order, at least one of which contains a function call with an in out or
   --  out parameter, then the construct is legal only if: for each name that
   --  is passed as a parameter of mode in out or out to some inner function
   --  call C2 (not including the construct N itself), there is no other name
   --  anywhere within a direct constituent of the construct C other than
   --  the one containing C2, that is known to refer to the same object (RM
   --  6.4.1(6.17/3)).

   procedure Check_Implicit_Dereference (Nam : Node_Id; Typ : Entity_Id);
   --  AI05-139-2: Accessors and iterators for containers. This procedure
   --  checks whether T is a reference type, and if so it adds an interprettion
   --  to Expr whose type is the designated type of the reference_discriminant.

   procedure Check_Internal_Protected_Use (N : Node_Id; Nam : Entity_Id);
   --  Within a protected function, the current object is a constant, and
   --  internal calls to a procedure or entry are illegal. Similarly, other
   --  uses of a protected procedure in a renaming or a generic instantiation
   --  in the context of a protected function are illegal (AI05-0225).

   procedure Check_Later_Vs_Basic_Declarations
     (Decls          : List_Id;
      During_Parsing : Boolean);
   --  If During_Parsing is True, check for misplacement of later vs basic
   --  declarations in Ada 83. If During_Parsing is False, and the SPARK
   --  restriction is set, do the same: although SPARK 95 removes the
   --  distinction between initial and later declarative items, the distinction
   --  remains in the Examiner (JB01-005). Note that the Examiner does not
   --  count package declarations in later declarative items.

   procedure Check_Nested_Access (Ent : Entity_Id);
   --  Check whether Ent denotes an entity declared in an uplevel scope, which
   --  is accessed inside a nested procedure, and set Has_Up_Level_Access flag
   --  accordingly. This is currently only enabled for VM_Target /= No_VM.

   procedure Check_No_Hidden_State (Id : Entity_Id);
   --  Determine whether object or state Id introduces a hidden state. If this
   --  is the case, emit an error.

   procedure Check_Potentially_Blocking_Operation (N : Node_Id);
   --  N is one of the statement forms that is a potentially blocking
   --  operation. If it appears within a protected action, emit warning.

   procedure Check_Result_And_Post_State
     (Prag        : Node_Id;
      Result_Seen : in out Boolean);
   --  Determine whether pragma Prag mentions attribute 'Result and whether
   --  the pragma contains an expression that evaluates differently in pre-
   --  and post-state. Prag is a [refined] postcondition or a contract-cases
   --  pragma. Result_Seen is set when the pragma mentions attribute 'Result.

   procedure Check_SPARK_Mode_In_Generic (N : Node_Id);
   --  Given a generic package [body] or a generic subprogram [body], inspect
   --  the aspect specifications (if any) and flag SPARK_Mode as illegal.

   procedure Check_Unprotected_Access
     (Context : Node_Id;
      Expr    : Node_Id);
   --  Check whether the expression is a pointer to a protected component,
   --  and the context is external to the protected operation, to warn against
   --  a possible unlocked access to data.

   procedure Check_VMS (Construct : Node_Id);
   --  Check that this the target is OpenVMS, and if so, return with no effect,
   --  otherwise post an error noting this can only be used with OpenVMS ports.
   --  The argument is the construct in question and is used to post the error
   --  message.

   procedure Collect_Interfaces
     (T               : Entity_Id;
      Ifaces_List     : out Elist_Id;
      Exclude_Parents : Boolean := False;
      Use_Full_View   : Boolean := True);
   --  Ada 2005 (AI-251): Collect whole list of abstract interfaces that are
   --  directly or indirectly implemented by T. Exclude_Parents is used to
   --  avoid the addition of inherited interfaces to the generated list.
   --  Use_Full_View is used to collect the interfaces using the full-view
   --  (if available).

   procedure Collect_Interface_Components
     (Tagged_Type     : Entity_Id;
      Components_List : out Elist_Id);
   --  Ada 2005 (AI-251): Collect all the tag components associated with the
   --  secondary dispatch tables of a tagged type.

   procedure Collect_Interfaces_Info
     (T               : Entity_Id;
      Ifaces_List     : out Elist_Id;
      Components_List : out Elist_Id;
      Tags_List       : out Elist_Id);
   --  Ada 2005 (AI-251): Collect all the interfaces associated with T plus
   --  the record component and tag associated with each of these interfaces.
   --  On exit Ifaces_List, Components_List and Tags_List have the same number
   --  of elements, and elements at the same position on these tables provide
   --  information on the same interface type.

   procedure Collect_Parents
     (T             : Entity_Id;
      List          : out Elist_Id;
      Use_Full_View : Boolean := True);
   --  Collect all the parents of Typ. Use_Full_View is used to collect them
   --  using the full-view of private parents (if available).

   function Collect_Primitive_Operations (T : Entity_Id) return Elist_Id;
   --  Called upon type derivation and extension. We scan the declarative part
   --  in which the type appears, and collect subprograms that have one
   --  subsidiary subtype of the type. These subprograms can only appear after
   --  the type itself.

   function Compile_Time_Constraint_Error
     (N    : Node_Id;
      Msg  : String;
      Ent  : Entity_Id  := Empty;
      Loc  : Source_Ptr := No_Location;
      Warn : Boolean    := False) return Node_Id;
   --  This is similar to Apply_Compile_Time_Constraint_Error in that it
   --  generates a warning (or error) message in the same manner, but it does
   --  not replace any nodes. For convenience, the function always returns its
   --  first argument. The message is a warning if the message ends with ?, or
   --  we are operating in Ada 83 mode, or the Warn parameter is set to True.

   procedure Conditional_Delay (New_Ent, Old_Ent : Entity_Id);
   --  Sets the Has_Delayed_Freeze flag of New if the Delayed_Freeze flag of
   --  Old is set and Old has no yet been Frozen (i.e. Is_Frozen is false).

   function Contains_Refined_State (Prag : Node_Id) return Boolean;
   --  Determine whether pragma Prag contains a reference to the entity of an
   --  abstract state with a visible refinement. Prag must denote one of the
   --  following pragmas:
   --    Depends
   --    Global

   function Copy_Parameter_List (Subp_Id : Entity_Id) return List_Id;
   --  Utility to create a parameter profile for a new subprogram spec, when
   --  the subprogram has a body that acts as spec. This is done for some cases
   --  of inlining, and for private protected ops. Also used to create bodies
   --  for stubbed subprograms.

   function Copy_Component_List
     (R_Typ : Entity_Id;
      Loc   : Source_Ptr) return List_Id;
   --  Copy components from record type R_Typ that come from source. Used to
   --  create a new compatible record type. Loc is the source location assigned
   --  to the created nodes.

   function Corresponding_Generic_Type (T : Entity_Id) return Entity_Id;
   --  If a type is a generic actual type, return the corresponding formal in
   --  the generic parent unit. There is no direct link in the tree for this
   --  attribute, except in the case of formal private and derived types.
   --  Possible optimization???

   function Current_Entity (N : Node_Id) return Entity_Id;
   pragma Inline (Current_Entity);
   --  Find the currently visible definition for a given identifier, that is to
   --  say the first entry in the visibility chain for the Chars of N.

   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id;
   --  Find whether there is a previous definition for identifier N in the
   --  current scope. Because declarations for a scope are not necessarily
   --  contiguous (e.g. for packages) the first entry on the visibility chain
   --  for N is not necessarily in the current scope.

   function Current_Scope return Entity_Id;
   --  Get entity representing current scope

   function Current_Subprogram return Entity_Id;
   --  Returns current enclosing subprogram. If Current_Scope is a subprogram,
   --  then that is what is returned, otherwise the Enclosing_Subprogram of the
   --  Current_Scope is returned. The returned value is Empty if this is called
   --  from a library package which is not within any subprogram.

   function Deepest_Type_Access_Level (Typ : Entity_Id) return Uint;
   --  Same as Type_Access_Level, except that if the type is the type of an Ada
   --  2012 stand-alone object of an anonymous access type, then return the
   --  static accesssibility level of the object. In that case, the dynamic
   --  accessibility level of the object may take on values in a range. The low
   --  bound of of that range is returned by Type_Access_Level; this function
   --  yields the high bound of that range. Also differs from Type_Access_Level
   --  in the case of a descendant of a generic formal type (returns Int'Last
   --  instead of 0).

   function Defining_Entity (N : Node_Id) return Entity_Id;
   --  Given a declaration N, returns the associated defining entity. If the
   --  declaration has a specification, the entity is obtained from the
   --  specification. If the declaration has a defining unit name, then the
   --  defining entity is obtained from the defining unit name ignoring any
   --  child unit prefixes.

   function Denotes_Discriminant
     (N                : Node_Id;
      Check_Concurrent : Boolean := False) return Boolean;
   --  Returns True if node N is an Entity_Name node for a discriminant. If the
   --  flag Check_Concurrent is true, function also returns true when N denotes
   --  the discriminal of the discriminant of a concurrent type. This is needed
   --  to disable some optimizations on private components of protected types,
   --  and constraint checks on entry families constrained by discriminants.

   function Denotes_Same_Object (A1, A2 : Node_Id) return Boolean;
   --  Detect suspicious overlapping between actuals in a call, when both are
   --  writable (RM 2012 6.4.1(6.4/3))

   function Denotes_Same_Prefix (A1, A2 : Node_Id) return Boolean;
   --  Functions to detect suspicious overlapping between actuals in a call,
   --  when one of them is writable. The predicates are those proposed in
   --  AI05-0144, to detect dangerous order dependence in complex calls.
   --  I would add a parameter Warn which enables more extensive testing of
   --  cases as we find appropriate when we are only warning ??? Or perhaps
   --  return an indication of (Error, Warn, OK) ???

   function Denotes_Variable (N : Node_Id) return Boolean;
   --  Returns True if node N denotes a single variable without parentheses

   function Depends_On_Discriminant (N : Node_Id) return Boolean;
   --  Returns True if N denotes a discriminant or if N is a range, a subtype
   --  indication or a scalar subtype where one of the bounds is a
   --  discriminant.

   function Designate_Same_Unit
     (Name1 : Node_Id;
      Name2 : Node_Id) return  Boolean;
   --  Return true if Name1 and Name2 designate the same unit name; each of
   --  these names is supposed to be a selected component name, an expanded
   --  name, a defining program unit name or an identifier.

   function Dynamic_Accessibility_Level (Expr : Node_Id) return Node_Id;
   --  Expr should be an expression of an access type. Builds an integer
   --  literal except in cases involving anonymous access types where
   --  accessibility levels are tracked at runtime (access parameters and Ada
   --  2012 stand-alone objects).

   function Effective_Extra_Accessibility (Id : Entity_Id) return Entity_Id;
   --  Same as Einfo.Extra_Accessibility except thtat object renames
   --  are looked through.

   function Effective_Reads_Enabled (Id : Entity_Id) return Boolean;
   --  Given the entity of an abstract state or a variable, determine whether
   --  Id is subject to external property Effective_Reads and if it is, the
   --  related expression evaluates to True.

   function Effective_Writes_Enabled (Id : Entity_Id) return Boolean;
   --  Given the entity of an abstract state or a variable, determine whether
   --  Id is subject to external property Effective_Writes and if it is, the
   --  related expression evaluates to True.

   function Enclosing_Comp_Unit_Node (N : Node_Id) return Node_Id;
   --  Returns the enclosing N_Compilation_Unit Node that is the root of a
   --  subtree containing N.

   function Enclosing_CPP_Parent (Typ : Entity_Id) return Entity_Id;
   --  Returns the closest ancestor of Typ that is a CPP type.

   function Enclosing_Generic_Body
     (N : Node_Id) return Node_Id;
   --  Returns the Node_Id associated with the innermost enclosing generic
   --  body, if any. If none, then returns Empty.

   function Enclosing_Generic_Unit
     (N : Node_Id) return Node_Id;
   --  Returns the Node_Id associated with the innermost enclosing generic
   --  unit, if any. If none, then returns Empty.

   function Enclosing_Lib_Unit_Entity
     (E : Entity_Id := Current_Scope) return Entity_Id;
   --  Returns the entity of enclosing library unit node which is the
   --  root of the current scope (which must not be Standard_Standard, and the
   --  caller is responsible for ensuring this condition) or other specified
   --  entity.

   function Enclosing_Package (E : Entity_Id) return Entity_Id;
   --  Utility function to return the Ada entity of the package enclosing
   --  the entity E, if any. Returns Empty if no enclosing package.

   function Enclosing_Subprogram (E : Entity_Id) return Entity_Id;
   --  Utility function to return the Ada entity of the subprogram enclosing
   --  the entity E, if any. Returns Empty if no enclosing subprogram.

   procedure Ensure_Freeze_Node (E : Entity_Id);
   --  Make sure a freeze node is allocated for entity E. If necessary, build
   --  and initialize a new freeze node and set Has_Delayed_Freeze True for E.

   procedure Enter_Name (Def_Id : Entity_Id);
   --  Insert new name in symbol table of current scope with check for
   --  duplications (error message is issued if a conflict is found).
   --  Note: Enter_Name is not used for overloadable entities, instead these
   --  are entered using Sem_Ch6.Enter_Overloadable_Entity.

   function Entity_Of (N : Node_Id) return Entity_Id;
   --  Return the entity of N or Empty. If N is a renaming, return the entity
   --  of the root renamed object.

   procedure Explain_Limited_Type (T : Entity_Id; N : Node_Id);
   --  This procedure is called after issuing a message complaining about an
   --  inappropriate use of limited type T. If useful, it adds additional
   --  continuation lines to the message explaining why type T is limited.
   --  Messages are placed at node N.

   procedure Find_Actual
     (N      : Node_Id;
      Formal : out Entity_Id;
      Call   : out Node_Id);
   --  Determines if the node N is an actual parameter of a function of a
   --  procedure call. If so, then Formal points to the entity for the formal
   --  (Ekind is E_In_Parameter, E_Out_Parameter, or E_In_Out_Parameter) and
   --  Call is set to the node for the corresponding call. If the node N is not
   --  an actual parameter then Formal and Call are set to Empty.

   function Find_Body_Discriminal
     (Spec_Discriminant : Entity_Id) return Entity_Id;
   --  Given a discriminant of the record type that implements a task or
   --  protected type, return the discriminal of the corresponding discriminant
   --  of the actual concurrent type.

   function Find_Corresponding_Discriminant
     (Id   : Node_Id;
      Typ  : Entity_Id) return Entity_Id;
   --  Because discriminants may have different names in a generic unit and in
   --  an instance, they are resolved positionally when possible. A reference
   --  to a discriminant carries the discriminant that it denotes when it is
   --  analyzed. Subsequent uses of this id on a different type denotes the
   --  discriminant at the same position in this new type.

   function Find_Enclosing_Iterator_Loop (Id : Entity_Id) return Entity_Id;
   --  Given an arbitrary entity, try to find the nearest enclosing iterator
   --  loop. If such a loop is found, return the entity of its identifier (the
   --  E_Loop scope), otherwise return Empty.

   function Find_Loop_In_Conditional_Block (N : Node_Id) return Node_Id;
   --  Find the nested loop statement in a conditional block. Loops subject to
   --  attribute 'Loop_Entry are transformed into blocks. Parts of the original
   --  loop are nested within the block.

   procedure Find_Overlaid_Entity
     (N   : Node_Id;
      Ent : out Entity_Id;
      Off : out Boolean);
   --  The node N should be an address representation clause. Determines if
   --  the target expression is the address of an entity with an optional
   --  offset. If so, set Ent to the entity and, if there is an offset, set
   --  Off to True, otherwise to False. If N is not an address representation
   --  clause, or if it is not possible to determine that the address is of
   --  this form, then set Ent to Empty.

   function Find_Parameter_Type (Param : Node_Id) return Entity_Id;
   --  Return the type of formal parameter Param as determined by its
   --  specification.

   --  The following type describes the placement of an arbitrary entity with
   --  respect to SPARK visible / hidden state space.

   type State_Space_Kind is
     (Not_In_Package,
      --  An entity is not in the visible, private or body state space when
      --  the immediate enclosing construct is not a package.

      Visible_State_Space,
      --  An entity is in the visible state space when it appears immediately
      --  within the visible declarations of a package or when it appears in
      --  the visible state space of a nested package which in turn is declared
      --  in the visible declarations of an enclosing package:

      --    package Pack is
      --       Visible_Variable : ...
      --       package Nested
      --         with Abstract_State => Visible_State
      --       is
      --          Visible_Nested_Variable : ...
      --       end Nested;
      --    end Pack;

      --  Entities associated with a package instantiation inherit the state
      --  space from the instance placement:

      --     generic
      --     package Gen is
      --        Generic_Variable : ...
      --     end Gen;

      --     with Gen;
      --     package Pack is
      --        package Inst is new Gen;
      --        --  Generic_Variable is in the visible state space of Pack
      --     end Pack;

      Private_State_Space,
      --  An entity is in the private state space when it appears immediately
      --  within the private declarations of a package or when it appears in
      --  the visible state space of a nested package which in turn is declared
      --  in the private declarations of an enclosing package:

      --    package Pack is
      --    private
      --       Private_Variable : ...
      --       package Nested
      --         with Abstract_State => Private_State
      --       is
      --          Private_Nested_Variable : ...
      --       end Nested;
      --    end Pack;

      --  The same placement principle applies to package instantiations

      Body_State_Space);
      --  An entity is in the body state space when it appears immediately
      --  within the declarations of a package body or when it appears in the
      --  visible state space of a nested package which in turn is declared in
      --  the declarations of an enclosing package body:

      --    package body Pack is
      --       Body_Variable : ...
      --       package Nested
      --         with Abstract_State => Body_State
      --       is
      --          Body_Nested_Variable : ...
      --       end Nested;
      --    end Pack;

      --  The same placement principle applies to package instantiations

   procedure Find_Placement_In_State_Space
     (Item_Id   : Entity_Id;
      Placement : out State_Space_Kind;
      Pack_Id   : out Entity_Id);
   --  Determine the state space placement of an item. Item_Id denotes the
   --  entity of an abstract state, variable or package instantiation.
   --  Placement captures the precise placement of the item in the enclosing
   --  state space. If the state space is that of a package, Pack_Id denotes
   --  its entity, otherwise Pack_Id is Empty.

   function Find_Static_Alternative (N : Node_Id) return Node_Id;
   --  N is a case statement whose expression is a compile-time value.
   --  Determine the alternative chosen, so that the code of non-selected
   --  alternatives, and the warnings that may apply to them, are removed.

   function First_Actual (Node : Node_Id) return Node_Id;
   --  Node is an N_Function_Call or N_Procedure_Call_Statement node. The
   --  result returned is the first actual parameter in declaration order
   --  (not the order of parameters as they appeared in the source, which
   --  can be quite different as a result of the use of named parameters).
   --  Empty is returned for a call with no parameters. The procedure for
   --  iterating through the actuals in declaration order is to use this
   --  function to find the first actual, and then use Next_Actual to obtain
   --  the next actual in declaration order. Note that the value returned
   --  is always the expression (not the N_Parameter_Association nodes,
   --  even if named association is used).

   procedure Gather_Components
     (Typ           : Entity_Id;
      Comp_List     : Node_Id;
      Governed_By   : List_Id;
      Into          : Elist_Id;
      Report_Errors : out Boolean);
   --  The purpose of this procedure is to gather the valid components in a
   --  record type according to the values of its discriminants, in order to
   --  validate the components of a record aggregate.
   --
   --    Typ is the type of the aggregate when its constrained discriminants
   --      need to be collected, otherwise it is Empty.
   --
   --    Comp_List is an N_Component_List node.
   --
   --    Governed_By is a list of N_Component_Association nodes, where each
   --     choice list contains the name of a discriminant and the expression
   --     field gives its value. The values of the discriminants governing
   --     the (possibly nested) variant parts in Comp_List are found in this
   --     Component_Association List.
   --
   --    Into is the list where the valid components are appended. Note that
   --     Into need not be an Empty list. If it's not, components are attached
   --     to its tail.
   --
   --    Report_Errors is set to True if the values of the discriminants are
   --     non-static.
   --
   --  This procedure is also used when building a record subtype. If the
   --  discriminant constraint of the subtype is static, the components of the
   --  subtype are only those of the variants selected by the values of the
   --  discriminants. Otherwise all components of the parent must be included
   --  in the subtype for semantic analysis.

   function Get_Actual_Subtype (N : Node_Id) return Entity_Id;
   --  Given a node for an expression, obtain the actual subtype of the
   --  expression. In the case of a parameter where the formal is an
   --  unconstrained array or discriminated type, this will be the previously
   --  constructed subtype of the actual. Note that this is not quite the
   --  "Actual Subtype" of the RM, since it is always a constrained type, i.e.
   --  it is the subtype of the value of the actual. The actual subtype is also
   --  returned in other cases where it has already been constructed for an
   --  object. Otherwise the expression type is returned unchanged, except for
   --  the case of an unconstrained array type, where an actual subtype is
   --  created, using Insert_Actions if necessary to insert any associated
   --  actions.

   function Get_Actual_Subtype_If_Available (N : Node_Id) return Entity_Id;
   --  This is like Get_Actual_Subtype, except that it never constructs an
   --  actual subtype. If an actual subtype is already available, i.e. the
   --  Actual_Subtype field of the corresponding entity is set, then it is
   --  returned. Otherwise the Etype of the node is returned.

   function Get_Body_From_Stub (N : Node_Id) return Node_Id;
   --  Return the body node for a stub (subprogram or package)

   function Get_Cursor_Type
     (Aspect : Node_Id;
      Typ    : Entity_Id) return Entity_Id;
   --  Find Cursor type in scope of formal container Typ, by locating primitive
   --  operation First. For use in resolving the other primitive operations
   --  of an Iterable type and expanding loops and quantified expressions
   --  over formal containers.

   function Get_Default_External_Name (E : Node_Or_Entity_Id) return Node_Id;
   --  This is used to construct the string literal node representing a
   --  default external name, i.e. one that is constructed from the name of an
   --  entity, or (in the case of extended DEC import/export pragmas, an
   --  identifier provided as the external name. Letters in the name are
   --  according to the setting of Opt.External_Name_Default_Casing.

   function Get_Enclosing_Object (N : Node_Id) return Entity_Id;
   --  If expression N references a part of an object, return this object.
   --  Otherwise return Empty. Expression N should have been resolved already.

   function Get_Ensures_From_CTC_Pragma (N : Node_Id) return Node_Id;
   --  Return the Ensures component of Test_Case pragma N, or Empty otherwise
   --  Bad name now that this no longer applies to Contract_Case ???

   function Get_Generic_Entity (N : Node_Id) return Entity_Id;
   --  Returns the true generic entity in an instantiation. If the name in the
   --  instantiation is a renaming, the function returns the renamed generic.

   function Get_Incomplete_View_Of_Ancestor (E : Entity_Id) return Entity_Id;
   --  Implements the notion introduced ever-so briefly in RM 7.3.1 (5.2/3):
   --  in a child unit a derived type is within the derivation class of an
   --  ancestor declared in a parent unit, even if there is an intermediate
   --  derivation that does not see the full view of that ancestor.

   procedure Get_Index_Bounds (N : Node_Id; L, H : out Node_Id);
   --  This procedure assigns to L and H respectively the values of the low and
   --  high bounds of node N, which must be a range, subtype indication, or the
   --  name of a scalar subtype. The result in L, H may be set to Error if
   --  there was an earlier error in the range.

   function Get_Enum_Lit_From_Pos
     (T   : Entity_Id;
      Pos : Uint;
      Loc : Source_Ptr) return Node_Id;
   --  This function returns an identifier denoting the E_Enumeration_Literal
   --  entity for the specified value from the enumeration type or subtype T.
   --  The second argument is the Pos value, which is assumed to be in range.
   --  The third argument supplies a source location for constructed nodes
   --  returned by this function.

   function Get_Iterable_Type_Primitive
     (Typ : Entity_Id;
      Nam : Name_Id) return Entity_Id;
   --  Retrieve one of the primitives First, Next, Has_Element, Element from
   --  the value of the Iterable aspect of a formal type.

   procedure Get_Library_Unit_Name_String (Decl_Node : Node_Id);
   --  Retrieve the fully expanded name of the library unit declared by
   --  Decl_Node into the name buffer.

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id;
   pragma Inline (Get_Name_Entity_Id);
   --  An entity value is associated with each name in the name table. The
   --  Get_Name_Entity_Id function fetches the Entity_Id of this entity, which
   --  is the innermost visible entity with the given name. See the body of
   --  Sem_Ch8 for further details on handling of entity visibility.

   function Get_Name_From_CTC_Pragma (N : Node_Id) return String_Id;
   --  Return the Name component of Test_Case pragma N
   --  Bad name now that this no longer applies to Contract_Case ???

   function Get_Pragma_Id (N : Node_Id) return Pragma_Id;
   pragma Inline (Get_Pragma_Id);
   --  Obtains the Pragma_Id from the Chars field of Pragma_Identifier (N)

   procedure Get_Reason_String (N : Node_Id);
   --  Recursive routine to analyze reason argument for pragma Warnings. The
   --  value of the reason argument is appended to the current string using
   --  Store_String_Chars. The reason argument is expected to be a string
   --  literal or concatenation of string literals. An error is given for
   --  any other form.

   function Get_Referenced_Object (N : Node_Id) return Node_Id;
   --  Given a node, return the renamed object if the node represents a renamed
   --  object, otherwise return the node unchanged. The node may represent an
   --  arbitrary expression.

   function Get_Renamed_Entity (E : Entity_Id) return Entity_Id;
   --  Given an entity for an exception, package, subprogram or generic unit,
   --  returns the ultimately renamed entity if this is a renaming. If this is
   --  not a renamed entity, returns its argument. It is an error to call this
   --  with any other kind of entity.

   function Get_Requires_From_CTC_Pragma (N : Node_Id) return Node_Id;
   --  Return the Requires component of Test_Case pragma N, or Empty otherwise
   --  Bad name now that this no longer applies to Contract_Case ???

   function Get_Subprogram_Entity (Nod : Node_Id) return Entity_Id;
   --  Nod is either a procedure call statement, or a function call, or an
   --  accept statement node. This procedure finds the Entity_Id of the related
   --  subprogram or entry and returns it, or if no subprogram can be found,
   --  returns Empty.

   function Get_Subprogram_Body (E : Entity_Id) return Node_Id;
   --  Given the entity for a subprogram (E_Function or E_Procedure), return
   --  the corresponding N_Subprogram_Body node. If the corresponding body
   --  is missing (as for an imported subprogram), return Empty.

   function Get_Task_Body_Procedure (E : Entity_Id) return Node_Id;
   pragma Inline (Get_Task_Body_Procedure);
   --  Given an entity for a task type or subtype, retrieves the
   --  Task_Body_Procedure field from the corresponding task type declaration.

   function Has_Access_Values (T : Entity_Id) return Boolean;
   --  Returns true if type or subtype T is an access type, or has a component
   --  (at any recursive level) that is an access type. This is a conservative
   --  predicate, if it is not known whether or not T contains access values
   --  (happens for generic formals in some cases), then False is returned.
   --  Note that tagged types return False. Even though the tag is implemented
   --  as an access type internally, this function tests only for access types
   --  known to the programmer. See also Has_Tagged_Component.

   type Alignment_Result is (Known_Compatible, Unknown, Known_Incompatible);
   --  Result of Has_Compatible_Alignment test, description found below. Note
   --  that the values are arranged in increasing order of problematicness.

   function Has_Compatible_Alignment
     (Obj  : Entity_Id;
      Expr : Node_Id) return Alignment_Result;
   --  Obj is an object entity, and expr is a node for an object reference. If
   --  the alignment of the object referenced by Expr is known to be compatible
   --  with the alignment of Obj (i.e. is larger or the same), then the result
   --  is Known_Compatible. If the alignment of the object referenced by Expr
   --  is known to be less than the alignment of Obj, then Known_Incompatible
   --  is returned. If neither condition can be reliably established at compile
   --  time, then Unknown is returned. This is used to determine if alignment
   --  checks are required for address clauses, and also whether copies must
   --  be made when objects are passed by reference.
   --
   --  Note: Known_Incompatible does not mean that at run time the alignment
   --  of Expr is known to be wrong for Obj, just that it can be determined
   --  that alignments have been explicitly or implicitly specified which are
   --  incompatible (whereas Unknown means that even this is not known). The
   --  appropriate reaction of a caller to Known_Incompatible is to treat it as
   --  Unknown, but issue a warning that there may be an alignment error.

   function Has_Declarations (N : Node_Id) return Boolean;
   --  Determines if the node can have declarations

   function Has_Denormals (E : Entity_Id) return Boolean;
   --  Determines if the floating-point type E supports denormal numbers.
   --  Returns False if E is not a floating-point type.

   function Has_Discriminant_Dependent_Constraint
     (Comp : Entity_Id) return Boolean;
   --  Returns True if and only if Comp has a constrained subtype that depends
   --  on a discriminant.

   function Has_Infinities (E : Entity_Id) return Boolean;
   --  Determines if the range of the floating-point type E includes
   --  infinities. Returns False if E is not a floating-point type.

   function Has_Interfaces
     (T             : Entity_Id;
      Use_Full_View : Boolean := True) return Boolean;
   --  Where T is a concurrent type or a record type, returns true if T covers
   --  any abstract interface types. In case of private types the argument
   --  Use_Full_View controls if the check is done using its full view (if
   --  available).

   function Has_No_Obvious_Side_Effects (N : Node_Id) return Boolean;
   --  This is a simple minded function for determining whether an expression
   --  has no obvious side effects. It is used only for determining whether
   --  warnings are needed in certain situations, and is not guaranteed to
   --  be accurate in either direction. Exceptions may mean an expression
   --  does in fact have side effects, but this may be ignored and True is
   --  returned, or a complex expression may in fact be side effect free
   --  but we don't recognize it here and return False. The Side_Effect_Free
   --  routine in Remove_Side_Effects is much more extensive and perhaps could
   --  be shared, so that this routine would be more accurate.

   function Has_Null_Exclusion (N : Node_Id) return Boolean;
   --  Determine whether node N has a null exclusion

   function Has_Overriding_Initialize (T : Entity_Id) return Boolean;
   --  Predicate to determine whether a controlled type has a user-defined
   --  Initialize primitive (and, in Ada 2012, whether that primitive is
   --  non-null), which causes the type to not have preelaborable
   --  initialization.

   function Has_Preelaborable_Initialization (E : Entity_Id) return Boolean;
   --  Return True iff type E has preelaborable initialization as defined in
   --  Ada 2005 (see AI-161 for details of the definition of this attribute).

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean;
   --  Check if a type has a (sub)component of a private type that has not
   --  yet received a full declaration.

   function Has_Signed_Zeros (E : Entity_Id) return Boolean;
   --  Determines if the floating-point type E supports signed zeros.
   --  Returns False if E is not a floating-point type.

   function Has_Static_Array_Bounds (Typ : Node_Id) return Boolean;
   --  Return whether an array type has static bounds

   function Has_Stream (T : Entity_Id) return Boolean;
   --  Tests if type T is derived from Ada.Streams.Root_Stream_Type, or in the
   --  case of a composite type, has a component for which this predicate is
   --  True, and if so returns True. Otherwise a result of False means that
   --  there is no Stream type in sight. For a private type, the test is
   --  applied to the underlying type (or returns False if there is no
   --  underlying type).

   function Has_Suffix (E : Entity_Id; Suffix : Character) return Boolean;
   --  Returns true if the last character of E is Suffix. Used in Assertions.

   function Add_Suffix (E : Entity_Id; Suffix : Character) return Name_Id;
   --  Returns the name of E adding Suffix

   function Remove_Suffix (E : Entity_Id; Suffix : Character) return Name_Id;
   --  Returns the name of E without Suffix

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean;
   --  Returns True if Typ is a composite type (array or record) which is
   --  either itself a tagged type, or has a component (recursively) which is
   --  a tagged type. Returns False for non-composite type, or if no tagged
   --  component is present. This function is used to check if "=" has to be
   --  expanded into a bunch component comparisons.

   function Has_Volatile_Component (Typ : Entity_Id) return Boolean;
   --  Given an arbitrary type, determine whether it contains at least one
   --  volatile component.

   function Implementation_Kind (Subp : Entity_Id) return Name_Id;
   --  Subp is a subprogram marked with pragma Implemented. Return the specific
   --  implementation requirement which the pragma imposes. The return value is
   --  either Name_By_Any, Name_By_Entry or Name_By_Protected_Procedure.

   function Implements_Interface
     (Typ_Ent         : Entity_Id;
      Iface_Ent       : Entity_Id;
      Exclude_Parents : Boolean := False) return Boolean;
   --  Returns true if the Typ_Ent implements interface Iface_Ent

   function In_Assertion_Expression_Pragma (N : Node_Id) return Boolean;
   --  Determine whether an arbitrary node appears in a pragma that acts as an
   --  assertion expression. See Sem_Prag for the list of qualifying pragmas.

   function In_Instance return Boolean;
   --  Returns True if the current scope is within a generic instance

   function In_Instance_Body return Boolean;
   --  Returns True if current scope is within the body of an instance, where
   --  several semantic checks (e.g. accessibility checks) are relaxed.

   function In_Instance_Not_Visible return Boolean;
   --  Returns True if current scope is with the private part or the body of
   --  an instance. Other semantic checks are suppressed in this context.

   function In_Instance_Visible_Part return Boolean;
   --  Returns True if current scope is within the visible part of a package
   --  instance, where several additional semantic checks apply.

   function In_Package_Body return Boolean;
   --  Returns True if current scope is within a package body

   function In_Parameter_Specification (N : Node_Id) return Boolean;
   --  Returns True if node N belongs to a parameter specification

   function In_Pragma_Expression (N : Node_Id; Nam : Name_Id) return Boolean;
   --  Returns true if the expression N occurs within a pragma with name Nam

   function In_Reverse_Storage_Order_Object (N : Node_Id) return Boolean;
   --  Returns True if N denotes a component or subcomponent in a record or
   --  array that has Reverse_Storage_Order.

   function In_Subprogram_Or_Concurrent_Unit return Boolean;
   --  Determines if the current scope is within a subprogram compilation unit
   --  (inside a subprogram declaration, subprogram body, or generic subprogram
   --  declaration) or within a task or protected body. The test is for
   --  appearing anywhere within such a construct (that is it does not need
   --  to be directly within).

   function In_Visible_Part (Scope_Id : Entity_Id) return Boolean;
   --  Determine whether a declaration occurs within the visible part of a
   --  package specification. The package must be on the scope stack, and the
   --  corresponding private part must not.

   function Incomplete_Or_Private_View (Typ : Entity_Id) return Entity_Id;
   --  Given the entity of a type, retrieve the incomplete or private view of
   --  the same type. Note that Typ may not have a partial view to begin with,
   --  in that case the function returns Empty.

   procedure Insert_Explicit_Dereference (N : Node_Id);
   --  In a context that requires a composite or subprogram type and where a
   --  prefix is an access type, rewrite the access type node N (which is the
   --  prefix, e.g. of an indexed component) as an explicit dereference.

   procedure Inspect_Deferred_Constant_Completion (Decls : List_Id);
   --  Examine all deferred constants in the declaration list Decls and check
   --  whether they have been completed by a full constant declaration or an
   --  Import pragma. Emit the error message if that is not the case.

   function Is_Actual_Out_Parameter (N : Node_Id) return Boolean;
   --  Determines if N is an actual parameter of out mode in a subprogram call

   function Is_Actual_Parameter (N : Node_Id) return Boolean;
   --  Determines if N is an actual parameter in a subprogram call

   function Is_Actual_Tagged_Parameter (N : Node_Id) return Boolean;
   --  Determines if N is an actual parameter of a formal of tagged type in a
   --  subprogram call.

   function Is_Aliased_View (Obj : Node_Id) return Boolean;
   --  Determine if Obj is an aliased view, i.e. the name of an object to which
   --  'Access or 'Unchecked_Access can apply. Note that this routine uses the
   --  rules of the language, it does not take into account the restriction
   --  No_Implicit_Aliasing, so it can return True if the restriction is active
   --  and Obj violates the restriction. The caller is responsible for calling
   --  Restrict.Check_No_Implicit_Aliasing if True is returned, but there is a
   --  requirement for obeying the restriction in the call context.

   function Is_Ancestor_Package
     (E1 : Entity_Id;
      E2 : Entity_Id) return Boolean;
   --  Determine whether package E1 is an ancestor of E2

   function Is_Atomic_Object (N : Node_Id) return Boolean;
   --  Determines if the given node denotes an atomic object in the sense of
   --  the legality checks described in RM C.6(12).

   function Is_Attribute_Result (N : Node_Id) return Boolean;
   --  Determine whether node N denotes attribute 'Result

   function Is_Body_Or_Package_Declaration (N : Node_Id) return Boolean;
   --  Determine whether node N denotes a body or a package declaration

   function Is_Bounded_String (T : Entity_Id) return Boolean;
   --  True if T is a bounded string type. Used to make sure "=" composes
   --  properly for bounded string types.

   function Is_Constant_Bound (Exp : Node_Id) return Boolean;
   --  Exp is the expression for an array bound. Determines whether the
   --  bound is a compile-time known value, or a constant entity, or an
   --  enumeration literal, or an expression composed of constant-bound
   --  subexpressions which are evaluated by means of standard operators.

   function Is_Container_Element (Exp : Node_Id) return Boolean;
   --  This routine recognizes expressions that denote an element of one of
   --  the predefined containers, when the source only contains an indexing
   --  operation and an implicit dereference is inserted by the compiler.
   --  In the absence of this optimization, the indexing creates a temporary
   --  controlled cursor that sets the tampering bit of the container, and
   --  restricts the use of the convenient notation C (X) to contexts that
   --  do not check the tampering bit (e.g. C.Include (X, C (Y)). Exp is an
   --  explicit dereference. The transformation applies when it has the form
   --  F (X).Discr.all.

   function Is_Controlling_Limited_Procedure
     (Proc_Nam : Entity_Id) return Boolean;
   --  Ada 2005 (AI-345): Determine whether Proc_Nam is a primitive procedure
   --  of a limited interface with a controlling first parameter.

   function Is_CPP_Constructor_Call (N : Node_Id) return Boolean;
   --  Returns True if N is a call to a CPP constructor

   function Is_Child_Or_Sibling
     (Pack_1 : Entity_Id;
      Pack_2 : Entity_Id) return Boolean;
   --  Determine the following relations between two arbitrary packages:
   --    1) One package is the parent of a child package
   --    2) Both packages are siblings and share a common parent

   function Is_Concurrent_Interface (T : Entity_Id) return Boolean;
   --  First determine whether type T is an interface and then check whether
   --  it is of protected, synchronized or task kind.

   function Is_Delegate (T : Entity_Id) return Boolean;
   --  Returns true if type T represents a delegate. A Delegate is the CIL
   --  object used to represent access-to-subprogram types. This is only
   --  relevant to CIL, will always return false for other targets.

   function Is_Dependent_Component_Of_Mutable_Object
     (Object : Node_Id) return Boolean;
   --  Returns True if Object is the name of a subcomponent that depends on
   --  discriminants of a variable whose nominal subtype is unconstrained and
   --  not indefinite, and the variable is not aliased. Otherwise returns
   --  False. The nodes passed to this function are assumed to denote objects.

   function Is_Dereferenced (N : Node_Id) return Boolean;
   --  N is a subexpression node of an access type. This function returns true
   --  if N appears as the prefix of a node that does a dereference of the
   --  access value (selected/indexed component, explicit dereference or a
   --  slice), and false otherwise.

   function Is_Descendent_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean;
   --  Returns True if type T1 is a descendent of type T2, and false otherwise.
   --  This is the RM definition, a type is a descendent of another type if it
   --  is the same type or is derived from a descendent of the other type.

   function Is_Expression_Function (Subp : Entity_Id) return Boolean;
   --  Predicate to determine whether a scope entity comes from a rewritten
   --  expression function call, and should be inlined unconditionally. Also
   --  used to determine that such a call does not constitute a freeze point.

   function Is_False (U : Uint) return Boolean;
   pragma Inline (Is_False);
   --  The argument is a Uint value which is the Boolean'Pos value of a Boolean
   --  operand (i.e. is either 0 for False, or 1 for True). This function tests
   --  if it is False (i.e. zero).

   function Is_Fixed_Model_Number (U : Ureal; T : Entity_Id) return Boolean;
   --  Returns True iff the number U is a model number of the fixed-point type
   --  T, i.e. if it is an exact multiple of Small.

   function Is_Fully_Initialized_Type (Typ : Entity_Id) return Boolean;
   --  Typ is a type entity. This function returns true if this type is fully
   --  initialized, meaning that an object of the type is fully initialized.
   --  Note that initialization resulting from use of pragma Normalized_Scalars
   --  does not count. Note that this is only used for the purpose of issuing
   --  warnings for objects that are potentially referenced uninitialized. This
   --  means that the result returned is not crucial, but should err on the
   --  side of thinking things are fully initialized if it does not know.

   function Is_Inherited_Operation (E : Entity_Id) return Boolean;
   --  E is a subprogram. Return True is E is an implicit operation inherited
   --  by a derived type declaration.

   function Is_Inherited_Operation_For_Type
     (E   : Entity_Id;
      Typ : Entity_Id) return Boolean;
   --  E is a subprogram. Return True is E is an implicit operation inherited
   --  by the derived type declaration for type Typ.

   function Is_Iterator (Typ : Entity_Id) return Boolean;
   --  AI05-0139-2: Check whether Typ is one of the predefined interfaces in
   --  Ada.Iterator_Interfaces, or it is derived from one.

   function Is_Junk_Name (N : Name_Id) return Boolean;
   --  Returns True if the given name contains any of the following substrings
   --    discard
   --    dummy
   --    ignore
   --    junk
   --    unused
   --  Used to suppress warnings on names matching these patterns. The contents
   --  of Name_Buffer and Name_Len are desteoyed by this call.

   type Is_LHS_Result is (Yes, No, Unknown);
   function Is_LHS (N : Node_Id) return Is_LHS_Result;
   --  Returns Yes if N is definitely used as Name in an assignment statement.
   --  Returns No if N is definitely NOT used as a Name in an assignment
   --  statement. Returns Unknown if we can't tell at this stage (happens in
   --  the case where we don't know the type of N yet, and we have something
   --  like N.A := 3, where this counts as N being used on the left side of
   --  an assignment only if N is not an access type. If it is an access type
   --  then it is N.all.A that is assigned, not N.

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean;
   --  A library-level declaration is one that is accessible from Standard,
   --  i.e. a library unit or an entity declared in a library package.

   function Is_Limited_Class_Wide_Type (Typ : Entity_Id) return Boolean;
   --  Determine whether a given type is a limited class-wide type, in which
   --  case it needs a Master_Id, because extensions of its designated type
   --  may include task components. A class-wide type that comes from a
   --  limited view must be treated in the same way.

   function Is_Local_Variable_Reference (Expr : Node_Id) return Boolean;
   --  Determines whether Expr is a reference to a variable or IN OUT mode
   --  parameter of the current enclosing subprogram.
   --  Why are OUT parameters not considered here ???

   function Is_Object_Reference (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents an object. Both
   --  variable and constant objects return True (compare Is_Variable).

   function Is_OK_Variable_For_Out_Formal (AV : Node_Id) return Boolean;
   --  Used to test if AV is an acceptable formal for an OUT or IN OUT formal.
   --  Note that the Is_Variable function is not quite the right test because
   --  this is a case in which conversions whose expression is a variable (in
   --  the Is_Variable sense) with a non-tagged type target are considered view
   --  conversions and hence variables.

   function Is_Partially_Initialized_Type
     (Typ              : Entity_Id;
      Include_Implicit : Boolean := True) return Boolean;
   --  Typ is a type entity. This function returns true if this type is partly
   --  initialized, meaning that an object of the type is at least partly
   --  initialized (in particular in the record case, that at least one
   --  component has an initialization expression). Note that initialization
   --  resulting from the use of pragma Normalized_Scalars does not count.
   --  Include_Implicit controls whether implicit initialization of access
   --  values to null, and of discriminant values, is counted as making the
   --  type be partially initialized. For the default setting of True, these
   --  implicit cases do count, and discriminated types or types containing
   --  access values not explicitly initialized will return True. Otherwise
   --  if Include_Implicit is False, these cases do not count as making the
   --  type be partially initialized.

   function Is_Potentially_Unevaluated (N : Node_Id) return Boolean;
   --  Predicate to implement definition given in RM 6.1.1 (20/3)

   function Is_Potentially_Persistent_Type (T : Entity_Id) return Boolean;
   --  Determines if type T is a potentially persistent type. A potentially
   --  persistent type is defined (recursively) as a scalar type, a non-tagged
   --  record whose components are all of a potentially persistent type, or an
   --  array with all static constraints whose component type is potentially
   --  persistent. A private type is potentially persistent if the full type
   --  is potentially persistent.

   function Is_Protected_Self_Reference (N : Node_Id) return Boolean;
   --  Return True if node N denotes a protected type name which represents
   --  the current instance of a protected object according to RM 9.4(21/2).

   function Is_RCI_Pkg_Spec_Or_Body (Cunit : Node_Id) return Boolean;
   --  Return True if a compilation unit is the specification or the
   --  body of a remote call interface package.

   function Is_Remote_Access_To_Class_Wide_Type (E : Entity_Id) return Boolean;
   --  Return True if E is a remote access-to-class-wide type

   function Is_Remote_Access_To_Subprogram_Type (E : Entity_Id) return Boolean;
   --  Return True if E is a remote access to subprogram type

   function Is_Remote_Call (N : Node_Id) return Boolean;
   --  Return True if N denotes a potentially remote call

   function Is_Renamed_Entry (Proc_Nam : Entity_Id) return Boolean;
   --  Return True if Proc_Nam is a procedure renaming of an entry

   function Is_Reversible_Iterator (Typ : Entity_Id) return Boolean;
   --  AI05-0139-2: Check whether Typ is derived from the predefined interface
   --  Ada.Iterator_Interfaces.Reversible_Iterator.

   function Is_Selector_Name (N : Node_Id) return Boolean;
   --  Given an N_Identifier node N, determines if it is a Selector_Name.
   --  As described in Sinfo, Selector_Names are special because they
   --  represent use of the N_Identifier node for a true identifier, when
   --  normally such nodes represent a direct name.

   function Is_SPARK_Initialization_Expr (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents an initialization
   --  expression in SPARK, suitable for initializing an object in an object
   --  declaration.

   function Is_SPARK_Object_Reference (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents an object in SPARK

   function Is_SPARK_Volatile (Id : Entity_Id) return Boolean;
   --  This routine is similar to predicate Is_Volatile, but it takes SPARK
   --  semantics into account. In SPARK volatile components to not render a
   --  type volatile.

   function Is_SPARK_Volatile_Object (N : Node_Id) return Boolean;
   --  Determine whether an arbitrary node denotes a volatile object reference
   --  according to the semantics of SPARK. To qualify as volatile, an object
   --  must be subject to aspect/pragma Volatile or Atomic, or have a [sub]type
   --  subject to the same attributes. Note that volatile components do not
   --  render an object volatile.

   function Is_Statement (N : Node_Id) return Boolean;
   pragma Inline (Is_Statement);
   --  Check if the node N is a statement node. Note that this includes
   --  the case of procedure call statements (unlike the direct use of
   --  the N_Statement_Other_Than_Procedure_Call subtype from Sinfo).
   --  Note that a label is *not* a statement, and will return False.

   function Is_Subprogram_Stub_Without_Prior_Declaration
     (N : Node_Id) return Boolean;
   --  Return True if N is a subprogram stub with no prior subprogram
   --  declaration.

   function Is_Synchronized_Tagged_Type (E : Entity_Id) return Boolean;
   --  Returns True if E is a synchronized tagged type (AARM 3.9.4 (6/2))

   function Is_Transfer (N : Node_Id) return Boolean;
   --  Returns True if the node N is a statement which is known to cause an
   --  unconditional transfer of control at runtime, i.e. the following
   --  statement definitely will not be executed.

   function Is_True (U : Uint) return Boolean;
   pragma Inline (Is_True);
   --  The argument is a Uint value which is the Boolean'Pos value of a Boolean
   --  operand (i.e. is either 0 for False, or 1 for True). This function tests
   --  if it is True (i.e. non-zero).

   function Is_Unchecked_Conversion_Instance (Id : Entity_Id) return Boolean;
   --  Determine whether an arbitrary entity denotes an instance of function
   --  Ada.Unchecked_Conversion.

   function Is_Universal_Numeric_Type (T : Entity_Id) return Boolean;
   pragma Inline (Is_Universal_Numeric_Type);
   --  True if T is Universal_Integer or Universal_Real

   function Is_Value_Type (T : Entity_Id) return Boolean;
   --  Returns true if type T represents a value type. This is only relevant to
   --  CIL, will always return false for other targets. A value type is a CIL
   --  object that is accessed directly, as opposed to the other CIL objects
   --  that are accessed through managed pointers.

   function Is_Variable_Size_Array (E : Entity_Id) return Boolean;
   --  Returns true if E has variable size components

   function Is_Variable_Size_Record (E : Entity_Id) return Boolean;
   --  Returns true if E has variable size components

   function Is_VMS_Operator (Op : Entity_Id) return Boolean;
   --  Determine whether an operator is one of the intrinsics defined
   --  in the DEC system extension.

   function Is_Variable
     (N                 : Node_Id;
      Use_Original_Node : Boolean := True) return Boolean;
   --  Determines if the tree referenced by N represents a variable, i.e. can
   --  appear on the left side of an assignment. There is one situation (formal
   --  parameters) in which non-tagged type conversions are also considered
   --  variables, but Is_Variable returns False for such cases, since it has
   --  no knowledge of the context. Note that this is the point at which
   --  Assignment_OK is checked, and True is returned for any tree thus marked.
   --  Use_Original_Node is used to perform the test on Original_Node (N). By
   --  default is True since this routine is commonly invoked as part of the
   --  semantic analysis and it must not be disturbed by the rewriten nodes.

   function Is_Visibly_Controlled (T : Entity_Id) return Boolean;
   --  Check whether T is derived from a visibly controlled type. This is true
   --  if the root type is declared in Ada.Finalization. If T is derived
   --  instead from a private type whose full view is controlled, an explicit
   --  Initialize/Adjust/Finalize subprogram does not override the inherited
   --  one.

   function Is_Volatile_Object (N : Node_Id) return Boolean;
   --  Determines if the given node denotes an volatile object in the sense of
   --  the legality checks described in RM C.6(12). Note that the test here is
   --  for something actually declared as volatile, not for an object that gets
   --  treated as volatile (see Einfo.Treat_As_Volatile).

   function Itype_Has_Declaration (Id : Entity_Id) return Boolean;
   --  Applies to Itypes. True if the Itype is attached to a declaration for
   --  the type through its Parent field, which may or not be present in the
   --  tree.

   procedure Kill_Current_Values (Last_Assignment_Only : Boolean := False);
   --  This procedure is called to clear all constant indications from all
   --  entities in the current scope and in any parent scopes if the current
   --  scope is a block or a package (and that recursion continues to the top
   --  scope that is not a block or a package). This is used when the
   --  sequential flow-of-control assumption is violated (occurrence of a
   --  label, head of a loop, or start of an exception handler). The effect of
   --  the call is to clear the Current_Value field (but we do not need to
   --  clear the Is_True_Constant flag, since that only gets reset if there
   --  really is an assignment somewhere in the entity scope). This procedure
   --  also calls Kill_All_Checks, since this is a special case of needing to
   --  forget saved values. This procedure also clears the Is_Known_Null and
   --  Is_Known_Non_Null and Is_Known_Valid flags in variables, constants or
   --  parameters since these are also not known to be trustable any more.
   --
   --  The Last_Assignment_Only flag is set True to clear only Last_Assignment
   --  fields and leave other fields unchanged. This is used when we encounter
   --  an unconditional flow of control change (return, goto, raise). In such
   --  cases we don't need to clear the current values, since it may be that
   --  the flow of control change occurs in a conditional context, and if it
   --  is not taken, then it is just fine to keep the current values. But the
   --  Last_Assignment field is different, if we have a sequence assign-to-v,
   --  conditional-return, assign-to-v, we do not want to complain that the
   --  second assignment clobbers the first.

   procedure Kill_Current_Values
     (Ent                  : Entity_Id;
      Last_Assignment_Only : Boolean := False);
   --  This performs the same processing as described above for the form with
   --  no argument, but for the specific entity given. The call has no effect
   --  if the entity Ent is not for an object. Last_Assignment_Only has the
   --  same meaning as for the call with no Ent.

   procedure Kill_Size_Check_Code (E : Entity_Id);
   --  Called when an address clause or pragma Import is applied to an entity.
   --  If the entity is a variable or a constant, and size check code is
   --  present, this size check code is killed, since the object will not be
   --  allocated by the program.

   function Known_To_Be_Assigned (N : Node_Id) return Boolean;
   --  The node N is an entity reference. This function determines whether the
   --  reference is for sure an assignment of the entity, returning True if
   --  so. This differs from May_Be_Lvalue in that it defaults in the other
   --  direction. Cases which may possibly be assignments but are not known to
   --  be may return True from May_Be_Lvalue, but False from this function.

   function Last_Source_Statement (HSS : Node_Id) return Node_Id;
   --  HSS is a handled statement sequence. This function returns the last
   --  statement in Statements (HSS) that has Comes_From_Source set. If no
   --  such statement exists, Empty is returned.

   function Matching_Static_Array_Bounds
     (L_Typ : Node_Id;
      R_Typ : Node_Id) return Boolean;
   --  L_Typ and R_Typ are two array types. Returns True when they have the
   --  same number of dimensions, and the same static bounds for each index
   --  position.

   procedure Mark_Coextensions (Context_Nod : Node_Id; Root_Nod : Node_Id);
   --  Given a node which designates the context of analysis and an origin in
   --  the tree, traverse from Root_Nod and mark all allocators as either
   --  dynamic or static depending on Context_Nod. Any incorrect marking is
   --  cleaned up during resolution.

   function May_Be_Lvalue (N : Node_Id) return Boolean;
   --  Determines if N could be an lvalue (e.g. an assignment left hand side).
   --  An lvalue is defined as any expression which appears in a context where
   --  a name is required by the syntax, and the identity, rather than merely
   --  the value of the node is needed (for example, the prefix of an Access
   --  attribute is in this category). Note that, as implied by the name, this
   --  test is conservative. If it cannot be sure that N is NOT an lvalue, then
   --  it returns True. It tries hard to get the answer right, but it is hard
   --  to guarantee this in all cases. Note that it is more possible to give
   --  correct answer if the tree is fully analyzed.

   function Must_Inline (Subp : Entity_Id) return Boolean;
   --  Return true if Subp must be inlined by the frontend

   function Needs_One_Actual (E : Entity_Id) return Boolean;
   --  Returns True if a function has defaults for all but its first
   --  formal. Used in Ada 2005 mode to solve the syntactic ambiguity that
   --  results from an indexing of a function call written in prefix form.

   function New_Copy_List_Tree (List : List_Id) return List_Id;
   --  Copy recursively an analyzed list of nodes. Uses New_Copy_Tree defined
   --  below. As for New_Copy_Tree, it is illegal to attempt to copy extended
   --  nodes (entities) either directly or indirectly using this function.

   function New_Copy_Tree
     (Source    : Node_Id;
      Map       : Elist_Id   := No_Elist;
      New_Sloc  : Source_Ptr := No_Location;
      New_Scope : Entity_Id  := Empty) return Node_Id;
   --  Given a node that is the root of a subtree, Copy_Tree copies the entire
   --  syntactic subtree, including recursively any descendents whose parent
   --  field references a copied node (descendents not linked to a copied node
   --  by the parent field are not copied, instead the copied tree references
   --  the same descendent as the original in this case, which is appropriate
   --  for non-syntactic fields such as Etype). The parent pointers in the
   --  copy are properly set. Copy_Tree (Empty/Error) returns Empty/Error.
   --  The one exception to the rule of not copying semantic fields is that
   --  any implicit types attached to the subtree are duplicated, so that
   --  the copy contains a distinct set of implicit type entities. Thus this
   --  function is used when it is necessary to duplicate an analyzed tree,
   --  declared in the same or some other compilation unit. This function is
   --  declared here rather than in atree because it uses semantic information
   --  in particular concerning the structure of itypes and the generation of
   --  public symbols.

   --  The Map argument, if set to a non-empty Elist, specifies a set of
   --  mappings to be applied to entities in the tree. The map has the form:
   --
   --     old entity 1
   --     new entity to replace references to entity 1
   --     old entity 2
   --     new entity to replace references to entity 2
   --     ...
   --
   --  The call destroys the contents of Map in this case
   --
   --  The parameter New_Sloc, if set to a value other than No_Location, is
   --  used as the Sloc value for all nodes in the new copy. If New_Sloc is
   --  set to its default value No_Location, then the Sloc values of the
   --  nodes in the copy are simply copied from the corresponding original.
   --
   --  The Comes_From_Source indication is unchanged if New_Sloc is set to
   --  the default No_Location value, but is reset if New_Sloc is given, since
   --  in this case the result clearly is neither a source node or an exact
   --  copy of a source node.
   --
   --  The parameter New_Scope, if set to a value other than Empty, is the
   --  value to use as the Scope for any Itypes that are copied. The most
   --  typical value for this parameter, if given, is Current_Scope.

   function New_External_Entity
     (Kind         : Entity_Kind;
      Scope_Id     : Entity_Id;
      Sloc_Value   : Source_Ptr;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat := 0;
      Prefix       : Character := ' ') return Entity_Id;
   --  This function creates an N_Defining_Identifier node for an internal
   --  created entity, such as an implicit type or subtype, or a record
   --  initialization procedure. The entity name is constructed with a call
   --  to New_External_Name (Related_Id, Suffix, Suffix_Index, Prefix), so
   --  that the generated name may be referenced as a public entry, and the
   --  Is_Public flag is set if needed (using Set_Public_Status). If the
   --  entity is for a type or subtype, the size/align fields are initialized
   --  to unknown (Uint_0).

   function New_Internal_Entity
     (Kind       : Entity_Kind;
      Scope_Id   : Entity_Id;
      Sloc_Value : Source_Ptr;
      Id_Char    : Character) return Entity_Id;
   --  This function is similar to New_External_Entity, except that the
   --  name is constructed by New_Internal_Name (Id_Char). This is used
   --  when the resulting entity does not have to be referenced as a
   --  public entity (and in this case Is_Public is not set).

   procedure Next_Actual (Actual_Id : in out Node_Id);
   pragma Inline (Next_Actual);
   --  Next_Actual (N) is equivalent to N := Next_Actual (N). Note that we
   --  inline this procedural form, but not the functional form that follows.

   function Next_Actual (Actual_Id : Node_Id) return Node_Id;
   --  Find next actual parameter in declaration order. As described for
   --  First_Actual, this is the next actual in the declaration order, not
   --  the call order, so this does not correspond to simply taking the
   --  next entry of the Parameter_Associations list. The argument is an
   --  actual previously returned by a call to First_Actual or Next_Actual.
   --  Note that the result produced is always an expression, not a parameter
   --  association node, even if named notation was used.

   function No_Scalar_Parts (T : Entity_Id) return Boolean;
   --  Tests if type T can be determined at compile time to have no scalar
   --  parts in the sense of the Valid_Scalars attribute. Returns True if
   --  this is the case, meaning that the result of Valid_Scalars is True.

   procedure Normalize_Actuals
     (N       : Node_Id;
      S       : Entity_Id;
      Report  : Boolean;
      Success : out Boolean);
   --  Reorders lists of actuals according to names of formals, value returned
   --  in Success indicates success of reordering. For more details, see body.
   --  Errors are reported only if Report is set to True.

   procedure Note_Possible_Modification (N : Node_Id; Sure : Boolean);
   --  This routine is called if the sub-expression N maybe the target of
   --  an assignment (e.g. it is the left side of an assignment, used as
   --  an out parameters, or used as prefixes of access attributes). It
   --  sets May_Be_Modified in the associated entity if there is one,
   --  taking into account the rule that in the case of renamed objects,
   --  it is the flag in the renamed object that must be set.
   --
   --  The parameter Sure is set True if the modification is sure to occur
   --  (e.g. target of assignment, or out parameter), and to False if the
   --  modification is only potential (e.g. address of entity taken).

   function Original_Corresponding_Operation (S : Entity_Id) return Entity_Id;
   --  [Ada 2012: AI05-0125-1]: If S is an inherited dispatching primitive S2,
   --  or overrides an inherited dispatching primitive S2, the original
   --  corresponding operation of S is the original corresponding operation of
   --  S2. Otherwise, it is S itself.

   function Object_Access_Level (Obj : Node_Id) return Uint;
   --  Return the accessibility level of the view of the object Obj. For
   --  convenience, qualified expressions applied to object names are also
   --  allowed as actuals for this function.

   function Original_Aspect_Name (N : Node_Id) return Name_Id;
   --  N is a pragma node or aspect specification node. This function returns
   --  the name of the pragma or aspect in original source form, taking into
   --  account possible rewrites, and also cases where a pragma comes from an
   --  aspect (in such cases, the name can be different from the pragma name,
   --  e.g. a Pre aspect generates a Precondition pragma). This also deals with
   --  the presence of 'Class, which results in one of the special names
   --  Name_uPre, Name_uPost, Name_uInvariant, or Name_uType_Invariant being
   --  returned to represent the corresponding aspects with x'Class names.

   function Primitive_Names_Match (E1, E2 : Entity_Id) return Boolean;
   --  Returns True if the names of both entities correspond with matching
   --  primitives. This routine includes support for the case in which one
   --  or both entities correspond with entities built by Derive_Subprogram
   --  with a special name to avoid being overridden (i.e. return true in case
   --  of entities with names "nameP" and "name" or vice versa).

   function Private_Component (Type_Id : Entity_Id) return Entity_Id;
   --  Returns some private component (if any) of the given Type_Id.
   --  Used to enforce the rules on visibility of operations on composite
   --  types, that depend on the full view of the component type. For a
   --  record type there may be several such components, we just return
   --  the first one.

   procedure Process_End_Label
     (N   : Node_Id;
      Typ : Character;
      Ent : Entity_Id);
   --  N is a node whose End_Label is to be processed, generating all
   --  appropriate cross-reference entries, and performing style checks
   --  for any identifier references in the end label. Typ is either
   --  'e' or 't indicating the type of the cross-reference entity
   --  (e for spec, t for body, see Lib.Xref spec for details). The
   --  parameter Ent gives the entity to which the End_Label refers,
   --  and to which cross-references are to be generated.

   function Referenced (Id : Entity_Id; Expr : Node_Id) return Boolean;
   --  Determine whether entity Id is referenced within expression Expr

   function References_Generic_Formal_Type (N : Node_Id) return Boolean;
   --  Returns True if the expression Expr contains any references to a
   --  generic type. This can only happen within a generic template.

   procedure Remove_Homonym (E : Entity_Id);
   --  Removes E from the homonym chain

   function Rep_To_Pos_Flag (E : Entity_Id; Loc : Source_Ptr) return Node_Id;
   --  This is used to construct the second argument in a call to Rep_To_Pos
   --  which is Standard_True if range checks are enabled (E is an entity to
   --  which the Range_Checks_Suppressed test is applied), and Standard_False
   --  if range checks are suppressed. Loc is the location for the node that
   --  is returned (which is a New_Occurrence of the appropriate entity).
   --
   --  Note: one might think that it would be fine to always use True and
   --  to ignore the suppress in this case, but it is generally better to
   --  believe a request to suppress exceptions if possible, and further
   --  more there is at least one case in the generated code (the code for
   --  array assignment in a loop) that depends on this suppression.

   procedure Require_Entity (N : Node_Id);
   --  N is a node which should have an entity value if it is an entity name.
   --  If not, then check if there were previous errors. If so, just fill
   --  in with Any_Id and ignore. Otherwise signal a program error exception.
   --  This is used as a defense mechanism against ill-formed trees caused by
   --  previous errors (particularly in -gnatq mode).

   function Requires_State_Refinement
     (Spec_Id : Entity_Id;
      Body_Id : Entity_Id) return Boolean;
   --  Determine whether a package denoted by its spec and body entities
   --  requires refinement of abstract states.

   function Requires_Transient_Scope (Id : Entity_Id) return Boolean;
   --  Id is a type entity. The result is True when temporaries of this type
   --  need to be wrapped in a transient scope to be reclaimed properly when a
   --  secondary stack is in use. Examples of types requiring such wrapping are
   --  controlled types and variable-sized types including unconstrained
   --  arrays.

   procedure Reset_Analyzed_Flags (N : Node_Id);
   --  Reset the Analyzed flags in all nodes of the tree whose root is N

   function Returns_Unconstrained_Type (Subp : Entity_Id) return Boolean;
   --  Return true if Subp is a function that returns an unconstrained type

   function Safe_To_Capture_Value
     (N    : Node_Id;
      Ent  : Entity_Id;
      Cond : Boolean := False) return Boolean;
   --  The caller is interested in capturing a value (either the current value,
   --  or an indication that the value is non-null) for the given entity Ent.
   --  This value can only be captured if sequential execution semantics can be
   --  properly guaranteed so that a subsequent reference will indeed be sure
   --  that this current value indication is correct. The node N is the
   --  construct which resulted in the possible capture of the value (this
   --  is used to check if we are in a conditional).
   --
   --  Cond is used to skip the test for being inside a conditional. It is used
   --  in the case of capturing values from if/while tests, which already do a
   --  proper job of handling scoping issues without this help.
   --
   --  The only entities whose values can be captured are OUT and IN OUT formal
   --  parameters, and variables unless Cond is True, in which case we also
   --  allow IN formals, loop parameters and constants, where we cannot ever
   --  capture actual value information, but we can capture conditional tests.

   function Same_Name (N1, N2 : Node_Id) return Boolean;
   --  Determine if two (possibly expanded) names are the same name. This is
   --  a purely syntactic test, and N1 and N2 need not be analyzed.

   function Same_Object (Node1, Node2 : Node_Id) return Boolean;
   --  Determine if Node1 and Node2 are known to designate the same object.
   --  This is a semantic test and both nodes must be fully analyzed. A result
   --  of True is decisively correct. A result of False does not necessarily
   --  mean that different objects are designated, just that this could not
   --  be reliably determined at compile time.

   function Same_Type (T1, T2 : Entity_Id) return Boolean;
   --  Determines if T1 and T2 represent exactly the same type. Two types
   --  are the same if they are identical, or if one is an unconstrained
   --  subtype of the other, or they are both common subtypes of the same
   --  type with identical constraints. The result returned is conservative.
   --  It is True if the types are known to be the same, but a result of
   --  False is indecisive (e.g. the compiler may not be able to tell that
   --  two constraints are identical).

   function Same_Value (Node1, Node2 : Node_Id) return Boolean;
   --  Determines if Node1 and Node2 are known to be the same value, which is
   --  true if they are both compile time known values and have the same value,
   --  or if they are the same object (in the sense of function Same_Object).
   --  A result of False does not necessarily mean they have different values,
   --  just that it is not possible to determine they have the same value.

   function Scope_Within_Or_Same (Scope1, Scope2 : Entity_Id) return Boolean;
   --  Determines if the entity Scope1 is the same as Scope2, or if it is
   --  inside it, where both entities represent scopes. Note that scopes
   --  are only partially ordered, so Scope_Within_Or_Same (A,B) and
   --  Scope_Within_Or_Same (B,A) can both be False for a given pair A,B.

   function Scope_Within (Scope1, Scope2 : Entity_Id) return Boolean;
   --  Like Scope_Within_Or_Same, except that this function returns
   --  False in the case where Scope1 and Scope2 are the same scope.

   procedure Set_Convention (E : Entity_Id; Val : Convention_Id);
   --  Same as Basic_Set_Convention, but with an extra check for access types.
   --  In particular, if E is an access-to-subprogram type, and Val is a
   --  foreign convention, then we set Can_Use_Internal_Rep to False on E.
   --  Also, if the Etype of E is set and is an anonymous access type with
   --  no convention set, this anonymous type inherits the convention of E.

   procedure Set_Current_Entity (E : Entity_Id);
   pragma Inline (Set_Current_Entity);
   --  Establish the entity E as the currently visible definition of its
   --  associated name (i.e. the Node_Id associated with its name).

   procedure Set_Debug_Info_Needed (T : Entity_Id);
   --  Sets the Debug_Info_Needed flag on entity T , and also on any entities
   --  that are needed by T (for an object, the type of the object is needed,
   --  and for a type, various subsidiary types are needed -- see body for
   --  details). Never has any effect on T if the Debug_Info_Off flag is set.
   --  This routine should always be used instead of Set_Needs_Debug_Info to
   --  ensure that subsidiary entities are properly handled.

   procedure Set_Entity_With_Checks (N : Node_Id; Val : Entity_Id);
   --  This procedure has the same calling sequence as Set_Entity, but it
   --  performs additional checks as follows:
   --
   --    If Style_Check is set, then it calls a style checking routine which
   --    can check identifier spelling style. This procedure also takes care
   --    of checking the restriction No_Implementation_Identifiers.
   --
   --    If restriction No_Abort_Statements is set, then it checks that the
   --    entity is not Ada.Task_Identification.Abort_Task.
   --
   --    If restriction No_Dynamic_Attachment is set, then it checks that the
   --    entity is not one of the restricted names for this restriction.
   --
   --    If restriction No_Implementation_Identifiers is set, then it checks
   --    that the entity is not implementation defined.

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id);
   pragma Inline (Set_Name_Entity_Id);
   --  Sets the Entity_Id value associated with the given name, which is the
   --  Id of the innermost visible entity with the given name. See the body
   --  of package Sem_Ch8 for further details on the handling of visibility.

   procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id);
   --  The arguments may be parameter associations, whose descendants
   --  are the optional formal name and the actual parameter. Positional
   --  parameters are already members of a list, and do not need to be
   --  chained separately. See also First_Actual and Next_Actual.

   procedure Set_Optimize_Alignment_Flags (E : Entity_Id);
   pragma Inline (Set_Optimize_Alignment_Flags);
   --  Sets Optimize_Alignment_Space/Time flags in E from current settings

   procedure Set_Public_Status (Id : Entity_Id);
   --  If an entity (visible or otherwise) is defined in a library
   --  package, or a package that is itself public, then this subprogram
   --  labels the entity public as well.

   procedure Set_Referenced_Modified (N : Node_Id; Out_Param : Boolean);
   --  N is the node for either a left hand side (Out_Param set to False),
   --  or an Out or In_Out parameter (Out_Param set to True). If there is
   --  an assignable entity being referenced, then the appropriate flag
   --  (Referenced_As_LHS if Out_Param is False, Referenced_As_Out_Parameter
   --  if Out_Param is True) is set True, and the other flag set False.

   procedure Set_Scope_Is_Transient (V : Boolean := True);
   --  Set the flag Is_Transient of the current scope

   procedure Set_Size_Info (T1, T2 : Entity_Id);
   pragma Inline (Set_Size_Info);
   --  Copies the Esize field and Has_Biased_Representation flag from sub(type)
   --  entity T2 to (sub)type entity T1. Also copies the Is_Unsigned_Type flag
   --  in the fixed-point and discrete cases, and also copies the alignment
   --  value from T2 to T1. It does NOT copy the RM_Size field, which must be
   --  separately set if this is required to be copied also.

   function Scope_Is_Transient return Boolean;
   --  True if the current scope is transient

   function Static_Boolean (N : Node_Id) return Uint;
   --  This function analyzes the given expression node and then resolves it
   --  as Standard.Boolean. If the result is static, then Uint_1 or Uint_0 is
   --  returned corresponding to the value, otherwise an error message is
   --  output and No_Uint is returned.

   function Static_Integer (N : Node_Id) return Uint;
   --  This function analyzes the given expression node and then resolves it
   --  as any integer type. If the result is static, then the value of the
   --  universal expression is returned, otherwise an error message is output
   --  and a value of No_Uint is returned.

   function Statically_Different (E1, E2 : Node_Id) return Boolean;
   --  Return True if it can be statically determined that the Expressions
   --  E1 and E2 refer to different objects

   function Subject_To_Loop_Entry_Attributes (N : Node_Id) return Boolean;
   --  Determine whether node N is a loop statement subject to at least one
   --  'Loop_Entry attribute.

   function Subprogram_Access_Level (Subp : Entity_Id) return Uint;
   --  Return the accessibility level of the view denoted by Subp

   function Support_Atomic_Primitives (Typ : Entity_Id) return Boolean;
   --  Return True if Typ supports the GCC built-in atomic operations (i.e. if
   --  Typ is properly sized and aligned).

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : String);
   --  Print debugging information on entry to each unit being analyzed

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id);
   --  Move a list of entities from one scope to another, and recompute
   --  Is_Public based upon the new scope.

   function Type_Access_Level (Typ : Entity_Id) return Uint;
   --  Return the accessibility level of Typ

   function Type_Without_Stream_Operation
     (T  : Entity_Id;
      Op : TSS_Name_Type := TSS_Null) return Entity_Id;
   --  AI05-0161: In Ada 2012, if the restriction No_Default_Stream_Attributes
   --  is active then we cannot generate stream subprograms for composite types
   --  with elementary subcomponents that lack user-defined stream subprograms.
   --  This predicate determines whether a type has such an elementary
   --  subcomponent. If Op is TSS_Null, a type that lacks either Read or Write
   --  prevents the construction of a composite stream operation. If Op is
   --  specified we check only for the given stream operation.

   function Unique_Defining_Entity (N : Node_Id) return Entity_Id;
   --  Return the entity which represents declaration N, so that different
   --  views of the same entity have the same unique defining entity:
   --  * package spec and body;
   --  * subprogram declaration, subprogram stub and subprogram body;
   --  * private view and full view of a type;
   --  * private view and full view of a deferred constant.
   --  In other cases, return the defining entity for N.

   function Unique_Entity (E : Entity_Id) return Entity_Id;
   --  Return the unique entity for entity E, which would be returned by
   --  Unique_Defining_Entity if applied to the enclosing declaration of E.

   function Unique_Name (E : Entity_Id) return String;
   --  Return a unique name for entity E, which could be used to identify E
   --  across compilation units.

   function Unit_Is_Visible (U : Entity_Id) return Boolean;
   --  Determine whether a compilation unit is visible in the current context,
   --  because there is a with_clause that makes the unit available. Used to
   --  provide better messages on common visiblity errors on operators.

   function Universal_Interpretation (Opnd : Node_Id) return Entity_Id;
   --  Yields Universal_Integer or Universal_Real if this is a candidate

   function Unqualify (Expr : Node_Id) return Node_Id;
   pragma Inline (Unqualify);
   --  Removes any qualifications from Expr. For example, for T1'(T2'(X)), this
   --  returns X. If Expr is not a qualified expression, returns Expr.

   function Visible_Ancestors (Typ : Entity_Id) return Elist_Id;
   --  [Ada 2012:AI-0125-1]: Collect all the visible parents and progenitors
   --  of a type extension or private extension declaration. If the full-view
   --  of private parents and progenitors is available then it is used to
   --  generate the list of visible ancestors; otherwise their partial
   --  view is added to the resulting list.

   function Within_Init_Proc return Boolean;
   --  Determines if Current_Scope is within an init proc

   function Within_Scope (E : Entity_Id; S : Entity_Id) return Boolean;
   --  Returns True if entity Id is declared within scope S

   procedure Wrong_Type (Expr : Node_Id; Expected_Type : Entity_Id);
   --  Output error message for incorrectly typed expression. Expr is the node
   --  for the incorrectly typed construct (Etype (Expr) is the type found),
   --  and Expected_Type is the entity for the expected type. Note that Expr
   --  does not have to be a subexpression, anything with an Etype field may
   --  be used.

end Sem_Util;
