------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Exp_Tss;        use Exp_Tss;
with Namet;          use Namet;
with Opt;            use Opt;
with Snames;         use Snames;
with Types;          use Types;
with Uintp;          use Uintp;
with Urealp;         use Urealp;

package Sem_Util is

   function Abstract_Interface_List (Typ : Entity_Id) return List_Id;
   --  The list of interfaces implemented by Typ. Empty if there are none,
   --  including the cases where there can't be any because e.g. the type is
   --  not tagged.

   function Acquire_Warning_Match_String (Str_Lit : Node_Id) return String;
   --  Used by pragma Warnings (Off, string), and Warn_As_Error (string) to get
   --  the given string argument, adding leading and trailing asterisks if they
   --  are not already present.  Str_Lit is the static value of the pragma
   --  argument.

   procedure Add_Access_Type_To_Process (E : Entity_Id; A : Entity_Id);
   --  Add A to the list of access types to process when expanding the
   --  freeze node of E.

   procedure Add_Global_Declaration (N : Node_Id);
   --  These procedures adds a declaration N at the library level, to be
   --  elaborated before any other code in the unit. It is used for example
   --  for the entity that marks whether a unit has been elaborated. The
   --  declaration is added to the Declarations list of the Aux_Decls_Node
   --  for the current unit. The declarations are added in the current scope,
   --  so the caller should push a new scope as required before the call.

   function Add_Suffix (E : Entity_Id; Suffix : Character) return Name_Id;
   --  Returns the name of E adding Suffix

   function Address_Integer_Convert_OK (T1, T2 : Entity_Id) return Boolean;
   --  Given two types, returns True if we are in Allow_Integer_Address mode
   --  and one of the types is (a descendant of) System.Address (and this type
   --  is private), and the other type is any integer type.

   function Address_Value (N : Node_Id) return Node_Id;
   --  Return the underlying value of the expression N of an address clause

   function Addressable (V : Uint) return Boolean;
   function Addressable (V : Int)  return Boolean;
   pragma Inline (Addressable);
   --  Returns True if the value of V is the word size or an addressable factor
   --  or multiple of the word size (typically 8, 16, 32, 64 or 128).

   procedure Aggregate_Constraint_Checks
     (Exp       : Node_Id;
      Check_Typ : Entity_Id);
   --  Checks expression Exp against subtype Check_Typ. If Exp is an aggregate
   --  and Check_Typ a constrained record type with discriminants, we generate
   --  the appropriate discriminant checks. If Exp is an array aggregate then
   --  emit the appropriate length checks. If Exp is a scalar type, or a string
   --  literal, Exp is changed into Check_Typ'(Exp) to ensure that range checks
   --  are performed at run time. Also used for expressions in the argument of
   --  'Update, which shares some of the features of an aggregate.

   function Alignment_In_Bits (E : Entity_Id) return Uint;
   --  If the alignment of the type or object E is currently known to the
   --  compiler, then this function returns the alignment value in bits.
   --  Otherwise Uint_0 is returned, indicating that the alignment of the
   --  entity is not yet known to the compiler.

   function All_Composite_Constraints_Static (Constr : Node_Id) return Boolean;
   --  Used to implement pragma Restrictions (No_Dynamic_Sized_Objects).
   --  Given a constraint or subtree of a constraint on a composite
   --  subtype/object, returns True if there are no nonstatic constraints,
   --  which might cause objects to be created with dynamic size.
   --  Called for subtype declarations (including implicit ones created for
   --  subtype indications in object declarations, as well as discriminated
   --  record aggregate cases). For record aggregates, only records containing
   --  discriminant-dependent arrays matter, because the discriminants must be
   --  static when governing a variant part. Access discriminants are
   --  irrelevant. Also called for array aggregates, but only named notation,
   --  because those are the only dynamic cases.

   procedure Append_Entity_Name (Buf : in out Bounded_String; E : Entity_Id);
   --  Construct a user-readable expanded name for E, for printing in messages,
   --  such as run-time errors for unhandled exceptions. Names created for
   --  internal use are not included. The name is appended to Buf.

   procedure Append_Inherited_Subprogram (S : Entity_Id);
   --  If the parent of the operation is declared in the visible part of
   --  the current scope, the inherited operation is visible even though the
   --  derived type that inherits the operation may be completed in the private
   --  part of the current package.

   procedure Apply_Compile_Time_Constraint_Error
     (N            : Node_Id;
      Msg          : String;
      Reason       : RT_Exception_Code;
      Ent          : Entity_Id  := Empty;
      Typ          : Entity_Id  := Empty;
      Loc          : Source_Ptr := No_Location;
      Warn         : Boolean    := False;
      Emit_Message : Boolean    := True);
   --  N is a subexpression that will raise Constraint_Error when evaluated
   --  at run time. Msg is a message that explains the reason for raising the
   --  exception. The last character is ? if the message is always a warning,
   --  even in Ada 95, and is not a ? if the message represents an illegality
   --  (because of violation of static expression rules) in Ada 95 (but not
   --  in Ada 83). Typically this routine posts all messages at the Sloc of
   --  node N. However, if Loc /= No_Location, Loc is the Sloc used to output
   --  the message. After posting the appropriate message, this routine
   --  replaces the expression with an appropriate N_Raise_Constraint_Error
   --  node using the given Reason code. This node is then marked as being
   --  static if the original node is static, but sets the flag
   --  Raises_Constraint_Error, preventing further evaluation. The error
   --  message may contain a } or & insertion character. This normally
   --  references Etype (N), unless the Ent argument is given explicitly, in
   --  which case it is used instead. The type of the raise node that is built
   --  is normally Etype (N), but if the Typ parameter is present, this is used
   --  instead. Warn is normally False. If it is True then the message is
   --  treated as a warning even though it does not end with a ? (this is used
   --  when the caller wants to parameterize whether an error or warning is
   --  given), or when the message should be treated as a warning even when
   --  SPARK_Mode is On (which otherwise would force an error).
   --  If Emit_Message is False, then do not emit any message.

   function Async_Readers_Enabled (Id : Entity_Id) return Boolean;
   --  Id should be the entity of a state abstraction, an object, or a type.
   --  Returns True iff Id is subject to external property Async_Readers.

   function Async_Writers_Enabled (Id : Entity_Id) return Boolean;
   --  Id should be the entity of a state abstraction, an object, or a type.
   --  Returns True iff Id is subject to external property Async_Writers.

   function Available_Full_View_Of_Component (T : Entity_Id) return Boolean;
   --  If at the point of declaration an array type has a private or limited
   --  component, several array operations are not available on the type, and
   --  the array type is flagged accordingly. If in the immediate scope of
   --  the array type the component becomes non-private or non-limited, these
   --  operations become available. This can happen if the scopes of both types
   --  are open, and the scope of the array is not outside the scope of the
   --  component.

   procedure Bad_Aspect
     (N    : Node_Id;
      Nam  : Name_Id;
      Warn : Boolean := False);
   --  Called when node N is expected to contain a valid aspect name, and
   --  Nam is found instead. If Warn is set True this is a warning, else this
   --  is an error.

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

   function Begin_Keyword_Location (N : Node_Id) return Source_Ptr;
   --  Given block statement, entry body, package body, subprogram body, or
   --  task body N, return the closest source location to the "begin" keyword.

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

   --  Handling of inherited primitives whose ancestors have class-wide
   --  pre/postconditions.

   --  If a primitive operation of a parent type has a class-wide pre/post-
   --  condition that includes calls to other primitives, and that operation
   --  is inherited by a descendant type that also overrides some of these
   --  other primitives, the condition that applies to the inherited
   --  operation has a modified condition in which the overridden primitives
   --  have been replaced by the primitives of the descendent type. A call
   --  to the inherited operation cannot be simply a call to the parent
   --  operation (with an appropriate conversion) as is the case for other
   --  inherited operations, but must appear with a wrapper subprogram to which
   --  the modified conditions apply. Furthermore the call to the parent
   --  operation must not be subject to the original class-wide condition,
   --  given that modified conditions apply. To implement these semantics
   --  economically we create a subprogram body (a "class-wide clone") to
   --  which no pre/postconditions apply, and we create bodies for the
   --  original and the inherited operation that have their respective
   --  pre/postconditions and simply call the clone. The following operations
   --  take care of constructing declaration and body of the clone, and
   --  building the calls to it within the appropriate wrappers.

   procedure Build_Constrained_Itype
     (N              : Node_Id;
      Typ            : Entity_Id;
      New_Assoc_List : List_Id);
   --  Build a constrained itype for the newly created record aggregate N and
   --  set it as a type of N. The itype will have Typ as its base type and
   --  will be constrained by the values of discriminants from the component
   --  association list New_Assoc_List.

   --  ??? This code used to be pretty much a copy of Build_Subtype, but now
   --  those two routines behave differently for types with unknown
   --  discriminants. They are both exported in from this package in the hope
   --  to eventually unify them (a not duplicate them even more until then).

   --  ??? Performance WARNING. The current implementation creates a new itype
   --  for all aggregates whose base type is discriminated. This means that
   --  for record aggregates nested inside an array aggregate we will create
   --  a new itype for each record aggregate if the array component type has
   --  discriminants. For large aggregates this may be a problem. What should
   --  be done in this case is to reuse itypes as much as possible.

   function Build_Default_Subtype
     (T : Entity_Id;
      N : Node_Id) return Entity_Id;
   --  If T is an unconstrained type with defaulted discriminants, build a
   --  subtype constrained by the default values, insert the subtype
   --  declaration in the tree before N, and return the entity of that
   --  subtype. Otherwise, simply return T.

   function Build_Default_Subtype_OK (T : Entity_Id) return Boolean;
   --  When analyzing object declarations, it is possible, in some cases, to
   --  build subtypes for discriminated types. This is worthwhile to avoid the
   --  backend allocating the maximum possible size for objects of the type.
   --  In particular, when T is limited, the discriminants and therefore the
   --  size of an object of type T cannot change. Furthermore, if T is definite
   --  with initialized defaulted discriminants, we are able and want to build
   --  a constrained subtype of the right size.

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

   function Build_Overriding_Spec
     (Op  : Entity_Id;
      Typ : Entity_Id) return Node_Id;
   --  Build a subprogram specification for the wrapper of an inherited
   --  operation with a modified pre- or postcondition (See AI12-0113).
   --  Op is the parent operation, and Typ is the descendant type that
   --  inherits the operation.

   function Build_Subtype
     (Related_Node : Node_Id;
      Loc          : Source_Ptr;
      Typ          : Entity_Id;
      Constraints  : List_Id)
      return Entity_Id;
   --  Typ is an array or discriminated type, Constraints is a list of
   --  constraints that apply to Typ. This routine builds the constrained
   --  subtype using Loc as the source location and attached this subtype
   --  declaration to Related_Node. The returned subtype inherits predicates
   --  from Typ.

   --  ??? The routine is mostly a duplicate of Build_Constrained_Itype, so be
   --  careful which of the two better suits your needs (and certainly do not
   --  duplicate their code).

   function Cannot_Raise_Constraint_Error (Expr : Node_Id) return Boolean;
   --  Returns True if the expression cannot possibly raise Constraint_Error.
   --  The response is conservative in the sense that a result of False does
   --  not necessarily mean that CE could be raised, but a response of True
   --  means that for sure CE cannot be raised.

   procedure Check_Ambiguous_Aggregate (Call : Node_Id);
   --  Additional information on an ambiguous call in Ada_2022 when a
   --  subprogram call has an actual that is an aggregate, and the
   --  presence of container aggregates (or types with the corresponding
   --  aspect)  provides an additional interpretation. Message indicates
   --  that an aggregate actual should carry a type qualification.

   procedure Check_Dynamically_Tagged_Expression
     (Expr        : Node_Id;
      Typ         : Entity_Id;
      Related_Nod : Node_Id);
   --  Check wrong use of dynamically tagged expression

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id);
   --  Verify that the full declaration of type T has been seen. If not, place
   --  error message on node N. Used in object declarations, type conversions
   --  and qualified expressions.

   procedure Check_Function_With_Address_Parameter (Subp_Id : Entity_Id);
   --  A subprogram that has an Address parameter and is declared in a Pure
   --  package is not considered Pure, because the parameter may be used as a
   --  pointer and the referenced data may change even if the address value
   --  itself does not.
   --  If the programmer gave an explicit Pure_Function pragma, then we respect
   --  the pragma and leave the subprogram Pure.

   procedure Check_Function_Writable_Actuals (N : Node_Id);
   --  (Ada 2012): If the construct N has two or more direct constituents that
   --  are names or expressions whose evaluation may occur in an arbitrary
   --  order, at least one of which contains a function call with an in out or
   --  out parameter, then the construct is legal only if: for each name that
   --  is passed as a parameter of mode in out or out to some inner function
   --  call C2 (not including the construct N itself), there is no other name
   --  anywhere within a direct constituent of the construct C other than
   --  the one containing C2, that is known to refer to the same object (RM
   --  6.4.1(6.18-6.19)).

   procedure Check_Implicit_Dereference (N : Node_Id; Typ : Entity_Id);
   --  AI05-139-2: Accessors and iterators for containers. This procedure
   --  checks whether T is a reference type, and if so it adds an interprettion
   --  to N whose type is the designated type of the reference_discriminant.
   --  If N is a generalized indexing operation, the interpretation is added
   --  both to the corresponding function call, and to the indexing node.

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

   procedure Check_No_Hidden_State (Id : Entity_Id);
   --  Determine whether object or state Id introduces a hidden state. If this
   --  is the case, emit an error.

   procedure Check_Inherited_Nonoverridable_Aspects
     (Inheritor      : Entity_Id;
      Interface_List : List_Id;
      Parent_Type    : Entity_Id);
   --  Verify consistency of inherited nonoverridable aspects
   --  when aspects are inherited from more than one source.
   --  Parent_Type may be void (e.g., for a tagged task/protected type
   --  whose declaration includes a non-empty interface list).
   --  In the error case, error message is associate with Inheritor;
   --  Inheritor parameter is otherwise unused.

   function Check_Parents (N : Node_Id; List : Elist_Id) return Boolean;
   --  Return True if all the occurrences of subtree N referencing entities in
   --  the given List have the right value in their Parent field.

   procedure Check_Part_Of_Reference (Var_Id : Entity_Id; Ref : Node_Id);
   --  Verify the legality of reference Ref to variable Var_Id when the
   --  variable is a constituent of a single protected/task type.

   procedure Check_Potentially_Blocking_Operation (N : Node_Id);
   --  N is one of the statement forms that is a potentially blocking
   --  operation. If it appears within a protected action, emit warning.

   procedure Check_Previous_Null_Procedure
     (Decl : Node_Id;
      Prev : Entity_Id);
   --  A null procedure or a subprogram renaming can complete a previous
   --  declaration, unless that previous declaration is itself a null
   --  procedure. This must be treated specially because the analysis of
   --  the null procedure leaves the corresponding entity as having no
   --  completion, because its completion is provided by a generated body
   --  inserted after all other declarations.

   procedure Check_Result_And_Post_State (Subp_Id : Entity_Id);
   --  Determine whether the contract of subprogram Subp_Id mentions attribute
   --  'Result and it contains an expression that evaluates differently in pre-
   --  and post-state.

   procedure Check_State_Refinements
     (Context      : Node_Id;
      Is_Main_Unit : Boolean := False);
   --  Verify that all abstract states declared in a block statement, entry
   --  body, package body, protected body, subprogram body, task body, or a
   --  package declaration denoted by Context have proper refinement. Emit an
   --  error if this is not the case. Flag Is_Main_Unit should be set when
   --  Context denotes the main compilation unit.

   procedure Check_Unused_Body_States (Body_Id : Entity_Id);
   --  Verify that all abstract states and objects declared in the state space
   --  of package body Body_Id are used as constituents. Emit an error if this
   --  is not the case.

   procedure Check_Unprotected_Access
     (Context : Node_Id;
      Expr    : Node_Id);
   --  Check whether the expression is a pointer to a protected component,
   --  and the context is external to the protected operation, to warn against
   --  a possible unlocked access to data.

   function Choice_List (N : Node_Id) return List_Id;
   --  Utility to retrieve the choices of a Component_Association or the
   --  Discrete_Choices of an Iterated_Component_Association. For various
   --  reasons these nodes have a different structure even though they play
   --  similar roles in array aggregates.

   type Condition_Kind is
     (Ignored_Class_Precondition,
      Ignored_Class_Postcondition,
      Class_Precondition,
      Class_Postcondition);
   --  Kind of class-wide conditions

   function Class_Condition
     (Kind : Condition_Kind;
      Subp : Entity_Id) return Node_Id;
   --  Class-wide Kind condition of Subp

   function Collect_Body_States (Body_Id : Entity_Id) return Elist_Id;
   --  Gather the entities of all abstract states and objects declared in the
   --  body state space of package body Body_Id.

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
     (N         : Node_Id;
      Msg       : String;
      Ent       : Entity_Id  := Empty;
      Loc       : Source_Ptr := No_Location;
      Warn      : Boolean    := False;
      Extra_Msg : String     := "") return Node_Id;
   --  This is similar to Apply_Compile_Time_Constraint_Error in that it
   --  generates a warning (or error) message in the same manner, but it does
   --  not replace any nodes. For convenience, the function always returns its
   --  first argument. The message is a warning if the message ends with ?, or
   --  we are operating in Ada 83 mode, or the Warn parameter is set to True.
   --  If Extra_Msg is not a null string, then it's associated with N and
   --  emitted immediately after the main message (and before output of any
   --  message indicating that Constraint_Error will be raised).

   procedure Compute_Returns_By_Ref (Func : Entity_Id);
   --  Set the Returns_By_Ref flag on Func if appropriate

   generic
      with function Predicate (Typ : Entity_Id) return Boolean;
   function Collect_Types_In_Hierarchy
     (Typ                : Entity_Id;
      Examine_Components : Boolean := False) return Elist_Id;
   --  Inspect the ancestor and progenitor types of Typ and Typ itself -
   --  collecting those for which function Predicate is True. The resulting
   --  list is ordered in a type-to-ultimate-ancestor fashion.

   --  When Examine_Components is True, components types in the hierarchy also
   --  get collected.

   procedure Conditional_Delay (New_Ent, Old_Ent : Entity_Id);
   --  Sets the Has_Delayed_Freeze flag of New_Ent if the Delayed_Freeze flag
   --  of Old_Ent is set and Old_Ent has not yet been Frozen (i.e. Is_Frozen is
   --  False).

   function Copy_Component_List
     (R_Typ : Entity_Id;
      Loc   : Source_Ptr) return List_Id;
   --  Copy components from record type R_Typ that come from source. Used to
   --  create a new compatible record type. Loc is the source location assigned
   --  to the created nodes.

   procedure Copy_Ghost_Aspect (From : Node_Id; To : Node_Id);
   --  Copy the Ghost aspect if present in the aspect specifications of node
   --  From to node To. On entry it is assumed that To does not have aspect
   --  specifications. If From has no aspects, the routine has no effect.

   function Copy_Parameter_List (Subp_Id : Entity_Id) return List_Id;
   --  Utility to create a parameter profile for a new subprogram spec, when
   --  the subprogram has a body that acts as spec. This is done for some cases
   --  of inlining, and for private protected ops. Also used to create bodies
   --  for stubbed subprograms.

   procedure Copy_SPARK_Mode_Aspect (From : Node_Id; To : Node_Id);
   --  Copy the SPARK_Mode aspect if present in the aspect specifications
   --  of node From to node To. On entry it is assumed that To does not have
   --  aspect specifications. If From has no aspects, the routine has no
   --  effect.

   function Copy_Subprogram_Spec
     (Spec     : Node_Id;
      New_Sloc : Source_Ptr := No_Location) return Node_Id;
   --  Replicate a function or a procedure specification denoted by Spec. The
   --  resulting tree is an exact duplicate of the original tree. New entities
   --  are created for the unit name and the formal parameters. For definition
   --  of New_Sloc, see the comment for New_Copy_Tree.

   function Corresponding_Generic_Type (T : Entity_Id) return Entity_Id;
   --  If a type is a generic actual type, return the corresponding formal in
   --  the generic parent unit. There is no direct link in the tree for this
   --  attribute, except in the case of formal private and derived types.
   --  Possible optimization???

   function Corresponding_Primitive_Op
     (Ancestor_Op     : Entity_Id;
      Descendant_Type : Entity_Id) return Entity_Id;
   --  Given a primitive subprogram of a first type and a (distinct)
   --  descendant type of that type, find the corresponding primitive
   --  subprogram of the descendant type.

   function Current_Entity (N : Node_Id) return Entity_Id;
   pragma Inline (Current_Entity);
   --  Find the currently visible definition for a given identifier, that is to
   --  say the first entry in the visibility chain for the Chars of N.

   function Current_Entity_In_Scope (N : Name_Id) return Entity_Id;
   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id;
   --  Find whether there is a previous definition for name or identifier N in
   --  the current scope. Because declarations for a scope are not necessarily
   --  contiguous (e.g. for packages) the first entry on the visibility chain
   --  for N is not necessarily in the current scope.

   function Current_Scope return Entity_Id;
   --  Get entity representing current scope

   function Current_Scope_No_Loops return Entity_Id;
   --  Return the current scope ignoring internally generated loops

   procedure Add_Block_Identifier
     (N     : Node_Id;
      Id    : out Entity_Id;
      Scope : Entity_Id := Current_Scope);
   --  Given a block statement N, generate an internal E_Block label and make
   --  it the identifier of the block. Scope denotes the scope in which the
   --  generated entity Id is created and defaults to the current scope. If the
   --  block already has an identifier, Id returns the entity of its label.

   function Current_Subprogram return Entity_Id;
   --  Returns current enclosing subprogram. If Current_Scope is a subprogram,
   --  then that is what is returned, otherwise the Enclosing_Subprogram of the
   --  Current_Scope is returned. The returned value is Empty if this is called
   --  from a library package which is not within any subprogram.

   function CW_Or_Needs_Finalization (Typ : Entity_Id) return Boolean;
   --  True if Typ is a class-wide type or requires finalization actions. Same
   --  as Needs_Finalization except with pragma Restrictions (No_Finalization),
   --  in which case we know that class-wide objects do not need finalization.

   function Defining_Entity (N : Node_Id) return Entity_Id;
   --  Given a declaration N, returns the associated defining entity. If the
   --  declaration has a specification, the entity is obtained from the
   --  specification. If the declaration has a defining unit name, then the
   --  defining entity is obtained from the defining unit name ignoring any
   --  child unit prefixes.
   --
   --  Iterator loops also have a defining entity, which holds the list of
   --  local entities declared during loop expansion. These entities need
   --  debugging information, generated through Qualify_Entity_Names, and
   --  the loop declaration must be placed in the table Name_Qualify_Units.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Defining_Entity_Or_Empty (N : Node_Id) return Entity_Id;
   --  This is equivalent to Defining_Entity but it returns Empty for nodes
   --  without an entity instead of raising Program_Error.

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
   --  writable (RM 2012 6.4.1(6.4/3)).

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

   function Derivation_Too_Early_To_Inherit
     (Typ : Entity_Id; Streaming_Op : TSS_Name_Type) return Boolean;
   --  Returns True if Typ is a derived type, the given Streaming_Op
   --  (one of Read, Write, Input, or Output) is explicitly specified
   --  for Typ's parent type, and that attribute specification is *not*
   --  inherited by Typ because the declaration of Typ precedes that
   --  of the attribute specification.

   function Designate_Same_Unit
     (Name1 : Node_Id;
      Name2 : Node_Id) return  Boolean;
   --  Returns True if Name1 and Name2 designate the same unit name; each of
   --  these names is supposed to be a selected component name, an expanded
   --  name, a defining program unit name or an identifier.

   procedure Diagnose_Iterated_Component_Association (N : Node_Id);
   --  Emit an error if iterated component association N is actually an illegal
   --  quantified expression lacking a quantifier.

   function Discriminated_Size (Comp : Entity_Id) return Boolean;
   --  If a component size is not static then a warning will be emitted
   --  in Ravenscar or other restricted contexts. When a component is non-
   --  static because of a discriminant constraint we can specialize the
   --  warning by mentioning discriminants explicitly. This was created for
   --  private components of protected objects, but is generally useful when
   --  restriction No_Implicit_Heap_Allocation is active.

   function Effective_Reads_Enabled (Id : Entity_Id) return Boolean;
   --  Id should be the entity of a state abstraction, an object, or a type.
   --  Returns True iff Id is subject to external property Effective_Reads.

   function Effective_Writes_Enabled (Id : Entity_Id) return Boolean;
   --  Id should be the entity of a state abstraction, an object, or a type.
   --  Returns True iff Id is subject to external property Effective_Writes.

   function Enclosing_Comp_Unit_Node (N : Node_Id) return Node_Id;
   --  Returns the enclosing N_Compilation_Unit node that is the root of a
   --  subtree containing N.

   function Enclosing_CPP_Parent (Typ : Entity_Id) return Entity_Id;
   --  Returns the closest ancestor of Typ that is a CPP type.

   function Enclosing_Declaration (N : Node_Id) return Node_Id;
   --  Returns the declaration node enclosing N (including possibly N itself),
   --  if any, or Empty otherwise.

   function Enclosing_Declaration_Or_Statement (N : Node_Id) return Node_Id;
   --  Return the nearest enclosing declaration or statement that houses
   --  arbitrary node N.

   function Enclosing_Generic_Body (N : Node_Id) return Node_Id;
   --  Returns the Node_Id associated with the innermost enclosing generic
   --  body, if any. If none, then returns Empty.

   function Enclosing_Generic_Unit (N : Node_Id) return Node_Id;
   --  Returns the Node_Id associated with the innermost enclosing generic
   --  unit, if any. If none, then returns Empty.

   function Enclosing_HSS (Stmt : Node_Id) return Node_Id;
   --  Returns the nearest handled sequence of statements that encloses a given
   --  statement, or Empty.

   function Enclosing_Lib_Unit_Entity
     (E : Entity_Id := Current_Scope) return Entity_Id;
   --  Returns the entity of enclosing library unit node which is the root of
   --  the current scope (which must not be Standard_Standard, and the caller
   --  is responsible for ensuring this condition) or other specified entity.

   function Enclosing_Lib_Unit_Node (N : Node_Id) return Node_Id;
   --  Returns the N_Compilation_Unit node of the library unit that is directly
   --  or indirectly (through a subunit) at the root of a subtree containing
   --  N. This may be either the same as Enclosing_Comp_Unit_Node, or if
   --  Enclosing_Comp_Unit_Node returns a subunit, then the corresponding
   --  library unit. If no such item is found, returns Empty.

   function Enclosing_Package (N : Node_Or_Entity_Id) return Entity_Id;
   --  Utility function to return the Ada entity of the package enclosing
   --  the entity or node N, if any. Returns Empty if no enclosing package.

   function Enclosing_Package_Or_Subprogram (E : Entity_Id) return Entity_Id;
   --  Returns the entity of the package or subprogram enclosing E, if any.
   --  Returns Empty if no enclosing package or subprogram.

   function Enclosing_Subprogram (N : Node_Or_Entity_Id) return Entity_Id;
   --  Utility function to return the Ada entity of the subprogram enclosing
   --  N, if any. Returns Empty if no enclosing subprogram.

   function End_Keyword_Location (N : Node_Id) return Source_Ptr;
   --  Given block statement, entry body, package body, package declaration,
   --  protected body, [single] protected type declaration, subprogram body,
   --  task body, or [single] task type declaration N, return the closest
   --  source location of the "end" keyword.

   procedure Ensure_Freeze_Node (E : Entity_Id);
   --  Make sure a freeze node is allocated for entity E. If necessary, build
   --  and initialize a new freeze node and set Has_Delayed_Freeze True for E.

   procedure Enter_Name (Def_Id : Entity_Id);
   --  Insert new name in symbol table of current scope with check for
   --  duplications (error message is issued if a conflict is found).
   --  Note: Enter_Name is not used for most overloadable entities, instead
   --  they are entered using Sem_Ch6.Enter_Overloaded_Entity. However,
   --  this is used for SOME overloadable entities, such as enumeration
   --  literals and certain operator symbols.

   function Entity_Of (N : Node_Id) return Entity_Id;
   --  Obtain the entity of arbitrary node N. If N is a renaming, return the
   --  entity of the earliest renamed source abstract state or whole object.
   --  If no suitable entity is available, return Empty. This routine carries
   --  out actions that are tied to SPARK semantics.

   function Exceptions_OK return Boolean;
   --  Determine whether exceptions are allowed to be caught, propagated, or
   --  raised.

   procedure Explain_Limited_Type (T : Entity_Id; N : Node_Id);
   --  This procedure is called after issuing a message complaining about an
   --  inappropriate use of limited type T. If useful, it adds additional
   --  continuation lines to the message explaining why type T is limited.
   --  Messages are placed at node N.

   function Expression_Of_Expression_Function
     (Subp : Entity_Id) return Node_Id;
   --  Return the expression of expression function Subp

   type Extensions_Visible_Mode is
     (Extensions_Visible_None,
      --  Extensions_Visible does not yield a mode when SPARK_Mode is off. This
      --  value acts as a default in a non-SPARK compilation.

      Extensions_Visible_False,
      --  A value of "False" signifies that Extensions_Visible is either
      --  missing or the pragma is present and the value of its Boolean
      --  expression is False.

      Extensions_Visible_True);
      --  A value of "True" signifies that Extensions_Visible is present and
      --  the value of its Boolean expression is True.

   function Extensions_Visible_Status
     (Id : Entity_Id) return Extensions_Visible_Mode;
   --  Given the entity of a subprogram or formal parameter subject to pragma
   --  Extensions_Visible, return the Boolean value denoted by the expression
   --  of the pragma.

   procedure Find_Actual
     (N      : Node_Id;
      Formal : out Entity_Id;
      Call   : out Node_Id);
   --  Determines if the node N is an actual parameter of a function or a
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

   function Find_DIC_Type (Typ : Entity_Id) return Entity_Id;
   --  Subsidiary to all Build_DIC_Procedure_xxx routines. Find the type which
   --  defines the Default_Initial_Condition pragma of type Typ. This is either
   --  Typ itself or a parent type when the pragma is inherited.

   function Find_Enclosing_Iterator_Loop (Id : Entity_Id) return Entity_Id;
   --  Find the nearest iterator loop which encloses arbitrary entity Id. If
   --  such a loop exists, return the entity of its identifier (E_Loop scope),
   --  otherwise return Empty.

   function Find_Enclosing_Scope (N : Node_Id) return Scope_Kind_Id with
     Post => Find_Enclosing_Scope'Result /= N;
   --  Find the nearest scope which encloses arbitrary node N

   function Find_Loop_In_Conditional_Block (N : Node_Id) return Node_Id;
   --  Find the nested loop statement in a conditional block. Loops subject to
   --  attribute 'Loop_Entry are transformed into blocks. Parts of the original
   --  loop are nested within the block.

   procedure Find_Overlaid_Entity
     (N   : Node_Id;
      Ent : out Entity_Id;
      Off : out Boolean);
   --  The node N should be an address representation clause. Determines if the
   --  target expression is the address of an entity with an optional offset.
   --  If so, set Ent to the entity and, if there is an offset, set Off to
   --  True, otherwise to False. If it is not possible to determine that the
   --  address is of this form, then set Ent to Empty.

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
   --  entity of an abstract state, object, or package instantiation. Placement
   --  captures the precise placement of the item in the enclosing state space.
   --  If the state space is that of a package, Pack_Id denotes its entity,
   --  otherwise Pack_Id is Empty.

   function Find_Primitive_Eq (Typ : Entity_Id) return Entity_Id;
   --  Locate primitive equality for type if it exists. Return Empty if it is
   --  not available.

   function Find_Specific_Type (CW : Entity_Id) return Entity_Id;
   --  Find specific type of a class-wide type, and handle the case of an
   --  incomplete type coming either from a limited_with clause or from an
   --  incomplete type declaration. If resulting type is private return its
   --  full view.

   function Find_Static_Alternative (N : Node_Id) return Node_Id;
   --  N is a case statement whose expression is a compile-time value.
   --  Determine the alternative chosen, so that the code of non-selected
   --  alternatives, and the warnings that may apply to them, are removed.

   function First_Actual (Node : Node_Id) return Node_Id;
   --  Node is an N_Function_Call, N_Procedure_Call_Statement or
   --  N_Entry_Call_Statement node. The result returned is the first actual
   --  parameter in declaration order (not the order of parameters as they
   --  appeared in the source, which can be quite different as a result of the
   --  use of named parameters). Empty is returned for a call with no
   --  parameters. The procedure for iterating through the actuals in
   --  declaration order is to use this function to find the first actual, and
   --  then use Next_Actual to obtain the next actual in declaration order.
   --  Note that the value returned is always the expression (not the
   --  N_Parameter_Association nodes, even if named association is used).

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function First_Global
     (Subp        : Entity_Id;
      Global_Mode : Name_Id;
      Refined     : Boolean := False) return Node_Id;
   --  Returns the first global item of mode Global_Mode (which can be
   --  Name_Input, Name_Output, Name_In_Out or Name_Proof_In) associated to
   --  subprogram Subp, or Empty otherwise. If Refined is True, the global item
   --  is retrieved from the Refined_Global aspect/pragma associated to the
   --  body of Subp if present. Next_Global can be used to get the next global
   --  item with the same mode.

   function Fix_Msg (Id : Entity_Id; Msg : String) return String;
   --  Replace all occurrences of a particular word in string Msg depending on
   --  the Ekind of Id as follows:
   --    * Replace "subprogram" with
   --      - "entry" when Id is an entry [family]
   --      - "task type" when Id is a single task object, task type or task
   --         body.
   --    * Replace "protected" with
   --      - "task" when Id is a single task object, task type or task body
   --  All other non-matching words remain as is

   function From_Nested_Package (T : Entity_Id) return Boolean;
   --  A type declared in a nested package may be frozen by a declaration
   --  appearing after the package but before the package is frozen. If the
   --  type has aspects that generate subprograms, these may contain references
   --  to entities local to the nested package. In that case the package must
   --  be installed on the scope stack to prevent spurious visibility errors.

   procedure Gather_Components
     (Typ                   : Entity_Id;
      Comp_List             : Node_Id;
      Governed_By           : List_Id;
      Into                  : Elist_Id;
      Report_Errors         : out Boolean;
      Allow_Compile_Time    : Boolean := False;
      Include_Interface_Tag : Boolean := False);
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
   --     insufficiently static (see body for details of what that means).
   --
   --    Allow_Compile_Time if set to True, allows compile time known values in
   --     Governed_By expressions in addition to static expressions.
   --
   --    Include_Interface_Tag if set to True, gather any interface tag
   --     component, otherwise exclude them.
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
   --  Return the body node for a stub

   function Get_Cursor_Type
     (Aspect : Node_Id;
      Typ    : Entity_Id) return Entity_Id;
   --  Find Cursor type in scope of type Typ with Iterable aspect, by locating
   --  primitive operation First. For use in resolving the other primitive
   --  operations of an Iterable type and expanding loops and quantified
   --  expressions over formal containers.

   function Get_Cursor_Type (Typ : Entity_Id) return Entity_Id;
   --  Find Cursor type in scope of type Typ with Iterable aspect, by locating
   --  primitive operation First. For use after resolving the primitive
   --  operations of an Iterable type.

   function Get_Default_External_Name (E : Node_Or_Entity_Id) return Node_Id;
   --  This is used to construct the string literal node representing a
   --  default external name, i.e. one that is constructed from the name of an
   --  entity, or (in the case of extended DEC import/export pragmas) an
   --  identifier provided as the external name. Letters in the name are
   --  according to the setting of Opt.External_Name_Default_Casing.

   function Get_Enclosing_Object (N : Node_Id) return Entity_Id;
   --  If expression N references a part of an object, return this object.
   --  Otherwise return Empty. Expression N should have been resolved already.

   function Get_Enclosing_Deep_Object (N : Node_Id) return Entity_Id;
   --  If expression N references a reachable part of an object (as defined in
   --  SPARK RM 6.9), return this object. Otherwise return Empty. It is similar
   --  to Get_Enclosing_Object, but treats pointer dereference like component
   --  selection. Expression N should have been resolved already.

   function Get_Generic_Entity (N : Node_Id) return Entity_Id;
   --  Returns the true generic entity in an instantiation. If the name in the
   --  instantiation is a renaming, the function returns the renamed generic.

   function Get_Incomplete_View_Of_Ancestor (E : Entity_Id) return Entity_Id;
   --  Implements the notion introduced ever-so briefly in RM 7.3.1 (5.2/3):
   --  in a child unit a derived type is within the derivation class of an
   --  ancestor declared in a parent unit, even if there is an intermediate
   --  derivation that does not see the full view of that ancestor.

   procedure Get_Index_Bounds
     (N             : Node_Id;
      L             : out Node_Id;
      H             : out Node_Id;
      Use_Full_View : Boolean := False);
   --  This procedure assigns to L and H respectively the values of the low and
   --  high bounds of node N, which must be a range, subtype indication, or the
   --  name of a scalar subtype. The result in L, H may be set to Error if
   --  there was an earlier error in the range.
   --  Use_Full_View is intended for use by clients other than the compiler
   --  (specifically, gnat2scil) to indicate that we want the full view if
   --  the index type turns out to be a partial view; this case should not
   --  arise during normal compilation of semantically correct programs.

   type Range_Nodes is record
      First, Last : Node_Id; -- First and Last nodes of a discrete_range
   end record;

   type Range_Values is record
      First, Last : Uint; -- First and Last values of a discrete_range
   end record;

   function Get_Index_Bounds
     (N             : Node_Id;
      Use_Full_View : Boolean := False) return Range_Nodes;
   --  Same as the above procedure, but returns the result as a record.
   --  ???This should probably replace the procedure.

   function Get_Index_Bounds
     (N             : Node_Id;
      Use_Full_View : Boolean := False) return Range_Values;
   --  Same as the above function, but returns the values, which must be known
   --  at compile time.

   procedure Get_Interfacing_Aspects
     (Iface_Asp : Node_Id;
      Conv_Asp  : out Node_Id;
      EN_Asp    : out Node_Id;
      Expo_Asp  : out Node_Id;
      Imp_Asp   : out Node_Id;
      LN_Asp    : out Node_Id;
      Do_Checks : Boolean := False);
   --  Given a single interfacing aspect Iface_Asp, retrieve other interfacing
   --  aspects that apply to the same related entity. The aspects considered by
   --  this routine are as follows:
   --
   --    Conv_Asp - aspect Convention
   --    EN_Asp   - aspect External_Name
   --    Expo_Asp - aspect Export
   --    Imp_Asp  - aspect Import
   --    LN_Asp   - aspect Link_Name
   --
   --  When flag Do_Checks is set, this routine will flag duplicate uses of
   --  aspects.

   function Get_Enum_Lit_From_Pos
     (T   : Entity_Id;
      Pos : Uint;
      Loc : Source_Ptr) return Node_Id;
   --  This function returns an identifier denoting the E_Enumeration_Literal
   --  entity for the specified value from the enumeration type or subtype T.
   --  The second argument is the Pos value. Constraint_Error is raised if
   --  argument Pos is not in range. The third argument supplies a source
   --  location for constructed nodes returned by this function. If No_Location
   --  is supplied as source location, the location of the returned node is
   --  copied from the original source location for the enumeration literal,
   --  when available.

   function Get_Iterable_Type_Primitive
     (Typ : Entity_Id;
      Nam : Name_Id) return Entity_Id;
   --  Retrieve one of the primitives First, Last, Next, Previous, Has_Element,
   --  Element from the value of the Iterable aspect of a type.

   function Get_Library_Unit_Name (Decl_Node : Node_Id) return String_Id;
   --  Return the full expanded name of the library unit declared by Decl_Node

   function Get_Max_Queue_Length (Id : Entity_Id) return Uint;
   --  Return the argument of pragma Max_Queue_Length or zero if the annotation
   --  is not present. It is assumed that Id denotes an entry.

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id;
   pragma Inline (Get_Name_Entity_Id);
   --  An entity value is associated with each name in the name table. The
   --  Get_Name_Entity_Id function fetches the Entity_Id of this entity, which
   --  is the innermost visible entity with the given name. See the body of
   --  Sem_Ch8 for further details on handling of entity visibility.

   function Get_Name_From_CTC_Pragma (N : Node_Id) return String_Id;
   --  Return the Name component of Test_Case pragma N
   --  Bad name now that this no longer applies to Contract_Case ???

   function Get_Parent_Entity (Unit : Node_Id) return Entity_Id;
   --  Get defining entity of parent unit of a child unit. In most cases this
   --  is the defining entity of the unit, but for a child instance whose
   --  parent needs a body for inlining, the instantiation node of the parent
   --  has not yet been rewritten as a package declaration, and the entity has
   --  to be retrieved from the Instance_Spec of the unit.

   function Get_Pragma_Id (N : Node_Id) return Pragma_Id;
   pragma Inline (Get_Pragma_Id);
   --  Obtains the Pragma_Id from Pragma_Name_Unmapped (N)

   function Get_Qualified_Name
     (Id     : Entity_Id;
      Suffix : Entity_Id := Empty) return Name_Id;
   --  Obtain the fully qualified form of entity Id. The format is:
   --    scope_of_id-1__scope_of_id__chars_of_id__chars_of_suffix

   function Get_Qualified_Name
     (Nam    : Name_Id;
      Suffix : Name_Id   := No_Name;
      Scop   : Entity_Id := Current_Scope) return Name_Id;
   --  Obtain the fully qualified form of name Nam assuming it appears in scope
   --  Scop. The format is:
   --    scop-1__scop__nam__suffix

   procedure Get_Reason_String (N : Node_Id);
   --  Recursive routine to analyze reason argument for pragma Warnings. The
   --  value of the reason argument is appended to the current string using
   --  Store_String_Chars. The reason argument is expected to be a string
   --  literal or concatenation of string literals. An error is given for
   --  any other form.

   function Get_Reference_Discriminant (Typ : Entity_Id) return Entity_Id;
   --  If Typ has Implicit_Dereference, return discriminant specified in the
   --  corresponding aspect.

   function Get_Referenced_Object (N : Node_Id) return Node_Id;
   --  Given an arbitrary node, return the renamed object if the node
   --  represents a renamed object; otherwise return the node unchanged.
   --  The node can represent an arbitrary expression or any other kind of
   --  node (such as the name of a type).

   function Get_Renamed_Entity (E : Entity_Id) return Entity_Id;
   --  Given an entity for an exception, package, subprogram or generic unit,
   --  returns the ultimately renamed entity if this is a renaming. If this is
   --  not a renamed entity, returns its argument. It is an error to call this
   --  with any other kind of entity.

   function Get_Return_Object (N : Node_Id) return Entity_Id;
   --  Given an extended return statement, return the corresponding return
   --  object, identified as the one for which Is_Return_Object = True.

   function Get_Subprogram_Entity (Nod : Node_Id) return Entity_Id;
   --  Nod is either a procedure call statement, or a function call, or an
   --  accept statement node. This procedure finds the Entity_Id of the related
   --  subprogram or entry and returns it, or if no subprogram can be found,
   --  returns Empty.

   function Get_Task_Body_Procedure (E : Entity_Id) return Entity_Id;
   --  Given an entity for a task type or subtype, retrieves the
   --  Task_Body_Procedure field from the corresponding task type declaration.

   function Get_User_Defined_Equality (E : Entity_Id) return Entity_Id;
   --  For a type entity, return the entity of the primitive equality function
   --  for the type if it exists, otherwise return Empty.

   procedure Get_Views
     (Typ       : Entity_Id;
      Priv_Typ  : out Entity_Id;
      Full_Typ  : out Entity_Id;
      UFull_Typ : out Entity_Id;
      CRec_Typ  : out Entity_Id);
   --  Obtain the partial and full views of type Typ and in addition any extra
   --  types the full views may have. The return entities are as follows:
   --
   --    Priv_Typ  - the partial view (a private type)
   --    Full_Typ  - the full view
   --    UFull_Typ - the underlying full view, if the full view is private
   --    CRec_Typ  - the corresponding record type of the full views

   function Get_Fullest_View
     (E           : Entity_Id;
      Include_PAT : Boolean := True;
      Recurse     : Boolean := True) return Entity_Id;
   --  Get the fullest possible view of E, looking through private, limited,
   --  packed array and other implementation types. If Include_PAT is False,
   --  don't look inside packed array types. If Recurse is False, just
   --  go down one level (so it's no longer the "fullest" view).

   type Alignment_Result is (Known_Compatible, Unknown, Known_Incompatible);
   --  Result of Has_Compatible_Alignment test, description found below. Note
   --  that the values are arranged in increasing order of problematicness.

   function Has_Compatible_Alignment
     (Obj         : Entity_Id;
      Expr        : Node_Id;
      Layout_Done : Boolean) return Alignment_Result;
   --  Obj is an object entity, and expr is a node for an object reference. If
   --  the alignment of the object referenced by Expr is known to be compatible
   --  with the alignment of Obj (i.e. is larger or the same), then the result
   --  is Known_Compatible. If the alignment of the object referenced by Expr
   --  is known to be less than the alignment of Obj, then Known_Incompatible
   --  is returned. If neither condition can be reliably established at compile
   --  time, then Unknown is returned. If Layout_Done is True, the function can
   --  assume that the information on size and alignment of types and objects
   --  is present in the tree. This is used to determine if alignment checks
   --  are required for address clauses (Layout_Done is False in this case) as
   --  well as to issue appropriate warnings for them in the post compilation
   --  phase (Layout_Done is True in this case).
   --
   --  Note: Known_Incompatible does not mean that at run time the alignment
   --  of Expr is known to be wrong for Obj, just that it can be determined
   --  that alignments have been explicitly or implicitly specified which are
   --  incompatible (whereas Unknown means that even this is not known). The
   --  appropriate reaction of a caller to Known_Incompatible is to treat it as
   --  Unknown, but issue a warning that there may be an alignment error.

   function Has_Declarations (N : Node_Id) return Boolean;
   --  Determines if the node can have declarations

   function Has_Defaulted_Discriminants (Typ : Entity_Id) return Boolean;
   --  Simple predicate to test for defaulted discriminants

   function Has_Denormals (E : Entity_Id) return Boolean;
   --  Determines if the floating-point type E supports denormal numbers.
   --  Returns False if E is not a floating-point type.

   function Has_Discriminant_Dependent_Constraint
     (Comp : Entity_Id) return Boolean;
   --  Returns True if and only if Comp has a constrained subtype that depends
   --  on a discriminant.

   function Has_Effectively_Volatile_Profile
     (Subp_Id : Entity_Id) return Boolean;
   --  Determine whether subprogram Subp_Id has an effectively volatile formal
   --  parameter for reading or returns an effectively volatile value for
   --  reading.

   function Has_Full_Default_Initialization (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ defines "full default initialization" as
   --  specified by SPARK RM 3.1. To qualify as such, the type must be
   --    * A scalar type with specified Default_Value
   --    * An array-of-scalar type with specified Default_Component_Value
   --    * An array type whose element type defines full default initialization
   --    * A protected type, record type or type extension whose components
   --      either include a default expression or have a type which defines
   --      full default initialization. In the case of type extensions, the
   --      parent type defines full default initialization.
   --    * A task type
   --    * A private type with pragma Default_Initial_Condition that provides
   --      full default initialization.
   --  This function is not used in GNATprove anymore, but is used in CodePeer.

   function Has_Fully_Default_Initializing_DIC_Pragma
     (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ has a suitable Default_Initial_Condition
   --  pragma which provides the full default initialization of the type.

   function Has_Inferable_Discriminants (N : Node_Id) return Boolean;
   --  Ada 2005 (AI-216): A view of an Unchecked_Union object has inferable
   --  discriminants if it has a constrained nominal type, unless the object
   --  is a component of an enclosing Unchecked_Union object that is subject
   --  to a per-object constraint and the enclosing object lacks inferable
   --  discriminants.
   --
   --  An expression of an Unchecked_Union type has inferable discriminants
   --  if it is either a name of an object with inferable discriminants or a
   --  qualified expression whose subtype mark denotes a constrained subtype.

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

   function Has_Max_Queue_Length (Id : Entity_Id) return Boolean;
   --  Determine whether Id is subject to pragma Max_Queue_Length. It is
   --  assumed that Id denotes an entry.

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

   function Has_Non_Null_Refinement (Id : Entity_Id) return Boolean;
   --  Determine whether abstract state Id has at least one nonnull constituent
   --  as expressed in pragma Refined_State. This function does not take into
   --  account the visible refinement region of abstract state Id.

   function Has_Non_Trivial_Precondition (Subp : Entity_Id) return Boolean;
   --  Determine whether subprogram Subp has a class-wide precondition that is
   --  not statically True.

   function Has_Null_Body (Proc_Id : Entity_Id) return Boolean;
   --  Determine whether the body of procedure Proc_Id contains a sole null
   --  statement, possibly followed by an optional return. Used to optimize
   --  useless calls to assertion checks.

   function Has_Null_Exclusion (N : Node_Id) return Boolean;
   --  Determine whether node N has a null exclusion

   function Has_Null_Refinement (Id : Entity_Id) return Boolean;
   --  Determine whether abstract state Id has a null refinement as expressed
   --  in pragma Refined_State. This function does not take into account the
   --  visible refinement region of abstract state Id.

   function Has_Non_Null_Statements (L : List_Id) return Boolean;
   --  Return True if L has non-null statements

   function Side_Effect_Free_Statements (L : List_Id) return Boolean;
   --  Return True if L has no statements with side effects

   function Side_Effect_Free_Loop (N : Node_Id) return Boolean;
   --  Return True if the loop has no side effect and can therefore be
   --  marked for removal. Return False if N is not a N_Loop_Statement.

   function Is_Container_Aggregate (Exp : Node_Id) return Boolean;
   --  Is the given expression a container aggregate?

   function Is_Function_With_Side_Effects (Subp : Entity_Id) return Boolean;
   --  Return True if Subp is a function with side effects, ie. it has a
   --  (direct or inherited) pragma Side_Effects with static value True.

   function Is_Newly_Constructed
     (Exp : Node_Id; Context_Requires_NC : Boolean) return Boolean;
   --  Indicates whether a given expression is "newly constructed" (RM 4.4).
   --  Context_Requires_NC determines the result returned for cases like a
   --  raise expression or a conditional expression where some-but-not-all
   --  operative constituents are newly constructed. Thus, this is a
   --  somewhat unusual predicate in that the result required in order to
   --  satisfy whatever legality rule is being checked can influence the
   --  result of the predicate. Context_Requires_NC might be True for
   --  something like the "newly constructed" rule for a limited expression
   --  of a return statement, and False for something like the
   --  "newly constructed" rule pertaining to a limited object renaming in a
   --  declare expression. Eventually, the code to implement every
   --  RM legality rule requiring/prohibiting a "newly constructed" expression
   --  should be implemented by calling this function; that's not done yet.
   --  The function name doesn't quite match the RM definition of the term if
   --  Context_Requires_NC = False; in that case, "Might_Be_Newly_Constructed"
   --  might be a more accurate name.

   function Is_Prim_Of_Abst_Type_With_Nonstatic_CW_Pre_Post
     (Subp : Entity_Id) return Boolean;
   --  Return True if Subp is a primitive of an abstract type, where the
   --  primitive has a class-wide pre- or postcondition whose expression
   --  is nonstatic.

   function Has_Overriding_Initialize (T : Entity_Id) return Boolean;
   --  Predicate to determine whether a controlled type has a user-defined
   --  Initialize primitive (and, in Ada 2012, whether that primitive is
   --  non-null), which causes the type to not have preelaborable
   --  initialization.

   function Has_Preelaborable_Initialization
     (E                 : Entity_Id;
      Preelab_Init_Expr : Node_Id := Empty) return Boolean;
   --  Return True iff type E has preelaborable initialization as defined in
   --  Ada 2005 (see AI-161 for details of the definition of this attribute).
   --  If Preelab_Init_Expr is present, indicates that the function should
   --  presume that for any subcomponent of E that is of a formal private or
   --  derived type that is referenced by a Preelaborable_Initialization
   --  attribute within the expression Preelab_Init_Expr, the formal type has
   --  preelaborable initialization (RM 10.2.1(11.8/5) and AI12-0409).

   function Has_Prefix (N : Node_Id) return Boolean;
   --  Return True if N has attribute Prefix

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean;
   --  Check if a type has a (sub)component of a private type that has not
   --  yet received a full declaration.

   function Has_Relaxed_Initialization (E : Entity_Id) return Boolean;
   --  Returns True iff entity E is subject to the Relaxed_Initialization
   --  aspect. Entity E can be either type, variable, constant, subprogram or
   --  entry. For private types and deferred constants E should be the private
   --  view, because aspect can only be attached there.

   function Has_Signed_Zeros (E : Entity_Id) return Boolean;
   --  Determines if the floating-point type E supports signed zeros.
   --  Returns False if E is not a floating-point type.

   function Has_Significant_Contract (Subp_Id : Entity_Id) return Boolean;
   --  Determine whether subprogram [body] Subp_Id has a significant contract.
   --  All subprograms have a N_Contract node, but this does not mean that the
   --  contract is useful.

   function Has_Static_Array_Bounds (Typ : Node_Id) return Boolean;
   --  Return whether an array type has static bounds

   function Has_Static_Empty_Array_Bounds (Typ : Node_Id) return Boolean;
   --  Return whether array type Typ has static empty bounds

   function Has_Static_Non_Empty_Array_Bounds (Typ : Node_Id) return Boolean;
   --  Determine whether array type Typ has static non-empty bounds

   function Has_Stream (T : Entity_Id) return Boolean;
   --  Tests if type T is derived from Ada.Streams.Root_Stream_Type, or in the
   --  case of a composite type, has a component for which this predicate is
   --  True, and if so returns True. Otherwise a result of False means that
   --  there is no Stream type in sight. For a private type, the test is
   --  applied to the underlying type (or returns False if there is no
   --  underlying type).

   function Has_Suffix (E : Entity_Id; Suffix : Character) return Boolean;
   --  Returns true if the last character of E is Suffix. Used in Assertions.

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean;
   --  Returns True if Typ is a composite type (array or record) that is either
   --  a tagged type or has a subcomponent that is tagged. Returns False for a
   --  noncomposite type, or if no tagged subcomponents are present.

   function Has_Undefined_Reference (Expr : Node_Id) return Boolean;
   --  Given arbitrary expression Expr, determine whether it contains at
   --  least one name whose entity is Any_Id.

   function Has_Effectively_Volatile_Component
     (Typ : Entity_Id) return Boolean;
   --  Given arbitrary type Typ, determine whether it contains at least one
   --  effectively volatile component.

   function Has_Enabled_Aspect (Id : Entity_Id; A : Aspect_Id) return Boolean
     with Pre => A in Boolean_Aspects;
   --  Returns True if a Boolean-valued aspect is enabled on entity Id; i.e. it
   --  is present and either has no aspect definition or its aspect definition
   --  statically evaluates to True.

   function Has_Volatile_Component (Typ : Entity_Id) return Boolean;
   --  Given arbitrary type Typ, determine whether it contains at least one
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

   function Implicitly_Designated_Type (Typ : Entity_Id) return Entity_Id;
   --  Called when Typ is the type of the prefix of an implicit dereference.
   --  Return the designated type of Typ, taking into account that this type
   --  may be a limited view, when the nonlimited view is visible.

   function In_Assertion_Expression_Pragma (N : Node_Id) return Boolean;
   --  Returns True if node N appears within a pragma that acts as an assertion
   --  expression. See Sem_Prag for the list of qualifying pragmas.

   function In_Check_Node (N : Node_Id) return Boolean;
   --  Return True if N is part of a N_Raise_xxx_Error node

   function In_Generic_Formal_Package (E : Entity_Id) return Boolean;
   --  Returns True if entity E is inside a generic formal package

   function In_Generic_Scope (E : Entity_Id) return Boolean;
   --  Returns True if entity E is inside a generic scope

   function In_Instance return Boolean;
   --  Returns True if the current scope is within a generic instance

   function In_Instance_Body return Boolean;
   --  Returns True if current scope is within the body of an instance, where
   --  several semantic checks (e.g. accessibility checks) are relaxed.

   function In_Instance_Not_Visible return Boolean;
   --  Returns True if current scope is with the private part or the body of
   --  an instance. Other semantic checks are suppressed in this context.

   function In_Instance_Visible_Part
     (Id : Entity_Id := Current_Scope) return Boolean;
   --  Returns True if arbitrary entity Id is within the visible part of a
   --  package instance, where several additional semantic checks apply.

   function In_Package_Body return Boolean;
   --  Returns True if current scope is within a package body

   function In_Pragma_Expression (N : Node_Id; Nam : Name_Id) return Boolean;
   --  Returns true if the expression N occurs within a pragma with name Nam

   function In_Pre_Post_Condition
     (N : Node_Id; Class_Wide_Only : Boolean := False) return Boolean;
   --  Returns True if node N appears within a pre/postcondition pragma. Note
   --  the pragma Check equivalents are NOT considered. If Class_Wide_Only is
   --  True, then tests for N appearing within a class-wide pre/postcondition.

   function In_Quantified_Expression (N : Node_Id) return Boolean;
   --  Returns true if the expression N occurs within a quantified expression

   function In_Return_Value (Expr : Node_Id) return Boolean;
   --  Returns true if the expression Expr occurs within a simple return
   --  statement or is part of an assignment to the return object in an
   --  extended return statement.

   function In_Reverse_Storage_Order_Object (N : Node_Id) return Boolean;
   --  Returns True if N denotes a component or subcomponent in a record or
   --  array that has Reverse_Storage_Order.

   function In_Same_Declarative_Part
     (Context : Node_Id;
      N       : Node_Id) return Boolean;
   --  True if the node N appears within the same declarative part denoted by
   --  the node Context.

   function In_Subprogram_Or_Concurrent_Unit return Boolean;
   --  Determines if the current scope is within a subprogram compilation unit
   --  (inside a subprogram declaration, subprogram body, or generic subprogram
   --  declaration) or within a task or protected body. The test is for
   --  appearing anywhere within such a construct (that is it does not need
   --  to be directly within).

   function In_Subtree (N : Node_Id; Root : Node_Id) return Boolean;
   --  Determine whether node N is within the subtree rooted at Root

   function In_Subtree
     (N     : Node_Id;
      Root1 : Node_Id;
      Root2 : Node_Id) return Boolean;
   --  Determine whether node N is within the subtree rooted at Root1 or Root2.
   --  This version is more efficient than calling the single root version of
   --  Is_Subtree twice.

   function In_Statement_Condition_With_Actions (N : Node_Id) return Boolean;
   --  Returns true if the expression N occurs within the condition of a
   --  statement node with actions. Subsidiary to inlining for GNATprove, where
   --  inlining of function calls in such expressions would expand the called
   --  body into actions list of the condition node. GNATprove cannot yet cope
   --  with such a complex AST.

   function In_Visible_Part (Scope_Id : Entity_Id) return Boolean;
   --  Determine whether a declaration occurs within the visible part of a
   --  package specification. The package must be on the scope stack, and the
   --  corresponding private part must not.

   function Incomplete_Or_Partial_View (Id : Entity_Id) return Entity_Id;
   --  Given the entity of a constant or a type, retrieve the incomplete or
   --  partial view of the same entity. Note that Id may not have a partial
   --  view in which case the function returns Empty.

   function Incomplete_View_From_Limited_With
     (Typ : Entity_Id) return Entity_Id;
   --  Typ is a type entity. This normally returns Typ. However, if there is
   --  an incomplete view of this entity that comes from a limited-with'ed
   --  package, then this returns that incomplete view.

   function Indexed_Component_Bit_Offset (N : Node_Id) return Uint;
   --  Given an N_Indexed_Component node, return the first bit position of the
   --  component if it is known at compile time. A value of No_Uint means that
   --  either the value is not yet known before back-end processing or it is
   --  not known at compile time after back-end processing.

   procedure Inherit_Predicate_Flags
     (Subt, Par  : Entity_Id;
      Only_Flags : Boolean := False);
   --  Propagate static and dynamic predicate flags from a parent to the
   --  subtype in a subtype declaration with and without constraints, or from
   --  a parent to the derived type in a derived type declaration. Only_Flags
   --  is True in the case of a derived type declaration to inherit only the
   --  flags, not the predicate functions.

   procedure Inherit_Rep_Item_Chain (Typ : Entity_Id; From_Typ : Entity_Id);
   --  Inherit the rep item chain of type From_Typ without clobbering any
   --  existing rep items on Typ's chain. Typ is the destination type.

   function Inherits_From_Tagged_Full_View (Typ : Entity_Id) return Boolean;
   pragma Inline (Inherits_From_Tagged_Full_View);
   --  Return True if Typ is an untagged private type completed with a
   --  derivation of an untagged private type declaration whose full view
   --  is a tagged type.

   procedure Insert_Explicit_Dereference (N : Node_Id);
   --  In a context that requires a composite or subprogram type and where a
   --  prefix is an access type, rewrite the access type node N (which is the
   --  prefix, e.g. of an indexed component) as an explicit dereference.

   procedure Inspect_Deferred_Constant_Completion (Decl : Node_Id);
   --  If Decl is a constant object declaration without a default value, check
   --  whether it has been completed by a full constant declaration or an
   --  Import pragma. Emit an error message if that is not the case.

   procedure Inspect_Deferred_Constant_Completion (Decls : List_Id);
   --  Examine all deferred constants in the declaration list Decls and check
   --  whether they have been completed by a full constant declaration or an
   --  Import pragma. Emit the error message if that is not the case.

   procedure Install_Elaboration_Model (Unit_Id : Entity_Id);
   --  Install the elaboration model specified by pragma Elaboration_Checks
   --  associated with compilation unit Unit_Id. No action is taken when the
   --  unit lacks such pragma.

   procedure Install_Generic_Formals (Subp_Id : Entity_Id);
   --  Install both the generic formal parameters and the formal parameters of
   --  generic subprogram Subp_Id into visibility.

   procedure Install_SPARK_Mode (Mode : SPARK_Mode_Type; Prag : Node_Id);
   --  Establish the SPARK_Mode and SPARK_Mode_Pragma currently in effect

   function Invalid_Scalar_Value
     (Loc      : Source_Ptr;
      Scal_Typ : Scalar_Id) return Node_Id;
   --  Obtain the invalid value for scalar type Scal_Typ as either specified by
   --  pragma Initialize_Scalars or by the binder. Return an expression created
   --  at source location Loc, which denotes the invalid value.

   function Is_Access_Subprogram_Wrapper (E : Entity_Id) return Boolean;
   --  True if E is the constructed wrapper for an access_to_subprogram
   --  type with Pre/Postconditions.

   function Is_Access_Variable (E : Entity_Id) return Boolean;
   --  Determines if type E is an access-to-variable

   function Is_Actual_In_Out_Parameter (N : Node_Id) return Boolean;
   --  Determines if N is an actual parameter of in-out mode in a subprogram
   --  call.

   function Is_Actual_Out_Parameter (N : Node_Id) return Boolean;
   --  Determines if N is an actual parameter of out mode in a subprogram call

   function Is_Actual_Out_Or_In_Out_Parameter (N : Node_Id) return Boolean;
   --  Determines if N is an actual parameter of out or in out mode in a
   --  subprogram call.

   function Is_Actual_Parameter (N : Node_Id) return Boolean;
   --  Determines if N is an actual parameter in a subprogram or entry call

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
   --  True if package E1 is an ancestor of E2 other than E2 itself

   function Is_Atomic_Object (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a reference to an atomic
   --  object as per RM C.6(7) and the crucial remark in RM C.6(8).

   function Is_Attribute_Loop_Entry (N : Node_Id) return Boolean;
   --  Determine whether node N denotes attribute 'Loop_Entry

   function Is_Attribute_Old (N : Node_Id) return Boolean;
   --  Determine whether node N denotes attribute 'Old

   function Is_Attribute_Result (N : Node_Id) return Boolean;
   --  Determine whether node N denotes attribute 'Result

   function Is_Attribute_Update (N : Node_Id) return Boolean;
   --  Determine whether node N denotes attribute 'Update

   function Is_Body_Or_Package_Declaration (N : Node_Id) return Boolean
     with Inline;
   --  Determine whether node N denotes a body or a package declaration

   function Is_Bounded_String (T : Entity_Id) return Boolean;
   --  True if T is a bounded string type. Used to make sure "=" composes
   --  properly for bounded string types (see 4.5.2(32.1/1)).

   function Is_By_Protected_Procedure (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id denotes a procedure with synchronization
   --  kind By_Protected_Procedure.

   function Is_Confirming (Aspect : Nonoverridable_Aspect_Id;
                           Aspect_Spec_1, Aspect_Spec_2 : Node_Id)
                          return Boolean;
   --  Returns true if the two specifications of the given
   --  nonoverridable aspect are compatible.

   function Is_Conjunction_Of_Formal_Preelab_Init_Attributes
     (Expr : Node_Id) return Boolean;
   --  Returns True if Expr is a Preelaborable_Initialization attribute applied
   --  to a formal type, or a sequence of two or more such attributes connected
   --  by "and" operators, or if the Original_Node of Expr or its constituents
   --  is such an attribute.

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

   function Is_Contract_Annotation (Item : Node_Id) return Boolean;
   --  Determine whether aspect specification or pragma Item is a contract
   --  annotation.

   function Is_Controlling_Limited_Procedure
     (Proc_Nam : Entity_Id) return Boolean;
   --  Ada 2005 (AI-345): Determine whether Proc_Nam is a primitive procedure
   --  of a limited interface with a controlling first parameter.

   function Is_CPP_Constructor_Call (N : Node_Id) return Boolean;
   --  Returns True if N is a call to a CPP constructor

   function Is_CCT_Instance
     (Ref_Id     : Entity_Id;
      Context_Id : Entity_Id) return Boolean;
   --  Subsidiary to the analysis of pragmas [Refined_]Depends and [Refined_]
   --  Global; also used when analyzing default expressions of protected and
   --  record components. Determine whether entity Ref_Id (which must represent
   --  either a protected type or a task type) denotes the current instance of
   --  a concurrent type. Context_Id denotes the associated context where the
   --  pragma appears.

   function Is_Child_Or_Sibling
     (Pack_1 : Entity_Id;
      Pack_2 : Entity_Id) return Boolean;
   --  Determine the following relations between two arbitrary packages:
   --    1) One package is the parent of a child package
   --    2) Both packages are siblings and share a common parent

   function Is_Concurrent_Interface (T : Entity_Id) return Boolean;
   --  First determine whether type T is an interface and then check whether
   --  it is of protected, synchronized or task kind.

   function Is_Current_Instance (N : Node_Id) return Boolean;
   --  Predicate is true if N legally denotes a type name within its own
   --  declaration. Prior to Ada 2012 this covered only synchronized type
   --  declarations. In Ada 2012 it also covers type and subtype declarations
   --  with aspects: Invariant, Predicate, and Default_Initial_Condition.

   function Is_Current_Instance_Reference_In_Type_Aspect
     (N : Node_Id) return Boolean;
   --  True if N is a reference to a current instance object that occurs within
   --  an aspect_specification for a type or subtype. In this case N will be
   --  a formal parameter of a subprogram created for a predicate, invariant,
   --  or Default_Initial_Condition aspect.

   function Is_Declaration
     (N                : Node_Id;
      Body_OK          : Boolean := True;
      Concurrent_OK    : Boolean := True;
      Formal_OK        : Boolean := True;
      Generic_OK       : Boolean := True;
      Instantiation_OK : Boolean := True;
      Renaming_OK      : Boolean := True;
      Stub_OK          : Boolean := True;
      Subprogram_OK    : Boolean := True;
      Type_OK          : Boolean := True) return Boolean;
   --  Determine whether arbitrary node N denotes a declaration depending
   --  on the allowed subsets of declarations. Set the following flags to
   --  consider specific subsets of declarations:
   --
   --    * Body_OK - body declarations
   --
   --    * Concurrent_OK - concurrent type declarations
   --
   --    * Formal_OK - formal declarations
   --
   --    * Generic_OK - generic declarations, including generic renamings
   --
   --    * Instantiation_OK - generic instantiations
   --
   --    * Renaming_OK - renaming declarations, including generic renamings
   --
   --    * Stub_OK - stub declarations
   --
   --    * Subprogram_OK - entry, expression function, and subprogram
   --      declarations.
   --
   --    * Type_OK - type declarations, including concurrent types

   function Is_Declared_Within_Variant (Comp : Entity_Id) return Boolean;
   --  Returns True iff component Comp is declared within a variant part

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

   function Is_Descendant_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean;
   --  Returns True if type T1 is a descendant of type T2, and false otherwise.
   --  This is the RM definition, a type is a descendant of another type if it
   --  is the same type or is derived from a descendant of the other type.

   function Is_Descendant_Of_Suspension_Object
     (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ is a descendant of type Suspension_Object
   --  defined in Ada.Synchronous_Task_Control. This version is different from
   --  Is_Descendant_Of as the detection of Suspension_Object does not involve
   --  an entity and by extension a call to RTSfind.

   function Is_Double_Precision_Floating_Point_Type
     (E : Entity_Id) return Boolean;
   --  Return whether E is a double precision floating point type,
   --  characterized by:
   --  . machine_radix = 2
   --  . machine_mantissa = 53
   --  . machine_emax = 2**10
   --  . machine_emin = 3 - machine_emax

   function Is_Effectively_Volatile
     (Id               : Entity_Id;
      Ignore_Protected : Boolean := False) return Boolean;
   --  Determine whether a type or object denoted by entity Id is effectively
   --  volatile (SPARK RM 7.1.2). To qualify as such, the entity must be either
   --    * Volatile without No_Caching
   --    * An array type subject to aspect Volatile_Components
   --    * An array type whose component type is effectively volatile
   --    * A protected type
   --    * Descendant of type Ada.Synchronous_Task_Control.Suspension_Object
   --
   --  If Ignore_Protected is True, then a protected object/type is treated
   --  like a non-protected record object/type for computing the result of
   --  this query.

   function Is_Effectively_Volatile_For_Reading
     (Id               : Entity_Id;
      Ignore_Protected : Boolean := False) return Boolean;
   --  Determine whether a type or object denoted by entity Id is effectively
   --  volatile for reading (SPARK RM 7.1.2). To qualify as such, the entity
   --  must be either
   --    * Volatile without No_Caching and have Async_Writers or
   --      Effective_Reads set to True
   --    * An array type subject to aspect Volatile_Components, unless it has
   --      Async_Writers and Effective_Reads set to False
   --    * An array type whose component type is effectively volatile for
   --      reading
   --    * A protected type
   --    * Descendant of type Ada.Synchronous_Task_Control.Suspension_Object
   --
   --  If Ignore_Protected is True, then a protected object/type is treated
   --  like a non-protected record object/type for computing the result of
   --  this query.

   function Is_Effectively_Volatile_Object
     (N : Node_Id) return Boolean;
   --  Determine whether an arbitrary node denotes an effectively volatile
   --  object (SPARK RM 7.1.2).

   function Is_Effectively_Volatile_Object_For_Reading
     (N : Node_Id) return Boolean;
   --  Determine whether an arbitrary node denotes an effectively volatile
   --  object for reading (SPARK RM 7.1.2).

   function Is_Entity_Of_Quantified_Expression (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id is the entity of a quantified expression

   function Is_Entry_Body (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id is the body entity of an entry [family]

   function Is_Entry_Declaration (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id is the spec entity of an entry [family]

   function Is_Explicitly_Aliased (N : Node_Id) return Boolean;
   --  Determine if a given node N is an explicitly aliased formal parameter.

   function Is_Expanded_Priority_Attribute (E : Entity_Id) return Boolean;
   --  Check whether a function in a call is an expanded priority attribute,
   --  which is transformed into an Rtsfind call to Get_Ceiling. This expansion
   --  does not take place in a configurable runtime.

   function Is_Expression_Function (Subp : Entity_Id) return Boolean;
   --  Determine whether subprogram [body] Subp denotes an expression function

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Is_Expression_Function_Or_Completion
     (Subp : Entity_Id) return Boolean;
   --  Determine whether subprogram [body] Subp denotes an expression function
   --  or is completed by an expression function body.

   function Is_Extended_Precision_Floating_Point_Type
     (E : Entity_Id) return Boolean;
   --  Return whether E is an extended precision floating point type,
   --  characterized by:
   --  . machine_radix = 2
   --  . machine_mantissa = 64
   --  . machine_emax = 2**14
   --  . machine_emin = 3 - machine_emax

   function Is_EVF_Expression (N : Node_Id) return Boolean;
   --  Determine whether node N denotes a reference to a formal parameter of
   --  a specific tagged type whose related subprogram is subject to pragma
   --  Extensions_Visible with value "False" (SPARK RM 6.1.7). Several other
   --  constructs fall under this category:
   --    1) A qualified expression whose operand is EVF
   --    2) A type conversion whose operand is EVF
   --    3) An if expression with at least one EVF dependent_expression
   --    4) A case expression with at least one EVF dependent_expression

   function Is_False (U : Opt_Ubool) return Boolean;
   pragma Inline (Is_False);
   --  True if U is Boolean'Pos (False) (i.e. Uint_0)

   function Is_True (U : Opt_Ubool) return Boolean;
   pragma Inline (Is_True);
   --  True if U is Boolean'Pos (True) (i.e. Uint_1). Also True if U is
   --  No_Uint; we allow No_Uint because Static_Boolean returns that in
   --  case of error. It doesn't really matter whether the error case is
   --  considered True or False, but we don't want this to blow up in that
   --  case.

   function Is_Fixed_Model_Number (U : Ureal; T : Entity_Id) return Boolean;
   --  Returns True iff the number U is a model number of the fixed-point type
   --  T, i.e. if it is an exact multiple of Small.

   function Is_Full_Access_Object (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a reference to a full access
   --  object as per Ada 2022 RM C.6(8.2).

   function Is_Fully_Initialized_Type (Typ : Entity_Id) return Boolean;
   --  Typ is a type entity. This function returns true if this type is fully
   --  initialized, meaning that an object of the type is fully initialized.
   --  Note that initialization resulting from use of pragma Normalize_Scalars
   --  does not count. Note that this is only used for the purpose of issuing
   --  warnings for objects that are potentially referenced uninitialized. This
   --  means that the result returned is not crucial, but should err on the
   --  side of thinking things are fully initialized if it does not know.

   function Is_Generic_Declaration_Or_Body (Decl : Node_Id) return Boolean;
   --  Determine whether arbitrary declaration Decl denotes a generic package,
   --  a generic subprogram or a generic body.

   function Is_Independent_Object (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a reference to an independent
   --  object as per RM C.6(8).

   function Is_Inherited_Operation (E : Entity_Id) return Boolean;
   --  E is a subprogram. Return True is E is an implicit operation inherited
   --  by a derived type declaration.

   function Is_Inlinable_Expression_Function (Subp : Entity_Id) return Boolean;
   --  Return True if Subp is an expression function that fulfills all the
   --  following requirements for inlining:
   --     1. pragma/aspect Inline_Always
   --     2. No formals
   --     3. No contracts
   --     4. No dispatching primitive
   --     5. Result subtype controlled (or with controlled components)
   --     6. Result subtype not subject to type-invariant checks
   --     7. Result subtype not a class-wide type
   --     8. Return expression naming an object global to the function
   --     9. Nominal subtype of the returned object statically compatible
   --        with the result subtype of the expression function.

   function Is_Internal_Block (N : Node_Id) return Boolean;
   pragma Inline (Is_Internal_Block);
   --  Determine if N is an N_Block_Statement with an internal label. See
   --  Add_Block_Identifier.

   function Is_Iterator (Typ : Entity_Id) return Boolean;
   --  AI05-0139-2: Check whether Typ is one of the predefined interfaces in
   --  Ada.Iterator_Interfaces, or it is derived from one.

   function Is_Iterator_Over_Array (N : Node_Id) return Boolean;
   --  N is an iterator specification. Returns True iff N is an iterator over
   --  an array, either inside a loop of the form 'for X of A' or a quantified
   --  expression of the form 'for all/some X of A' where A is of array type.

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean;
   --  A library-level declaration is one that is accessible from Standard,
   --  i.e. a library unit or an entity declared in a library package.

   function Is_Limited_Class_Wide_Type (Typ : Entity_Id) return Boolean;
   --  Determine whether a given type is a limited class-wide type, in which
   --  case it needs a Master_Id, because extensions of its designated type
   --  may include task components. A class-wide type that comes from a
   --  limited view must be treated in the same way.

   function Is_Local_Variable_Reference (Expr : Node_Id) return Boolean;
   --  Determines whether Expr is a reference to a variable or formal parameter
   --  of mode OUT or IN OUT of the current enclosing subprogram.

   function Is_Master (N : Node_Id) return Boolean;
   --  Determine if the given node N constitutes a finalization master

   function Is_Name_Reference (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N is a reference to a name. This is
   --  similar to Is_Object_Reference but returns True only if N can be renamed
   --  without the need for a temporary, the typical example of an object not
   --  in this category being a function call.

   function Is_Non_Preelaborable_Construct (N : Node_Id) return Boolean;
   --  Determine whether arbitrary construct N violates preelaborability as
   --  defined in ARM 10.2.1 5-9/3. This routine takes into account both the
   --  syntactic and semantic properties of the construct.

   function Is_Nontrivial_DIC_Procedure (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id denotes the procedure that verifies the
   --  assertion expression of pragma Default_Initial_Condition and if it does,
   --  the encapsulated expression is nontrivial.

   function Is_Null_Extension
    (T : Entity_Id; Ignore_Privacy : Boolean := False) return Boolean;
   --  Given a tagged type, returns True if argument is a type extension
   --  that introduces no new components (discriminant or nondiscriminant).
   --  Ignore_Privacy should be True for use in implementing dynamic semantics.
   --  Cannot be called with class-wide types.

   function Is_Null_Extension_Of
     (Descendant, Ancestor : Entity_Id) return Boolean;
   --  Given two tagged types, the first a descendant of the second,
   --  returns True if every component of Descendant is inherited
   --  (directly or indirectly) from Ancestor. Privacy is ignored.
   --  Cannot be called with class-wide types.

   function Is_Null_Record_Definition (Record_Def : Node_Id) return Boolean;
   --  Returns True for an N_Record_Definition node that has no user-defined
   --  components (and no variant part).

   function Is_Null_Record_Type
     (T : Entity_Id; Ignore_Privacy : Boolean := False) return Boolean;
   --  Determine whether T is declared with a null record definition, a
   --  null component list, or as a type derived from a null record type
   --  (with a null extension if tagged). Returns True for interface types,
   --  False for discriminated types.

   function Is_Object_Image (Prefix : Node_Id) return Boolean;
   --  Returns True if an 'Img, 'Image, 'Wide_Image, or 'Wide_Wide_Image
   --  attribute is applied to an object.

   function Is_Object_Reference (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents an object. Both
   --  variable and constant objects return True (compare Is_Variable).

   function Is_OK_Variable_For_Out_Formal (AV : Node_Id) return Boolean;
   --  Used to test if AV is an acceptable formal for an OUT or IN OUT formal.
   --  Note that the Is_Variable function is not quite the right test because
   --  this is a case in which conversions whose expression is a variable (in
   --  the Is_Variable sense) with an untagged type target are considered view
   --  conversions and hence variables.

   function Is_OK_Volatile_Context
     (Context       : Node_Id;
      Obj_Ref       : Node_Id;
      Check_Actuals : Boolean) return Boolean;
   --  Determine whether node Context denotes a "non-interfering context" (as
   --  defined in SPARK RM 7.1.3(9)) where volatile reference Obj_Ref can
   --  safely reside. When examining references that might be located within
   --  actual parameters of a subprogram call that has not been resolved yet,
   --  Check_Actuals should be False; such references will be assumed to be
   --  legal. They will need to be checked again after subprogram call has
   --  been resolved.

   function Is_Package_Contract_Annotation (Item : Node_Id) return Boolean;
   --  Determine whether aspect specification or pragma Item is one of the
   --  following package contract annotations:
   --    Abstract_State
   --    Initial_Condition
   --    Initializes
   --    Refined_State

   function Is_Partially_Initialized_Type
     (Typ              : Entity_Id;
      Include_Implicit : Boolean := True) return Boolean;
   --  Typ is a type entity. This function returns true if this type is partly
   --  initialized, meaning that an object of the type is at least partly
   --  initialized (in particular in the record case, that at least one
   --  component has an initialization expression, including via Default_Value
   --  and Default_Component_Value aspects). Note that initialization
   --  resulting from the use of pragma Normalize_Scalars does not count.
   --  Include_Implicit controls whether implicit initialization of access
   --  values to null, and of discriminant values, is counted as making the
   --  type be partially initialized. For the default setting of True, these
   --  implicit cases do count, and discriminated types or types containing
   --  access values not explicitly initialized will return True. Otherwise
   --  if Include_Implicit is False, these cases do not count as making the
   --  type be partially initialized.

   function Is_Potentially_Unevaluated (N : Node_Id) return Boolean;
   --  Predicate to implement definition given in RM 2012 6.1.1 (20/3)

   function Is_Potentially_Persistent_Type (T : Entity_Id) return Boolean;
   --  Determines if type T is a potentially persistent type. A potentially
   --  persistent type is defined (recursively) as a scalar type, an untagged
   --  record whose components are all of a potentially persistent type, or an
   --  array with all static constraints whose component type is potentially
   --  persistent. A private type is potentially persistent if the full type
   --  is potentially persistent.

   function Is_Predefined_Dispatching_Operation (E : Entity_Id) return Boolean;
   --  Ada 2005 (AI-251): Determines if E is a predefined primitive operation

   function Is_Predefined_Interface_Primitive (E : Entity_Id) return Boolean;
   --  Ada 2005 (AI-345): Returns True if E is one of the predefined primitives
   --  required to implement interfaces.

   function Is_Predefined_Internal_Operation (E : Entity_Id) return Boolean;
   --  Similar to the previous one, but excludes stream operations, because
   --  these may be overridden, and need extra formals, like user-defined
   --  operations.

   function Is_Preelaborable_Aggregate (Aggr : Node_Id) return Boolean;
   --  Determine whether aggregate Aggr violates the restrictions of
   --  preelaborable constructs as defined in ARM 10.2.1(5-9).

   function Is_Preelaborable_Construct (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N violates the restrictions of
   --  preelaborable constructs as defined in ARM 10.2.1(5-9). Routine
   --  Is_Non_Preelaborable_Construct takes into account the syntactic
   --  and semantic properties of N for a more accurate diagnostic.

   function Is_Private_Library_Unit (Unit : Entity_Id) return Boolean;
   --  Returns True if and only if the library unit is declared with an
   --  explicit designation of private.

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

   function Is_Single_Concurrent_Object (Id : Entity_Id) return Boolean;
   --  Determine whether arbitrary entity Id denotes the anonymous object
   --  created for a single protected or single task type.

   function Is_Single_Concurrent_Type (Id : Entity_Id) return Boolean;
   --  Determine whether arbitrary entity Id denotes a single protected or
   --  single task type.

   function Is_Single_Concurrent_Type_Declaration (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes the declaration of a single
   --  protected type or single task type.

   function Is_Single_Precision_Floating_Point_Type
     (E : Entity_Id) return Boolean;
   --  Return whether E is a single precision floating point type,
   --  characterized by:
   --  . machine_radix = 2
   --  . machine_mantissa = 24
   --  . machine_emax = 2**7
   --  . machine_emin = 3 - machine_emax

   function Is_Single_Protected_Object (Id : Entity_Id) return Boolean;
   --  Determine whether arbitrary entity Id denotes the anonymous object
   --  created for a single protected type.

   function Is_Single_Task_Object (Id : Entity_Id) return Boolean;
   --  Determine whether arbitrary entity Id denotes the anonymous object
   --  created for a single task type.

   function Is_Specific_Tagged_Type (Typ : Entity_Id) return Boolean;
   --  Determine whether an arbitrary [private] type is specifically tagged

   function Is_Statement (N : Node_Id) return Boolean;
   pragma Inline (Is_Statement);
   --  Check if the node N is a statement node. Note that this includes
   --  the case of procedure call statements (unlike the direct use of
   --  the N_Statement_Other_Than_Procedure_Call subtype from Sinfo).
   --  Note that a label is *not* a statement, and will return False.

   function Is_Static_Discriminant_Component (N : Node_Id) return Boolean;
   --  Return True if N is guaranteed to a selected component containing a
   --  statically known discriminant.
   --  Note that this routine takes a conservative view and may return False
   --  in some cases where N would match the criteria. In other words this
   --  routine should be used to simplify or optimize the expanded code.

   function Is_Static_Function (Subp : Entity_Id) return Boolean;
   --  Determine whether subprogram Subp denotes a static function,
   --  which is a function with the aspect Static with value True.

   function Is_Static_Function_Call (Call : Node_Id) return Boolean;
   --  Determine whether Call is a static call to a static function,
   --  meaning that the name of the call denotes a static function
   --  and all of the call's actual parameters are given by static expressions.

   function Is_Subcomponent_Of_Full_Access_Object (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a reference to a subcomponent
   --  of a full access object as per RM C.6(7).

   function Is_Subprogram_Contract_Annotation (Item : Node_Id) return Boolean;
   --  Determine whether aspect specification or pragma Item is one of the
   --  following subprogram contract annotations:
   --    Always_Terminates
   --    Contract_Cases
   --    Depends
   --    Exceptional_Cases
   --    Extensions_Visible
   --    Global
   --    Post
   --    Post_Class
   --    Postcondition
   --    Pre
   --    Pre_Class
   --    Precondition
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Subprogram_Variant
   --    Test_Case

   function Is_Subprogram_Stub_Without_Prior_Declaration
     (N : Node_Id) return Boolean;
   --  Given an N_Subprogram_Body_Stub node N, return True if N is a subprogram
   --  stub with no prior subprogram declaration.

   function Is_Suitable_Primitive (Subp_Id : Entity_Id) return Boolean;
   --  Determine whether arbitrary subprogram Subp_Id may act as a primitive of
   --  an arbitrary tagged type.

   function Is_Synchronized_Object (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id denotes an object and if it does, whether
   --  this object is synchronized as specified in SPARK RM 9.1. To qualify as
   --  such, the object must be
   --    * Of a type that yields a synchronized object
   --    * An atomic object with enabled Async_Writers
   --    * A constant not of access-to-variable type
   --    * A variable subject to pragma Constant_After_Elaboration

   function Is_Synchronized_Tagged_Type (E : Entity_Id) return Boolean;
   --  Returns True if E is a synchronized tagged type (AARM 3.9.4 (6/2))

   function Is_Transfer (N : Node_Id) return Boolean;
   --  Returns True if the node N is a statement which is known to cause an
   --  unconditional transfer of control at run time, i.e. the following
   --  statement definitely will not be executed.

   function Is_Trivial_Boolean (N : Node_Id) return Boolean;
   --  Determine whether source node N denotes "True" or "False". Note that
   --  this is not true for expressions that got folded to True or False.

   function Is_Unchecked_Conversion_Instance (Id : Entity_Id) return Boolean;
   --  Determine whether an arbitrary entity denotes an instance of function
   --  Ada.Unchecked_Conversion.

   function Is_Universal_Numeric_Type (T : Entity_Id) return Boolean;
   pragma Inline (Is_Universal_Numeric_Type);
   --  True if T is Universal_Integer or Universal_Real

   function Is_Unconstrained_Or_Tagged_Item (Item : Entity_Id) return Boolean;
   --  Subsidiary to Collect_Subprogram_Inputs_Outputs and the analysis of
   --  pragma Depends. Determine whether the type of dependency item Item is
   --  tagged, unconstrained array or unconstrained record.

   function Is_User_Defined_Equality (Id : Entity_Id) return Boolean;
   --  Determine whether an entity denotes a user-defined equality

   function Is_User_Defined_Literal
     (N   : Node_Id;
      Typ : Entity_Id) return Boolean;
   pragma Inline (Is_User_Defined_Literal);
   --  Determine whether N is a user-defined literal for Typ, including
   --  the case where N denotes a named number of the appropriate kind
   --  when Typ has an Integer_Literal or Real_Literal aspect.

   function Is_Validation_Variable_Reference (N : Node_Id) return Boolean;
   --  Determine whether N denotes a reference to a variable which captures the
   --  value of an object for validation purposes.

   function Is_Variable_Size_Array (E : Entity_Id) return Boolean;
   --  Returns true if E has variable size components

   function Is_Variable_Size_Record (E : Entity_Id) return Boolean;
   --  Returns true if E has variable size components

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Is_Variable
     (N                 : Node_Id;
      Use_Original_Node : Boolean := True) return Boolean;
   --  Determines if the tree referenced by N represents a variable, i.e. can
   --  appear on the left side of an assignment. There is one situation (formal
   --  parameters) in which untagged type conversions are also considered
   --  variables, but Is_Variable returns False for such cases, since it has
   --  no knowledge of the context. Note that this is the point at which
   --  Assignment_OK is checked, and True is returned for any tree thus marked.
   --  Use_Original_Node is used to perform the test on Original_Node (N). By
   --  default is True since this routine is commonly invoked as part of the
   --  semantic analysis and it must not be disturbed by the rewriten nodes.

   function Is_View_Conversion (N : Node_Id) return Boolean;
   --  Returns True if N is a type_conversion whose operand is the name of an
   --  object and both its target type and operand type are tagged, or it
   --  appears in a call as an actual parameter of mode out or in out
   --  (RM 4.6(5/2)).

   function Is_Visibly_Controlled (T : Entity_Id) return Boolean;
   --  Check whether T is derived from a visibly controlled type. This is true
   --  if the root type is declared in Ada.Finalization. If T is derived
   --  instead from a private type whose full view is controlled, an explicit
   --  Initialize/Adjust/Finalize subprogram does not override the inherited
   --  one.

   function Is_Volatile_Full_Access_Object_Ref (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a reference to an object
   --  which is Volatile_Full_Access.

   function Is_Volatile_Function (Func_Id : Entity_Id) return Boolean;
   --  Determine whether [generic] function Func_Id is subject to enabled
   --  pragma Volatile_Function. Protected functions are treated as volatile
   --  (SPARK RM 7.1.2).

   function Is_Volatile_Object_Ref (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a reference to a volatile
   --  object as per RM C.6(8). Note that the test here is for something that
   --  is actually declared as volatile, not for an object that gets treated
   --  as volatile (see Einfo.Treat_As_Volatile).

   generic
      with procedure Handle_Parameter (Formal : Entity_Id; Actual : Node_Id);
   procedure Iterate_Call_Parameters (Call : Node_Id);
   --  Calls Handle_Parameter for each pair of formal and actual parameters of
   --  a function, procedure, or entry call.

   function Iterator_Interface_Ancestor (Typ : Entity_Id) return Entity_Id;
   --  If Typ has an ancestor that is an iterator interface type declared in
   --  an instance of Ada.Iterator_Interfaces, then returns that interface
   --  type. Otherwise returns Empty. (It's not clear what it means if there
   --  is more than one such ancestor, perhaps coming from multiple instances,
   --  but this function returns the first such ancestor it finds. ???)

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

   function Known_To_Be_Assigned
     (N        : Node_Id;
      Only_LHS : Boolean := False) return Boolean;
   --  The node N is an entity reference. This function determines whether the
   --  reference is for sure an assignment of the entity, returning True if
   --  so. Only_LHS will modify this behavior such that actuals for out or
   --  in out parameters will not be considered assigned.

   function Last_Source_Statement (HSS : Node_Id) return Node_Id;
   --  HSS is a handled statement sequence. This function returns the last
   --  statement in Statements (HSS) that has Comes_From_Source set. If no
   --  such statement exists, Empty is returned.

   procedure Mark_Coextensions (Context_Nod : Node_Id; Root_Nod : Node_Id);
   --  Given a node which designates the context of analysis and an origin in
   --  the tree, traverse from Root_Nod and mark all allocators as either
   --  dynamic or static depending on Context_Nod. Any incorrect marking is
   --  cleaned up during resolution.

   procedure Mark_Elaboration_Attributes
     (N_Id     : Node_Or_Entity_Id;
      Checks   : Boolean := False;
      Level    : Boolean := False;
      Modes    : Boolean := False;
      Warnings : Boolean := False);
   --  Preserve relevant elaboration-related properties of the context in
   --  arbitrary entity or node N_Id. The flags control the properties as
   --  follows:
   --
   --    Checks   - Save the status of Elaboration_Check
   --    Level    - Save the declaration level of N_Id (if applicable)
   --    Modes    - Save the Ghost and SPARK modes in effect (if applicable)
   --    Warnings - Save the status of Elab_Warnings

   procedure Mark_Save_Invocation_Graph_Of_Body;
   --  Notify the body of the main unit that the invocation constructs and
   --  relations expressed within it must be recorded by the ABE mechanism.

   function Matching_Static_Array_Bounds
     (L_Typ : Node_Id;
      R_Typ : Node_Id) return Boolean;
   --  L_Typ and R_Typ are two array types. Returns True when they have the
   --  same number of dimensions, and the same static bounds for each index
   --  position.

   function Might_Raise (N : Node_Id) return Boolean;
   --  True if evaluation of N might raise an exception. This is conservative;
   --  if we're not sure, we return True. If N is a subprogram body, this is
   --  about whether execution of that body can raise.

   function Nearest_Class_Condition_Subprogram
     (Kind    : Condition_Kind;
      Spec_Id : Entity_Id) return Entity_Id;
   --  Return the nearest ancestor containing the merged class-wide conditions
   --  that statically apply to Spec_Id; return Empty otherwise.

   function Nearest_Enclosing_Instance (E : Entity_Id) return Entity_Id;
   --  Return the entity of the nearest enclosing instance which encapsulates
   --  entity E. If no such instance exits, return Empty.

   function Needs_Finalization (Typ : Entity_Id) return Boolean;
   --  True if Typ requires finalization actions

   function Needs_One_Actual (E : Entity_Id) return Boolean;
   --  Returns True if a function has defaults for all but its first formal,
   --  which is a controlling formal. Used in Ada 2005 mode to solve the
   --  syntactic ambiguity that results from an indexing of a function call
   --  that returns an array, so that Obj.F (X, Y) may mean F (Ob) (X, Y).

   function Needs_Secondary_Stack (Id : Entity_Id) return Boolean;
   --  Return true if functions whose result type is Id must return on the
   --  secondary stack, i.e. allocate the return object on this stack.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Needs_Simple_Initialization
     (Typ         : Entity_Id;
      Consider_IS : Boolean := True) return Boolean;
   --  Certain types need initialization even though there is no specific
   --  initialization routine:
   --    Access types (which need initializing to null)
   --    All scalar types if Normalize_Scalars mode set
   --    Descendants of standard string types if Normalize_Scalars mode set
   --    Scalar types having a Default_Value attribute
   --  Regarding Initialize_Scalars mode, this is ignored if Consider_IS is
   --  set to False, but if Consider_IS is set to True, then the cases above
   --  mentioning Normalize_Scalars also apply for Initialize_Scalars mode.

   function Needs_Variable_Reference_Marker
     (N        : Node_Id;
      Calls_OK : Boolean) return Boolean;
   --  Determine whether arbitrary node N denotes a reference to a variable
   --  which is suitable for SPARK elaboration checks. Flag Calls_OK should
   --  be set when the reference is allowed to appear within calls.

   function New_Copy_List_Tree (List : List_Id) return List_Id;
   --  Copy recursively an analyzed list of nodes. Uses New_Copy_Tree defined
   --  below. As for New_Copy_Tree, it is illegal to attempt to copy extended
   --  nodes (entities) either directly or indirectly using this function.

   function New_Copy_Tree
     (Source    : Node_Id;
      Map       : Elist_Id   := No_Elist;
      New_Sloc  : Source_Ptr := No_Location;
      New_Scope : Entity_Id  := Empty) return Node_Id;
   --  Perform a deep copy of the subtree rooted at Source. Entities, itypes,
   --  and nodes are handled separately as follows:
   --
   --    * A node is replicated by first creating a shallow copy, then copying
   --      its syntactic fields, where all Parent pointers of the fields are
   --      updated to refer to the copy. In addition, the following semantic
   --      fields are recreated after the replication takes place.
   --
   --        First_Named_Actual
   --        Next_Named_Actual
   --        Controlling_Argument
   --
   --      If applicable, the Etype field (if any) is updated to refer to a
   --      local itype or type (see below).
   --
   --    * An entity defined within an N_Expression_With_Actions node in the
   --      subtree is given a new entity, and all references to the original
   --      entity are updated to refer to the new entity. In addition, the
   --      following semantic fields are replicated and/or updated to refer
   --      to a local entity or itype.
   --
   --        Discriminant_Constraint
   --        Etype
   --        First_Index
   --        Next_Entity
   --        Packed_Array_Impl_Type
   --        Scalar_Range
   --        Scope
   --
   --      Note that currently no other expression can define entities.
   --
   --    * An itype whose Associated_Node_For_Itype node is in the subtree
   --      is given a new entity, and all references to the original itype
   --      are updated to refer to the new itype. In addition, the following
   --      semantic fields are replicated and/or updated to refer to a local
   --      entity or itype.
   --
   --        Discriminant_Constraint
   --        Etype
   --        First_Index
   --        Next_Entity
   --        Packed_Array_Impl_Type
   --        Scalar_Range
   --        Scope
   --
   --      The Associated_Node_For_Itype is updated to refer to a replicated
   --      node.
   --
   --  The routine can replicate both analyzed and unanalyzed trees. Copying an
   --  Empty or Error node yields the same node.
   --
   --  Parameter Map may be used to specify a set of mappings between entities.
   --  These mappings are then taken into account when replicating entities.
   --  The format of Map must be as follows:
   --
   --    old entity 1
   --    new entity to replace references to entity 1
   --    old entity 2
   --    new entity to replace references to entity 2
   --    ...
   --
   --  Map and its contents are left unchanged.
   --
   --  Parameter New_Sloc may be used to specify a new source location for all
   --  replicated entities, itypes, and nodes. The Comes_From_Source indicator
   --  is defaulted if a new source location is provided.
   --
   --  Parameter New_Scope may be used to specify a new scope for all copied
   --  entities and itypes.

   function New_External_Entity
     (Kind         : Entity_Kind;
      Scope_Id     : Entity_Id;
      Sloc_Value   : Source_Ptr;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Int := 0;
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

   function Next_Actual (Actual_Id : Node_Id) return Node_Id;
   --  Find next actual parameter in declaration order. As described for
   --  First_Actual, this is the next actual in the declaration order, not
   --  the call order, so this does not correspond to simply taking the
   --  next entry of the Parameter_Associations list. The argument is an
   --  actual previously returned by a call to First_Actual or Next_Actual.
   --  Note that the result produced is always an expression, not a parameter
   --  association node, even if named notation was used.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   procedure Next_Actual (Actual_Id : in out Node_Id);
   pragma Inline (Next_Actual);
   --  Next_Actual (N) is equivalent to N := Next_Actual (N). Note that we
   --  inline this procedural form, but not the functional form above.

   function Next_Global (Node : Node_Id) return Node_Id;
   --  Node is a global item from a list, obtained through calling First_Global
   --  and possibly Next_Global a number of times. Returns the next global item
   --  with the same mode.

   procedure Next_Global (Node : in out Node_Id);
   pragma Inline (Next_Global);
   --  Next_Global (N) is equivalent to N := Next_Global (N). Note that we
   --  inline this procedural form, but not the functional form above.

   function No_Caching_Enabled (Id : Entity_Id) return Boolean;
   --  Given any entity Id, determine whether Id is subject to volatility
   --  property No_Caching and if it is, the related expression evaluates
   --  to True.

   function No_Heap_Finalization (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ is subject to pragma No_Heap_Finalization

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
   --  sets Never_Set_In_Source in the associated entity if there is one,
   --  taking into account the rule that in the case of renamed objects,
   --  it is the flag in the renamed object that must be set.
   --
   --  The parameter Sure is set True if the modification is sure to occur
   --  (e.g. target of assignment, or out parameter), and to False if the
   --  modification is only potential (e.g. address of entity taken).

   function Null_To_Null_Address_Convert_OK
     (N   : Node_Id;
      Typ : Entity_Id := Empty) return Boolean;
   --  Return True if we are compiling in relaxed RM semantics mode and:
   --   1) N is a N_Null node and Typ is a descendant of System.Address, or
   --   2) N is a comparison operator, one of the operands is null, and the
   --      type of the other operand is a descendant of System.Address.

   function Number_Of_Elements_In_Array (T : Entity_Id) return Nat;
   --  Returns the number of elements in the array T if the index bounds of T
   --  is known at compile time. If the bounds are not known at compile time,
   --  the function returns the value zero.

   function Original_Aspect_Pragma_Name (N : Node_Id) return Name_Id;
   --  Retrieve the name of aspect or pragma N, taking into account a possible
   --  rewrite and whether the pragma is generated from an aspect as the names
   --  may be different. The routine also deals with 'Class in which case it
   --  returns the following values:
   --
   --    Invariant            -> Name_uInvariant
   --    Post'Class           -> Name_uPost
   --    Pre'Class            -> Name_uPre
   --    Type_Invariant       -> Name_uType_Invariant
   --    Type_Invariant'Class -> Name_uType_Invariant

   function Original_Corresponding_Operation (S : Entity_Id) return Entity_Id;
   --  [Ada 2012: AI05-0125-1]: If S is an inherited dispatching primitive S2,
   --  or overrides an inherited dispatching primitive S2, the original
   --  corresponding operation of S is the original corresponding operation of
   --  S2. Otherwise, it is S itself.

   function Original_View_In_Visible_Part (Typ : Entity_Id) return Boolean;
   --  Returns True if the type Typ has a private view or if the public view
   --  appears in the visible part of a package spec.

   procedure Output_Entity (Id : Entity_Id);
   --  Print entity Id to standard output. The name of the entity appears in
   --  fully qualified form.
   --
   --  WARNING: this routine should be used in debugging scenarios such as
   --  tracking down undefined symbols as it is fairly low level.

   procedure Output_Name (Nam : Name_Id; Scop : Entity_Id := Current_Scope);
   --  Print name Nam to standard output. The name appears in fully qualified
   --  form assuming it appears in scope Scop. Note that this may not reflect
   --  the final qualification as the entity which carries the name may be
   --  relocated to a different scope.
   --
   --  WARNING: this routine should be used in debugging scenarios such as
   --  tracking down undefined symbols as it is fairly low level.

   function Param_Entity (N : Node_Id) return Entity_Id;
   --  Given an expression N, determines if the expression is a reference
   --  to a formal (of a subprogram or entry), and if so returns the Id
   --  of the corresponding formal entity, otherwise returns Empty. Also
   --  handles the case of references to renamings of formals.

   function Policy_In_Effect (Policy : Name_Id) return Name_Id;
   --  Given a policy, return the policy identifier associated with it. If no
   --  such policy is in effect, the value returned is No_Name.

   function Predicate_Enabled (Typ : Entity_Id) return Boolean;
   --  Return True if a predicate check should be emitted for the given type
   --  Typ, taking into account Predicates_Ignored and
   --  Predicate_Checks_Suppressed.

   function Predicate_Failure_Expression
    (Typ : Entity_Id; Inherited_OK : Boolean) return Node_Id;
   --  If the given type or subtype is subject to a Predicate_Failure
   --  aspect specification, then returns the specified expression.
   --  Otherwise, if Inherited_OK is False then returns Empty.
   --  Otherwise, if Typ denotes a subtype or a derived type then
   --  returns the result of recursing on the ancestor subtype.
   --  Otherwise, returns Empty.

   function Predicate_Function_Needs_Membership_Parameter (Typ : Entity_Id)
     return Boolean is
     (Present (Predicate_Failure_Expression (Typ, Inherited_OK => True)));
   --  The predicate function for some, but not all, subtypes needs to
   --  know whether the predicate is being evaluated as part of a membership
   --  test. The predicate function for such a subtype takes an additional
   --  boolean to convey this information. This function returns True if this
   --  additional parameter is needed. More specifically, this function
   --  returns true if the Predicate_Failure aspect is specified for the
   --  given subtype or for any of its "ancestor" subtypes.

   function Predicate_Tests_On_Arguments (Subp : Entity_Id) return Boolean;
   --  Subp is the entity for a subprogram call. This function returns True if
   --  predicate tests are required for the arguments in this call (this is the
   --  normal case). It returns False for special cases where these predicate
   --  tests should be skipped (see body for details).

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

   procedure Propagate_Concurrent_Flags
     (Typ      : Entity_Id;
      Comp_Typ : Entity_Id);
   --  Set Has_Task, Has_Protected, and Has_Timing_Event on Typ when the flags
   --  are set on Comp_Typ. This follows the definition of these flags, which
   --  are set (recursively) on any composite type that has a component marked
   --  by one of these flags. This procedure can only set flags for Typ, and
   --  never clear them. Comp_Typ is the type of a component or a parent.

   procedure Propagate_Controlled_Flags
     (Typ      : Entity_Id;
      From_Typ : Entity_Id;
      Comp     : Boolean := False;
      Deriv    : Boolean := False);
   --  Set Disable_Controlled, Finalize_Storage_Only, Has_Controlled_Component,
   --  Has_Relaxed_Finalization, and Is_Controlled_Active on Typ when the flags
   --  are set on From_Typ. If Comp is True, From_Typ is assumed to be the type
   --  of a component of Typ while, if Deriv is True, From_Typ is assumed to be
   --  the parent type of Typ. This procedure can only set flags for Typ, and
   --  never clear them.

   procedure Propagate_DIC_Attributes
     (Typ      : Entity_Id;
      From_Typ : Entity_Id);
   --  Inherit all Default_Initial_Condition-related attributes from type
   --  From_Typ. Typ is the destination type.

   procedure Propagate_Invariant_Attributes
     (Typ      : Entity_Id;
      From_Typ : Entity_Id);
   --  Inherit all invariant-related attributes from type From_Typ. Typ is the
   --  destination type.

   procedure Propagate_Predicate_Attributes
     (Typ      : Entity_Id;
      From_Typ : Entity_Id);
   --  Inherit predicate functions and Has_Predicates flag from type From_Typ.
   --  Typ is the destination type.

   procedure Record_Possible_Part_Of_Reference
     (Var_Id : Entity_Id;
      Ref    : Node_Id);
   --  Save reference Ref to variable Var_Id when the variable is subject to
   --  pragma Part_Of. If the variable is known to be a constituent of a single
   --  protected/task type, the legality of the reference is verified and the
   --  save does not take place.

   function Referenced (Id : Entity_Id; Expr : Node_Id) return Boolean;
   --  Determine whether entity Id is referenced within expression Expr

   function References_Generic_Formal_Type (N : Node_Id) return Boolean;
   --  Returns True if the expression Expr contains any references to a generic
   --  type. This can only happen within a generic template.

   procedure Remove_Entity_And_Homonym (Id : Entity_Id);
   --  Remove arbitrary entity Id from both the homonym and scope chains. Use
   --  Remove_Overloaded_Entity for overloadable entities. Note: the removal
   --  performed by this routine does not affect the visibility of existing
   --  homonyms.

   procedure Remove_Homonym (Id : Entity_Id);
   --  Removes entity Id from the homonym chain

   procedure Remove_Overloaded_Entity (Id : Entity_Id);
   --  Remove arbitrary entity Id from the homonym chain, the scope chain and
   --  the primitive operations list of the associated controlling type. Use
   --  Remove_Entity for non-overloadable entities. Note: the removal performed
   --  by this routine does not affect the visibility of existing homonyms.

   function Remove_Suffix (E : Entity_Id; Suffix : Character) return Name_Id;
   --  Returns the name of E without Suffix

   procedure Replace_Null_By_Null_Address (N : Node_Id);
   --  N is N_Null or a binary comparison operator, we are compiling in relaxed
   --  RM semantics mode, and one of the operands is null. Replace null with
   --  System.Null_Address.

   function Rep_To_Pos_Flag (E : Entity_Id; Loc : Source_Ptr) return Node_Id;
   --  This is used to construct the second argument in a call to Rep_To_Pos
   --  which is True if range checks are enabled (E is an entity to which the
   --  Range_Checks_Suppressed test is applied), and False if range checks are
   --  suppressed. Loc is the location for the node that is returned (which is
   --  a New_Occurrence of the appropriate entity).
   --
   --  Note: one might think that it would be fine to always use True and to
   --  ignore the suppress in this case, but there is at least one case in the
   --  generated code (the code for array assignment in a loop) that depends on
   --  this suppression. Anyway, it is generally better to believe a request to
   --  suppress exceptions if possible.

   procedure Require_Entity (N : Node_Id);
   --  N is a node which should have an entity value if it is an entity name.
   --  If not, then check if there were previous errors. If so, just fill
   --  in with Any_Id and ignore. Otherwise signal a program error exception.
   --  This is used as a defense mechanism against ill-formed trees caused by
   --  previous errors (particularly in -gnatq mode).

   function Requires_Transient_Scope (Typ : Entity_Id) return Boolean;
   pragma Inline (Requires_Transient_Scope);
   --  Return true if temporaries of Typ need to be wrapped in a transient
   --  scope, either because they are allocated on the secondary stack or
   --  finalization actions must be generated before the next instruction.
   --  Examples of types requiring such wrapping are variable-sized types,
   --  including unconstrained arrays, and controlled types.

   procedure Reset_Analyzed_Flags (N : Node_Id);
   --  Reset the Analyzed flags in all nodes of the tree whose root is N

   procedure Restore_SPARK_Mode (Mode : SPARK_Mode_Type; Prag : Node_Id);
   --  Set the current SPARK_Mode to Mode and SPARK_Mode_Pragma to Prag. This
   --  routine must be used in tandem with Set_SPARK_Mode.

   function Returns_Unconstrained_Type (Subp : Entity_Id) return Boolean;
   --  Return true if Subp is a function that returns an unconstrained type

   function Root_Type_Of_Full_View (T : Entity_Id) return Entity_Id;
   --  Similar to attribute Root_Type, but this version always follows the
   --  Full_View of a private type (if available) while searching for the
   --  ultimate derivation ancestor.

   function Safe_To_Capture_Value
     (N    : Node_Id;
      Ent  : Entity_Id;
      Cond : Boolean := False) return Boolean;
   --  The caller is interested in capturing a value (either the current
   --  value, an indication that the value is [non-]null or an indication that
   --  the value is valid) for the given entity Ent. This value can only be
   --  captured if sequential execution semantics can be properly guaranteed so
   --  that a subsequent reference will indeed be sure that this current value
   --  indication is correct. The node N is the construct that resulted in the
   --  possible capture of the value (this is used to check if we are in a
   --  conditional).
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

   function Same_Or_Aliased_Subprograms
     (S : Entity_Id;
      E : Entity_Id) return Boolean;
   --  Returns True if the subprogram entity S is the same as E or else S is an
   --  alias of E.

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

   function Scalar_Part_Present (Typ : Entity_Id) return Boolean;
   --  Determine whether arbitrary type Typ is a scalar type, or contains at
   --  least one scalar subcomponent.

   function Scope_Within
     (Inner : Entity_Id;
      Outer : Entity_Id) return Boolean;
   --  Determine whether scope Inner appears within scope Outer. Note that
   --  scopes are partially ordered, so Scope_Within (A, B) and Scope_Within
   --  (B, A) may both return False.

   function Scope_Within_Or_Same
     (Inner : Entity_Id;
      Outer : Entity_Id) return Boolean;
   --  Determine whether scope Inner appears within scope Outer or both denote
   --  the same scope. Note that scopes are partially ordered, so Scope_Within
   --  (A, B) and Scope_Within (B, A) may both return False.

   procedure Set_Current_Entity (E : Entity_Id);
   pragma Inline (Set_Current_Entity);
   --  Establish the entity E as the currently visible definition of its
   --  associated name (i.e. the Node_Id associated with its name).

   procedure Set_Debug_Info_Defining_Id (N : Node_Id);
   --  Call Set_Debug_Info_Needed on Defining_Identifier (N) if it comes from
   --  source or we are in -gnatD mode, where we are debugging generated code.

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
   --    If Style_Check is set, then it calls a style checking routine that
   --    can check identifier spelling style.
   --
   --    If restriction No_Abort_Statements is set, then it checks that the
   --    entity is not Ada.Task_Identification.Abort_Task.
   --
   --    If restriction No_Dynamic_Attachment is set, then it checks that the
   --    entity is not one of the restricted names for this restriction.
   --
   --    If restriction No_Long_Long_Integers is set, then it checks that the
   --    entity is not Standard.Long_Long_Integer.
   --
   --    If restriction No_Implementation_Identifiers is set, then it checks
   --    that the entity is not implementation defined.

   procedure Set_Invalid_Scalar_Value
     (Scal_Typ : Float_Scalar_Id;
      Value    : Ureal);
   --  Associate invalid value Value with scalar type Scal_Typ as specified by
   --  pragma Initialize_Scalars.

   procedure Set_Invalid_Scalar_Value
     (Scal_Typ : Integer_Scalar_Id;
      Value    : Uint);
   --  Associate invalid value Value with scalar type Scal_Typ as specified by
   --  pragma Initialize_Scalars.

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

   procedure Set_Rep_Info (T1 : Entity_Id; T2 : Entity_Id);
   pragma Inline (Set_Rep_Info);
   --  Copies the Is_Atomic, Is_Independent and Is_Volatile_Full_Access flags
   --  from sub(type) entity T2 to (sub)type entity T1, as well as Is_Volatile
   --  if T1 is a base type.

   procedure Set_Size_Info (T1, T2 : Entity_Id);
   pragma Inline (Set_Size_Info);
   --  Copies the Esize field and Has_Biased_Representation flag from sub(type)
   --  entity T2 to (sub)type entity T1. Also copies the Is_Unsigned_Type flag
   --  in the fixed-point and discrete cases, and also copies the alignment
   --  value from T2 to T1. It does NOT copy the RM_Size field, which must be
   --  separately set if this is required to be copied also.

   procedure Set_SPARK_Mode (Context : Entity_Id);
   --  Establish the SPARK_Mode and SPARK_Mode_Pragma (if any) of a package or
   --  a subprogram denoted by Context. This routine must be used in tandem
   --  with Restore_SPARK_Mode.

   function Scope_Is_Transient return Boolean;
   --  True if the current scope is transient

   function Should_Ignore_Pragma_Par (Prag_Name : Name_Id) return Boolean;
   function Should_Ignore_Pragma_Sem (N : Node_Id) return Boolean;
   --  True if we should ignore pragmas with the specified name. In particular,
   --  this returns True if pragma Ignore_Pragma applies, and we are not in a
   --  predefined unit. The _Par version should be called only from the parser;
   --  the _Sem version should be called only during semantic analysis.

   function Static_Boolean (N : Node_Id) return Opt_Ubool;
   --  This function analyzes the given expression node and then resolves it
   --  as Standard.Boolean. If the result is static, then Uint_1 or Uint_0 is
   --  returned corresponding to the value, otherwise an error message is
   --  output and No_Uint is returned.

   function Static_Integer (N : Node_Id) return Uint;
   --  This function analyzes the given expression node and then resolves it
   --  as any integer type. If the result is static, then the value of the
   --  universal expression is returned, otherwise an error message is output
   --  and a value of No_Uint is returned.

   function Statically_Denotes_Entity (N : Node_Id) return Boolean;
   --  Return True iff N is a name that "statically denotes" an entity.

   function Statically_Denotes_Object (N : Node_Id) return Boolean;
   --  Return True iff N is a name that "statically denotes" an object.

   function Statically_Different (E1, E2 : Node_Id) return Boolean;
   --  Return True if it can be statically determined that the Expressions
   --  E1 and E2 refer to different objects

   function Statically_Names_Object (N : Node_Id) return Boolean;
   --  Return True iff N is a name that "statically names" an object.

   function String_From_Numeric_Literal (N : Node_Id) return String_Id;
   --  Return the string that corresponds to the numeric literal N as it
   --  appears in the source.

   function Subject_To_Loop_Entry_Attributes (N : Node_Id) return Boolean;
   --  Determine whether node N is a loop statement subject to at least one
   --  'Loop_Entry attribute.

   function Support_Atomic_Primitives (Typ : Entity_Id) return Boolean;
   --  Return True if Typ supports the GCC built-in atomic operations (i.e. if
   --  Typ is properly sized and aligned).

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : String);
   --  Print debugging information on entry to each unit being analyzed

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id);
   --  Move a list of entities from one scope to another, and recompute
   --  Is_Public based upon the new scope.

   generic
      with function Process (N : Node_Id) return Traverse_Result is <>;
      Process_Itypes : Boolean := False;
   function Traverse_More_Func (Node : Node_Id) return Traverse_Final_Result;
   --  This is a version of Atree.Traverse_Func that not only traverses
   --  syntactic children of nodes, but also semantic children which are
   --  logically children of the node. This concerns currently lists of
   --  action nodes and ranges under Itypes, both inserted by the compiler.
   --  Itypes are only traversed when Process_Itypes is True.

   generic
      with function Process (N : Node_Id) return Traverse_Result is <>;
      Process_Itypes : Boolean := False;
   procedure Traverse_More_Proc (Node : Node_Id);
   pragma Inline (Traverse_More_Proc);
   --  This is the same as Traverse_More_Func except that no result is
   --  returned, i.e. Traverse_More_Func is called and the result is simply
   --  discarded.

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

   function Ultimate_Overlaid_Entity (E : Entity_Id) return Entity_Id;
   --  If entity E is overlaying some other entity via an Address clause (which
   --  possibly overlays yet another entity via its own Address clause), then
   --  return the ultimate overlaid entity. If entity E is not overlaying any
   --  other entity (or the overlaid entity cannot be determined statically),
   --  then return Empty.
   --
   --  Subsidiary to the analysis of object overlays in SPARK.

   function Ultimate_Prefix (N : Node_Id) return Node_Id;
   --  Obtain the "outermost" prefix of arbitrary node N. Return N if no such
   --  prefix exists.

   function Unique_Defining_Entity (N : Node_Id) return Entity_Id;
   --  Return the entity that represents declaration N, so that different
   --  views of the same entity have the same unique defining entity:
   --    * private view and full view of a deferred constant
   --        --> full view
   --    * entry spec and entry body
   --        --> entry spec
   --    * formal parameter on spec and body
   --        --> formal parameter on spec
   --    * package spec, body, and body stub
   --        --> package spec
   --    * protected type, protected body, and protected body stub
   --        --> protected type (full view if private)
   --    * subprogram spec, body, and body stub
   --        --> subprogram spec
   --    * task type, task body, and task body stub
   --        --> task type (full view if private)
   --    * private or incomplete view and full view of a type
   --        --> full view
   --  In other cases, return the defining entity for N.

   function Unique_Entity (E : Entity_Id) return Entity_Id;
   --  Return the unique entity for entity E, which would be returned by
   --  Unique_Defining_Entity if applied to the enclosing declaration of E.

   function Unique_Name (E : Entity_Id) return String;
   --  Return a unique name for entity E, which could be used to identify E
   --  across compilation units.

   Child_Prefix : constant String := "ada___";
   --  Prefix for child packages when building a unique name for an entity. It
   --  is included here to share between Unique_Name and gnatprove.

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

   function Unqual_Conv (Expr : Node_Id) return Node_Id;
   pragma Inline (Unqual_Conv);
   --  Similar to Unqualify, but removes qualified expressions, type
   --  conversions, and unchecked conversions.

   function Validated_View (Typ : Entity_Id) return Entity_Id;
   --  Obtain the "validated view" of arbitrary type Typ which is suitable for
   --  verification by attribute 'Valid_Scalars. This view is the type itself
   --  or its full view or nonlimited view, while stripping away concurrency,
   --  derivations, and privacy.

   function Visible_Ancestors (Typ : Entity_Id) return Elist_Id;
   --  [Ada 2012:AI-0125-1]: Collect all the visible parents and progenitors
   --  of a type extension or private extension declaration. If the full-view
   --  of private parents and progenitors is available then it is used to
   --  generate the list of visible ancestors; otherwise their partial
   --  view is added to the resulting list.

   function Within_Init_Proc return Boolean;
   --  Determines if Current_Scope is within an init proc

   function Within_Protected_Type (E : Entity_Id) return Boolean;
   --  Returns True if entity E is declared within a protected type

   function Within_Scope (E : Entity_Id; S : Entity_Id) return Boolean;
   --  Returns True if entity E is declared within scope S

   procedure Warn_On_Hiding_Entity
     (N               : Node_Id;
      Hidden, Visible : Entity_Id;
      On_Use_Clause   : Boolean);
   --  Warn on hiding of an entity, either because a new declaration hides
   --  an entity directly visible or potentially visible through a use_clause
   --  (On_Use_Clause = False), or because the entity would be potentially
   --  visible through a use_clause if it was now hidden by a visible
   --  declaration (On_Use_Clause = True). N is the node on which the warning
   --  is potentially issued: it is the visible entity in the former case, and
   --  the use_clause in the latter case.

   procedure Wrong_Type
     (Expr          : Node_Id;
      Expected_Type : Entity_Id;
      Multiple      : Boolean := False);
   --  Output error message for incorrectly typed expression. Expr is the node
   --  for the incorrectly typed construct (Etype (Expr) is the type found),
   --  and Expected_Type is the entity for the expected type. Note that Expr
   --  does not have to be a subexpression, anything with an Etype field may
   --  be used. If Multiple is False, do not output the message if an error
   --  has already been posted for Expr.

   function Yields_Synchronized_Object (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ "yields synchronized object" as specified by
   --  SPARK RM 9.1. To qualify as such, a type must be
   --    * An array type whose element type yields a synchronized object
   --    * A descendant of type Ada.Synchronous_Task_Control.Suspension_Object
   --    * A protected type
   --    * A record type or type extension without defaulted discriminants
   --      whose components are of a type that yields a synchronized object.
   --    * A synchronized interface type
   --    * A task type

   function Yields_Universal_Type (N : Node_Id) return Boolean;
   --  Determine whether unanalyzed node N yields a universal type

   procedure Preanalyze_And_Resolve_Without_Errors (N : Node_Id);
   --  Preanalyze and resolve N without reporting errors

   procedure Preanalyze_Without_Errors (N : Node_Id);
   --  Preanalyze N without reporting errors

   package Interval_Lists is
      type Discrete_Interval is
         record
            Low, High : Uint;
         end record;

      type Discrete_Interval_List is
        array (Pos range <>) of Discrete_Interval;
      --  A sorted (in ascending order) list of non-empty pairwise-disjoint
      --  intervals, always with a gap of at least one value between
      --  successive intervals (i.e., mergeable intervals are merged).
      --  Low bound is one; high bound is nonnegative.

      function Aggregate_Intervals (N : Node_Id) return Discrete_Interval_List;
      --  Given an array aggregate N, returns the (unique) interval list
      --  representing the values of the aggregate choices; if all the array
      --  components are covered by the others choice then the length of the
      --  result is zero.

      function Choice_List_Intervals
        (Discrete_Choices : List_Id) return Discrete_Interval_List;
      --  Given a discrete choice list, returns the (unique) interval
      --  list representing the chosen values.

      function Type_Intervals (Typ : Entity_Id) return Discrete_Interval_List;
      --  Given a static discrete type or subtype, returns the (unique)
      --  interval list representing the values of the type/subtype.
      --  If no static predicates are involved, the length of the result
      --  will be at most one.

      function Is_Subset (Subset, Of_Set : Discrete_Interval_List)
        return Boolean;
      --  Returns True iff every value belonging to some interval of
      --  Subset also belongs to some interval of Of_Set.

      --  When we get around to implementing "is statically compatible"
      --  correctly for real types with static predicates, we may need
      --  an analogous Real_Interval_List type. Most of the language
      --  rules that reference "is statically compatible" pertain to
      --  discriminants and therefore do not require support for real types;
      --  the exception is 12.5.1(8).

      Intervals_Error : exception;
      --  Raised when the list of non-empty pair-wise disjoint intervals cannot
      --  be built.
   end Interval_Lists;

   package Old_Attr_Util is
      --  Operations related to 'Old attribute evaluation. This
      --  includes cases where a level of indirection is needed due to
      --  conditional evaluation as well as support for the
      --  "known on entry" rules.

      package Conditional_Evaluation is
         function Eligible_For_Conditional_Evaluation
           (Expr : Node_Id) return Boolean;
         --  Given a subexpression of a Postcondition expression
         --  (typically a 'Old attribute reference), returns True if
         --     - the expression is conditionally evaluated; and
         --     - its determining expressions are all known on entry; and
         --     - Ada_Version >= Ada_2022.
         --  See RM 6.1.1 for definitions of these terms.
         --
         --  Also returns True if Expr is of an anonymous access type;
         --  this is just because we want the code that knows how to build
         --  'Old temps in that case to reside in only one place.

         function Conditional_Evaluation_Condition
           (Expr : Node_Id) return Node_Id;
         --  Given an expression which is eligible for conditional evaluation,
         --  build a Boolean expression whose value indicates whether the
         --  expression should be evaluated.
      end Conditional_Evaluation;

      package Indirect_Temps is
         generic
            with procedure Append_Item (N : Node_Id; Is_Eval_Stmt : Boolean);
            --  If Is_Eval_Stmt is True, then N is a statement that should
            --  only be executed in the case where the 'Old prefix is to be
            --  evaluated. If Is_Eval_Stmt is False, then N is a declaration
            --  which should be elaborated unconditionally.
            --  Client is responsible for ensuring that any appended
            --  Eval_Stmt nodes are eventually analyzed.

            Append_Decls_In_Reverse_Order : Boolean := False;
            --  This parameter is for the convenience of exp_prag.adb, where we
            --  want to Prepend rather than Append so it is better to get the
            --  Append calls in reverse order.

         procedure Declare_Indirect_Temp
           (Attr_Prefix   : Node_Id; -- prefix of 'Old attribute (or similar?)
            Indirect_Temp : out Entity_Id);
         --  Indirect_Temp is of an access type; it is unconditionally
         --  declared but only conditionally initialized to reference the
         --  saved value of Attr_Prefix.

         function Indirect_Temp_Needed (Typ : Entity_Id) return Boolean;
         --  Returns True for a specific tagged type because the temp must
         --  be of the class-wide type in order to preserve the underlying tag.
         --
         --  Also returns True in the case of an anonymous access type
         --  because we want the code that knows how to deal with
         --  this case to reside in only one place.
         --
         --  For an unconstrained-but-definite discriminated subtype, returns
         --  True if the potential difference in size between an
         --  unconstrained object and a constrained object is large.
         --  [This part is not implemented yet.]
         --
         --  Otherwise, returns False if a declaration of the form
         --     Temp : Typ;
         --  is legal and side-effect-free (assuming that default
         --  initialization is suppressed). For example, returns True if Typ is
         --  indefinite, or if Typ has a controlled part.
         --

         function Indirect_Temp_Value
           (Temp : Entity_Id;
            Typ  : Entity_Id;
            Loc  : Source_Ptr) return Node_Id;
         --  Evaluate a temp declared by Declare_Indirect_Temp.

         function Is_Access_Type_For_Indirect_Temp
           (T : Entity_Id) return Boolean;
         --  True for an access type that was declared via a call
         --  to Declare_Indirect_Temp.
         --  Indicates that the given access type should be treated
         --  the same with respect to finalization as a
         --  user-defined "comes from source" access type.

      end Indirect_Temps;
   end Old_Attr_Util;

   package Storage_Model_Support is

      --  This package provides a set of utility functions related to support
      --  for the Storage_Model feature. These functions provide an interface
      --  that the compiler (in particular back-end phases such as gigi and
      --  GNAT-LLVM) can use to easily obtain entities and operations that
      --  are specified for types that have aspects Storage_Model_Type or
      --  Designated_Storage_Model.

      function Has_Storage_Model_Type_Aspect (Typ : Entity_Id) return Boolean;
      --  Returns True iff Typ specifies aspect Storage_Model_Type

      --  WARNING: There is a matching C declaration of this subprogram in fe.h

      function Has_Designated_Storage_Model_Aspect
        (Typ : Entity_Id) return Boolean;
      --  Returns True iff Typ specifies aspect Designated_Storage_Model

      --  WARNING: There is a matching C declaration of this subprogram in fe.h

      function Storage_Model_Object (Typ : Entity_Id) return Entity_Id;
      --  Given an access type Typ with aspect Designated_Storage_Model,
      --  returns the storage-model object associated with that type.
      --  The object Entity_Ids returned by this function can be passed
      --  other functions declared in this interface to retrieve operations
      --  associated with Storage_Model_Type aspect of the object's type.

      --  WARNING: There is a matching C declaration of this subprogram in fe.h

      function Storage_Model_Type (Obj : Entity_Id) return Entity_Id;
      --  Given an object Obj of a type specifying aspect Storage_Model_Type,
      --  returns that type.

      function Get_Storage_Model_Type_Entity
        (SM_Obj_Or_Type : Entity_Id;
         Nam            : Name_Id) return Entity_Id;
      --  Given a type with aspect Storage_Model_Type or an object of such a
      --  type, and Nam denoting the name of one of the argument kinds allowed
      --  for that aspect, returns the Entity_Id corresponding to the entity
      --  associated with Nam in the aspect. If an entity was not explicitly
      --  specified for Nam, then returns Empty, except that in the defaulted
      --  Address_Type case, System.Address will be returned, and in the
      --  defaulted Null_Address case, System.Null_Address will be returned.
      --  (Note: This function is modeled on Get_Iterable_Type_Primitive.)

      function Storage_Model_Address_Type
        (SM_Obj_Or_Type : Entity_Id) return Entity_Id;
      --  Given a type with aspect Storage_Model_Type or an object of such a
      --  type, returns the type specified for the Address_Type choice in that
      --  aspect; returns type System.Address if the address type was not
      --  explicitly specified (indicating use of the native memory model).

      function Storage_Model_Null_Address
        (SM_Obj_Or_Type : Entity_Id) return Entity_Id;
      --  Given a type with aspect Storage_Model_Type or an object of such a
      --  type, returns the constant specified for the Null_Address choice in
      --  that aspect; returns Empty if the constant object isn't specified,
      --  unless the native memory model is in use (System.Address), in which
      --  case it returns System.Null_Address.

      function Storage_Model_Allocate
        (SM_Obj_Or_Type : Entity_Id) return Entity_Id;
      --  Given a type with aspect Storage_Model_Type or an object of such a
      --  type, returns the procedure specified for the Allocate choice in that
      --  aspect; returns Empty if the procedure isn't specified.

      function Storage_Model_Deallocate
        (SM_Obj_Or_Type : Entity_Id) return Entity_Id;
      --  Given a type with aspect Storage_Model_Type or an object of such a
      --  type, returns the procedure specified for the Deallocate choice in
      --  that aspect; returns Empty if the procedure isn't specified.

      function Storage_Model_Copy_From
        (SM_Obj_Or_Type : Entity_Id) return Entity_Id;
      --  Given a type with aspect Storage_Model_Type or an object of such a
      --  type, returns the procedure specified for the Copy_From choice in
      --  that aspect; returns Empty if the procedure isn't specified.

      --  WARNING: There is a matching C declaration of this subprogram in fe.h

      function Storage_Model_Copy_To
        (SM_Obj_Or_Type : Entity_Id) return Entity_Id;
      --  Given a type with aspect Storage_Model_Type or an object of such a
      --  type, returns the procedure specified for the Copy_To choice in that
      --  aspect; returns Empty if the procedure isn't specified.

      --  WARNING: There is a matching C declaration of this subprogram in fe.h

      function Storage_Model_Storage_Size
        (SM_Obj_Or_Type : Entity_Id) return Entity_Id;
      --  Given a type with aspect Storage_Model_Type or an object of such a
      --  type, returns the function specified for the Storage_Size choice in
      --  that aspect; returns Empty if the procedure isn't specified.

   end Storage_Model_Support;

end Sem_Util;
