------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Checks;   use Checks;
with Elists;   use Elists;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Eval_Fat; use Eval_Fat;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Dist; use Exp_Dist;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Layout;   use Layout;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Case; use Sem_Case;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Elim; use Sem_Elim;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Smem; use Sem_Smem;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Sem_Ch3 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Interface_Tag_Components
     (N : Node_Id; Typ : Entity_Id);
   --  Ada 2005 (AI-251): Add the tag components corresponding to all the
   --  abstract interface types implemented by a record type or a derived
   --  record type.

   procedure Build_Derived_Type
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id;
      Is_Completion : Boolean;
      Derive_Subps  : Boolean := True);
   --  Create and decorate a Derived_Type given the Parent_Type entity. N is
   --  the N_Full_Type_Declaration node containing the derived type definition.
   --  Parent_Type is the entity for the parent type in the derived type
   --  definition and Derived_Type the actual derived type. Is_Completion must
   --  be set to False if Derived_Type is the N_Defining_Identifier node in N
   --  (ie Derived_Type = Defining_Identifier (N)). In this case N is not the
   --  completion of a private type declaration. If Is_Completion is set to
   --  True, N is the completion of a private type declaration and Derived_Type
   --  is different from the defining identifier inside N (i.e. Derived_Type /=
   --  Defining_Identifier (N)). Derive_Subps indicates whether the parent
   --  subprograms should be derived. The only case where this parameter is
   --  False is when Build_Derived_Type is recursively called to process an
   --  implicit derived full type for a type derived from a private type (in
   --  that case the subprograms must only be derived for the private view of
   --  the type).

   --  ??? These flags need a bit of re-examination and re-documentation:
   --  ???  are they both necessary (both seem related to the recursion)?

   procedure Build_Derived_Access_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For a derived access type,
   --  create an implicit base if the parent type is constrained or if the
   --  subtype indication has a constraint.

   procedure Build_Derived_Array_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For a derived array type,
   --  create an implicit base if the parent type is constrained or if the
   --  subtype indication has a constraint.

   procedure Build_Derived_Concurrent_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For a derived task or pro-
   --  tected type, inherit entries and protected subprograms, check legality
   --  of discriminant constraints if any.

   procedure Build_Derived_Enumeration_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For a derived enumeration
   --  type, we must create a new list of literals. Types derived from
   --  Character and Wide_Character are special-cased.

   procedure Build_Derived_Numeric_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For numeric types, create
   --  an anonymous base type, and propagate constraint to subtype if needed.

   procedure Build_Derived_Private_Type
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id;
      Is_Completion : Boolean;
      Derive_Subps  : Boolean := True);
   --  Subsidiary procedure to Build_Derived_Type. This procedure is complex
   --  because the parent may or may not have a completion, and the derivation
   --  may itself be a completion.

   procedure Build_Derived_Record_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Derive_Subps : Boolean := True);
   --  Subsidiary procedure for Build_Derived_Type and
   --  Analyze_Private_Extension_Declaration used for tagged and untagged
   --  record types. All parameters are as in Build_Derived_Type except that
   --  N, in addition to being an N_Full_Type_Declaration node, can also be an
   --  N_Private_Extension_Declaration node. See the definition of this routine
   --  for much more info. Derive_Subps indicates whether subprograms should
   --  be derived from the parent type. The only case where Derive_Subps is
   --  False is for an implicit derived full type for a type derived from a
   --  private type (see Build_Derived_Type).

   procedure Complete_Subprograms_Derivation
     (Partial_View : Entity_Id;
      Derived_Type : Entity_Id);
   --  Ada 2005 (AI-251): Used to complete type derivation of private tagged
   --  types implementing interfaces. In this case some interface primitives
   --  may have been overriden with the partial-view and, instead of
   --  re-calculating them, they are included in the list of primitive
   --  operations of the full-view.

   function Inherit_Components
     (N             : Node_Id;
      Parent_Base   : Entity_Id;
      Derived_Base  : Entity_Id;
      Is_Tagged     : Boolean;
      Inherit_Discr : Boolean;
      Discs         : Elist_Id) return Elist_Id;
   --  Called from Build_Derived_Record_Type to inherit the components of
   --  Parent_Base (a base type) into the Derived_Base (the derived base type).
   --  For more information on derived types and component inheritance please
   --  consult the comment above the body of Build_Derived_Record_Type.
   --
   --    N is the original derived type declaration
   --
   --    Is_Tagged is set if we are dealing with tagged types
   --
   --    If Inherit_Discr is set, Derived_Base inherits its discriminants
   --    from Parent_Base, otherwise no discriminants are inherited.
   --
   --    Discs gives the list of constraints that apply to Parent_Base in the
   --    derived type declaration. If Discs is set to No_Elist, then we have
   --    the following situation:
   --
   --      type Parent (D1..Dn : ..) is [tagged] record ...;
   --      type Derived is new Parent [with ...];
   --
   --    which gets treated as
   --
   --      type Derived (D1..Dn : ..) is new Parent (D1,..,Dn) [with ...];
   --
   --  For untagged types the returned value is an association list. The list
   --  starts from the association (Parent_Base => Derived_Base), and then it
   --  contains a sequence of the associations of the form
   --
   --    (Old_Component => New_Component),
   --
   --  where Old_Component is the Entity_Id of a component in Parent_Base
   --  and New_Component is the Entity_Id of the corresponding component
   --  in Derived_Base. For untagged records, this association list is
   --  needed when copying the record declaration for the derived base.
   --  In the tagged case the value returned is irrelevant.

   procedure Build_Discriminal (Discrim : Entity_Id);
   --  Create the discriminal corresponding to discriminant Discrim, that is
   --  the parameter corresponding to Discrim to be used in initialization
   --  procedures for the type where Discrim is a discriminant. Discriminals
   --  are not used during semantic analysis, and are not fully defined
   --  entities until expansion. Thus they are not given a scope until
   --  initialization procedures are built.

   function Build_Discriminant_Constraints
     (T           : Entity_Id;
      Def         : Node_Id;
      Derived_Def : Boolean := False) return Elist_Id;
   --  Validate discriminant constraints, and return the list of the
   --  constraints in order of discriminant declarations. T is the
   --  discriminated unconstrained type. Def is the N_Subtype_Indication node
   --  where the discriminants constraints for T are specified. Derived_Def is
   --  True if we are building the discriminant constraints in a derived type
   --  definition of the form "type D (...) is new T (xxx)". In this case T is
   --  the parent type and Def is the constraint "(xxx)" on T and this routine
   --  sets the Corresponding_Discriminant field of the discriminants in the
   --  derived type D to point to the corresponding discriminants in the parent
   --  type T.

   procedure Build_Discriminated_Subtype
     (T           : Entity_Id;
      Def_Id      : Entity_Id;
      Elist       : Elist_Id;
      Related_Nod : Node_Id;
      For_Access  : Boolean := False);
   --  Subsidiary procedure to Constrain_Discriminated_Type and to
   --  Process_Incomplete_Dependents. Given
   --
   --     T (a possibly discriminated base type)
   --     Def_Id (a very partially built subtype for T),
   --
   --  the call completes Def_Id to be the appropriate E_*_Subtype.
   --
   --  The Elist is the list of discriminant constraints if any (it is set to
   --  No_Elist if T is not a discriminated type, and to an empty list if
   --  T has discriminants but there are no discriminant constraints). The
   --  Related_Nod is the same as Decl_Node in Create_Constrained_Components.
   --  The For_Access says whether or not this subtype is really constraining
   --  an access type. That is its sole purpose is the designated type of an
   --  access type -- in which case a Private_Subtype Is_For_Access_Subtype
   --  is built to avoid freezing T when the access subtype is frozen.

   function Build_Scalar_Bound
     (Bound : Node_Id;
      Par_T : Entity_Id;
      Der_T : Entity_Id) return Node_Id;
   --  The bounds of a derived scalar type are conversions of the bounds of
   --  the parent type. Optimize the representation if the bounds are literals.
   --  Needs a more complete spec--what are the parameters exactly, and what
   --  exactly is the returned value, and how is Bound affected???

   procedure Build_Underlying_Full_View
     (N   : Node_Id;
      Typ : Entity_Id;
      Par : Entity_Id);
   --  If the completion of a private type is itself derived from a private
   --  type, or if the full view of a private subtype is itself private, the
   --  back-end has no way to compute the actual size of this type. We build
   --  an internal subtype declaration of the proper parent type to convey
   --  this information. This extra mechanism is needed because a full
   --  view cannot itself have a full view (it would get clobbered during
   --  view exchanges).

   procedure Check_Access_Discriminant_Requires_Limited
     (D   : Node_Id;
      Loc : Node_Id);
   --  Check the restriction that the type to which an access discriminant
   --  belongs must be a concurrent type or a descendant of a type with
   --  the reserved word 'limited' in its declaration.

   procedure Check_Delta_Expression (E : Node_Id);
   --  Check that the expression represented by E is suitable for use
   --  as a delta expression, i.e. it is of real type and is static.

   procedure Check_Digits_Expression (E : Node_Id);
   --  Check that the expression represented by E is suitable for use as
   --  a digits expression, i.e. it is of integer type, positive and static.

   procedure Check_Initialization (T : Entity_Id; Exp : Node_Id);
   --  Validate the initialization of an object declaration. T is the
   --  required type, and Exp is the initialization expression.

   procedure Check_Or_Process_Discriminants
     (N    : Node_Id;
      T    : Entity_Id;
      Prev : Entity_Id := Empty);
   --  If T is the full declaration of an incomplete or private type, check
   --  the conformance of the discriminants, otherwise process them. Prev
   --  is the entity of the partial declaration, if any.

   procedure Check_Real_Bound (Bound : Node_Id);
   --  Check given bound for being of real type and static. If not, post an
   --  appropriate message, and rewrite the bound with the real literal zero.

   procedure Constant_Redeclaration
     (Id : Entity_Id;
      N  : Node_Id;
      T  : out Entity_Id);
   --  Various checks on legality of full declaration of deferred constant.
   --  Id is the entity for the redeclaration, N is the N_Object_Declaration,
   --  node. The caller has not yet set any attributes of this entity.

   procedure Convert_Scalar_Bounds
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Loc          : Source_Ptr);
   --  For derived scalar types, convert the bounds in the type definition
   --  to the derived type, and complete their analysis. Given a constraint
   --  of the form:
   --                   ..  new T range Lo .. Hi;
   --  Lo and Hi are analyzed and resolved with T'Base, the parent_type.
   --  The bounds of the derived type (the anonymous base) are copies of
   --  Lo and Hi.  Finally, the bounds of the derived subtype are conversions
   --  of those bounds to the derived_type, so that their typing is
   --  consistent.

   procedure Copy_Array_Base_Type_Attributes (T1, T2 : Entity_Id);
   --  Copies attributes from array base type T2 to array base type T1.
   --  Copies only attributes that apply to base types, but not subtypes.

   procedure Copy_Array_Subtype_Attributes (T1, T2 : Entity_Id);
   --  Copies attributes from array subtype T2 to array subtype T1. Copies
   --  attributes that apply to both subtypes and base types.

   procedure Create_Constrained_Components
     (Subt        : Entity_Id;
      Decl_Node   : Node_Id;
      Typ         : Entity_Id;
      Constraints : Elist_Id);
   --  Build the list of entities for a constrained discriminated record
   --  subtype. If a component depends on a discriminant, replace its subtype
   --  using the discriminant values in the discriminant constraint.
   --  Subt is the defining identifier for the subtype whose list of
   --  constrained entities we will create. Decl_Node is the type declaration
   --  node where we will attach all the itypes created. Typ is the base
   --  discriminated type for the subtype Subt. Constraints is the list of
   --  discriminant constraints for Typ.

   function Constrain_Component_Type
     (Comp            : Entity_Id;
      Constrained_Typ : Entity_Id;
      Related_Node    : Node_Id;
      Typ             : Entity_Id;
      Constraints     : Elist_Id) return Entity_Id;
   --  Given a discriminated base type Typ, a list of discriminant constraint
   --  Constraints for Typ and a component of Typ, with type Compon_Type,
   --  create and return the type corresponding to Compon_type where all
   --  discriminant references are replaced with the corresponding
   --  constraint. If no discriminant references occur in Compon_Typ then
   --  return it as is. Constrained_Typ is the final constrained subtype to
   --  which the constrained Compon_Type belongs. Related_Node is the node
   --  where we will attach all the itypes created.

   procedure Constrain_Access
     (Def_Id      : in out Entity_Id;
      S           : Node_Id;
      Related_Nod : Node_Id);
   --  Apply a list of constraints to an access type. If Def_Id is empty, it is
   --  an anonymous type created for a subtype indication. In that case it is
   --  created in the procedure and attached to Related_Nod.

   procedure Constrain_Array
     (Def_Id      : in out Entity_Id;
      SI          : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id;
      Suffix      : Character);
   --  Apply a list of index constraints to an unconstrained array type. The
   --  first parameter is the entity for the resulting subtype. A value of
   --  Empty for Def_Id indicates that an implicit type must be created, but
   --  creation is delayed (and must be done by this procedure) because other
   --  subsidiary implicit types must be created first (which is why Def_Id
   --  is an in/out parameter). The second parameter is a subtype indication
   --  node for the constrained array to be created (e.g. something of the
   --  form string (1 .. 10)). Related_Nod gives the place where this type
   --  has to be inserted in the tree. The Related_Id and Suffix parameters
   --  are used to build the associated Implicit type name.

   procedure Constrain_Concurrent
     (Def_Id      : in out Entity_Id;
      SI          : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id;
      Suffix      : Character);
   --  Apply list of discriminant constraints to an unconstrained concurrent
   --  type.
   --
   --    SI is the N_Subtype_Indication node containing the constraint and
   --    the unconstrained type to constrain.
   --
   --    Def_Id is the entity for the resulting constrained subtype. A value
   --    of Empty for Def_Id indicates that an implicit type must be created,
   --    but creation is delayed (and must be done by this procedure) because
   --    other subsidiary implicit types must be created first (which is why
   --    Def_Id is an in/out parameter).
   --
   --    Related_Nod gives the place where this type has to be inserted
   --    in the tree
   --
   --  The last two arguments are used to create its external name if needed.

   function Constrain_Corresponding_Record
     (Prot_Subt   : Entity_Id;
      Corr_Rec    : Entity_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id) return Entity_Id;
   --  When constraining a protected type or task type with discriminants,
   --  constrain the corresponding record with the same discriminant values.

   procedure Constrain_Decimal (Def_Id : Node_Id; S : Node_Id);
   --  Constrain a decimal fixed point type with a digits constraint and/or a
   --  range constraint, and build E_Decimal_Fixed_Point_Subtype entity.

   procedure Constrain_Discriminated_Type
     (Def_Id      : Entity_Id;
      S           : Node_Id;
      Related_Nod : Node_Id;
      For_Access  : Boolean := False);
   --  Process discriminant constraints of composite type. Verify that values
   --  have been provided for all discriminants, that the original type is
   --  unconstrained, and that the types of the supplied expressions match
   --  the discriminant types. The first three parameters are like in routine
   --  Constrain_Concurrent. See Build_Discriminated_Subtype for an explanation
   --  of For_Access.

   procedure Constrain_Enumeration (Def_Id : Node_Id; S : Node_Id);
   --  Constrain an enumeration type with a range constraint. This is identical
   --  to Constrain_Integer, but for the Ekind of the resulting subtype.

   procedure Constrain_Float (Def_Id : Node_Id; S : Node_Id);
   --  Constrain a floating point type with either a digits constraint
   --  and/or a range constraint, building a E_Floating_Point_Subtype.

   procedure Constrain_Index
     (Index        : Node_Id;
      S            : Node_Id;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat);
   --  Process an index constraint in a constrained array declaration. The
   --  constraint can be a subtype name, or a range with or without an
   --  explicit subtype mark. The index is the corresponding index of the
   --  unconstrained array. The Related_Id and Suffix parameters are used to
   --  build the associated Implicit type name.

   procedure Constrain_Integer (Def_Id : Node_Id; S : Node_Id);
   --  Build subtype of a signed or modular integer type

   procedure Constrain_Ordinary_Fixed (Def_Id : Node_Id; S : Node_Id);
   --  Constrain an ordinary fixed point type with a range constraint, and
   --  build an E_Ordinary_Fixed_Point_Subtype entity.

   procedure Copy_And_Swap (Priv, Full : Entity_Id);
   --  Copy the Priv entity into the entity of its full declaration
   --  then swap the two entities in such a manner that the former private
   --  type is now seen as a full type.

   procedure Decimal_Fixed_Point_Type_Declaration
     (T   : Entity_Id;
      Def : Node_Id);
   --  Create a new decimal fixed point type, and apply the constraint to
   --  obtain a subtype of this new type.

   procedure Complete_Private_Subtype
     (Priv        : Entity_Id;
      Full        : Entity_Id;
      Full_Base   : Entity_Id;
      Related_Nod : Node_Id);
   --  Complete the implicit full view of a private subtype by setting the
   --  appropriate semantic fields. If the full view of the parent is a record
   --  type, build constrained components of subtype.

   procedure Derive_Interface_Subprograms
     (Derived_Type : Entity_Id);
   --  Ada 2005 (AI-251): Subsidiary procedure to Build_Derived_Record_Type.
   --  Traverse the list of implemented interfaces and derive all their
   --  subprograms.

   procedure Derived_Standard_Character
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Enumeration_Type which handles
   --  derivations from types Standard.Character and Standard.Wide_Character.

   procedure Derived_Type_Declaration
     (T             : Entity_Id;
      N             : Node_Id;
      Is_Completion : Boolean);
   --  Process a derived type declaration. This routine will invoke
   --  Build_Derived_Type to process the actual derived type definition.
   --  Parameters N and Is_Completion have the same meaning as in
   --  Build_Derived_Type. T is the N_Defining_Identifier for the entity
   --  defined in the N_Full_Type_Declaration node N, that is T is the derived
   --  type.

   procedure Enumeration_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Insert each literal in symbol table, as an overloadable identifier. Each
   --  enumeration type is mapped into a sequence of integers, and each literal
   --  is defined as a constant with integer value. If any of the literals are
   --  character literals, the type is a character type, which means that
   --  strings are legal aggregates for arrays of components of the type.

   function Expand_To_Stored_Constraint
     (Typ        : Entity_Id;
      Constraint : Elist_Id) return Elist_Id;
   --  Given a Constraint (i.e. a list of expressions) on the discriminants of
   --  Typ, expand it into a constraint on the stored discriminants and return
   --  the new list of expressions constraining the stored discriminants.

   function Find_Type_Of_Object
     (Obj_Def     : Node_Id;
      Related_Nod : Node_Id) return Entity_Id;
   --  Get type entity for object referenced by Obj_Def, attaching the
   --  implicit types generated to Related_Nod

   procedure Floating_Point_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create a new float, and apply the constraint to obtain subtype of it

   function Has_Range_Constraint (N : Node_Id) return Boolean;
   --  Given an N_Subtype_Indication node N, return True if a range constraint
   --  is present, either directly, or as part of a digits or delta constraint.
   --  In addition, a digits constraint in the decimal case returns True, since
   --  it establishes a default range if no explicit range is present.

   function Is_Valid_Constraint_Kind
     (T_Kind          : Type_Kind;
      Constraint_Kind : Node_Kind) return Boolean;
   --  Returns True if it is legal to apply the given kind of constraint to the
   --  given kind of type (index constraint to an array type, for example).

   procedure Modular_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create new modular type. Verify that modulus is in  bounds and is
   --  a power of two (implementation restriction).

   procedure New_Concatenation_Op (Typ : Entity_Id);
   --  Create an abbreviated declaration for an operator in order to
   --  materialize concatenation on array types.

   procedure Ordinary_Fixed_Point_Type_Declaration
     (T   : Entity_Id;
      Def : Node_Id);
   --  Create a new ordinary fixed point type, and apply the constraint to
   --  obtain subtype of it.

   procedure Prepare_Private_Subtype_Completion
     (Id          : Entity_Id;
      Related_Nod : Node_Id);
   --  Id is a subtype of some private type. Creates the full declaration
   --  associated with Id whenever possible, i.e. when the full declaration
   --  of the base type is already known. Records each subtype into
   --  Private_Dependents of the base type.

   procedure Process_Incomplete_Dependents
     (N      : Node_Id;
      Full_T : Entity_Id;
      Inc_T  : Entity_Id);
   --  Process all entities that depend on an incomplete type. There include
   --  subtypes, subprogram types that mention the incomplete type in their
   --  profiles, and subprogram with access parameters that designate the
   --  incomplete type.

   --  Inc_T is the defining identifier of an incomplete type declaration, its
   --  Ekind is E_Incomplete_Type.
   --
   --    N is the corresponding N_Full_Type_Declaration for Inc_T.
   --
   --    Full_T is N's defining identifier.
   --
   --  Subtypes of incomplete types with discriminants are completed when the
   --  parent type is. This is simpler than private subtypes, because they can
   --  only appear in the same scope, and there is no need to exchange views.
   --  Similarly, access_to_subprogram types may have a parameter or a return
   --  type that is an incomplete type, and that must be replaced with the
   --  full type.

   --  If the full type is tagged, subprogram with access parameters that
   --  designated the incomplete may be primitive operations of the full type,
   --  and have to be processed accordingly.

   procedure Process_Real_Range_Specification (Def : Node_Id);
   --  Given the type definition for a real type, this procedure processes
   --  and checks the real range specification of this type definition if
   --  one is present. If errors are found, error messages are posted, and
   --  the Real_Range_Specification of Def is reset to Empty.

   procedure Record_Type_Declaration
     (T    : Entity_Id;
      N    : Node_Id;
      Prev : Entity_Id);
   --  Process a record type declaration (for both untagged and tagged
   --  records). Parameters T and N are exactly like in procedure
   --  Derived_Type_Declaration, except that no flag Is_Completion is needed
   --  for this routine. If this is the completion of an incomplete type
   --  declaration, Prev is the entity of the incomplete declaration, used for
   --  cross-referencing. Otherwise Prev = T.

   procedure Record_Type_Definition (Def : Node_Id; Prev_T : Entity_Id);
   --  This routine is used to process the actual record type definition
   --  (both for untagged and tagged records). Def is a record type
   --  definition node. This procedure analyzes the components in this
   --  record type definition. Prev_T is the entity for the enclosing record
   --  type. It is provided so that its Has_Task flag can be set if any of
   --  the component have Has_Task set. If the declaration is the completion
   --  of an incomplete type declaration, Prev_T is the original incomplete
   --  type, whose full view is the record type.

   procedure Replace_Components (Typ : Entity_Id; Decl : Node_Id);
   --  Subsidiary to Build_Derived_Record_Type. For untagged records, we
   --  build a copy of the declaration tree of the parent, and we create
   --  independently the list of components for the derived type. Semantic
   --  information uses the component entities, but record representation
   --  clauses are validated on the declaration tree. This procedure replaces
   --  discriminants and components in the declaration with those that have
   --  been created by Inherit_Components.

   procedure Set_Fixed_Range
     (E   : Entity_Id;
      Loc : Source_Ptr;
      Lo  : Ureal;
      Hi  : Ureal);
   --  Build a range node with the given bounds and set it as the Scalar_Range
   --  of the given fixed-point type entity. Loc is the source location used
   --  for the constructed range. See body for further details.

   procedure Set_Scalar_Range_For_Subtype
     (Def_Id : Entity_Id;
      R      : Node_Id;
      Subt   : Entity_Id);
   --  This routine is used to set the scalar range field for a subtype
   --  given Def_Id, the entity for the subtype, and R, the range expression
   --  for the scalar range. Subt provides the parent subtype to be used
   --  to analyze, resolve, and check the given range.

   procedure Signed_Integer_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create a new signed integer entity, and apply the constraint to obtain
   --  the required first named subtype of this type.

   procedure Set_Stored_Constraint_From_Discriminant_Constraint
     (E : Entity_Id);
   --  E is some record type. This routine computes E's Stored_Constraint
   --  from its Discriminant_Constraint.

   -----------------------
   -- Access_Definition --
   -----------------------

   function Access_Definition
     (Related_Nod : Node_Id;
      N           : Node_Id) return Entity_Id
   is
      Anon_Type : constant Entity_Id :=
                    Create_Itype (E_Anonymous_Access_Type, Related_Nod,
                                  Scope_Id => Scope (Current_Scope));
      Desig_Type : Entity_Id;

   begin
      if Is_Entry (Current_Scope)
        and then Is_Task_Type (Etype (Scope (Current_Scope)))
      then
         Error_Msg_N ("task entries cannot have access parameters", N);
      end if;

      --  Ada 2005: for an object declaration or function with an anonymous
      --  access result, the corresponding anonymous type is declared in the
      --  current scope. For access formals, access components, and access
      --  discriminants, the scope is that of the enclosing declaration,
      --  as set above. This special-case handling of resetting the scope
      --  is awkward, and it might be better to pass in the required scope
      --  as a parameter. ???

      if Nkind (Related_Nod) = N_Object_Declaration then
         Set_Scope (Anon_Type, Current_Scope);

      --  For the anonymous function result case, retrieve the scope of
      --  the function specification's associated entity rather than using
      --  the current scope. The current scope will be the function itself
      --  if the formal part is currently being analyzed, but will be the
      --  parent scope in the case of a parameterless function, and we
      --  always want to use the function's parent scope.

      elsif Nkind (Related_Nod) = N_Function_Specification
         and then Nkind (Parent (N)) /= N_Parameter_Specification
      then
         Set_Scope (Anon_Type, Scope (Defining_Unit_Name (Related_Nod)));
      end if;

      if All_Present (N)
        and then Ada_Version >= Ada_05
      then
         Error_Msg_N ("ALL is not permitted for anonymous access types", N);
      end if;

      --  Ada 2005 (AI-254): In case of anonymous access to subprograms
      --  call the corresponding semantic routine

      if Present (Access_To_Subprogram_Definition (N)) then
         Access_Subprogram_Declaration
           (T_Name => Anon_Type,
            T_Def  => Access_To_Subprogram_Definition (N));

         if Ekind (Anon_Type) = E_Access_Protected_Subprogram_Type then
            Set_Ekind
              (Anon_Type, E_Anonymous_Access_Protected_Subprogram_Type);
         else
            Set_Ekind
              (Anon_Type, E_Anonymous_Access_Subprogram_Type);
         end if;

         return Anon_Type;
      end if;

      Find_Type (Subtype_Mark (N));
      Desig_Type := Entity (Subtype_Mark (N));

      Set_Directly_Designated_Type
                             (Anon_Type, Desig_Type);
      Set_Etype              (Anon_Type, Anon_Type);
      Init_Size_Align        (Anon_Type);
      Set_Depends_On_Private (Anon_Type, Has_Private_Component (Anon_Type));

      --  Ada 2005 (AI-231): Ada 2005 semantics for anonymous access differs
      --  from Ada 95 semantics. In Ada 2005, anonymous access must specify
      --  if the null value is allowed. In Ada 95 the null value is never
      --  allowed.

      if Ada_Version >= Ada_05 then
         Set_Can_Never_Be_Null (Anon_Type, Null_Exclusion_Present (N));
      else
         Set_Can_Never_Be_Null (Anon_Type, True);
      end if;

      --  The anonymous access type is as public as the discriminated type or
      --  subprogram that defines it. It is imported (for back-end purposes)
      --  if the designated type is.

      Set_Is_Public (Anon_Type, Is_Public (Scope (Anon_Type)));

      --  Ada 2005 (AI-50217): Propagate the attribute that indicates that the
      --  designated type comes from the limited view (for back-end purposes).

      Set_From_With_Type (Anon_Type, From_With_Type (Desig_Type));

      --  Ada 2005 (AI-231): Propagate the access-constant attribute

      Set_Is_Access_Constant (Anon_Type, Constant_Present (N));

      --  The context is either a subprogram declaration, object declaration,
      --  or an access discriminant, in a private or a full type declaration.
      --  In the case of a subprogram, if the designated type is incomplete,
      --  the operation will be a primitive operation of the full type, to be
      --  updated subsequently. If the type is imported through a limited_with
      --  clause, the subprogram is not a primitive operation of the type
      --  (which is declared elsewhere in some other scope).

      if Ekind (Desig_Type) = E_Incomplete_Type
        and then not From_With_Type (Desig_Type)
        and then Is_Overloadable (Current_Scope)
      then
         Append_Elmt (Current_Scope, Private_Dependents (Desig_Type));
         Set_Has_Delayed_Freeze (Current_Scope);
      end if;

      --  Ada 2005: if the designated type is an interface that may contain
      --  tasks, create a Master entity for the declaration. This must be done
      --  before expansion of the full declaration, because the declaration
      --  may include an expression that is an allocator, whose expansion needs
      --  the proper Master for the created tasks.

      if Nkind (Related_Nod) = N_Object_Declaration
         and then Expander_Active
         and then Is_Interface (Desig_Type)
         and then Is_Limited_Record (Desig_Type)
      then
         Build_Class_Wide_Master (Anon_Type);
      end if;

      return Anon_Type;
   end Access_Definition;

   -----------------------------------
   -- Access_Subprogram_Declaration --
   -----------------------------------

   procedure Access_Subprogram_Declaration
     (T_Name : Entity_Id;
      T_Def  : Node_Id)
   is
      Formals : constant List_Id := Parameter_Specifications (T_Def);
      Formal  : Entity_Id;
      D_Ityp  : Node_Id;

      Desig_Type : constant Entity_Id :=
                     Create_Itype (E_Subprogram_Type, Parent (T_Def));

   begin
      --  Associate the Itype node with the inner full-type declaration
      --  or subprogram spec. This is required to handle nested anonymous
      --  declarations. For example:

      --      procedure P
      --       (X : access procedure
      --                     (Y : access procedure
      --                                   (Z : access T)))

      D_Ityp := Associated_Node_For_Itype (Desig_Type);
      while Nkind (D_Ityp) /= N_Full_Type_Declaration
         and then Nkind (D_Ityp) /= N_Procedure_Specification
         and then Nkind (D_Ityp) /= N_Function_Specification
         and then Nkind (D_Ityp) /= N_Object_Declaration
         and then Nkind (D_Ityp) /= N_Object_Renaming_Declaration
         and then Nkind (D_Ityp) /= N_Formal_Type_Declaration
      loop
         D_Ityp := Parent (D_Ityp);
         pragma Assert (D_Ityp /= Empty);
      end loop;

      Set_Associated_Node_For_Itype (Desig_Type, D_Ityp);

      if Nkind (D_Ityp) = N_Procedure_Specification
        or else Nkind (D_Ityp) = N_Function_Specification
      then
         Set_Scope (Desig_Type, Scope (Defining_Unit_Name (D_Ityp)));

      elsif Nkind (D_Ityp) = N_Full_Type_Declaration
        or else Nkind (D_Ityp) = N_Object_Declaration
        or else Nkind (D_Ityp) = N_Object_Renaming_Declaration
        or else Nkind (D_Ityp) = N_Formal_Type_Declaration
      then
         Set_Scope (Desig_Type, Scope (Defining_Identifier (D_Ityp)));
      end if;

      if Nkind (T_Def) = N_Access_Function_Definition then
         if Nkind (Result_Definition (T_Def)) = N_Access_Definition then
            Set_Etype
              (Desig_Type,
               Access_Definition (T_Def, Result_Definition (T_Def)));
         else
            Analyze (Result_Definition (T_Def));
            Set_Etype (Desig_Type, Entity (Result_Definition (T_Def)));
         end if;

         if not (Is_Type (Etype (Desig_Type))) then
            Error_Msg_N
              ("expect type in function specification",
               Result_Definition (T_Def));
         end if;

      else
         Set_Etype (Desig_Type, Standard_Void_Type);
      end if;

      if Present (Formals) then
         New_Scope (Desig_Type);
         Process_Formals (Formals, Parent (T_Def));

         --  A bit of a kludge here, End_Scope requires that the parent
         --  pointer be set to something reasonable, but Itypes don't have
         --  parent pointers. So we set it and then unset it ??? If and when
         --  Itypes have proper parent pointers to their declarations, this
         --  kludge can be removed.

         Set_Parent (Desig_Type, T_Name);
         End_Scope;
         Set_Parent (Desig_Type, Empty);
      end if;

      --  The return type and/or any parameter type may be incomplete. Mark
      --  the subprogram_type as depending on the incomplete type, so that
      --  it can be updated when the full type declaration is seen.

      if Present (Formals) then
         Formal := First_Formal (Desig_Type);
         while Present (Formal) loop
            if Ekind (Formal) /= E_In_Parameter
              and then Nkind (T_Def) = N_Access_Function_Definition
            then
               Error_Msg_N ("functions can only have IN parameters", Formal);
            end if;

            if Ekind (Etype (Formal)) = E_Incomplete_Type then
               Append_Elmt (Desig_Type, Private_Dependents (Etype (Formal)));
               Set_Has_Delayed_Freeze (Desig_Type);
            end if;

            Next_Formal (Formal);
         end loop;
      end if;

      if Ekind (Etype (Desig_Type)) = E_Incomplete_Type
        and then not Has_Delayed_Freeze (Desig_Type)
      then
         Append_Elmt (Desig_Type, Private_Dependents (Etype (Desig_Type)));
         Set_Has_Delayed_Freeze (Desig_Type);
      end if;

      Check_Delayed_Subprogram (Desig_Type);

      if Protected_Present (T_Def) then
         Set_Ekind (T_Name, E_Access_Protected_Subprogram_Type);
         Set_Convention (Desig_Type, Convention_Protected);
      else
         Set_Ekind (T_Name, E_Access_Subprogram_Type);
      end if;

      Set_Etype                    (T_Name, T_Name);
      Init_Size_Align              (T_Name);
      Set_Directly_Designated_Type (T_Name, Desig_Type);

      --  Ada 2005 (AI-231): Propagate the null-excluding attribute

      Set_Can_Never_Be_Null (T_Name, Null_Exclusion_Present (T_Def));

      Check_Restriction (No_Access_Subprograms, T_Def);
   end Access_Subprogram_Declaration;

   ----------------------------
   -- Access_Type_Declaration --
   ----------------------------

   procedure Access_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      S : constant Node_Id := Subtype_Indication (Def);
      P : constant Node_Id := Parent (Def);

      Desig : Entity_Id;
      --  Designated type

   begin
      --  Check for permissible use of incomplete type

      if Nkind (S) /= N_Subtype_Indication then
         Analyze (S);

         if Ekind (Root_Type (Entity (S))) = E_Incomplete_Type then
            Set_Directly_Designated_Type (T, Entity (S));
         else
            Set_Directly_Designated_Type (T,
              Process_Subtype (S, P, T, 'P'));
         end if;

      else
         Set_Directly_Designated_Type (T,
           Process_Subtype (S, P, T, 'P'));
      end if;

      if All_Present (Def) or Constant_Present (Def) then
         Set_Ekind (T, E_General_Access_Type);
      else
         Set_Ekind (T, E_Access_Type);
      end if;

      if Base_Type (Designated_Type (T)) = T then
         Error_Msg_N ("access type cannot designate itself", S);

      --  In Ada 2005, the type may have a limited view through some unit
      --  in its own context, allowing the following circularity that cannot
      --  be detected earlier

      elsif Is_Class_Wide_Type (Designated_Type (T))
        and then Etype (Designated_Type (T)) = T
      then
         Error_Msg_N
           ("access type cannot designate its own classwide type", S);

         --  Clean up indication of tagged status to prevent cascaded errors

         Set_Is_Tagged_Type (T, False);
      end if;

      Set_Etype (T, T);

      --  If the type has appeared already in a with_type clause, it is
      --  frozen and the pointer size is already set. Else, initialize.

      if not From_With_Type (T) then
         Init_Size_Align (T);
      end if;

      Set_Is_Access_Constant (T, Constant_Present (Def));

      Desig := Designated_Type (T);

      --  If designated type is an imported tagged type, indicate that the
      --  access type is also imported, and therefore restricted in its use.
      --  The access type may already be imported, so keep setting otherwise.

      --  Ada 2005 (AI-50217): If the non-limited view of the designated type
      --  is available, use it as the designated type of the access type, so
      --  that the back-end gets a usable entity.

      declare
         N_Desig : Entity_Id;

      begin
         if From_With_Type (Desig)
           and then Ekind (Desig) /= E_Access_Type
         then
            Set_From_With_Type (T);

            if Ekind (Desig) = E_Incomplete_Type then
               N_Desig := Non_Limited_View (Desig);

            else pragma Assert (Ekind (Desig) = E_Class_Wide_Type);
               if From_With_Type (Etype (Desig)) then
                  N_Desig := Non_Limited_View (Etype (Desig));
               else
                  N_Desig := Etype (Desig);
               end if;
            end if;

            pragma Assert (Present (N_Desig));
            Set_Directly_Designated_Type (T, N_Desig);
         end if;
      end;

      --  Note that Has_Task is always false, since the access type itself
      --  is not a task type. See Einfo for more description on this point.
      --  Exactly the same consideration applies to Has_Controlled_Component.

      Set_Has_Task (T, False);
      Set_Has_Controlled_Component (T, False);

      --  Ada 2005 (AI-231): Propagate the null-excluding and access-constant
      --  attributes

      Set_Can_Never_Be_Null  (T, Null_Exclusion_Present (Def));
      Set_Is_Access_Constant (T, Constant_Present (Def));
   end Access_Type_Declaration;

   ----------------------------------
   -- Add_Interface_Tag_Components --
   ----------------------------------

   procedure Add_Interface_Tag_Components
     (N        : Node_Id;
      Typ      : Entity_Id)
   is
      Loc      : constant Source_Ptr := Sloc (N);
      Elmt     : Elmt_Id;
      Ext      : Node_Id;
      L        : List_Id;
      Last_Tag : Node_Id;
      Comp     : Node_Id;

      procedure Add_Tag (Iface : Entity_Id);
      --  Comment required ???

      -------------
      -- Add_Tag --
      -------------

      procedure Add_Tag (Iface : Entity_Id) is
         Def      : Node_Id;
         Tag      : Entity_Id;
         Decl     : Node_Id;

      begin
         pragma Assert (Is_Tagged_Type (Iface)
           and then Is_Interface (Iface));

         Def :=
           Make_Component_Definition (Loc,
             Aliased_Present    => True,
             Subtype_Indication =>
               New_Occurrence_Of (RTE (RE_Interface_Tag), Loc));

         Tag := Make_Defining_Identifier (Loc, New_Internal_Name ('V'));

         Decl :=
           Make_Component_Declaration (Loc,
             Defining_Identifier  => Tag,
             Component_Definition => Def);

         Analyze_Component_Declaration (Decl);

         Set_Analyzed (Decl);
         Set_Ekind               (Tag, E_Component);
         Set_Is_Limited_Record   (Tag);
         Set_Is_Tag              (Tag);
         Init_Component_Location (Tag);

         pragma Assert (Is_Frozen (Iface));

         Set_DT_Entry_Count    (Tag,
           DT_Entry_Count (First_Entity (Iface)));

         if not Present (Last_Tag) then
            Prepend (Decl, L);
         else
            Insert_After (Last_Tag, Decl);
         end if;

         Last_Tag := Decl;
      end Add_Tag;

   --  Start of processing for Add_Interface_Tag_Components

   begin
      if Ekind (Typ) /= E_Record_Type
        or else not Present (Abstract_Interfaces (Typ))
        or else Is_Empty_Elmt_List (Abstract_Interfaces (Typ))
      then
         return;
      end if;

      if Present (Abstract_Interfaces (Typ)) then

         --  Find the current last tag

         if Nkind (Type_Definition (N)) = N_Derived_Type_Definition then
            Ext := Record_Extension_Part (Type_Definition (N));
         else
            pragma Assert (Nkind (Type_Definition (N)) = N_Record_Definition);
            Ext := Type_Definition (N);
         end if;

         Last_Tag := Empty;

         if not (Present (Component_List (Ext))) then
            Set_Null_Present (Ext, False);
            L := New_List;
            Set_Component_List (Ext,
              Make_Component_List (Loc,
                Component_Items => L,
                Null_Present => False));
         else
            if Nkind (Type_Definition (N)) = N_Derived_Type_Definition then
               L := Component_Items
                      (Component_List
                        (Record_Extension_Part
                          (Type_Definition (N))));
            else
               L := Component_Items
                      (Component_List
                        (Type_Definition (N)));
            end if;

            --  Find the last tag component

            Comp := First (L);
            while Present (Comp) loop
               if Is_Tag (Defining_Identifier (Comp)) then
                  Last_Tag := Comp;
               end if;

               Next (Comp);
            end loop;
         end if;

         --  At this point L references the list of components and Last_Tag
         --  references the current last tag (if any). Now we add the tag
         --  corresponding with all the interfaces that are not implemented
         --  by the parent.

         pragma Assert (Present
                        (First_Elmt (Abstract_Interfaces (Typ))));

         Elmt := First_Elmt (Abstract_Interfaces (Typ));
         while Present (Elmt) loop
            Add_Tag (Node (Elmt));
            Next_Elmt (Elmt);
         end loop;
      end if;
   end Add_Interface_Tag_Components;

   -----------------------------------
   -- Analyze_Component_Declaration --
   -----------------------------------

   procedure Analyze_Component_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);
      T  : Entity_Id;
      P  : Entity_Id;

      function Contains_POC (Constr : Node_Id) return Boolean;
      --  Determines whether a constraint uses the discriminant of a record
      --  type thus becoming a per-object constraint (POC).

      ------------------
      -- Contains_POC --
      ------------------

      function Contains_POC (Constr : Node_Id) return Boolean is
      begin
         case Nkind (Constr) is
            when N_Attribute_Reference =>
               return Attribute_Name (Constr) = Name_Access
                        and
                      Prefix (Constr) = Scope (Entity (Prefix (Constr)));

            when N_Discriminant_Association =>
               return Denotes_Discriminant (Expression (Constr));

            when N_Identifier =>
               return Denotes_Discriminant (Constr);

            when N_Index_Or_Discriminant_Constraint =>
               declare
                  IDC : Node_Id;

               begin
                  IDC := First (Constraints (Constr));
                  while Present (IDC) loop

                     --  One per-object constraint is sufficient

                     if Contains_POC (IDC) then
                        return True;
                     end if;

                     Next (IDC);
                  end loop;

                  return False;
               end;

            when N_Range =>
               return Denotes_Discriminant (Low_Bound (Constr))
                        or else
                      Denotes_Discriminant (High_Bound (Constr));

            when N_Range_Constraint =>
               return Denotes_Discriminant (Range_Expression (Constr));

            when others =>
               return False;

         end case;
      end Contains_POC;

   --  Start of processing for Analyze_Component_Declaration

   begin
      Generate_Definition (Id);
      Enter_Name (Id);

      if Present (Subtype_Indication (Component_Definition (N))) then
         T := Find_Type_Of_Object
                (Subtype_Indication (Component_Definition (N)), N);

      --  Ada 2005 (AI-230): Access Definition case

      else
         pragma Assert (Present
                          (Access_Definition (Component_Definition (N))));

         T := Access_Definition
                (Related_Nod => N,
                 N => Access_Definition (Component_Definition (N)));
         Set_Is_Local_Anonymous_Access (T);

         --  Ada 2005 (AI-254)

         if Present (Access_To_Subprogram_Definition
                      (Access_Definition (Component_Definition (N))))
           and then Protected_Present (Access_To_Subprogram_Definition
                                        (Access_Definition
                                          (Component_Definition (N))))
         then
            T := Replace_Anonymous_Access_To_Protected_Subprogram (N, T);
         end if;
      end if;

      --  If the subtype is a constrained subtype of the enclosing record,
      --  (which must have a partial view) the back-end does not properly
      --  handle the recursion. Rewrite the component declaration with an
      --  explicit subtype indication, which is acceptable to Gigi. We can copy
      --  the tree directly because side effects have already been removed from
      --  discriminant constraints.

      if Ekind (T) = E_Access_Subtype
        and then Is_Entity_Name (Subtype_Indication (Component_Definition (N)))
        and then Comes_From_Source (T)
        and then Nkind (Parent (T)) = N_Subtype_Declaration
        and then Etype (Directly_Designated_Type (T)) = Current_Scope
      then
         Rewrite
           (Subtype_Indication (Component_Definition (N)),
             New_Copy_Tree (Subtype_Indication (Parent (T))));
         T := Find_Type_Of_Object
                 (Subtype_Indication (Component_Definition (N)), N);
      end if;

      --  If the component declaration includes a default expression, then we
      --  check that the component is not of a limited type (RM 3.7(5)),
      --  and do the special preanalysis of the expression (see section on
      --  "Handling of Default and Per-Object Expressions" in the spec of
      --  package Sem).

      if Present (Expression (N)) then
         Analyze_Per_Use_Expression (Expression (N), T);
         Check_Initialization (T, Expression (N));
      end if;

      --  The parent type may be a private view with unknown discriminants,
      --  and thus unconstrained. Regular components must be constrained.

      if Is_Indefinite_Subtype (T) and then Chars (Id) /= Name_uParent then
         if Is_Class_Wide_Type (T) then
            Error_Msg_N
               ("class-wide subtype with unknown discriminants" &
                 " in component declaration",
                 Subtype_Indication (Component_Definition (N)));
         else
            Error_Msg_N
              ("unconstrained subtype in component declaration",
               Subtype_Indication (Component_Definition (N)));
         end if;

      --  Components cannot be abstract, except for the special case of
      --  the _Parent field (case of extending an abstract tagged type)

      elsif Is_Abstract (T) and then Chars (Id) /= Name_uParent then
         Error_Msg_N ("type of a component cannot be abstract", N);
      end if;

      Set_Etype (Id, T);
      Set_Is_Aliased (Id, Aliased_Present (Component_Definition (N)));

      --  The component declaration may have a per-object constraint, set
      --  the appropriate flag in the defining identifier of the subtype.

      if Present (Subtype_Indication (Component_Definition (N))) then
         declare
            Sindic : constant Node_Id :=
                       Subtype_Indication (Component_Definition (N));

         begin
            if Nkind (Sindic) = N_Subtype_Indication
              and then Present (Constraint (Sindic))
              and then Contains_POC (Constraint (Sindic))
            then
               Set_Has_Per_Object_Constraint (Id);
            end if;
         end;
      end if;

      --  Ada 2005 (AI-231): Propagate the null-excluding attribute and carry
      --  out some static checks.

      if Ada_Version >= Ada_05
        and then Can_Never_Be_Null (T)
      then
         Null_Exclusion_Static_Checks (N);
      end if;

      --  If this component is private (or depends on a private type), flag the
      --  record type to indicate that some operations are not available.

      P := Private_Component (T);

      if Present (P) then
         --  Check for circular definitions

         if P = Any_Type then
            Set_Etype (Id, Any_Type);

         --  There is a gap in the visibility of operations only if the
         --  component type is not defined in the scope of the record type.

         elsif Scope (P) = Scope (Current_Scope) then
            null;

         elsif Is_Limited_Type (P) then
            Set_Is_Limited_Composite (Current_Scope);

         else
            Set_Is_Private_Composite (Current_Scope);
         end if;
      end if;

      if P /= Any_Type
        and then Is_Limited_Type (T)
        and then Chars (Id) /= Name_uParent
        and then Is_Tagged_Type (Current_Scope)
      then
         if Is_Derived_Type (Current_Scope)
           and then not Is_Limited_Record (Root_Type (Current_Scope))
         then
            Error_Msg_N
              ("extension of nonlimited type cannot have limited components",
               N);
            Explain_Limited_Type (T, N);
            Set_Etype (Id, Any_Type);
            Set_Is_Limited_Composite (Current_Scope, False);

         elsif not Is_Derived_Type (Current_Scope)
           and then not Is_Limited_Record (Current_Scope)
         then
            Error_Msg_N
              ("nonlimited tagged type cannot have limited components", N);
            Explain_Limited_Type (T, N);
            Set_Etype (Id, Any_Type);
            Set_Is_Limited_Composite (Current_Scope, False);
         end if;
      end if;

      Set_Original_Record_Component (Id, Id);
   end Analyze_Component_Declaration;

   --------------------------
   -- Analyze_Declarations --
   --------------------------

   procedure Analyze_Declarations (L : List_Id) is
      D           : Node_Id;
      Next_Node   : Node_Id;
      Freeze_From : Entity_Id := Empty;

      procedure Adjust_D;
      --  Adjust D not to include implicit label declarations, since these
      --  have strange Sloc values that result in elaboration check problems.
      --  (They have the sloc of the label as found in the source, and that
      --  is ahead of the current declarative part).

      --------------
      -- Adjust_D --
      --------------

      procedure Adjust_D is
      begin
         while Present (Prev (D))
           and then Nkind (D) = N_Implicit_Label_Declaration
         loop
            Prev (D);
         end loop;
      end Adjust_D;

   --  Start of processing for Analyze_Declarations

   begin
      D := First (L);
      while Present (D) loop

         --  Complete analysis of declaration

         Analyze (D);
         Next_Node := Next (D);

         if No (Freeze_From) then
            Freeze_From := First_Entity (Current_Scope);
         end if;

         --  At the end of a declarative part, freeze remaining entities
         --  declared in it. The end of the visible declarations of package
         --  specification is not the end of a declarative part if private
         --  declarations are present. The end of a package declaration is a
         --  freezing point only if it a library package. A task definition or
         --  protected type definition is not a freeze point either. Finally,
         --  we do not freeze entities in generic scopes, because there is no
         --  code generated for them and freeze nodes will be generated for
         --  the instance.

         --  The end of a package instantiation is not a freeze point, but
         --  for now we make it one, because the generic body is inserted
         --  (currently) immediately after. Generic instantiations will not
         --  be a freeze point once delayed freezing of bodies is implemented.
         --  (This is needed in any case for early instantiations ???).

         if No (Next_Node) then
            if Nkind (Parent (L)) = N_Component_List
              or else Nkind (Parent (L)) = N_Task_Definition
              or else Nkind (Parent (L)) = N_Protected_Definition
            then
               null;

            elsif Nkind (Parent (L)) /= N_Package_Specification then
               if Nkind (Parent (L)) = N_Package_Body then
                  Freeze_From := First_Entity (Current_Scope);
               end if;

               Adjust_D;
               Freeze_All (Freeze_From, D);
               Freeze_From := Last_Entity (Current_Scope);

            elsif Scope (Current_Scope) /= Standard_Standard
              and then not Is_Child_Unit (Current_Scope)
              and then No (Generic_Parent (Parent (L)))
            then
               null;

            elsif L /= Visible_Declarations (Parent (L))
               or else No (Private_Declarations (Parent (L)))
               or else Is_Empty_List (Private_Declarations (Parent (L)))
            then
               Adjust_D;
               Freeze_All (Freeze_From, D);
               Freeze_From := Last_Entity (Current_Scope);
            end if;

         --  If next node is a body then freeze all types before the body.
         --  An exception occurs for expander generated bodies, which can
         --  be recognized by their already being analyzed. The expander
         --  ensures that all types needed by these bodies have been frozen
         --  but it is not necessary to freeze all types (and would be wrong
         --  since it would not correspond to an RM defined freeze point).

         elsif not Analyzed (Next_Node)
           and then (Nkind (Next_Node) = N_Subprogram_Body
             or else Nkind (Next_Node) = N_Entry_Body
             or else Nkind (Next_Node) = N_Package_Body
             or else Nkind (Next_Node) = N_Protected_Body
             or else Nkind (Next_Node) = N_Task_Body
             or else Nkind (Next_Node) in N_Body_Stub)
         then
            Adjust_D;
            Freeze_All (Freeze_From, D);
            Freeze_From := Last_Entity (Current_Scope);
         end if;

         D := Next_Node;
      end loop;
   end Analyze_Declarations;

   ----------------------------------
   -- Analyze_Incomplete_Type_Decl --
   ----------------------------------

   procedure Analyze_Incomplete_Type_Decl (N : Node_Id) is
      F : constant Boolean := Is_Pure (Current_Scope);
      T : Entity_Id;

   begin
      Generate_Definition (Defining_Identifier (N));

      --  Process an incomplete declaration. The identifier must not have been
      --  declared already in the scope. However, an incomplete declaration may
      --  appear in the private part of a package, for a private type that has
      --  already been declared.

      --  In this case, the discriminants (if any) must match

      T := Find_Type_Name (N);

      Set_Ekind (T, E_Incomplete_Type);
      Init_Size_Align (T);
      Set_Is_First_Subtype (T, True);
      Set_Etype (T, T);

      --  Ada 2005 (AI-326): Minimum decoration to give support to tagged
      --  incomplete types.

      if Tagged_Present (N) then
         Set_Is_Tagged_Type (T);
         Make_Class_Wide_Type (T);
         Set_Primitive_Operations (T, New_Elmt_List);
      end if;

      New_Scope (T);

      Set_Stored_Constraint (T, No_Elist);

      if Present (Discriminant_Specifications (N)) then
         Process_Discriminants (N);
      end if;

      End_Scope;

      --  If the type has discriminants, non-trivial subtypes may be be
      --  declared before the full view of the type. The full views of those
      --  subtypes will be built after the full view of the type.

      Set_Private_Dependents (T, New_Elmt_List);
      Set_Is_Pure (T, F);
   end Analyze_Incomplete_Type_Decl;

   -----------------------------------
   -- Analyze_Interface_Declaration --
   -----------------------------------

   procedure Analyze_Interface_Declaration (T : Entity_Id; Def : Node_Id) is
   begin
      Set_Is_Tagged_Type      (T);

      Set_Is_Limited_Record   (T, Limited_Present (Def)
                                   or else Task_Present (Def)
                                   or else Protected_Present (Def)
                                   or else Synchronized_Present (Def));

      --  Type is abstract if full declaration carries keyword, or if
      --  previous partial view did.

      Set_Is_Abstract  (T);
      Set_Is_Interface (T);

      Set_Is_Limited_Interface      (T, Limited_Present (Def));
      Set_Is_Protected_Interface    (T, Protected_Present (Def));
      Set_Is_Synchronized_Interface (T, Synchronized_Present (Def));
      Set_Is_Task_Interface         (T, Task_Present (Def));
      Set_Abstract_Interfaces       (T, New_Elmt_List);
      Set_Primitive_Operations      (T, New_Elmt_List);
   end Analyze_Interface_Declaration;

   -----------------------------
   -- Analyze_Itype_Reference --
   -----------------------------

   --  Nothing to do. This node is placed in the tree only for the benefit of
   --  back end processing, and has no effect on the semantic processing.

   procedure Analyze_Itype_Reference (N : Node_Id) is
   begin
      pragma Assert (Is_Itype (Itype (N)));
      null;
   end Analyze_Itype_Reference;

   --------------------------------
   -- Analyze_Number_Declaration --
   --------------------------------

   procedure Analyze_Number_Declaration (N : Node_Id) is
      Id    : constant Entity_Id := Defining_Identifier (N);
      E     : constant Node_Id   := Expression (N);
      T     : Entity_Id;
      Index : Interp_Index;
      It    : Interp;

   begin
      Generate_Definition (Id);
      Enter_Name (Id);

      --  This is an optimization of a common case of an integer literal

      if Nkind (E) = N_Integer_Literal then
         Set_Is_Static_Expression (E, True);
         Set_Etype                (E, Universal_Integer);

         Set_Etype     (Id, Universal_Integer);
         Set_Ekind     (Id, E_Named_Integer);
         Set_Is_Frozen (Id, True);
         return;
      end if;

      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      --  Process expression, replacing error by integer zero, to avoid
      --  cascaded errors or aborts further along in the processing

      --  Replace Error by integer zero, which seems least likely to
      --  cause cascaded errors.

      if E = Error then
         Rewrite (E, Make_Integer_Literal (Sloc (E), Uint_0));
         Set_Error_Posted (E);
      end if;

      Analyze (E);

      --  Verify that the expression is static and numeric. If
      --  the expression is overloaded, we apply the preference
      --  rule that favors root numeric types.

      if not Is_Overloaded (E) then
         T := Etype (E);

      else
         T := Any_Type;

         Get_First_Interp (E, Index, It);
         while Present (It.Typ) loop
            if (Is_Integer_Type (It.Typ)
                 or else Is_Real_Type (It.Typ))
              and then (Scope (Base_Type (It.Typ))) = Standard_Standard
            then
               if T = Any_Type then
                  T := It.Typ;

               elsif It.Typ = Universal_Real
                 or else It.Typ = Universal_Integer
               then
                  --  Choose universal interpretation over any other

                  T := It.Typ;
                  exit;
               end if;
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;

      if Is_Integer_Type (T)  then
         Resolve (E, T);
         Set_Etype (Id, Universal_Integer);
         Set_Ekind (Id, E_Named_Integer);

      elsif Is_Real_Type (T) then

         --  Because the real value is converted to universal_real, this is a
         --  legal context for a universal fixed expression.

         if T = Universal_Fixed then
            declare
               Loc  : constant Source_Ptr := Sloc (N);
               Conv : constant Node_Id := Make_Type_Conversion (Loc,
                        Subtype_Mark =>
                          New_Occurrence_Of (Universal_Real, Loc),
                        Expression => Relocate_Node (E));

            begin
               Rewrite (E, Conv);
               Analyze (E);
            end;

         elsif T = Any_Fixed then
            Error_Msg_N ("illegal context for mixed mode operation", E);

            --  Expression is of the form : universal_fixed * integer. Try to
            --  resolve as universal_real.

            T := Universal_Real;
            Set_Etype (E, T);
         end if;

         Resolve (E, T);
         Set_Etype (Id, Universal_Real);
         Set_Ekind (Id, E_Named_Real);

      else
         Wrong_Type (E, Any_Numeric);
         Resolve (E, T);

         Set_Etype               (Id, T);
         Set_Ekind               (Id, E_Constant);
         Set_Never_Set_In_Source (Id, True);
         Set_Is_True_Constant    (Id, True);
         return;
      end if;

      if Nkind (E) = N_Integer_Literal
        or else Nkind (E) = N_Real_Literal
      then
         Set_Etype (E, Etype (Id));
      end if;

      if not Is_OK_Static_Expression (E) then
         Flag_Non_Static_Expr
           ("non-static expression used in number declaration!", E);
         Rewrite (E, Make_Integer_Literal (Sloc (N), 1));
         Set_Etype (E, Any_Type);
      end if;
   end Analyze_Number_Declaration;

   --------------------------------
   -- Analyze_Object_Declaration --
   --------------------------------

   procedure Analyze_Object_Declaration (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Id    : constant Entity_Id  := Defining_Identifier (N);
      T     : Entity_Id;
      Act_T : Entity_Id;

      E : Node_Id := Expression (N);
      --  E is set to Expression (N) throughout this routine. When
      --  Expression (N) is modified, E is changed accordingly.

      Prev_Entity : Entity_Id := Empty;

      function Build_Default_Subtype return Entity_Id;
      --  If the object is limited or aliased, and if the type is unconstrained
      --  and there is no expression, the discriminants cannot be modified and
      --  the subtype of the object is constrained by the defaults, so it is
      --  worthwhile building the corresponding subtype.

      function Count_Tasks (T : Entity_Id) return Uint;
      --  This function is called when a library level object of type is
      --  declared. It's function is to count the static number of tasks
      --  declared within the type (it is only called if Has_Tasks is set for
      --  T). As a side effect, if an array of tasks with non-static bounds or
      --  a variant record type is encountered, Check_Restrictions is called
      --  indicating the count is unknown.

      ---------------------------
      -- Build_Default_Subtype --
      ---------------------------

      function Build_Default_Subtype return Entity_Id is
         Constraints : constant List_Id := New_List;
         Act         : Entity_Id;
         Decl        : Node_Id;
         Disc        : Entity_Id;

      begin
         Disc  := First_Discriminant (T);

         if No (Discriminant_Default_Value (Disc)) then
            return T;   --   previous error.
         end if;

         Act := Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
         while Present (Disc) loop
            Append (
              New_Copy_Tree (
                Discriminant_Default_Value (Disc)), Constraints);
            Next_Discriminant (Disc);
         end loop;

         Decl :=
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Act,
             Subtype_Indication =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => New_Occurrence_Of (T, Loc),
                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint
                     (Loc, Constraints)));

         Insert_Before (N, Decl);
         Analyze (Decl);
         return Act;
      end Build_Default_Subtype;

      -----------------
      -- Count_Tasks --
      -----------------

      function Count_Tasks (T : Entity_Id) return Uint is
         C : Entity_Id;
         X : Node_Id;
         V : Uint;

      begin
         if Is_Task_Type (T) then
            return Uint_1;

         elsif Is_Record_Type (T) then
            if Has_Discriminants (T) then
               Check_Restriction (Max_Tasks, N);
               return Uint_0;

            else
               V := Uint_0;
               C := First_Component (T);
               while Present (C) loop
                  V := V + Count_Tasks (Etype (C));
                  Next_Component (C);
               end loop;

               return V;
            end if;

         elsif Is_Array_Type (T) then
            X := First_Index (T);
            V := Count_Tasks (Component_Type (T));
            while Present (X) loop
               C := Etype (X);

               if not Is_Static_Subtype (C) then
                  Check_Restriction (Max_Tasks, N);
                  return Uint_0;
               else
                  V := V * (UI_Max (Uint_0,
                                    Expr_Value (Type_High_Bound (C)) -
                                    Expr_Value (Type_Low_Bound (C)) + Uint_1));
               end if;

               Next_Index (X);
            end loop;

            return V;

         else
            return Uint_0;
         end if;
      end Count_Tasks;

   --  Start of processing for Analyze_Object_Declaration

   begin
      --  There are three kinds of implicit types generated by an
      --  object declaration:

      --   1. Those for generated by the original Object Definition

      --   2. Those generated by the Expression

      --   3. Those used to constrained the Object Definition with the
      --       expression constraints when it is unconstrained

      --  They must be generated in this order to avoid order of elaboration
      --  issues. Thus the first step (after entering the name) is to analyze
      --  the object definition.

      if Constant_Present (N) then
         Prev_Entity := Current_Entity_In_Scope (Id);

         --  If homograph is an implicit subprogram, it is overridden by the
         --  current declaration.

         if Present (Prev_Entity)
           and then Is_Overloadable (Prev_Entity)
           and then Is_Inherited_Operation (Prev_Entity)
         then
            Prev_Entity := Empty;
         end if;
      end if;

      if Present (Prev_Entity) then
         Constant_Redeclaration (Id, N, T);

         Generate_Reference (Prev_Entity, Id, 'c');
         Set_Completion_Referenced (Id);

         if Error_Posted (N) then

            --  Type mismatch or illegal redeclaration, Do not analyze
            --  expression to avoid cascaded errors.

            T := Find_Type_Of_Object (Object_Definition (N), N);
            Set_Etype (Id, T);
            Set_Ekind (Id, E_Variable);
            return;
         end if;

      --  In the normal case, enter identifier at the start to catch premature
      --  usage in the initialization expression.

      else
         Generate_Definition (Id);
         Enter_Name (Id);

         T := Find_Type_Of_Object (Object_Definition (N), N);

         if Error_Posted (Id) then
            Set_Etype (Id, T);
            Set_Ekind (Id, E_Variable);
            return;
         end if;
      end if;

      --  Ada 2005 (AI-231): Propagate the null-excluding attribute and carry
      --  out some static checks

      if Ada_Version >= Ada_05
        and then Can_Never_Be_Null (T)
      then
         --  In case of aggregates we must also take care of the correct
         --  initialization of nested aggregates bug this is done at the
         --  point of the analysis of the aggregate (see sem_aggr.adb)

         if Present (Expression (N))
           and then Nkind (Expression (N)) = N_Aggregate
         then
            null;

         else
            declare
               Save_Typ : constant Entity_Id := Etype (Id);
            begin
               Set_Etype (Id, T); --  Temp. decoration for static checks
               Null_Exclusion_Static_Checks (N);
               Set_Etype (Id, Save_Typ);
            end;
         end if;
      end if;

      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      --  If deferred constant, make sure context is appropriate. We detect
      --  a deferred constant as a constant declaration with no expression.
      --  A deferred constant can appear in a package body if its completion
      --  is by means of an interface pragma.

      if Constant_Present (N)
        and then No (E)
      then
         if not Is_Package_Or_Generic_Package (Current_Scope) then
            Error_Msg_N
              ("invalid context for deferred constant declaration ('R'M 7.4)",
                N);
            Error_Msg_N
              ("\declaration requires an initialization expression",
                N);
            Set_Constant_Present (N, False);

         --  In Ada 83, deferred constant must be of private type

         elsif not Is_Private_Type (T) then
            if Ada_Version = Ada_83 and then Comes_From_Source (N) then
               Error_Msg_N
                 ("(Ada 83) deferred constant must be private type", N);
            end if;
         end if;

      --  If not a deferred constant, then object declaration freezes its type

      else
         Check_Fully_Declared (T, N);
         Freeze_Before (N, T);
      end if;

      --  If the object was created by a constrained array definition, then
      --  set the link in both the anonymous base type and anonymous subtype
      --  that are built to represent the array type to point to the object.

      if Nkind (Object_Definition (Declaration_Node (Id))) =
                        N_Constrained_Array_Definition
      then
         Set_Related_Array_Object (T, Id);
         Set_Related_Array_Object (Base_Type (T), Id);
      end if;

      --  Special checks for protected objects not at library level

      if Is_Protected_Type (T)
        and then not Is_Library_Level_Entity (Id)
      then
         Check_Restriction (No_Local_Protected_Objects, Id);

         --  Protected objects with interrupt handlers must be at library level

         --  Ada 2005: this test is not needed (and the corresponding clause
         --  in the RM is removed) because accessibility checks are sufficient
         --  to make handlers not at the library level illegal.

         if Has_Interrupt_Handler (T)
           and then Ada_Version < Ada_05
         then
            Error_Msg_N
              ("interrupt object can only be declared at library level", Id);
         end if;
      end if;

      --  The actual subtype of the object is the nominal subtype, unless
      --  the nominal one is unconstrained and obtained from the expression.

      Act_T := T;

      --  Process initialization expression if present and not in error

      if Present (E) and then E /= Error then
         Analyze (E);

         --  In case of errors detected in the analysis of the expression,
         --  decorate it with the expected type to avoid cascade errors

         if not Present (Etype (E)) then
            Set_Etype (E, T);
         end if;

         --  If an initialization expression is present, then we set the
         --  Is_True_Constant flag. It will be reset if this is a variable
         --  and it is indeed modified.

         Set_Is_True_Constant (Id, True);

         --  If we are analyzing a constant declaration, set its completion
         --  flag after analyzing the expression.

         if Constant_Present (N) then
            Set_Has_Completion (Id);
         end if;

         if not Assignment_OK (N) then
            Check_Initialization (T, E);
         end if;

         Set_Etype (Id, T);             --  may be overridden later on
         Resolve (E, T);
         Check_Unset_Reference (E);

         if Compile_Time_Known_Value (E) then
            Set_Current_Value (Id, E);
         end if;

         --  Check incorrect use of dynamically tagged expressions. Note
         --  the use of Is_Tagged_Type (T) which seems redundant but is in
         --  fact important to avoid spurious errors due to expanded code
         --  for dispatching functions over an anonymous access type

         if (Is_Class_Wide_Type (Etype (E)) or else Is_Dynamically_Tagged (E))
           and then Is_Tagged_Type (T)
           and then not Is_Class_Wide_Type (T)
         then
            Error_Msg_N ("dynamically tagged expression not allowed!", E);
         end if;

         Apply_Scalar_Range_Check (E, T);
         Apply_Static_Length_Check (E, T);
      end if;

      --  If the No_Streams restriction is set, check that the type of the
      --  object is not, and does not contain, any subtype derived from
      --  Ada.Streams.Root_Stream_Type. Note that we guard the call to
      --  Has_Stream just for efficiency reasons. There is no point in
      --  spending time on a Has_Stream check if the restriction is not set.

      if Restrictions.Set (No_Streams) then
         if Has_Stream (T) then
            Check_Restriction (No_Streams, N);
         end if;
      end if;

      --  Abstract type is never permitted for a variable or constant.
      --  Note: we inhibit this check for objects that do not come from
      --  source because there is at least one case (the expansion of
      --  x'class'input where x is abstract) where we legitimately
      --  generate an abstract object.

      if Is_Abstract (T) and then Comes_From_Source (N) then
         Error_Msg_N ("type of object cannot be abstract",
                      Object_Definition (N));

         if Is_CPP_Class (T) then
            Error_Msg_NE ("\} may need a cpp_constructor",
              Object_Definition (N), T);
         end if;

      --  Case of unconstrained type

      elsif Is_Indefinite_Subtype (T) then

         --  Nothing to do in deferred constant case

         if Constant_Present (N) and then No (E) then
            null;

         --  Case of no initialization present

         elsif No (E) then
            if No_Initialization (N) then
               null;

            elsif Is_Class_Wide_Type (T) then
               Error_Msg_N
                 ("initialization required in class-wide declaration ", N);

            else
               Error_Msg_N
                 ("unconstrained subtype not allowed (need initialization)",
                  Object_Definition (N));
            end if;

         --  Case of initialization present but in error. Set initial
         --  expression as absent (but do not make above complaints)

         elsif E = Error then
            Set_Expression (N, Empty);
            E := Empty;

         --  Case of initialization present

         else
            --  Not allowed in Ada 83

            if not Constant_Present (N) then
               if Ada_Version = Ada_83
                 and then Comes_From_Source (Object_Definition (N))
               then
                  Error_Msg_N
                    ("(Ada 83) unconstrained variable not allowed",
                     Object_Definition (N));
               end if;
            end if;

            --  Now we constrain the variable from the initializing expression

            --  If the expression is an aggregate, it has been expanded into
            --  individual assignments. Retrieve the actual type from the
            --  expanded construct.

            if Is_Array_Type (T)
              and then No_Initialization (N)
              and then Nkind (Original_Node (E)) = N_Aggregate
            then
               Act_T := Etype (E);

            else
               Expand_Subtype_From_Expr (N, T, Object_Definition (N), E);
               Act_T := Find_Type_Of_Object (Object_Definition (N), N);
            end if;

            Set_Is_Constr_Subt_For_U_Nominal (Act_T);

            if Aliased_Present (N) then
               Set_Is_Constr_Subt_For_UN_Aliased (Act_T);
            end if;

            Freeze_Before (N, Act_T);
            Freeze_Before (N, T);
         end if;

      elsif Is_Array_Type (T)
        and then No_Initialization (N)
        and then Nkind (Original_Node (E)) = N_Aggregate
      then
         if not Is_Entity_Name (Object_Definition (N)) then
            Act_T := Etype (E);
            Check_Compile_Time_Size (Act_T);

            if Aliased_Present (N) then
               Set_Is_Constr_Subt_For_UN_Aliased (Act_T);
            end if;
         end if;

         --  When the given object definition and the aggregate are specified
         --  independently, and their lengths might differ do a length check.
         --  This cannot happen if the aggregate is of the form (others =>...)

         if not Is_Constrained (T) then
            null;

         elsif Nkind (E) = N_Raise_Constraint_Error then

            --  Aggregate is statically illegal. Place back in declaration

            Set_Expression (N, E);
            Set_No_Initialization (N, False);

         elsif T = Etype (E) then
            null;

         elsif Nkind (E) = N_Aggregate
           and then Present (Component_Associations (E))
           and then Present (Choices (First (Component_Associations (E))))
           and then Nkind (First
            (Choices (First (Component_Associations (E))))) = N_Others_Choice
         then
            null;

         else
            Apply_Length_Check (E, T);
         end if;

      elsif (Is_Limited_Record (T)
               or else Is_Concurrent_Type (T))
        and then not Is_Constrained (T)
        and then Has_Discriminants (T)
      then
         Act_T := Build_Default_Subtype;
         Rewrite (Object_Definition (N), New_Occurrence_Of (Act_T, Loc));

      elsif Present (Underlying_Type (T))
        and then not Is_Constrained (Underlying_Type (T))
        and then Has_Discriminants (Underlying_Type (T))
        and then Nkind (E) = N_Function_Call
        and then Constant_Present (N)
      then
         --  The back-end has problems with constants of a discriminated type
         --  with defaults, if the initial value is a function call. We
         --  generate an intermediate temporary for the result of the call.
         --  It is unclear why this should make it acceptable to gcc. ???

         Remove_Side_Effects (E);
      end if;

      if T = Standard_Wide_Character or else T = Standard_Wide_Wide_Character
        or else Root_Type (T) = Standard_Wide_String
        or else Root_Type (T) = Standard_Wide_Wide_String
      then
         Check_Restriction (No_Wide_Characters, Object_Definition (N));
      end if;

      --  Now establish the proper kind and type of the object

      if Constant_Present (N) then
         Set_Ekind               (Id, E_Constant);
         Set_Never_Set_In_Source (Id, True);
         Set_Is_True_Constant    (Id, True);

      else
         Set_Ekind (Id, E_Variable);

         --  A variable is set as shared passive if it appears in a shared
         --  passive package, and is at the outer level. This is not done
         --  for entities generated during expansion, because those are
         --  always manipulated locally.

         if Is_Shared_Passive (Current_Scope)
           and then Is_Library_Level_Entity (Id)
           and then Comes_From_Source (Id)
         then
            Set_Is_Shared_Passive (Id);
            Check_Shared_Var (Id, T, N);
         end if;

         --  Case of no initializing expression present. If the type is not
         --  fully initialized, then we set Never_Set_In_Source, since this
         --  is a case of a potentially uninitialized object. Note that we
         --  do not consider access variables to be fully initialized for
         --  this purpose, since it still seems dubious if someone declares

         --  Note that we only do this for source declarations. If the object
         --  is declared by a generated declaration, we assume that it is not
         --  appropriate to generate warnings in that case.

         if No (E) then
            if (Is_Access_Type (T)
                 or else not Is_Fully_Initialized_Type (T))
              and then Comes_From_Source (N)
            then
               Set_Never_Set_In_Source (Id);
            end if;
         end if;
      end if;

      Init_Alignment (Id);
      Init_Esize     (Id);

      if Aliased_Present (N) then
         Set_Is_Aliased (Id);

         if No (E)
           and then Is_Record_Type (T)
           and then not Is_Constrained (T)
           and then Has_Discriminants (T)
         then
            Set_Actual_Subtype (Id, Build_Default_Subtype);
         end if;
      end if;

      Set_Etype (Id, Act_T);

      if Has_Controlled_Component (Etype (Id))
        or else Is_Controlled (Etype (Id))
      then
         if not Is_Library_Level_Entity (Id) then
            Check_Restriction (No_Nested_Finalization, N);
         else
            Validate_Controlled_Object (Id);
         end if;

         --  Generate a warning when an initialization causes an obvious ABE
         --  violation. If the init expression is a simple aggregate there
         --  shouldn't be any initialize/adjust call generated. This will be
         --  true as soon as aggregates are built in place when possible.

         --  ??? at the moment we do not generate warnings for temporaries
         --  created for those aggregates although Program_Error might be
         --  generated if compiled with -gnato.

         if Is_Controlled (Etype (Id))
            and then Comes_From_Source (Id)
         then
            declare
               BT : constant Entity_Id := Base_Type (Etype (Id));

               Implicit_Call : Entity_Id;
               pragma Warnings (Off, Implicit_Call);
               --  ??? what is this for (never referenced!)

               function Is_Aggr (N : Node_Id) return Boolean;
               --  Check that N is an aggregate

               -------------
               -- Is_Aggr --
               -------------

               function Is_Aggr (N : Node_Id) return Boolean is
               begin
                  case Nkind (Original_Node (N)) is
                     when N_Aggregate | N_Extension_Aggregate =>
                        return True;

                     when N_Qualified_Expression |
                          N_Type_Conversion      |
                          N_Unchecked_Type_Conversion =>
                        return Is_Aggr (Expression (Original_Node (N)));

                     when others =>
                        return False;
                  end case;
               end Is_Aggr;

            begin
               --  If no underlying type, we already are in an error situation.
               --  Do not try to add a warning since we do not have access to
               --  prim-op list.

               if No (Underlying_Type (BT)) then
                  Implicit_Call := Empty;

               --  A generic type does not have usable primitive operators.
               --  Initialization calls are built for instances.

               elsif Is_Generic_Type (BT) then
                  Implicit_Call := Empty;

               --  If the init expression is not an aggregate, an adjust call
               --  will be generated

               elsif Present (E) and then not Is_Aggr (E) then
                  Implicit_Call := Find_Prim_Op (BT, Name_Adjust);

               --  If no init expression and we are not in the deferred
               --  constant case, an Initialize call will be generated

               elsif No (E) and then not Constant_Present (N) then
                  Implicit_Call := Find_Prim_Op (BT, Name_Initialize);

               else
                  Implicit_Call := Empty;
               end if;
            end;
         end if;
      end if;

      if Has_Task (Etype (Id)) then
         Check_Restriction (No_Tasking, N);

         if Is_Library_Level_Entity (Id) then
            Check_Restriction (Max_Tasks, N, Count_Tasks (Etype (Id)));
         else
            Check_Restriction (Max_Tasks, N);
            Check_Restriction (No_Task_Hierarchy, N);
            Check_Potentially_Blocking_Operation (N);
         end if;

         --  A rather specialized test. If we see two tasks being declared
         --  of the same type in the same object declaration, and the task
         --  has an entry with an address clause, we know that program error
         --  will be raised at run-time since we can't have two tasks with
         --  entries at the same address.

         if Is_Task_Type (Etype (Id)) and then More_Ids (N) then
            declare
               E : Entity_Id;

            begin
               E := First_Entity (Etype (Id));
               while Present (E) loop
                  if Ekind (E) = E_Entry
                    and then Present (Get_Attribute_Definition_Clause
                                        (E, Attribute_Address))
                  then
                     Error_Msg_N
                       ("?more than one task with same entry address", N);
                     Error_Msg_N
                       ("\?Program_Error will be raised at run time", N);
                     Insert_Action (N,
                       Make_Raise_Program_Error (Loc,
                         Reason => PE_Duplicated_Entry_Address));
                     exit;
                  end if;

                  Next_Entity (E);
               end loop;
            end;
         end if;
      end if;

      --  Some simple constant-propagation: if the expression is a constant
      --  string initialized with a literal, share the literal. This avoids
      --  a run-time copy.

      if Present (E)
        and then Is_Entity_Name (E)
        and then Ekind (Entity (E)) = E_Constant
        and then Base_Type (Etype (E)) = Standard_String
      then
         declare
            Val : constant Node_Id := Constant_Value (Entity (E));
         begin
            if Present (Val)
              and then Nkind (Val) = N_String_Literal
            then
               Rewrite (E, New_Copy (Val));
            end if;
         end;
      end if;

      --  Another optimization: if the nominal subtype is unconstrained and
      --  the expression is a function call that returns an unconstrained
      --  type, rewrite the declaration as a renaming of the result of the
      --  call. The exceptions below are cases where the copy is expected,
      --  either by the back end (Aliased case) or by the semantics, as for
      --  initializing controlled types or copying tags for classwide types.

      if Present (E)
        and then Nkind (E) = N_Explicit_Dereference
        and then Nkind (Original_Node (E)) = N_Function_Call
        and then not Is_Library_Level_Entity (Id)
        and then not Is_Constrained (Underlying_Type (T))
        and then not Is_Aliased (Id)
        and then not Is_Class_Wide_Type (T)
        and then not Is_Controlled (T)
        and then not Has_Controlled_Component (Base_Type (T))
        and then Expander_Active
      then
         Rewrite (N,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Id,
             Access_Definition   => Empty,
             Subtype_Mark        => New_Occurrence_Of
                                      (Base_Type (Etype (Id)), Loc),
             Name                => E));

         Set_Renamed_Object (Id, E);

         --  Force generation of debugging information for the constant and for
         --  the renamed function call.

         Set_Needs_Debug_Info (Id);
         Set_Needs_Debug_Info (Entity (Prefix (E)));
      end if;

      if Present (Prev_Entity)
        and then Is_Frozen (Prev_Entity)
        and then not Error_Posted (Id)
      then
         Error_Msg_N ("full constant declaration appears too late", N);
      end if;

      Check_Eliminated (Id);
   end Analyze_Object_Declaration;

   ---------------------------
   -- Analyze_Others_Choice --
   ---------------------------

   --  Nothing to do for the others choice node itself, the semantic analysis
   --  of the others choice will occur as part of the processing of the parent

   procedure Analyze_Others_Choice (N : Node_Id) is
      pragma Warnings (Off, N);
   begin
      null;
   end Analyze_Others_Choice;

   --------------------------------
   -- Analyze_Per_Use_Expression --
   --------------------------------

   procedure Analyze_Per_Use_Expression (N : Node_Id; T : Entity_Id) is
      Save_In_Default_Expression : constant Boolean := In_Default_Expression;
   begin
      In_Default_Expression := True;
      Pre_Analyze_And_Resolve (N, T);
      In_Default_Expression := Save_In_Default_Expression;
   end Analyze_Per_Use_Expression;

   -------------------------------------------
   -- Analyze_Private_Extension_Declaration --
   -------------------------------------------

   procedure Analyze_Private_Extension_Declaration (N : Node_Id) is
      T           : constant Entity_Id := Defining_Identifier (N);
      Indic       : constant Node_Id   := Subtype_Indication (N);
      Parent_Type : Entity_Id;
      Parent_Base : Entity_Id;

   begin
      --  Ada 2005 (AI-251): Decorate all names in list of ancestor interfaces

      if Is_Non_Empty_List (Interface_List (N)) then
         declare
            Intf : Node_Id;
            T    : Entity_Id;

         begin
            Intf := First (Interface_List (N));
            while Present (Intf) loop
               T := Find_Type_Of_Subtype_Indic (Intf);

               if not Is_Interface (T) then
                  Error_Msg_NE ("(Ada 2005) & must be an interface", Intf, T);
               end if;

               Next (Intf);
            end loop;
         end;
      end if;

      Generate_Definition (T);
      Enter_Name (T);

      Parent_Type := Find_Type_Of_Subtype_Indic (Indic);
      Parent_Base := Base_Type (Parent_Type);

      if Parent_Type = Any_Type
        or else Etype (Parent_Type) = Any_Type
      then
         Set_Ekind (T, Ekind (Parent_Type));
         Set_Etype (T, Any_Type);
         return;

      elsif not Is_Tagged_Type (Parent_Type) then
         Error_Msg_N
           ("parent of type extension must be a tagged type ", Indic);
         return;

      elsif Ekind (Parent_Type) = E_Void
        or else Ekind (Parent_Type) = E_Incomplete_Type
      then
         Error_Msg_N ("premature derivation of incomplete type", Indic);
         return;
      end if;

      --  Perhaps the parent type should be changed to the class-wide type's
      --  specific type in this case to prevent cascading errors ???

      if Is_Class_Wide_Type (Parent_Type) then
         Error_Msg_N
           ("parent of type extension must not be a class-wide type", Indic);
         return;
      end if;

      if (not Is_Package_Or_Generic_Package (Current_Scope)
           and then Nkind (Parent (N)) /= N_Generic_Subprogram_Declaration)
        or else In_Private_Part (Current_Scope)

      then
         Error_Msg_N ("invalid context for private extension", N);
      end if;

      --  Set common attributes

      Set_Is_Pure          (T, Is_Pure (Current_Scope));
      Set_Scope            (T, Current_Scope);
      Set_Ekind            (T, E_Record_Type_With_Private);
      Init_Size_Align      (T);

      Set_Etype            (T,            Parent_Base);
      Set_Has_Task         (T, Has_Task  (Parent_Base));

      Set_Convention       (T, Convention     (Parent_Type));
      Set_First_Rep_Item   (T, First_Rep_Item (Parent_Type));
      Set_Is_First_Subtype (T);
      Make_Class_Wide_Type (T);

      if Unknown_Discriminants_Present (N) then
         Set_Discriminant_Constraint (T, No_Elist);
      end if;

      Build_Derived_Record_Type (N, Parent_Type, T);
   end Analyze_Private_Extension_Declaration;

   ---------------------------------
   -- Analyze_Subtype_Declaration --
   ---------------------------------

   procedure Analyze_Subtype_Declaration (N : Node_Id) is
      Id       : constant Entity_Id := Defining_Identifier (N);
      T        : Entity_Id;
      R_Checks : Check_Result;

   begin
      Generate_Definition (Id);
      Set_Is_Pure (Id, Is_Pure (Current_Scope));
      Init_Size_Align (Id);

      --  The following guard condition on Enter_Name is to handle cases where
      --  the defining identifier has already been entered into the scope but
      --  the declaration as a whole needs to be analyzed.

      --  This case in particular happens for derived enumeration types. The
      --  derived enumeration type is processed as an inserted enumeration type
      --  declaration followed by a rewritten subtype declaration. The defining
      --  identifier, however, is entered into the name scope very early in the
      --  processing of the original type declaration and therefore needs to be
      --  avoided here, when the created subtype declaration is analyzed. (See
      --  Build_Derived_Types)

      --  This also happens when the full view of a private type is derived
      --  type with constraints. In this case the entity has been introduced
      --  in the private declaration.

      if Present (Etype (Id))
        and then (Is_Private_Type (Etype (Id))
                   or else Is_Task_Type (Etype (Id))
                   or else Is_Rewrite_Substitution (N))
      then
         null;

      else
         Enter_Name (Id);
      end if;

      T := Process_Subtype (Subtype_Indication (N), N, Id, 'P');

      --  Inherit common attributes

      Set_Is_Generic_Type   (Id, Is_Generic_Type   (Base_Type (T)));
      Set_Is_Volatile       (Id, Is_Volatile       (T));
      Set_Treat_As_Volatile (Id, Treat_As_Volatile (T));
      Set_Is_Atomic         (Id, Is_Atomic         (T));
      Set_Is_Ada_2005       (Id, Is_Ada_2005       (T));

      --  In the case where there is no constraint given in the subtype
      --  indication, Process_Subtype just returns the Subtype_Mark, so its
      --  semantic attributes must be established here.

      if Nkind (Subtype_Indication (N)) /= N_Subtype_Indication then
         Set_Etype (Id, Base_Type (T));

         case Ekind (T) is
            when Array_Kind =>
               Set_Ekind                       (Id, E_Array_Subtype);
               Copy_Array_Subtype_Attributes   (Id, T);

            when Decimal_Fixed_Point_Kind =>
               Set_Ekind                (Id, E_Decimal_Fixed_Point_Subtype);
               Set_Digits_Value         (Id, Digits_Value       (T));
               Set_Delta_Value          (Id, Delta_Value        (T));
               Set_Scale_Value          (Id, Scale_Value        (T));
               Set_Small_Value          (Id, Small_Value        (T));
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Machine_Radix_10     (Id, Machine_Radix_10   (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_RM_Size              (Id, RM_Size            (T));

            when Enumeration_Kind =>
               Set_Ekind                (Id, E_Enumeration_Subtype);
               Set_First_Literal        (Id, First_Literal (Base_Type (T)));
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Is_Character_Type    (Id, Is_Character_Type  (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_RM_Size              (Id, RM_Size            (T));

            when Ordinary_Fixed_Point_Kind =>
               Set_Ekind                (Id, E_Ordinary_Fixed_Point_Subtype);
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Small_Value          (Id, Small_Value        (T));
               Set_Delta_Value          (Id, Delta_Value        (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_RM_Size              (Id, RM_Size            (T));

            when Float_Kind =>
               Set_Ekind                (Id, E_Floating_Point_Subtype);
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Digits_Value         (Id, Digits_Value       (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));

            when Signed_Integer_Kind =>
               Set_Ekind                (Id, E_Signed_Integer_Subtype);
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_RM_Size              (Id, RM_Size            (T));

            when Modular_Integer_Kind =>
               Set_Ekind                (Id, E_Modular_Integer_Subtype);
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_RM_Size              (Id, RM_Size            (T));

            when Class_Wide_Kind =>
               Set_Ekind                (Id, E_Class_Wide_Subtype);
               Set_First_Entity         (Id, First_Entity       (T));
               Set_Last_Entity          (Id, Last_Entity        (T));
               Set_Class_Wide_Type      (Id, Class_Wide_Type    (T));
               Set_Cloned_Subtype       (Id, T);
               Set_Is_Tagged_Type       (Id, True);
               Set_Has_Unknown_Discriminants
                                        (Id, True);

               if Ekind (T) = E_Class_Wide_Subtype then
                  Set_Equivalent_Type   (Id, Equivalent_Type    (T));
               end if;

            when E_Record_Type | E_Record_Subtype =>
               Set_Ekind                (Id, E_Record_Subtype);

               if Ekind (T) = E_Record_Subtype
                 and then Present (Cloned_Subtype (T))
               then
                  Set_Cloned_Subtype    (Id, Cloned_Subtype (T));
               else
                  Set_Cloned_Subtype    (Id, T);
               end if;

               Set_First_Entity         (Id, First_Entity       (T));
               Set_Last_Entity          (Id, Last_Entity        (T));
               Set_Has_Discriminants    (Id, Has_Discriminants  (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_Is_Limited_Record    (Id, Is_Limited_Record  (T));
               Set_Has_Unknown_Discriminants
                                        (Id, Has_Unknown_Discriminants (T));

               if Has_Discriminants (T) then
                  Set_Discriminant_Constraint
                                        (Id, Discriminant_Constraint (T));
                  Set_Stored_Constraint_From_Discriminant_Constraint (Id);

               elsif Has_Unknown_Discriminants (Id) then
                  Set_Discriminant_Constraint (Id, No_Elist);
               end if;

               if Is_Tagged_Type (T) then
                  Set_Is_Tagged_Type    (Id);
                  Set_Is_Abstract       (Id, Is_Abstract (T));
                  Set_Primitive_Operations
                                        (Id, Primitive_Operations (T));
                  Set_Class_Wide_Type   (Id, Class_Wide_Type (T));
               end if;

            when Private_Kind =>
               Set_Ekind              (Id, Subtype_Kind (Ekind   (T)));
               Set_Has_Discriminants  (Id, Has_Discriminants     (T));
               Set_Is_Constrained     (Id, Is_Constrained        (T));
               Set_First_Entity       (Id, First_Entity          (T));
               Set_Last_Entity        (Id, Last_Entity           (T));
               Set_Private_Dependents (Id, New_Elmt_List);
               Set_Is_Limited_Record  (Id, Is_Limited_Record     (T));
               Set_Has_Unknown_Discriminants
                                      (Id, Has_Unknown_Discriminants (T));

               if Is_Tagged_Type (T) then
                  Set_Is_Tagged_Type  (Id);
                  Set_Is_Abstract     (Id, Is_Abstract (T));
                  Set_Primitive_Operations
                                        (Id, Primitive_Operations (T));
                  Set_Class_Wide_Type (Id, Class_Wide_Type (T));
               end if;

               --  In general the attributes of the subtype of a private type
               --  are the attributes of the partial view of parent. However,
               --  the full view may be a discriminated type, and the subtype
               --  must share the discriminant constraint to generate correct
               --  calls to initialization procedures.

               if Has_Discriminants (T) then
                  Set_Discriminant_Constraint
                                     (Id, Discriminant_Constraint (T));
                  Set_Stored_Constraint_From_Discriminant_Constraint (Id);

               elsif Present (Full_View (T))
                 and then Has_Discriminants (Full_View (T))
               then
                  Set_Discriminant_Constraint
                               (Id, Discriminant_Constraint (Full_View (T)));
                  Set_Stored_Constraint_From_Discriminant_Constraint (Id);

                  --  This would seem semantically correct, but apparently
                  --  confuses the back-end (4412-009). To be explained ???

                  --  Set_Has_Discriminants (Id);
               end if;

               Prepare_Private_Subtype_Completion (Id, N);

            when Access_Kind =>
               Set_Ekind             (Id, E_Access_Subtype);
               Set_Is_Constrained    (Id, Is_Constrained        (T));
               Set_Is_Access_Constant
                                     (Id, Is_Access_Constant    (T));
               Set_Directly_Designated_Type
                                     (Id, Designated_Type       (T));
               Set_Can_Never_Be_Null (Id, Can_Never_Be_Null     (T));

               --  A Pure library_item must not contain the declaration of a
               --  named access type, except within a subprogram, generic
               --  subprogram, task unit, or protected unit (RM 10.2.1(16)).

               if Comes_From_Source (Id)
                 and then In_Pure_Unit
                 and then not In_Subprogram_Task_Protected_Unit
               then
                  Error_Msg_N
                    ("named access types not allowed in pure unit", N);
               end if;

            when Concurrent_Kind =>
               Set_Ekind                (Id, Subtype_Kind (Ekind   (T)));
               Set_Corresponding_Record_Type (Id,
                                         Corresponding_Record_Type (T));
               Set_First_Entity         (Id, First_Entity          (T));
               Set_First_Private_Entity (Id, First_Private_Entity  (T));
               Set_Has_Discriminants    (Id, Has_Discriminants     (T));
               Set_Is_Constrained       (Id, Is_Constrained        (T));
               Set_Last_Entity          (Id, Last_Entity           (T));

               if Has_Discriminants (T) then
                  Set_Discriminant_Constraint (Id,
                                           Discriminant_Constraint (T));
                  Set_Stored_Constraint_From_Discriminant_Constraint (Id);
               end if;

            --  If the subtype name denotes an incomplete type an error was
            --  already reported by Process_Subtype.

            when E_Incomplete_Type =>
               Set_Etype (Id, Any_Type);

            when others =>
               raise Program_Error;
         end case;
      end if;

      if Etype (Id) = Any_Type then
         return;
      end if;

      --  Some common processing on all types

      Set_Size_Info      (Id,                 T);
      Set_First_Rep_Item (Id, First_Rep_Item (T));

      T := Etype (Id);

      Set_Is_Immediately_Visible (Id, True);
      Set_Depends_On_Private     (Id, Has_Private_Component (T));

      if Present (Generic_Parent_Type (N))
        and then
          (Nkind
             (Parent (Generic_Parent_Type (N))) /= N_Formal_Type_Declaration
            or else Nkind
              (Formal_Type_Definition (Parent (Generic_Parent_Type (N))))
                /=  N_Formal_Private_Type_Definition)
      then
         if Is_Tagged_Type (Id) then
            if Is_Class_Wide_Type (Id) then
               Derive_Subprograms (Generic_Parent_Type (N), Id, Etype (T));
            else
               Derive_Subprograms (Generic_Parent_Type (N), Id, T);
            end if;

         elsif Scope (Etype (Id)) /= Standard_Standard then
            Derive_Subprograms (Generic_Parent_Type (N), Id);
         end if;
      end if;

      if Is_Private_Type (T)
        and then Present (Full_View (T))
      then
         Conditional_Delay (Id, Full_View (T));

      --  The subtypes of components or subcomponents of protected types
      --  do not need freeze nodes, which would otherwise appear in the
      --  wrong scope (before the freeze node for the protected type). The
      --  proper subtypes are those of the subcomponents of the corresponding
      --  record.

      elsif Ekind (Scope (Id)) /= E_Protected_Type
        and then Present (Scope (Scope (Id))) -- error defense!
        and then Ekind (Scope (Scope (Id))) /= E_Protected_Type
      then
         Conditional_Delay (Id, T);
      end if;

      --  Check that constraint_error is raised for a scalar subtype
      --  indication when the lower or upper bound of a non-null range
      --  lies outside the range of the type mark.

      if Nkind (Subtype_Indication (N)) = N_Subtype_Indication then
         if Is_Scalar_Type (Etype (Id))
            and then Scalar_Range (Id) /=
                     Scalar_Range (Etype (Subtype_Mark
                                           (Subtype_Indication (N))))
         then
            Apply_Range_Check
              (Scalar_Range (Id),
               Etype (Subtype_Mark (Subtype_Indication (N))));

         elsif Is_Array_Type (Etype (Id))
           and then Present (First_Index (Id))
         then
            --  This really should be a subprogram that finds the indications
            --  to check???

            if ((Nkind (First_Index (Id)) = N_Identifier
                   and then Ekind (Entity (First_Index (Id))) in Scalar_Kind)
                 or else Nkind (First_Index (Id)) = N_Subtype_Indication)
              and then
                Nkind (Scalar_Range (Etype (First_Index (Id)))) = N_Range
            then
               declare
                  Target_Typ : constant Entity_Id :=
                                 Etype
                                   (First_Index (Etype
                                     (Subtype_Mark (Subtype_Indication (N)))));
               begin
                  R_Checks :=
                    Range_Check
                      (Scalar_Range (Etype (First_Index (Id))),
                       Target_Typ,
                       Etype (First_Index (Id)),
                       Defining_Identifier (N));

                  Insert_Range_Checks
                    (R_Checks,
                     N,
                     Target_Typ,
                     Sloc (Defining_Identifier (N)));
               end;
            end if;
         end if;
      end if;

      Check_Eliminated (Id);
   end Analyze_Subtype_Declaration;

   --------------------------------
   -- Analyze_Subtype_Indication --
   --------------------------------

   procedure Analyze_Subtype_Indication (N : Node_Id) is
      T : constant Entity_Id := Subtype_Mark (N);
      R : constant Node_Id   := Range_Expression (Constraint (N));

   begin
      Analyze (T);

      if R /= Error then
         Analyze (R);
         Set_Etype (N, Etype (R));
      else
         Set_Error_Posted (R);
         Set_Error_Posted (T);
      end if;
   end Analyze_Subtype_Indication;

   ------------------------------
   -- Analyze_Type_Declaration --
   ------------------------------

   procedure Analyze_Type_Declaration (N : Node_Id) is
      Def    : constant Node_Id   := Type_Definition (N);
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      T      : Entity_Id;
      Prev   : Entity_Id;

      Is_Remote : constant Boolean :=
                    (Is_Remote_Types (Current_Scope)
                          or else Is_Remote_Call_Interface (Current_Scope))
                       and then not (In_Private_Part (Current_Scope)
                                       or else
                                     In_Package_Body (Current_Scope));

      procedure Check_Ops_From_Incomplete_Type;
      --  If there is a tagged incomplete partial view of the type, transfer
      --  its operations to the full view, and indicate that the type of the
      --  controlling parameter (s) is this full view.

      ------------------------------------
      -- Check_Ops_From_Incomplete_Type --
      ------------------------------------

      procedure Check_Ops_From_Incomplete_Type is
         Elmt   : Elmt_Id;
         Formal : Entity_Id;
         Op     : Entity_Id;

      begin
         if Prev /= T
           and then Ekind (Prev) = E_Incomplete_Type
           and then Is_Tagged_Type (Prev)
           and then Is_Tagged_Type (T)
         then
            Elmt := First_Elmt (Primitive_Operations (Prev));
            while Present (Elmt) loop
               Op := Node (Elmt);
               Prepend_Elmt (Op, Primitive_Operations (T));

               Formal := First_Formal (Op);
               while Present (Formal) loop
                  if Etype (Formal) = Prev then
                     Set_Etype (Formal, T);
                  end if;

                  Next_Formal (Formal);
               end loop;

               if Etype (Op) = Prev then
                  Set_Etype (Op, T);
               end if;

               Next_Elmt (Elmt);
            end loop;
         end if;
      end Check_Ops_From_Incomplete_Type;

   --  Start of processing for Analyze_Type_Declaration

   begin
      Prev := Find_Type_Name (N);

      --  The full view, if present, now points to the current type

      --  Ada 2005 (AI-50217): If the type was previously decorated when
      --  imported through a LIMITED WITH clause, it appears as incomplete
      --  but has no full view.

      if Ekind (Prev) = E_Incomplete_Type
        and then Present (Full_View (Prev))
      then
         T := Full_View (Prev);
      else
         T := Prev;
      end if;

      Set_Is_Pure (T, Is_Pure (Current_Scope));

      --  We set the flag Is_First_Subtype here. It is needed to set the
      --  corresponding flag for the Implicit class-wide-type created
      --  during tagged types processing.

      Set_Is_First_Subtype (T, True);

      --  Only composite types other than array types are allowed to have
      --  discriminants.

      case Nkind (Def) is

         --  For derived types, the rule will be checked once we've figured
         --  out the parent type.

         when N_Derived_Type_Definition =>
            null;

         --  For record types, discriminants are allowed

         when N_Record_Definition =>
            null;

         when others =>
            if Present (Discriminant_Specifications (N)) then
               Error_Msg_N
                 ("elementary or array type cannot have discriminants",
                  Defining_Identifier
                  (First (Discriminant_Specifications (N))));
            end if;
      end case;

      --  Elaborate the type definition according to kind, and generate
      --  subsidiary (implicit) subtypes where needed. We skip this if
      --  it was already done (this happens during the reanalysis that
      --  follows a call to the high level optimizer).

      if not Analyzed (T) then
         Set_Analyzed (T);

         case Nkind (Def) is

            when N_Access_To_Subprogram_Definition =>
               Access_Subprogram_Declaration (T, Def);

               --  If this is a remote access to subprogram, we must create
               --  the equivalent fat pointer type, and related subprograms.

               if Is_Remote then
                  Process_Remote_AST_Declaration (N);
               end if;

               --  Validate categorization rule against access type declaration
               --  usually a violation in Pure unit, Shared_Passive unit.

               Validate_Access_Type_Declaration (T, N);

            when N_Access_To_Object_Definition =>
               Access_Type_Declaration (T, Def);

               --  Validate categorization rule against access type declaration
               --  usually a violation in Pure unit, Shared_Passive unit.

               Validate_Access_Type_Declaration (T, N);

               --  If we are in a Remote_Call_Interface package and define
               --  a RACW, Read and Write attribute must be added.

               if Is_Remote
                 and then Is_Remote_Access_To_Class_Wide_Type (Def_Id)
               then
                  Add_RACW_Features (Def_Id);
               end if;

               --  Set no strict aliasing flag if config pragma seen

               if Opt.No_Strict_Aliasing then
                  Set_No_Strict_Aliasing (Base_Type (Def_Id));
               end if;

            when N_Array_Type_Definition =>
               Array_Type_Declaration (T, Def);

            when N_Derived_Type_Definition =>
               Derived_Type_Declaration (T, N, T /= Def_Id);

            when N_Enumeration_Type_Definition =>
               Enumeration_Type_Declaration (T, Def);

            when N_Floating_Point_Definition =>
               Floating_Point_Type_Declaration (T, Def);

            when N_Decimal_Fixed_Point_Definition =>
               Decimal_Fixed_Point_Type_Declaration (T, Def);

            when N_Ordinary_Fixed_Point_Definition =>
               Ordinary_Fixed_Point_Type_Declaration (T, Def);

            when N_Signed_Integer_Type_Definition =>
               Signed_Integer_Type_Declaration (T, Def);

            when N_Modular_Type_Definition =>
               Modular_Type_Declaration (T, Def);

            when N_Record_Definition =>
               Record_Type_Declaration (T, N, Prev);

            when others =>
               raise Program_Error;

         end case;
      end if;

      if Etype (T) = Any_Type then
         return;
      end if;

      --  Some common processing for all types

      Set_Depends_On_Private (T, Has_Private_Component (T));
      Check_Ops_From_Incomplete_Type;

      --  Both the declared entity, and its anonymous base type if one
      --  was created, need freeze nodes allocated.

      declare
         B : constant Entity_Id := Base_Type (T);

      begin
         --  In the case where the base type is different from the first
         --  subtype, we pre-allocate a freeze node, and set the proper link
         --  to the first subtype. Freeze_Entity will use this preallocated
         --  freeze node when it freezes the entity.

         if B /= T then
            Ensure_Freeze_Node (B);
            Set_First_Subtype_Link (Freeze_Node (B), T);
         end if;

         if not From_With_Type (T) then
            Set_Has_Delayed_Freeze (T);
         end if;
      end;

      --  Case of T is the full declaration of some private type which has
      --  been swapped in Defining_Identifier (N).

      if T /= Def_Id and then Is_Private_Type (Def_Id) then
         Process_Full_View (N, T, Def_Id);

         --  Record the reference. The form of this is a little strange,
         --  since the full declaration has been swapped in. So the first
         --  parameter here represents the entity to which a reference is
         --  made which is the "real" entity, i.e. the one swapped in,
         --  and the second parameter provides the reference location.

         Generate_Reference (T, T, 'c');
         Set_Completion_Referenced (Def_Id);

      --  For completion of incomplete type, process incomplete dependents
      --  and always mark the full type as referenced (it is the incomplete
      --  type that we get for any real reference).

      elsif Ekind (Prev) = E_Incomplete_Type then
         Process_Incomplete_Dependents (N, T, Prev);
         Generate_Reference (Prev, Def_Id, 'c');
         Set_Completion_Referenced (Def_Id);

      --  If not private type or incomplete type completion, this is a real
      --  definition of a new entity, so record it.

      else
         Generate_Definition (Def_Id);
      end if;

      Check_Eliminated (Def_Id);
   end Analyze_Type_Declaration;

   --------------------------
   -- Analyze_Variant_Part --
   --------------------------

   procedure Analyze_Variant_Part (N : Node_Id) is

      procedure Non_Static_Choice_Error (Choice : Node_Id);
      --  Error routine invoked by the generic instantiation below when
      --  the variant part has a non static choice.

      procedure Process_Declarations (Variant : Node_Id);
      --  Analyzes all the declarations associated with a Variant.
      --  Needed by the generic instantiation below.

      package Variant_Choices_Processing is new
        Generic_Choices_Processing
          (Get_Alternatives          => Variants,
           Get_Choices               => Discrete_Choices,
           Process_Empty_Choice      => No_OP,
           Process_Non_Static_Choice => Non_Static_Choice_Error,
           Process_Associated_Node   => Process_Declarations);
      use Variant_Choices_Processing;
      --  Instantiation of the generic choice processing package

      -----------------------------
      -- Non_Static_Choice_Error --
      -----------------------------

      procedure Non_Static_Choice_Error (Choice : Node_Id) is
      begin
         Flag_Non_Static_Expr
           ("choice given in variant part is not static!", Choice);
      end Non_Static_Choice_Error;

      --------------------------
      -- Process_Declarations --
      --------------------------

      procedure Process_Declarations (Variant : Node_Id) is
      begin
         if not Null_Present (Component_List (Variant)) then
            Analyze_Declarations (Component_Items (Component_List (Variant)));

            if Present (Variant_Part (Component_List (Variant))) then
               Analyze (Variant_Part (Component_List (Variant)));
            end if;
         end if;
      end Process_Declarations;

      --  Variables local to Analyze_Case_Statement

      Discr_Name : Node_Id;
      Discr_Type : Entity_Id;

      Case_Table     : Choice_Table_Type (1 .. Number_Of_Choices (N));
      Last_Choice    : Nat;
      Dont_Care      : Boolean;
      Others_Present : Boolean := False;

   --  Start of processing for Analyze_Variant_Part

   begin
      Discr_Name := Name (N);
      Analyze (Discr_Name);

      if Ekind (Entity (Discr_Name)) /= E_Discriminant then
         Error_Msg_N ("invalid discriminant name in variant part", Discr_Name);
      end if;

      Discr_Type := Etype (Entity (Discr_Name));

      if not Is_Discrete_Type (Discr_Type) then
         Error_Msg_N
           ("discriminant in a variant part must be of a discrete type",
             Name (N));
         return;
      end if;

      --  Call the instantiated Analyze_Choices which does the rest of the work

      Analyze_Choices
        (N, Discr_Type, Case_Table, Last_Choice, Dont_Care, Others_Present);
   end Analyze_Variant_Part;

   ----------------------------
   -- Array_Type_Declaration --
   ----------------------------

   procedure Array_Type_Declaration (T : in out Entity_Id; Def : Node_Id) is
      Component_Def : constant Node_Id := Component_Definition (Def);
      Element_Type  : Entity_Id;
      Implicit_Base : Entity_Id;
      Index         : Node_Id;
      Related_Id    : Entity_Id := Empty;
      Nb_Index      : Nat;
      P             : constant Node_Id := Parent (Def);
      Priv          : Entity_Id;

   begin
      if Nkind (Def) = N_Constrained_Array_Definition then
         Index := First (Discrete_Subtype_Definitions (Def));
      else
         Index := First (Subtype_Marks (Def));
      end if;

      --  Find proper names for the implicit types which may be public.
      --  in case of anonymous arrays we use the name of the first object
      --  of that type as prefix.

      if No (T) then
         Related_Id :=  Defining_Identifier (P);
      else
         Related_Id := T;
      end if;

      Nb_Index := 1;
      while Present (Index) loop
         Analyze (Index);
         Make_Index (Index, P, Related_Id, Nb_Index);
         Next_Index (Index);
         Nb_Index := Nb_Index + 1;
      end loop;

      if Present (Subtype_Indication (Component_Def)) then
         Element_Type := Process_Subtype (Subtype_Indication (Component_Def),
                                          P, Related_Id, 'C');

      --  Ada 2005 (AI-230): Access Definition case

      else pragma Assert (Present (Access_Definition (Component_Def)));
         Element_Type := Access_Definition
                           (Related_Nod => Related_Id,
                            N           => Access_Definition (Component_Def));
         Set_Is_Local_Anonymous_Access (Element_Type);

         --  Ada 2005 (AI-230): In case of components that are anonymous
         --  access types the level of accessibility depends on the enclosing
         --  type declaration

         Set_Scope (Element_Type, Current_Scope); -- Ada 2005 (AI-230)

         --  Ada 2005 (AI-254)

         declare
            CD : constant Node_Id :=
                   Access_To_Subprogram_Definition
                     (Access_Definition (Component_Def));
         begin
            if Present (CD) and then Protected_Present (CD) then
               Element_Type :=
                 Replace_Anonymous_Access_To_Protected_Subprogram
                   (Def, Element_Type);
            end if;
         end;
      end if;

      --  Constrained array case

      if No (T) then
         T := Create_Itype (E_Void, P, Related_Id, 'T');
      end if;

      if Nkind (Def) = N_Constrained_Array_Definition then

         --  Establish Implicit_Base as unconstrained base type

         Implicit_Base := Create_Itype (E_Array_Type, P, Related_Id, 'B');

         Init_Size_Align        (Implicit_Base);
         Set_Etype              (Implicit_Base, Implicit_Base);
         Set_Scope              (Implicit_Base, Current_Scope);
         Set_Has_Delayed_Freeze (Implicit_Base);

         --  The constrained array type is a subtype of the unconstrained one

         Set_Ekind          (T, E_Array_Subtype);
         Init_Size_Align    (T);
         Set_Etype          (T, Implicit_Base);
         Set_Scope          (T, Current_Scope);
         Set_Is_Constrained (T, True);
         Set_First_Index    (T, First (Discrete_Subtype_Definitions (Def)));
         Set_Has_Delayed_Freeze (T);

         --  Complete setup of implicit base type

         Set_First_Index    (Implicit_Base, First_Index (T));
         Set_Component_Type (Implicit_Base, Element_Type);
         Set_Has_Task       (Implicit_Base, Has_Task      (Element_Type));
         Set_Component_Size (Implicit_Base, Uint_0);
         Set_Has_Controlled_Component
                            (Implicit_Base, Has_Controlled_Component
                                                          (Element_Type)
                                              or else
                                            Is_Controlled (Element_Type));
         Set_Finalize_Storage_Only
                            (Implicit_Base, Finalize_Storage_Only
                                                          (Element_Type));

      --  Unconstrained array case

      else
         Set_Ekind                    (T, E_Array_Type);
         Init_Size_Align              (T);
         Set_Etype                    (T, T);
         Set_Scope                    (T, Current_Scope);
         Set_Component_Size           (T, Uint_0);
         Set_Is_Constrained           (T, False);
         Set_First_Index              (T, First (Subtype_Marks (Def)));
         Set_Has_Delayed_Freeze       (T, True);
         Set_Has_Task                 (T, Has_Task      (Element_Type));
         Set_Has_Controlled_Component (T, Has_Controlled_Component
                                                        (Element_Type)
                                            or else
                                          Is_Controlled (Element_Type));
         Set_Finalize_Storage_Only    (T, Finalize_Storage_Only
                                                        (Element_Type));
      end if;

      Set_Component_Type (Base_Type (T), Element_Type);

      if Aliased_Present (Component_Definition (Def)) then
         Set_Has_Aliased_Components (Etype (T));
      end if;

      --  Ada 2005 (AI-231): Propagate the null-excluding attribute to the
      --  array type to ensure that objects of this type are initialized.

      if Ada_Version >= Ada_05
        and then Can_Never_Be_Null (Element_Type)
      then
         Set_Can_Never_Be_Null (T);

         if Null_Exclusion_Present (Component_Definition (Def))
           and then Can_Never_Be_Null (Element_Type)

            --  No need to check itypes because in their case this check
            --  was done at their point of creation

           and then not Is_Itype (Element_Type)
         then
            Error_Msg_N
              ("(Ada 2005) already a null-excluding type",
               Subtype_Indication (Component_Definition (Def)));
         end if;
      end if;

      Priv := Private_Component (Element_Type);

      if Present (Priv) then

         --  Check for circular definitions

         if Priv = Any_Type then
            Set_Component_Type (Etype (T), Any_Type);

         --  There is a gap in the visibility of operations on the composite
         --  type only if the component type is defined in a different scope.

         elsif Scope (Priv) = Current_Scope then
            null;

         elsif Is_Limited_Type (Priv) then
            Set_Is_Limited_Composite (Etype (T));
            Set_Is_Limited_Composite (T);
         else
            Set_Is_Private_Composite (Etype (T));
            Set_Is_Private_Composite (T);
         end if;
      end if;

      --  Create a concatenation operator for the new type. Internal
      --  array types created for packed entities do not need such, they
      --  are compatible with the user-defined type.

      if Number_Dimensions (T) = 1
         and then not Is_Packed_Array_Type (T)
      then
         New_Concatenation_Op (T);
      end if;

      --  In the case of an unconstrained array the parser has already
      --  verified that all the indices are unconstrained but we still
      --  need to make sure that the element type is constrained.

      if Is_Indefinite_Subtype (Element_Type) then
         Error_Msg_N
           ("unconstrained element type in array declaration",
            Subtype_Indication (Component_Def));

      elsif Is_Abstract (Element_Type) then
         Error_Msg_N
           ("the type of a component cannot be abstract",
            Subtype_Indication (Component_Def));
      end if;

   end Array_Type_Declaration;

   ------------------------------------------------------
   -- Replace_Anonymous_Access_To_Protected_Subprogram --
   ------------------------------------------------------

   function Replace_Anonymous_Access_To_Protected_Subprogram
     (N      : Node_Id;
      Prev_E : Entity_Id) return Entity_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

      Curr_Scope : constant Scope_Stack_Entry :=
                     Scope_Stack.Table (Scope_Stack.Last);

      Anon : constant Entity_Id :=
               Make_Defining_Identifier (Loc,
                 Chars => New_Internal_Name ('S'));

      Acc  : Node_Id;
      Comp : Node_Id;
      Decl : Node_Id;
      P    : Node_Id;

   begin
      Set_Is_Internal (Anon);

      case Nkind (N) is
         when N_Component_Declaration       |
           N_Unconstrained_Array_Definition |
           N_Constrained_Array_Definition   =>
            Comp := Component_Definition (N);
            Acc  := Access_Definition (Component_Definition (N));

         when N_Discriminant_Specification =>
            Comp := Discriminant_Type (N);
            Acc  := Discriminant_Type (N);

         when N_Parameter_Specification =>
            Comp := Parameter_Type (N);
            Acc  := Parameter_Type (N);

         when others =>
            raise Program_Error;
      end case;

      Decl := Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Anon,
                Type_Definition   =>
                  Copy_Separate_Tree (Access_To_Subprogram_Definition (Acc)));

      Mark_Rewrite_Insertion (Decl);

      --  Insert the new declaration in the nearest enclosing scope

      P := Parent (N);
      while Present (P) and then not Has_Declarations (P) loop
         P := Parent (P);
      end loop;

      pragma Assert (Present (P));

      if Nkind (P) = N_Package_Specification then
         Prepend (Decl, Visible_Declarations (P));
      else
         Prepend (Decl, Declarations (P));
      end if;

      --  Replace the anonymous type with an occurrence of the new declaration.
      --  In all cases the rewritten node does not have the null-exclusion
      --  attribute because (if present) it was already inherited by the
      --  anonymous entity (Anon). Thus, in case of components we do not
      --  inherit this attribute.

      if Nkind (N) = N_Parameter_Specification then
         Rewrite (Comp, New_Occurrence_Of (Anon, Loc));
         Set_Etype (Defining_Identifier (N), Anon);
         Set_Null_Exclusion_Present (N, False);
      else
         Rewrite (Comp,
           Make_Component_Definition (Loc,
             Subtype_Indication => New_Occurrence_Of (Anon, Loc)));
      end if;

      Mark_Rewrite_Insertion (Comp);

      --  Temporarily remove the current scope from the stack to add the new
      --  declarations to the enclosing scope

      Scope_Stack.Decrement_Last;
      Analyze (Decl);
      Scope_Stack.Append (Curr_Scope);

      Set_Original_Access_Type (Anon, Prev_E);
      return Anon;
   end Replace_Anonymous_Access_To_Protected_Subprogram;

   -------------------------------
   -- Build_Derived_Access_Type --
   -------------------------------

   procedure Build_Derived_Access_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      S : constant Node_Id := Subtype_Indication (Type_Definition (N));

      Desig_Type      : Entity_Id;
      Discr           : Entity_Id;
      Discr_Con_Elist : Elist_Id;
      Discr_Con_El    : Elmt_Id;
      Subt            : Entity_Id;

   begin
      --  Set the designated type so it is available in case this is
      --  an access to a self-referential type, e.g. a standard list
      --  type with a next pointer. Will be reset after subtype is built.

      Set_Directly_Designated_Type
        (Derived_Type, Designated_Type (Parent_Type));

      Subt := Process_Subtype (S, N);

      if Nkind (S) /= N_Subtype_Indication
        and then Subt /= Base_Type (Subt)
      then
         Set_Ekind (Derived_Type, E_Access_Subtype);
      end if;

      if Ekind (Derived_Type) = E_Access_Subtype then
         declare
            Pbase      : constant Entity_Id := Base_Type (Parent_Type);
            Ibase      : constant Entity_Id :=
                           Create_Itype (Ekind (Pbase), N, Derived_Type, 'B');
            Svg_Chars  : constant Name_Id   := Chars (Ibase);
            Svg_Next_E : constant Entity_Id := Next_Entity (Ibase);

         begin
            Copy_Node (Pbase, Ibase);

            Set_Chars             (Ibase, Svg_Chars);
            Set_Next_Entity       (Ibase, Svg_Next_E);
            Set_Sloc              (Ibase, Sloc (Derived_Type));
            Set_Scope             (Ibase, Scope (Derived_Type));
            Set_Freeze_Node       (Ibase, Empty);
            Set_Is_Frozen         (Ibase, False);
            Set_Comes_From_Source (Ibase, False);
            Set_Is_First_Subtype  (Ibase, False);

            Set_Etype (Ibase, Pbase);
            Set_Etype (Derived_Type, Ibase);
         end;
      end if;

      Set_Directly_Designated_Type
        (Derived_Type, Designated_Type (Subt));

      Set_Is_Constrained     (Derived_Type, Is_Constrained (Subt));
      Set_Is_Access_Constant (Derived_Type, Is_Access_Constant (Parent_Type));
      Set_Size_Info          (Derived_Type,                     Parent_Type);
      Set_RM_Size            (Derived_Type, RM_Size            (Parent_Type));
      Set_Depends_On_Private (Derived_Type,
                              Has_Private_Component (Derived_Type));
      Conditional_Delay      (Derived_Type, Subt);

      --  Ada 2005 (AI-231). Set the null-exclusion attribute

      if Null_Exclusion_Present (Type_Definition (N))
        or else Can_Never_Be_Null (Parent_Type)
      then
         Set_Can_Never_Be_Null (Derived_Type);
      end if;

      --  Note: we do not copy the Storage_Size_Variable, since
      --  we always go to the root type for this information.

      --  Apply range checks to discriminants for derived record case
      --  ??? THIS CODE SHOULD NOT BE HERE REALLY.

      Desig_Type := Designated_Type (Derived_Type);
      if Is_Composite_Type (Desig_Type)
        and then (not Is_Array_Type (Desig_Type))
        and then Has_Discriminants (Desig_Type)
        and then Base_Type (Desig_Type) /= Desig_Type
      then
         Discr_Con_Elist := Discriminant_Constraint (Desig_Type);
         Discr_Con_El := First_Elmt (Discr_Con_Elist);

         Discr := First_Discriminant (Base_Type (Desig_Type));
         while Present (Discr_Con_El) loop
            Apply_Range_Check (Node (Discr_Con_El), Etype (Discr));
            Next_Elmt (Discr_Con_El);
            Next_Discriminant (Discr);
         end loop;
      end if;
   end Build_Derived_Access_Type;

   ------------------------------
   -- Build_Derived_Array_Type --
   ------------------------------

   procedure Build_Derived_Array_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Tdef          : constant Node_Id    := Type_Definition (N);
      Indic         : constant Node_Id    := Subtype_Indication (Tdef);
      Parent_Base   : constant Entity_Id  := Base_Type (Parent_Type);
      Implicit_Base : Entity_Id;
      New_Indic     : Node_Id;

      procedure Make_Implicit_Base;
      --  If the parent subtype is constrained, the derived type is a
      --  subtype of an implicit base type derived from the parent base.

      ------------------------
      -- Make_Implicit_Base --
      ------------------------

      procedure Make_Implicit_Base is
      begin
         Implicit_Base :=
           Create_Itype (Ekind (Parent_Base), N, Derived_Type, 'B');

         Set_Ekind (Implicit_Base, Ekind (Parent_Base));
         Set_Etype (Implicit_Base, Parent_Base);

         Copy_Array_Subtype_Attributes   (Implicit_Base, Parent_Base);
         Copy_Array_Base_Type_Attributes (Implicit_Base, Parent_Base);

         Set_Has_Delayed_Freeze (Implicit_Base, True);
      end Make_Implicit_Base;

   --  Start of processing for Build_Derived_Array_Type

   begin
      if not Is_Constrained (Parent_Type) then
         if Nkind (Indic) /= N_Subtype_Indication then
            Set_Ekind (Derived_Type, E_Array_Type);

            Copy_Array_Subtype_Attributes   (Derived_Type, Parent_Type);
            Copy_Array_Base_Type_Attributes (Derived_Type, Parent_Type);

            Set_Has_Delayed_Freeze (Derived_Type, True);

         else
            Make_Implicit_Base;
            Set_Etype (Derived_Type, Implicit_Base);

            New_Indic :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => Derived_Type,
                Subtype_Indication  =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark => New_Reference_To (Implicit_Base, Loc),
                    Constraint => Constraint (Indic)));

            Rewrite (N, New_Indic);
            Analyze (N);
         end if;

      else
         if Nkind (Indic) /= N_Subtype_Indication then
            Make_Implicit_Base;

            Set_Ekind             (Derived_Type, Ekind (Parent_Type));
            Set_Etype             (Derived_Type, Implicit_Base);
            Copy_Array_Subtype_Attributes (Derived_Type, Parent_Type);

         else
            Error_Msg_N ("illegal constraint on constrained type", Indic);
         end if;
      end if;

      --  If parent type is not a derived type itself, and is declared in
      --  closed scope (e.g. a subprogram), then we must explicitly introduce
      --  the new type's concatenation operator since Derive_Subprograms
      --  will not inherit the parent's operator. If the parent type is
      --  unconstrained, the operator is of the unconstrained base type.

      if Number_Dimensions (Parent_Type) = 1
        and then not Is_Limited_Type (Parent_Type)
        and then not Is_Derived_Type (Parent_Type)
        and then not Is_Package_Or_Generic_Package
                       (Scope (Base_Type (Parent_Type)))
      then
         if not Is_Constrained (Parent_Type)
           and then Is_Constrained (Derived_Type)
         then
            New_Concatenation_Op (Implicit_Base);
         else
            New_Concatenation_Op (Derived_Type);
         end if;
      end if;
   end Build_Derived_Array_Type;

   -----------------------------------
   -- Build_Derived_Concurrent_Type --
   -----------------------------------

   procedure Build_Derived_Concurrent_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      D_Constraint : Node_Id;
      Disc_Spec    : Node_Id;
      Old_Disc     : Entity_Id;
      New_Disc     : Entity_Id;

      Constraint_Present : constant Boolean :=
                             Nkind (Subtype_Indication (Type_Definition (N)))
                                                     = N_Subtype_Indication;

   begin
      Set_Stored_Constraint (Derived_Type, No_Elist);

      if Is_Task_Type (Parent_Type) then
         Set_Storage_Size_Variable (Derived_Type,
           Storage_Size_Variable (Parent_Type));
      end if;

      if Present (Discriminant_Specifications (N)) then
         New_Scope (Derived_Type);
         Check_Or_Process_Discriminants (N, Derived_Type);
         End_Scope;

      elsif Constraint_Present then

         --  Build constrained subtype and derive from it

         declare
            Loc  : constant Source_Ptr := Sloc (N);
            Anon : constant Entity_Id :=
                     Make_Defining_Identifier (Loc,
                       New_External_Name (Chars (Derived_Type), 'T'));
            Decl : Node_Id;

         begin
            Decl :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => Anon,
                Subtype_Indication =>
                  New_Copy_Tree (Subtype_Indication (Type_Definition (N))));
            Insert_Before (N, Decl);
            Rewrite (Subtype_Indication (Type_Definition (N)),
              New_Occurrence_Of (Anon, Loc));
            Analyze (Decl);
            Set_Analyzed (Derived_Type, False);
            Analyze (N);
            return;
         end;
      end if;

      --  All attributes are inherited from parent. In particular,
      --  entries and the corresponding record type are the same.
      --  Discriminants may be renamed, and must be treated separately.

      Set_Has_Discriminants
        (Derived_Type, Has_Discriminants         (Parent_Type));
      Set_Corresponding_Record_Type
        (Derived_Type, Corresponding_Record_Type (Parent_Type));

      if Constraint_Present then
         if not Has_Discriminants (Parent_Type) then
            Error_Msg_N ("untagged parent must have discriminants", N);

         elsif Present (Discriminant_Specifications (N)) then

            --  Verify that new discriminants are used to constrain old ones

            D_Constraint :=
              First
                (Constraints
                  (Constraint (Subtype_Indication (Type_Definition (N)))));

            Old_Disc  := First_Discriminant (Parent_Type);
            New_Disc  := First_Discriminant (Derived_Type);
            Disc_Spec := First (Discriminant_Specifications (N));
            while Present (Old_Disc) and then Present (Disc_Spec) loop
               if Nkind (Discriminant_Type (Disc_Spec)) /=
                                              N_Access_Definition
               then
                  Analyze (Discriminant_Type (Disc_Spec));

                  if not Subtypes_Statically_Compatible (
                             Etype (Discriminant_Type (Disc_Spec)),
                               Etype (Old_Disc))
                  then
                     Error_Msg_N
                       ("not statically compatible with parent discriminant",
                        Discriminant_Type (Disc_Spec));
                  end if;
               end if;

               if Nkind (D_Constraint) = N_Identifier
                 and then Chars (D_Constraint) /=
                   Chars (Defining_Identifier (Disc_Spec))
               then
                  Error_Msg_N ("new discriminants must constrain old ones",
                    D_Constraint);
               else
                  Set_Corresponding_Discriminant (New_Disc, Old_Disc);
               end if;

               Next_Discriminant (Old_Disc);
               Next_Discriminant (New_Disc);
               Next (Disc_Spec);
            end loop;

            if Present (Old_Disc) or else Present (Disc_Spec) then
               Error_Msg_N ("discriminant mismatch in derivation", N);
            end if;

         end if;

      elsif Present (Discriminant_Specifications (N)) then
         Error_Msg_N
           ("missing discriminant constraint in untagged derivation",
            N);
      end if;

      if Present (Discriminant_Specifications (N)) then
         Old_Disc := First_Discriminant (Parent_Type);
         while Present (Old_Disc) loop

            if No (Next_Entity (Old_Disc))
              or else Ekind (Next_Entity (Old_Disc)) /= E_Discriminant
            then
               Set_Next_Entity (Last_Entity (Derived_Type),
                                         Next_Entity (Old_Disc));
               exit;
            end if;

            Next_Discriminant (Old_Disc);
         end loop;

      else
         Set_First_Entity (Derived_Type, First_Entity (Parent_Type));
         if Has_Discriminants (Parent_Type) then
            Set_Is_Constrained (Derived_Type, Is_Constrained (Parent_Type));
            Set_Discriminant_Constraint (
              Derived_Type, Discriminant_Constraint (Parent_Type));
         end if;
      end if;

      Set_Last_Entity  (Derived_Type, Last_Entity  (Parent_Type));

      Set_Has_Completion (Derived_Type);
   end Build_Derived_Concurrent_Type;

   ------------------------------------
   -- Build_Derived_Enumeration_Type --
   ------------------------------------

   procedure Build_Derived_Enumeration_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Def           : constant Node_Id    := Type_Definition (N);
      Indic         : constant Node_Id    := Subtype_Indication (Def);
      Implicit_Base : Entity_Id;
      Literal       : Entity_Id;
      New_Lit       : Entity_Id;
      Literals_List : List_Id;
      Type_Decl     : Node_Id;
      Hi, Lo        : Node_Id;
      Rang_Expr     : Node_Id;

   begin
      --  Since types Standard.Character and Standard.Wide_Character do
      --  not have explicit literals lists we need to process types derived
      --  from them specially. This is handled by Derived_Standard_Character.
      --  If the parent type is a generic type, there are no literals either,
      --  and we construct the same skeletal representation as for the generic
      --  parent type.

      if Root_Type (Parent_Type) = Standard_Character
        or else Root_Type (Parent_Type) = Standard_Wide_Character
        or else Root_Type (Parent_Type) = Standard_Wide_Wide_Character
      then
         Derived_Standard_Character (N, Parent_Type, Derived_Type);

      elsif Is_Generic_Type (Root_Type (Parent_Type)) then
         declare
            Lo : Node_Id;
            Hi : Node_Id;

         begin
            Lo :=
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_First,
                 Prefix => New_Reference_To (Derived_Type, Loc));
            Set_Etype (Lo, Derived_Type);

            Hi :=
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Last,
                 Prefix => New_Reference_To (Derived_Type, Loc));
            Set_Etype (Hi, Derived_Type);

            Set_Scalar_Range (Derived_Type,
               Make_Range (Loc,
                 Low_Bound => Lo,
                 High_Bound => Hi));
         end;

      else
         --  If a constraint is present, analyze the bounds to catch
         --  premature usage of the derived literals.

         if Nkind (Indic) = N_Subtype_Indication
           and then Nkind (Range_Expression (Constraint (Indic))) = N_Range
         then
            Analyze (Low_Bound  (Range_Expression (Constraint (Indic))));
            Analyze (High_Bound (Range_Expression (Constraint (Indic))));
         end if;

         --  Introduce an implicit base type for the derived type even
         --  if there is no constraint attached to it, since this seems
         --  closer to the Ada semantics. Build a full type declaration
         --  tree for the derived type using the implicit base type as
         --  the defining identifier. The build a subtype declaration
         --  tree which applies the constraint (if any) have it replace
         --  the derived type declaration.

         Literal := First_Literal (Parent_Type);
         Literals_List := New_List;
         while Present (Literal)
           and then Ekind (Literal) = E_Enumeration_Literal
         loop
            --  Literals of the derived type have the same representation as
            --  those of the parent type, but this representation can be
            --  overridden by an explicit representation clause. Indicate
            --  that there is no explicit representation given yet. These
            --  derived literals are implicit operations of the new type,
            --  and can be overridden by explicit ones.

            if Nkind (Literal) = N_Defining_Character_Literal then
               New_Lit :=
                 Make_Defining_Character_Literal (Loc, Chars (Literal));
            else
               New_Lit := Make_Defining_Identifier (Loc, Chars (Literal));
            end if;

            Set_Ekind                (New_Lit, E_Enumeration_Literal);
            Set_Enumeration_Pos      (New_Lit, Enumeration_Pos (Literal));
            Set_Enumeration_Rep      (New_Lit, Enumeration_Rep (Literal));
            Set_Enumeration_Rep_Expr (New_Lit, Empty);
            Set_Alias                (New_Lit, Literal);
            Set_Is_Known_Valid       (New_Lit, True);

            Append (New_Lit, Literals_List);
            Next_Literal (Literal);
         end loop;

         Implicit_Base :=
           Make_Defining_Identifier (Sloc (Derived_Type),
             New_External_Name (Chars (Derived_Type), 'B'));

         --  Indicate the proper nature of the derived type. This must
         --  be done before analysis of the literals, to recognize cases
         --  when a literal may be hidden by a previous explicit function
         --  definition (cf. c83031a).

         Set_Ekind (Derived_Type, E_Enumeration_Subtype);
         Set_Etype (Derived_Type, Implicit_Base);

         Type_Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Implicit_Base,
             Discriminant_Specifications => No_List,
             Type_Definition =>
               Make_Enumeration_Type_Definition (Loc, Literals_List));

         Mark_Rewrite_Insertion (Type_Decl);
         Insert_Before (N, Type_Decl);
         Analyze (Type_Decl);

         --  After the implicit base is analyzed its Etype needs to be changed
         --  to reflect the fact that it is derived from the parent type which
         --  was ignored during analysis. We also set the size at this point.

         Set_Etype (Implicit_Base, Parent_Type);

         Set_Size_Info      (Implicit_Base,                 Parent_Type);
         Set_RM_Size        (Implicit_Base, RM_Size        (Parent_Type));
         Set_First_Rep_Item (Implicit_Base, First_Rep_Item (Parent_Type));

         Set_Has_Non_Standard_Rep
                            (Implicit_Base, Has_Non_Standard_Rep
                                                           (Parent_Type));
         Set_Has_Delayed_Freeze (Implicit_Base);

         --  Process the subtype indication including a validation check
         --  on the constraint, if any. If a constraint is given, its bounds
         --  must be implicitly converted to the new type.

         if Nkind (Indic) = N_Subtype_Indication then
            declare
               R : constant Node_Id :=
                     Range_Expression (Constraint (Indic));

            begin
               if Nkind (R) = N_Range then
                  Hi := Build_Scalar_Bound
                          (High_Bound (R), Parent_Type, Implicit_Base);
                  Lo := Build_Scalar_Bound
                          (Low_Bound  (R), Parent_Type, Implicit_Base);

               else
                  --  Constraint is a Range attribute. Replace with the
                  --  explicit mention of the bounds of the prefix, which must
                  --  be a subtype.

                  Analyze (Prefix (R));
                  Hi :=
                    Convert_To (Implicit_Base,
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Last,
                        Prefix =>
                          New_Occurrence_Of (Entity (Prefix (R)), Loc)));

                  Lo :=
                    Convert_To (Implicit_Base,
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_First,
                        Prefix =>
                          New_Occurrence_Of (Entity (Prefix (R)), Loc)));
               end if;
            end;

         else
            Hi :=
              Build_Scalar_Bound
                (Type_High_Bound (Parent_Type),
                 Parent_Type, Implicit_Base);
            Lo :=
               Build_Scalar_Bound
                 (Type_Low_Bound (Parent_Type),
                  Parent_Type, Implicit_Base);
         end if;

         Rang_Expr :=
           Make_Range (Loc,
             Low_Bound  => Lo,
             High_Bound => Hi);

         --  If we constructed a default range for the case where no range
         --  was given, then the expressions in the range must not freeze
         --  since they do not correspond to expressions in the source.

         if Nkind (Indic) /= N_Subtype_Indication then
            Set_Must_Not_Freeze (Lo);
            Set_Must_Not_Freeze (Hi);
            Set_Must_Not_Freeze (Rang_Expr);
         end if;

         Rewrite (N,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Derived_Type,
             Subtype_Indication =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => New_Occurrence_Of (Implicit_Base, Loc),
                 Constraint =>
                   Make_Range_Constraint (Loc,
                     Range_Expression => Rang_Expr))));

         Analyze (N);

         --  If pragma Discard_Names applies on the first subtype of the
         --  parent type, then it must be applied on this subtype as well.

         if Einfo.Discard_Names (First_Subtype (Parent_Type)) then
            Set_Discard_Names (Derived_Type);
         end if;

         --  Apply a range check. Since this range expression doesn't have an
         --  Etype, we have to specifically pass the Source_Typ parameter. Is
         --  this right???

         if Nkind (Indic) = N_Subtype_Indication then
            Apply_Range_Check (Range_Expression (Constraint (Indic)),
                               Parent_Type,
                               Source_Typ => Entity (Subtype_Mark (Indic)));
         end if;
      end if;
   end Build_Derived_Enumeration_Type;

   --------------------------------
   -- Build_Derived_Numeric_Type --
   --------------------------------

   procedure Build_Derived_Numeric_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Tdef          : constant Node_Id    := Type_Definition (N);
      Indic         : constant Node_Id    := Subtype_Indication (Tdef);
      Parent_Base   : constant Entity_Id  := Base_Type (Parent_Type);
      No_Constraint : constant Boolean    := Nkind (Indic) /=
                                                  N_Subtype_Indication;
      Implicit_Base : Entity_Id;

      Lo : Node_Id;
      Hi : Node_Id;

   begin
      --  Process the subtype indication including a validation check on
      --  the constraint if any.

      Discard_Node (Process_Subtype (Indic, N));

      --  Introduce an implicit base type for the derived type even if there
      --  is no constraint attached to it, since this seems closer to the Ada
      --  semantics.

      Implicit_Base :=
        Create_Itype (Ekind (Parent_Base), N, Derived_Type, 'B');

      Set_Etype          (Implicit_Base, Parent_Base);
      Set_Ekind          (Implicit_Base, Ekind          (Parent_Base));
      Set_Size_Info      (Implicit_Base,                 Parent_Base);
      Set_RM_Size        (Implicit_Base, RM_Size        (Parent_Base));
      Set_First_Rep_Item (Implicit_Base, First_Rep_Item (Parent_Base));
      Set_Parent         (Implicit_Base, Parent (Derived_Type));

      if Is_Discrete_Or_Fixed_Point_Type (Parent_Base) then
         Set_RM_Size (Implicit_Base, RM_Size (Parent_Base));
      end if;

      Set_Has_Delayed_Freeze (Implicit_Base);

      Lo := New_Copy_Tree (Type_Low_Bound  (Parent_Base));
      Hi := New_Copy_Tree (Type_High_Bound (Parent_Base));

      Set_Scalar_Range (Implicit_Base,
        Make_Range (Loc,
          Low_Bound  => Lo,
          High_Bound => Hi));

      if Has_Infinities (Parent_Base) then
         Set_Includes_Infinities (Scalar_Range (Implicit_Base));
      end if;

      --  The Derived_Type, which is the entity of the declaration, is a
      --  subtype of the implicit base. Its Ekind is a subtype, even in the
      --  absence of an explicit constraint.

      Set_Etype (Derived_Type, Implicit_Base);

      --  If we did not have a constraint, then the Ekind is set from the
      --  parent type (otherwise Process_Subtype has set the bounds)

      if No_Constraint then
         Set_Ekind (Derived_Type, Subtype_Kind (Ekind (Parent_Type)));
      end if;

      --  If we did not have a range constraint, then set the range from the
      --  parent type. Otherwise, the call to Process_Subtype has set the
      --  bounds.

      if No_Constraint
        or else not Has_Range_Constraint (Indic)
      then
         Set_Scalar_Range (Derived_Type,
           Make_Range (Loc,
             Low_Bound  => New_Copy_Tree (Type_Low_Bound  (Parent_Type)),
             High_Bound => New_Copy_Tree (Type_High_Bound (Parent_Type))));
         Set_Is_Constrained (Derived_Type, Is_Constrained (Parent_Type));

         if Has_Infinities (Parent_Type) then
            Set_Includes_Infinities (Scalar_Range (Derived_Type));
         end if;
      end if;

      --  Set remaining type-specific fields, depending on numeric type

      if Is_Modular_Integer_Type (Parent_Type) then
         Set_Modulus (Implicit_Base, Modulus (Parent_Base));

         Set_Non_Binary_Modulus
           (Implicit_Base, Non_Binary_Modulus (Parent_Base));

      elsif Is_Floating_Point_Type (Parent_Type) then

         --  Digits of base type is always copied from the digits value of
         --  the parent base type, but the digits of the derived type will
         --  already have been set if there was a constraint present.

         Set_Digits_Value (Implicit_Base, Digits_Value (Parent_Base));
         Set_Vax_Float    (Implicit_Base, Vax_Float    (Parent_Base));

         if No_Constraint then
            Set_Digits_Value (Derived_Type, Digits_Value (Parent_Type));
         end if;

      elsif Is_Fixed_Point_Type (Parent_Type) then

         --  Small of base type and derived type are always copied from the
         --  parent base type, since smalls never change. The delta of the
         --  base type is also copied from the parent base type. However the
         --  delta of the derived type will have been set already if a
         --  constraint was present.

         Set_Small_Value (Derived_Type,  Small_Value (Parent_Base));
         Set_Small_Value (Implicit_Base, Small_Value (Parent_Base));
         Set_Delta_Value (Implicit_Base, Delta_Value (Parent_Base));

         if No_Constraint then
            Set_Delta_Value (Derived_Type,  Delta_Value (Parent_Type));
         end if;

         --  The scale and machine radix in the decimal case are always
         --  copied from the parent base type.

         if Is_Decimal_Fixed_Point_Type (Parent_Type) then
            Set_Scale_Value (Derived_Type,  Scale_Value (Parent_Base));
            Set_Scale_Value (Implicit_Base, Scale_Value (Parent_Base));

            Set_Machine_Radix_10
              (Derived_Type,  Machine_Radix_10 (Parent_Base));
            Set_Machine_Radix_10
              (Implicit_Base, Machine_Radix_10 (Parent_Base));

            Set_Digits_Value (Implicit_Base, Digits_Value (Parent_Base));

            if No_Constraint then
               Set_Digits_Value (Derived_Type, Digits_Value (Parent_Base));

            else
               --  the analysis of the subtype_indication sets the
               --  digits value of the derived type.

               null;
            end if;
         end if;
      end if;

      --  The type of the bounds is that of the parent type, and they
      --  must be converted to the derived type.

      Convert_Scalar_Bounds (N, Parent_Type, Derived_Type, Loc);

      --  The implicit_base should be frozen when the derived type is frozen,
      --  but note that it is used in the conversions of the bounds. For fixed
      --  types we delay the determination of the bounds until the proper
      --  freezing point. For other numeric types this is rejected by GCC, for
      --  reasons that are currently unclear (???), so we choose to freeze the
      --  implicit base now. In the case of integers and floating point types
      --  this is harmless because subsequent representation clauses cannot
      --  affect anything, but it is still baffling that we cannot use the
      --  same mechanism for all derived numeric types.

      if Is_Fixed_Point_Type (Parent_Type) then
         Conditional_Delay (Implicit_Base, Parent_Type);
      else
         Freeze_Before (N, Implicit_Base);
      end if;
   end Build_Derived_Numeric_Type;

   --------------------------------
   -- Build_Derived_Private_Type --
   --------------------------------

   procedure Build_Derived_Private_Type
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id;
      Is_Completion : Boolean;
      Derive_Subps  : Boolean := True)
   is
      Der_Base    : Entity_Id;
      Discr       : Entity_Id;
      Full_Decl   : Node_Id := Empty;
      Full_Der    : Entity_Id;
      Full_P      : Entity_Id;
      Last_Discr  : Entity_Id;
      Par_Scope   : constant Entity_Id := Scope (Base_Type (Parent_Type));
      Swapped     : Boolean := False;

      procedure Copy_And_Build;
      --  Copy derived type declaration, replace parent with its full view,
      --  and analyze new declaration.

      --------------------
      -- Copy_And_Build --
      --------------------

      procedure Copy_And_Build is
         Full_N : Node_Id;

      begin
         if Ekind (Parent_Type) in Record_Kind
           or else
             (Ekind (Parent_Type) in Enumeration_Kind
               and then Root_Type (Parent_Type) /= Standard_Character
               and then Root_Type (Parent_Type) /= Standard_Wide_Character
               and then Root_Type (Parent_Type) /= Standard_Wide_Wide_Character
               and then not Is_Generic_Type (Root_Type (Parent_Type)))
         then
            Full_N := New_Copy_Tree (N);
            Insert_After (N, Full_N);
            Build_Derived_Type (
              Full_N, Parent_Type, Full_Der, True, Derive_Subps => False);

         else
            Build_Derived_Type (
              N, Parent_Type, Full_Der, True, Derive_Subps => False);
         end if;
      end Copy_And_Build;

   --  Start of processing for Build_Derived_Private_Type

   begin
      if Is_Tagged_Type (Parent_Type) then
         Build_Derived_Record_Type
           (N, Parent_Type, Derived_Type, Derive_Subps);
         return;

      elsif Has_Discriminants (Parent_Type) then
         if Present (Full_View (Parent_Type)) then
            if not Is_Completion then

               --  Copy declaration for subsequent analysis, to provide a
               --  completion for what is a private declaration. Indicate that
               --  the full type is internally generated.

               Full_Decl := New_Copy_Tree (N);
               Full_Der  := New_Copy (Derived_Type);
               Set_Comes_From_Source (Full_Decl, False);
               Set_Comes_From_Source (Full_Der, False);

               Insert_After (N, Full_Decl);

            else
               --  If this is a completion, the full view being built is
               --  itself private. We build a subtype of the parent with
               --  the same constraints as this full view, to convey to the
               --  back end the constrained components and the size of this
               --  subtype. If the parent is constrained, its full view can
               --  serve as the underlying full view of the derived type.

               if No (Discriminant_Specifications (N)) then
                  if Nkind (Subtype_Indication (Type_Definition (N))) =
                                                        N_Subtype_Indication
                  then
                     Build_Underlying_Full_View (N, Derived_Type, Parent_Type);

                  elsif Is_Constrained (Full_View (Parent_Type)) then
                     Set_Underlying_Full_View (Derived_Type,
                       Full_View (Parent_Type));
                  end if;

               else
                  --  If there are new discriminants, the parent subtype is
                  --  constrained by them, but it is not clear how to build
                  --  the underlying_full_view in this case ???

                  null;
               end if;
            end if;
         end if;

         --  Build partial view of derived type from partial view of parent

         Build_Derived_Record_Type
           (N, Parent_Type, Derived_Type, Derive_Subps);

         if Present (Full_View (Parent_Type))
           and then not Is_Completion
         then
            if not In_Open_Scopes (Par_Scope)
              or else not In_Same_Source_Unit (N, Parent_Type)
            then
               --  Swap partial and full views temporarily

               Install_Private_Declarations (Par_Scope);
               Install_Visible_Declarations (Par_Scope);
               Swapped := True;
            end if;

            --  Build full view of derived type from full view of parent which
            --  is now installed. Subprograms have been derived on the partial
            --  view, the completion does not derive them anew.

            if not Is_Tagged_Type (Parent_Type) then

               --  If the parent is itself derived from another private type,
               --  installing the private declarations has not affected its
               --  privacy status, so use its own full view explicitly.

               if Is_Private_Type (Parent_Type) then
                  Build_Derived_Record_Type
                    (Full_Decl, Full_View (Parent_Type), Full_Der, False);
               else
                  Build_Derived_Record_Type
                    (Full_Decl, Parent_Type, Full_Der, False);
               end if;

            else
               --  If full view of parent is tagged, the completion
               --  inherits the proper primitive operations.

               Set_Defining_Identifier (Full_Decl, Full_Der);
               Build_Derived_Record_Type
                 (Full_Decl, Parent_Type, Full_Der, Derive_Subps);
               Set_Analyzed (Full_Decl);
            end if;

            if Swapped then
               Uninstall_Declarations (Par_Scope);

               if In_Open_Scopes (Par_Scope) then
                  Install_Visible_Declarations (Par_Scope);
               end if;
            end if;

            Der_Base := Base_Type (Derived_Type);
            Set_Full_View (Derived_Type, Full_Der);
            Set_Full_View (Der_Base, Base_Type (Full_Der));

            --  Copy the discriminant list from full view to the partial views
            --  (base type and its subtype). Gigi requires that the partial
            --  and full views have the same discriminants.

            --  Note that since the partial view is pointing to discriminants
            --  in the full view, their scope will be that of the full view.
            --  This might cause some front end problems and need
            --  adjustment???

            Discr := First_Discriminant (Base_Type (Full_Der));
            Set_First_Entity (Der_Base, Discr);

            loop
               Last_Discr := Discr;
               Next_Discriminant (Discr);
               exit when No (Discr);
            end loop;

            Set_Last_Entity (Der_Base, Last_Discr);

            Set_First_Entity (Derived_Type, First_Entity (Der_Base));
            Set_Last_Entity  (Derived_Type, Last_Entity  (Der_Base));
            Set_Stored_Constraint (Full_Der, Stored_Constraint (Derived_Type));

         else
            --  If this is a completion, the derived type stays private
            --  and there is no need to create a further full view, except
            --  in the unusual case when the derivation is nested within a
            --  child unit, see below.

            null;
         end if;

      elsif Present (Full_View (Parent_Type))
        and then  Has_Discriminants (Full_View (Parent_Type))
      then
         if Has_Unknown_Discriminants (Parent_Type)
           and then Nkind (Subtype_Indication (Type_Definition (N)))
             = N_Subtype_Indication
         then
            Error_Msg_N
              ("cannot constrain type with unknown discriminants",
               Subtype_Indication (Type_Definition (N)));
            return;
         end if;

         --  If full view of parent is a record type, Build full view as
         --  a derivation from the parent's full view. Partial view remains
         --  private. For code generation and linking, the full view must
         --  have the same public status as the partial one. This full view
         --  is only needed if the parent type is in an enclosing scope, so
         --  that the full view may actually become visible, e.g. in a child
         --  unit. This is both more efficient, and avoids order of freezing
         --  problems with the added entities.

         if not Is_Private_Type (Full_View (Parent_Type))
           and then (In_Open_Scopes (Scope (Parent_Type)))
         then
            Full_Der := Make_Defining_Identifier (Sloc (Derived_Type),
                                              Chars (Derived_Type));
            Set_Is_Itype (Full_Der);
            Set_Has_Private_Declaration (Full_Der);
            Set_Has_Private_Declaration (Derived_Type);
            Set_Associated_Node_For_Itype (Full_Der, N);
            Set_Parent (Full_Der, Parent (Derived_Type));
            Set_Full_View (Derived_Type, Full_Der);
            Set_Is_Public (Full_Der, Is_Public (Derived_Type));
            Full_P := Full_View (Parent_Type);
            Exchange_Declarations (Parent_Type);
            Copy_And_Build;
            Exchange_Declarations (Full_P);

         else
            Build_Derived_Record_Type
              (N, Full_View (Parent_Type), Derived_Type,
                Derive_Subps => False);
         end if;

         --  In any case, the primitive operations are inherited from
         --  the parent type, not from the internal full view.

         Set_Etype (Base_Type (Derived_Type), Base_Type (Parent_Type));

         if Derive_Subps then
            Derive_Subprograms (Parent_Type, Derived_Type);
         end if;

      else
         --  Untagged type, No discriminants on either view

         if Nkind (Subtype_Indication (Type_Definition (N))) =
                                                   N_Subtype_Indication
         then
            Error_Msg_N
              ("illegal constraint on type without discriminants", N);
         end if;

         if Present (Discriminant_Specifications (N))
           and then Present (Full_View (Parent_Type))
           and then not Is_Tagged_Type (Full_View (Parent_Type))
         then
            Error_Msg_N
              ("cannot add discriminants to untagged type", N);
         end if;

         Set_Stored_Constraint (Derived_Type, No_Elist);
         Set_Is_Constrained    (Derived_Type, Is_Constrained (Parent_Type));
         Set_Is_Controlled     (Derived_Type, Is_Controlled  (Parent_Type));
         Set_Has_Controlled_Component
                               (Derived_Type, Has_Controlled_Component
                                                             (Parent_Type));

         --  Direct controlled types do not inherit Finalize_Storage_Only flag

         if not Is_Controlled  (Parent_Type) then
            Set_Finalize_Storage_Only
              (Base_Type (Derived_Type), Finalize_Storage_Only (Parent_Type));
         end if;

         --  Construct the implicit full view by deriving from full view of
         --  the parent type. In order to get proper visibility, we install
         --  the parent scope and its declarations.

         --  ??? if the parent is untagged private and its completion is
         --  tagged, this mechanism will not work because we cannot derive
         --  from the tagged full view unless we have an extension

         if Present (Full_View (Parent_Type))
           and then not Is_Tagged_Type (Full_View (Parent_Type))
           and then not Is_Completion
         then
            Full_Der :=
              Make_Defining_Identifier (Sloc (Derived_Type),
                Chars => Chars (Derived_Type));
            Set_Is_Itype (Full_Der);
            Set_Has_Private_Declaration (Full_Der);
            Set_Has_Private_Declaration (Derived_Type);
            Set_Associated_Node_For_Itype (Full_Der, N);
            Set_Parent (Full_Der, Parent (Derived_Type));
            Set_Full_View (Derived_Type, Full_Der);

            if not In_Open_Scopes (Par_Scope) then
               Install_Private_Declarations (Par_Scope);
               Install_Visible_Declarations (Par_Scope);
               Copy_And_Build;
               Uninstall_Declarations (Par_Scope);

            --  If parent scope is open and in another unit, and parent has a
            --  completion, then the derivation is taking place in the visible
            --  part of a child unit. In that case retrieve the full view of
            --  the parent momentarily.

            elsif not In_Same_Source_Unit (N, Parent_Type) then
               Full_P := Full_View (Parent_Type);
               Exchange_Declarations (Parent_Type);
               Copy_And_Build;
               Exchange_Declarations (Full_P);

            --  Otherwise it is a local derivation

            else
               Copy_And_Build;
            end if;

            Set_Scope                (Full_Der, Current_Scope);
            Set_Is_First_Subtype     (Full_Der,
                                       Is_First_Subtype (Derived_Type));
            Set_Has_Size_Clause      (Full_Der, False);
            Set_Has_Alignment_Clause (Full_Der, False);
            Set_Next_Entity          (Full_Der, Empty);
            Set_Has_Delayed_Freeze   (Full_Der);
            Set_Is_Frozen            (Full_Der, False);
            Set_Freeze_Node          (Full_Der, Empty);
            Set_Depends_On_Private   (Full_Der,
                                        Has_Private_Component    (Full_Der));
            Set_Public_Status        (Full_Der);
         end if;
      end if;

      Set_Has_Unknown_Discriminants (Derived_Type,
        Has_Unknown_Discriminants (Parent_Type));

      if Is_Private_Type (Derived_Type) then
         Set_Private_Dependents (Derived_Type, New_Elmt_List);
      end if;

      if Is_Private_Type (Parent_Type)
        and then Base_Type (Parent_Type) = Parent_Type
        and then In_Open_Scopes (Scope (Parent_Type))
      then
         Append_Elmt (Derived_Type, Private_Dependents (Parent_Type));

         if Is_Child_Unit (Scope (Current_Scope))
           and then Is_Completion
           and then In_Private_Part (Current_Scope)
           and then Scope (Parent_Type) /= Current_Scope
         then
            --  This is the unusual case where a type completed by a private
            --  derivation occurs within a package nested in a child unit,
            --  and the parent is declared in an ancestor. In this case, the
            --  full view of the parent type will become visible in the body
            --  of the enclosing child, and only then will the current type
            --  be possibly non-private. We build a underlying full view that
            --  will be installed when the enclosing child body is compiled.

            declare
               IR : constant Node_Id := Make_Itype_Reference (Sloc (N));

            begin
               Full_Der :=
                 Make_Defining_Identifier (Sloc (Derived_Type),
                   Chars (Derived_Type));
               Set_Is_Itype (Full_Der);
               Set_Itype (IR, Full_Der);
               Insert_After (N, IR);

               --  The full view will be used to swap entities on entry/exit
               --  to the body, and must appear in the entity list for the
               --  package.

               Append_Entity (Full_Der, Scope (Derived_Type));
               Set_Has_Private_Declaration (Full_Der);
               Set_Has_Private_Declaration (Derived_Type);
               Set_Associated_Node_For_Itype (Full_Der, N);
               Set_Parent (Full_Der, Parent (Derived_Type));
               Full_P := Full_View (Parent_Type);
               Exchange_Declarations (Parent_Type);
               Copy_And_Build;
               Exchange_Declarations (Full_P);
               Set_Underlying_Full_View (Derived_Type, Full_Der);
            end;
         end if;
      end if;
   end Build_Derived_Private_Type;

   -------------------------------
   -- Build_Derived_Record_Type --
   -------------------------------

   --  1. INTRODUCTION

   --  Ideally we would like to use the same model of type derivation for
   --  tagged and untagged record types. Unfortunately this is not quite
   --  possible because the semantics of representation clauses is different
   --  for tagged and untagged records under inheritance. Consider the
   --  following:

   --     type R (...) is [tagged] record ... end record;
   --     type T (...) is new R (...) [with ...];

   --  The representation clauses of T can specify a completely different
   --  record layout from R's. Hence the same component can be placed in
   --  two very different positions in objects of type T and R. If R and T
   --  are tagged types, representation clauses for T can only specify the
   --  layout of non inherited components, thus components that are common
   --  in R and T have the same position in objects of type R and T.

   --  This has two implications. The first is that the entire tree for R's
   --  declaration needs to be copied for T in the untagged case, so that T
   --  can be viewed as a record type of its own with its own representation
   --  clauses. The second implication is the way we handle discriminants.
   --  Specifically, in the untagged case we need a way to communicate to Gigi
   --  what are the real discriminants in the record, while for the semantics
   --  we need to consider those introduced by the user to rename the
   --  discriminants in the parent type. This is handled by introducing the
   --  notion of stored discriminants. See below for more.

   --  Fortunately the way regular components are inherited can be handled in
   --  the same way in tagged and untagged types.

   --  To complicate things a bit more the private view of a private extension
   --  cannot be handled in the same way as the full view (for one thing the
   --  semantic rules are somewhat different). We will explain what differs
   --  below.

   --  2. DISCRIMINANTS UNDER INHERITANCE

   --  The semantic rules governing the discriminants of derived types are
   --  quite subtle.

   --   type Derived_Type_Name [KNOWN_DISCRIMINANT_PART] is new
   --      [abstract]  Parent_Type_Name [CONSTRAINT] [RECORD_EXTENSION_PART]

   --  If parent type has discriminants, then the discriminants that are
   --  declared in the derived type are [3.4 (11)]:

   --  o The discriminants specified by a new KNOWN_DISCRIMINANT_PART, if
   --    there is one;

   --  o Otherwise, each discriminant of the parent type (implicitly declared
   --    in the same order with the same specifications). In this case, the
   --    discriminants are said to be "inherited", or if unknown in the parent
   --    are also unknown in the derived type.

   --  Furthermore if a KNOWN_DISCRIMINANT_PART is provided, then [3.7(13-18)]:

   --  o The parent subtype shall be constrained;

   --  o If the parent type is not a tagged type, then each discriminant of
   --    the derived type shall be used in the constraint defining a parent
   --    subtype [Implementation note: this ensures that the new discriminant
   --    can share storage with an existing discriminant.].

   --  For the derived type each discriminant of the parent type is either
   --  inherited, constrained to equal some new discriminant of the derived
   --  type, or constrained to the value of an expression.

   --  When inherited or constrained to equal some new discriminant, the
   --  parent discriminant and the discriminant of the derived type are said
   --  to "correspond".

   --  If a discriminant of the parent type is constrained to a specific value
   --  in the derived type definition, then the discriminant is said to be
   --  "specified" by that derived type definition.

   --  3. DISCRIMINANTS IN DERIVED UNTAGGED RECORD TYPES

   --  We have spoken about stored discriminants in point 1 (introduction)
   --  above. There are two sort of stored discriminants: implicit and
   --  explicit. As long as the derived type inherits the same discriminants as
   --  the root record type, stored discriminants are the same as regular
   --  discriminants, and are said to be implicit. However, if any discriminant
   --  in the root type was renamed in the derived type, then the derived
   --  type will contain explicit stored discriminants. Explicit stored
   --  discriminants are discriminants in addition to the semantically visible
   --  discriminants defined for the derived type. Stored discriminants are
   --  used by Gigi to figure out what are the physical discriminants in
   --  objects of the derived type (see precise definition in einfo.ads).
   --  As an example, consider the following:

   --           type R  (D1, D2, D3 : Int) is record ... end record;
   --           type T1 is new R;
   --           type T2 (X1, X2: Int) is new T1 (X2, 88, X1);
   --           type T3 is new T2;
   --           type T4 (Y : Int) is new T3 (Y, 99);

   --  The following table summarizes the discriminants and stored
   --  discriminants in R and T1 through T4.

   --   Type      Discrim     Stored Discrim  Comment
   --    R      (D1, D2, D3)   (D1, D2, D3)   Girder discrims implicit in R
   --    T1     (D1, D2, D3)   (D1, D2, D3)   Girder discrims implicit in T1
   --    T2     (X1, X2)       (D1, D2, D3)   Girder discrims EXPLICIT in T2
   --    T3     (X1, X2)       (D1, D2, D3)   Girder discrims EXPLICIT in T3
   --    T4     (Y)            (D1, D2, D3)   Girder discrims EXPLICIT in T4

   --  Field Corresponding_Discriminant (abbreviated CD below) allows us to
   --  find the corresponding discriminant in the parent type, while
   --  Original_Record_Component (abbreviated ORC below), the actual physical
   --  component that is renamed. Finally the field Is_Completely_Hidden
   --  (abbreviated ICH below) is set for all explicit stored discriminants
   --  (see einfo.ads for more info). For the above example this gives:

   --                 Discrim     CD        ORC     ICH
   --                 ^^^^^^^     ^^        ^^^     ^^^
   --                 D1 in R    empty     itself    no
   --                 D2 in R    empty     itself    no
   --                 D3 in R    empty     itself    no

   --                 D1 in T1  D1 in R    itself    no
   --                 D2 in T1  D2 in R    itself    no
   --                 D3 in T1  D3 in R    itself    no

   --                 X1 in T2  D3 in T1  D3 in T2   no
   --                 X2 in T2  D1 in T1  D1 in T2   no
   --                 D1 in T2   empty    itself    yes
   --                 D2 in T2   empty    itself    yes
   --                 D3 in T2   empty    itself    yes

   --                 X1 in T3  X1 in T2  D3 in T3   no
   --                 X2 in T3  X2 in T2  D1 in T3   no
   --                 D1 in T3   empty    itself    yes
   --                 D2 in T3   empty    itself    yes
   --                 D3 in T3   empty    itself    yes

   --                 Y  in T4  X1 in T3  D3 in T3   no
   --                 D1 in T3   empty    itself    yes
   --                 D2 in T3   empty    itself    yes
   --                 D3 in T3   empty    itself    yes

   --  4. DISCRIMINANTS IN DERIVED TAGGED RECORD TYPES

   --  Type derivation for tagged types is fairly straightforward. if no
   --  discriminants are specified by the derived type, these are inherited
   --  from the parent. No explicit stored discriminants are ever necessary.
   --  The only manipulation that is done to the tree is that of adding a
   --  _parent field with parent type and constrained to the same constraint
   --  specified for the parent in the derived type definition. For instance:

   --           type R  (D1, D2, D3 : Int) is tagged record ... end record;
   --           type T1 is new R with null record;
   --           type T2 (X1, X2: Int) is new T1 (X2, 88, X1) with null record;

   --  are changed into:

   --           type T1 (D1, D2, D3 : Int) is new R (D1, D2, D3) with record
   --              _parent : R (D1, D2, D3);
   --           end record;

   --           type T2 (X1, X2: Int) is new T1 (X2, 88, X1) with record
   --              _parent : T1 (X2, 88, X1);
   --           end record;

   --  The discriminants actually present in R, T1 and T2 as well as their CD,
   --  ORC and ICH fields are:

   --                 Discrim     CD        ORC     ICH
   --                 ^^^^^^^     ^^        ^^^     ^^^
   --                 D1 in R    empty     itself    no
   --                 D2 in R    empty     itself    no
   --                 D3 in R    empty     itself    no

   --                 D1 in T1  D1 in R    D1 in R   no
   --                 D2 in T1  D2 in R    D2 in R   no
   --                 D3 in T1  D3 in R    D3 in R   no

   --                 X1 in T2  D3 in T1   D3 in R   no
   --                 X2 in T2  D1 in T1   D1 in R   no

   --  5. FIRST TRANSFORMATION FOR DERIVED RECORDS
   --
   --  Regardless of whether we dealing with a tagged or untagged type
   --  we will transform all derived type declarations of the form
   --
   --               type T is new R (...) [with ...];
   --  or
   --               subtype S is R (...);
   --               type T is new S [with ...];
   --  into
   --               type BT is new R [with ...];
   --               subtype T is BT (...);
   --
   --  That is, the base derived type is constrained only if it has no
   --  discriminants. The reason for doing this is that GNAT's semantic model
   --  assumes that a base type with discriminants is unconstrained.
   --
   --  Note that, strictly speaking, the above transformation is not always
   --  correct. Consider for instance the following excerpt from ACVC b34011a:
   --
   --       procedure B34011A is
   --          type REC (D : integer := 0) is record
   --             I : Integer;
   --          end record;

   --          package P is
   --             type T6 is new Rec;
   --             function F return T6;
   --          end P;

   --          use P;
   --          package Q6 is
   --             type U is new T6 (Q6.F.I);                   -- ERROR: Q6.F.
   --          end Q6;
   --
   --  The definition of Q6.U is illegal. However transforming Q6.U into

   --             type BaseU is new T6;
   --             subtype U is BaseU (Q6.F.I)

   --  turns U into a legal subtype, which is incorrect. To avoid this problem
   --  we always analyze the constraint (in this case (Q6.F.I)) before applying
   --  the transformation described above.

   --  There is another instance where the above transformation is incorrect.
   --  Consider:

   --          package Pack is
   --             type Base (D : Integer) is tagged null record;
   --             procedure P (X : Base);

   --             type Der is new Base (2) with null record;
   --             procedure P (X : Der);
   --          end Pack;

   --  Then the above transformation turns this into

   --             type Der_Base is new Base with null record;
   --             --  procedure P (X : Base) is implicitly inherited here
   --             --  as procedure P (X : Der_Base).

   --             subtype Der is Der_Base (2);
   --             procedure P (X : Der);
   --             --  The overriding of P (X : Der_Base) is illegal since we
   --             --  have a parameter conformance problem.

   --  To get around this problem, after having semantically processed Der_Base
   --  and the rewritten subtype declaration for Der, we copy Der_Base field
   --  Discriminant_Constraint from Der so that when parameter conformance is
   --  checked when P is overridden, no semantic errors are flagged.

   --  6. SECOND TRANSFORMATION FOR DERIVED RECORDS

   --  Regardless of whether we are dealing with a tagged or untagged type
   --  we will transform all derived type declarations of the form

   --               type R (D1, .., Dn : ...) is [tagged] record ...;
   --               type T is new R [with ...];
   --  into
   --               type T (D1, .., Dn : ...) is new R (D1, .., Dn) [with ...];

   --  The reason for such transformation is that it allows us to implement a
   --  very clean form of component inheritance as explained below.

   --  Note that this transformation is not achieved by direct tree rewriting
   --  and manipulation, but rather by redoing the semantic actions that the
   --  above transformation will entail. This is done directly in routine
   --  Inherit_Components.

   --  7. TYPE DERIVATION AND COMPONENT INHERITANCE

   --  In both tagged and untagged derived types, regular non discriminant
   --  components are inherited in the derived type from the parent type. In
   --  the absence of discriminants component, inheritance is straightforward
   --  as components can simply be copied from the parent.

   --  If the parent has discriminants, inheriting components constrained with
   --  these discriminants requires caution. Consider the following example:

   --      type R  (D1, D2 : Positive) is [tagged] record
   --         S : String (D1 .. D2);
   --      end record;

   --      type T1                is new R        [with null record];
   --      type T2 (X : positive) is new R (1, X) [with null record];

   --  As explained in 6. above, T1 is rewritten as
   --      type T1 (D1, D2 : Positive) is new R (D1, D2) [with null record];
   --  which makes the treatment for T1 and T2 identical.

   --  What we want when inheriting S, is that references to D1 and D2 in R are
   --  replaced with references to their correct constraints, ie D1 and D2 in
   --  T1 and 1 and X in T2. So all R's discriminant references are replaced
   --  with either discriminant references in the derived type or expressions.
   --  This replacement is achieved as follows: before inheriting R's
   --  components, a subtype R (D1, D2) for T1 (resp. R (1, X) for T2) is
   --  created in the scope of T1 (resp. scope of T2) so that discriminants D1
   --  and D2 of T1 are visible (resp. discriminant X of T2 is visible).
   --  For T2, for instance, this has the effect of replacing String (D1 .. D2)
   --  by String (1 .. X).

   --  8. TYPE DERIVATION IN PRIVATE TYPE EXTENSIONS

   --  We explain here the rules governing private type extensions relevant to
   --  type derivation. These rules are explained on the following example:

   --      type D [(...)] is new A [(...)] with private;      <-- partial view
   --      type D [(...)] is new P [(...)] with null record;  <-- full view

   --  Type A is called the ancestor subtype of the private extension.
   --  Type P is the parent type of the full view of the private extension. It
   --  must be A or a type derived from A.

   --  The rules concerning the discriminants of private type extensions are
   --  [7.3(10-13)]:

   --  o If a private extension inherits known discriminants from the ancestor
   --    subtype, then the full view shall also inherit its discriminants from
   --    the ancestor subtype and the parent subtype of the full view shall be
   --    constrained if and only if the ancestor subtype is constrained.

   --  o If a partial view has unknown discriminants, then the full view may
   --    define a definite or an indefinite subtype, with or without
   --    discriminants.

   --  o If a partial view has neither known nor unknown discriminants, then
   --    the full view shall define a definite subtype.

   --  o If the ancestor subtype of a private extension has constrained
   --    discriminants, then the parent subtype of the full view shall impose a
   --    statically matching constraint on those discriminants.

   --  This means that only the following forms of private extensions are
   --  allowed:

   --      type D is new A with private;      <-- partial view
   --      type D is new P with null record;  <-- full view

   --  If A has no discriminants than P has no discriminants, otherwise P must
   --  inherit A's discriminants.

   --      type D is new A (...) with private;      <-- partial view
   --      type D is new P (:::) with null record;  <-- full view

   --  P must inherit A's discriminants and (...) and (:::) must statically
   --  match.

   --      subtype A is R (...);
   --      type D is new A with private;      <-- partial view
   --      type D is new P with null record;  <-- full view

   --  P must have inherited R's discriminants and must be derived from A or
   --  any of its subtypes.

   --      type D (..) is new A with private;              <-- partial view
   --      type D (..) is new P [(:::)] with null record;  <-- full view

   --  No specific constraints on P's discriminants or constraint (:::).
   --  Note that A can be unconstrained, but the parent subtype P must either
   --  be constrained or (:::) must be present.

   --      type D (..) is new A [(...)] with private;      <-- partial view
   --      type D (..) is new P [(:::)] with null record;  <-- full view

   --  P's constraints on A's discriminants must statically match those
   --  imposed by (...).

   --  9. IMPLEMENTATION OF TYPE DERIVATION FOR PRIVATE EXTENSIONS

   --  The full view of a private extension is handled exactly as described
   --  above. The model chose for the private view of a private extension is
   --  the same for what concerns discriminants (ie they receive the same
   --  treatment as in the tagged case). However, the private view of the
   --  private extension always inherits the components of the parent base,
   --  without replacing any discriminant reference. Strictly speaking this is
   --  incorrect. However, Gigi never uses this view to generate code so this
   --  is a purely semantic issue. In theory, a set of transformations similar
   --  to those given in 5. and 6. above could be applied to private views of
   --  private extensions to have the same model of component inheritance as
   --  for non private extensions. However, this is not done because it would
   --  further complicate private type processing. Semantically speaking, this
   --  leaves us in an uncomfortable situation. As an example consider:

   --          package Pack is
   --             type R (D : integer) is tagged record
   --                S : String (1 .. D);
   --             end record;
   --             procedure P (X : R);
   --             type T is new R (1) with private;
   --          private
   --             type T is new R (1) with null record;
   --          end;

   --  This is transformed into:

   --          package Pack is
   --             type R (D : integer) is tagged record
   --                S : String (1 .. D);
   --             end record;
   --             procedure P (X : R);
   --             type T is new R (1) with private;
   --          private
   --             type BaseT is new R with null record;
   --             subtype  T is BaseT (1);
   --          end;

   --  (strictly speaking the above is incorrect Ada)

   --  From the semantic standpoint the private view of private extension T
   --  should be flagged as constrained since one can clearly have
   --
   --             Obj : T;
   --
   --  in a unit withing Pack. However, when deriving subprograms for the
   --  private view of private extension T, T must be seen as unconstrained
   --  since T has discriminants (this is a constraint of the current
   --  subprogram derivation model). Thus, when processing the private view of
   --  a private extension such as T, we first mark T as unconstrained, we
   --  process it, we perform program derivation and just before returning from
   --  Build_Derived_Record_Type we mark T as constrained.

   --  ??? Are there are other uncomfortable cases that we will have to
   --      deal with.

   --  10. RECORD_TYPE_WITH_PRIVATE complications

   --  Types that are derived from a visible record type and have a private
   --  extension present other peculiarities. They behave mostly like private
   --  types, but if they have primitive operations defined, these will not
   --  have the proper signatures for further inheritance, because other
   --  primitive operations will use the implicit base that we define for
   --  private derivations below. This affect subprogram inheritance (see
   --  Derive_Subprograms for details). We also derive the implicit base from
   --  the base type of the full view, so that the implicit base is a record
   --  type and not another private type, This avoids infinite loops.

   procedure Build_Derived_Record_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Derive_Subps : Boolean := True)
   is
      Loc          : constant Source_Ptr := Sloc (N);
      Parent_Base  : Entity_Id;
      Type_Def     : Node_Id;
      Indic        : Node_Id;
      Discrim      : Entity_Id;
      Last_Discrim : Entity_Id;
      Constrs      : Elist_Id;

      Discs        : Elist_Id := New_Elmt_List;
      --  An empty Discs list means that there were no constraints in the
      --  subtype indication or that there was an error processing it.

      Assoc_List         : Elist_Id;
      New_Discrs         : Elist_Id;
      New_Base           : Entity_Id;
      New_Decl           : Node_Id;
      New_Indic          : Node_Id;

      Is_Tagged          : constant Boolean := Is_Tagged_Type (Parent_Type);
      Discriminant_Specs : constant Boolean :=
                             Present (Discriminant_Specifications (N));
      Private_Extension  : constant Boolean :=
                             (Nkind (N) = N_Private_Extension_Declaration);

      Constraint_Present     : Boolean;
      Has_Interfaces         : Boolean := False;
      Inherit_Discrims       : Boolean := False;
      Last_Inherited_Prim_Op : Elmt_Id;
      Tagged_Partial_View    : Entity_Id;
      Save_Etype             : Entity_Id;
      Save_Discr_Constr      : Elist_Id;
      Save_Next_Entity       : Entity_Id;

   begin
      if Ekind (Parent_Type) = E_Record_Type_With_Private
        and then Present (Full_View (Parent_Type))
        and then Has_Discriminants (Parent_Type)
      then
         Parent_Base := Base_Type (Full_View (Parent_Type));
      else
         Parent_Base := Base_Type (Parent_Type);
      end if;

      --  Before we start the previously documented transformations, here is
      --  a little fix for size and alignment of tagged types. Normally when
      --  we derive type D from type P, we copy the size and alignment of P
      --  as the default for D, and in the absence of explicit representation
      --  clauses for D, the size and alignment are indeed the same as the
      --  parent.

      --  But this is wrong for tagged types, since fields may be added,
      --  and the default size may need to be larger, and the default
      --  alignment may need to be larger.

      --  We therefore reset the size and alignment fields in the tagged
      --  case. Note that the size and alignment will in any case be at
      --  least as large as the parent type (since the derived type has
      --  a copy of the parent type in the _parent field)

      if Is_Tagged then
         Init_Size_Align (Derived_Type);
      end if;

      --  STEP 0a: figure out what kind of derived type declaration we have

      if Private_Extension then
         Type_Def := N;
         Set_Ekind (Derived_Type, E_Record_Type_With_Private);

      else
         Type_Def := Type_Definition (N);

         --  Ekind (Parent_Base) in not necessarily E_Record_Type since
         --  Parent_Base can be a private type or private extension. However,
         --  for tagged types with an extension the newly added fields are
         --  visible and hence the Derived_Type is always an E_Record_Type.
         --  (except that the parent may have its own private fields).
         --  For untagged types we preserve the Ekind of the Parent_Base.

         if Present (Record_Extension_Part (Type_Def)) then
            Set_Ekind (Derived_Type, E_Record_Type);
         else
            Set_Ekind (Derived_Type, Ekind (Parent_Base));
         end if;
      end if;

      --  Indic can either be an N_Identifier if the subtype indication
      --  contains no constraint or an N_Subtype_Indication if the subtype
      --  indication has a constraint.

      Indic := Subtype_Indication (Type_Def);
      Constraint_Present := (Nkind (Indic) = N_Subtype_Indication);

      --  Check that the type has visible discriminants. The type may be
      --  a private type with unknown discriminants whose full view has
      --  discriminants which are invisible.

      if Constraint_Present then
         if not Has_Discriminants (Parent_Base)
           or else
             (Has_Unknown_Discriminants (Parent_Base)
                and then Is_Private_Type (Parent_Base))
         then
            Error_Msg_N
              ("invalid constraint: type has no discriminant",
                 Constraint (Indic));

            Constraint_Present := False;
            Rewrite (Indic, New_Copy_Tree (Subtype_Mark (Indic)));

         elsif Is_Constrained (Parent_Type) then
            Error_Msg_N
               ("invalid constraint: parent type is already constrained",
                  Constraint (Indic));

            Constraint_Present := False;
            Rewrite (Indic, New_Copy_Tree (Subtype_Mark (Indic)));
         end if;
      end if;

      --  STEP 0b: If needed, apply transformation given in point 5. above

      if not Private_Extension
        and then Has_Discriminants (Parent_Type)
        and then not Discriminant_Specs
        and then (Is_Constrained (Parent_Type) or else Constraint_Present)
      then
         --  First, we must analyze the constraint (see comment in point 5.)

         if Constraint_Present then
            New_Discrs := Build_Discriminant_Constraints (Parent_Type, Indic);

            if Has_Discriminants (Derived_Type)
              and then Has_Private_Declaration (Derived_Type)
              and then Present (Discriminant_Constraint (Derived_Type))
            then
               --  Verify that constraints of the full view conform to those
               --  given in partial view.

               declare
                  C1, C2 : Elmt_Id;

               begin
                  C1 := First_Elmt (New_Discrs);
                  C2 := First_Elmt (Discriminant_Constraint (Derived_Type));
                  while Present (C1) and then Present (C2) loop
                     if not
                       Fully_Conformant_Expressions (Node (C1), Node (C2))
                     then
                        Error_Msg_N (
                          "constraint not conformant to previous declaration",
                             Node (C1));
                     end if;

                     Next_Elmt (C1);
                     Next_Elmt (C2);
                  end loop;
               end;
            end if;
         end if;

         --  Insert and analyze the declaration for the unconstrained base type

         New_Base := Create_Itype (Ekind (Derived_Type), N, Derived_Type, 'B');

         New_Decl :=
           Make_Full_Type_Declaration (Loc,
              Defining_Identifier => New_Base,
              Type_Definition     =>
                Make_Derived_Type_Definition (Loc,
                  Abstract_Present      => Abstract_Present (Type_Def),
                  Subtype_Indication    =>
                    New_Occurrence_Of (Parent_Base, Loc),
                  Record_Extension_Part =>
                    Relocate_Node (Record_Extension_Part (Type_Def))));

         Set_Parent (New_Decl, Parent (N));
         Mark_Rewrite_Insertion (New_Decl);
         Insert_Before (N, New_Decl);

         --  Note that this call passes False for the Derive_Subps parameter
         --  because subprogram derivation is deferred until after creating
         --  the subtype (see below).

         Build_Derived_Type
           (New_Decl, Parent_Base, New_Base,
            Is_Completion => True, Derive_Subps => False);

         --  ??? This needs re-examination to determine whether the
         --  above call can simply be replaced by a call to Analyze.

         Set_Analyzed (New_Decl);

         --  Insert and analyze the declaration for the constrained subtype

         if Constraint_Present then
            New_Indic :=
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Occurrence_Of (New_Base, Loc),
                Constraint   => Relocate_Node (Constraint (Indic)));

         else
            declare
               Constr_List : constant List_Id := New_List;
               C           : Elmt_Id;
               Expr        : Node_Id;

            begin
               C := First_Elmt (Discriminant_Constraint (Parent_Type));
               while Present (C) loop
                  Expr := Node (C);

                  --  It is safe here to call New_Copy_Tree since
                  --  Force_Evaluation was called on each constraint in
                  --  Build_Discriminant_Constraints.

                  Append (New_Copy_Tree (Expr), To => Constr_List);

                  Next_Elmt (C);
               end loop;

               New_Indic :=
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark => New_Occurrence_Of (New_Base, Loc),
                   Constraint   =>
                     Make_Index_Or_Discriminant_Constraint (Loc, Constr_List));
            end;
         end if;

         Rewrite (N,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Derived_Type,
             Subtype_Indication  => New_Indic));

         Analyze (N);

         --  Derivation of subprograms must be delayed until the full subtype
         --  has been established to ensure proper overriding of subprograms
         --  inherited by full types. If the derivations occurred as part of
         --  the call to Build_Derived_Type above, then the check for type
         --  conformance would fail because earlier primitive subprograms
         --  could still refer to the full type prior the change to the new
         --  subtype and hence would not match the new base type created here.

         Derive_Subprograms (Parent_Type, Derived_Type);

         --  For tagged types the Discriminant_Constraint of the new base itype
         --  is inherited from the first subtype so that no subtype conformance
         --  problem arise when the first subtype overrides primitive
         --  operations inherited by the implicit base type.

         if Is_Tagged then
            Set_Discriminant_Constraint
              (New_Base, Discriminant_Constraint (Derived_Type));
         end if;

         return;
      end if;

      --  If we get here Derived_Type will have no discriminants or it will be
      --  a discriminated unconstrained base type.

      --  STEP 1a: perform preliminary actions/checks for derived tagged types

      if Is_Tagged then

         --  The parent type is frozen for non-private extensions (RM 13.14(7))

         if not Private_Extension then
            Freeze_Before (N, Parent_Type);
         end if;

         --  In Ada 2005 (AI-344), the restriction that a derived tagged type
         --  cannot be declared at a deeper level than its parent type is
         --  removed. The check on derivation within a generic body is also
         --  relaxed, but there's a restriction that a derived tagged type
         --  cannot be declared in a generic body if it's derived directly
         --  or indirectly from a formal type of that generic.

         if Ada_Version >= Ada_05 then
            if Present (Enclosing_Generic_Body (Derived_Type)) then
               declare
                  Ancestor_Type : Entity_Id;

               begin
                  --  Check to see if any ancestor of the derived type is a
                  --  formal type.

                  Ancestor_Type := Parent_Type;
                  while not Is_Generic_Type (Ancestor_Type)
                    and then Etype (Ancestor_Type) /= Ancestor_Type
                  loop
                     Ancestor_Type := Etype (Ancestor_Type);
                  end loop;

                  --  If the derived type does have a formal type as an
                  --  ancestor, then it's an error if the derived type is
                  --  declared within the body of the generic unit that
                  --  declares the formal type in its generic formal part. It's
                  --  sufficient to check whether the ancestor type is declared
                  --  inside the same generic body as the derived type (such as
                  --  within a nested generic spec), in which case the
                  --  derivation is legal. If the formal type is declared
                  --  outside of that generic body, then it's guaranteed that
                  --  the derived type is declared within the generic body of
                  --  the generic unit declaring the formal type.

                  if Is_Generic_Type (Ancestor_Type)
                    and then Enclosing_Generic_Body (Ancestor_Type) /=
                               Enclosing_Generic_Body (Derived_Type)
                  then
                     Error_Msg_NE
                       ("parent type of& must not be descendant of formal type"
                          & " of an enclosing generic body",
                            Indic, Derived_Type);
                  end if;
               end;
            end if;

         elsif Type_Access_Level (Derived_Type) /=
                 Type_Access_Level (Parent_Type)
           and then not Is_Generic_Type (Derived_Type)
         then
            if Is_Controlled (Parent_Type) then
               Error_Msg_N
                 ("controlled type must be declared at the library level",
                  Indic);
            else
               Error_Msg_N
                 ("type extension at deeper accessibility level than parent",
                  Indic);
            end if;

         else
            declare
               GB : constant Node_Id := Enclosing_Generic_Body (Derived_Type);

            begin
               if Present (GB)
                 and then GB /= Enclosing_Generic_Body (Parent_Base)
               then
                  Error_Msg_NE
                    ("parent type of& must not be outside generic body"
                       & " ('R'M 3.9.1(4))",
                         Indic, Derived_Type);
               end if;
            end;
         end if;
      end if;

      --  Ada 2005 (AI-251)

      if Ada_Version = Ada_05
        and then Is_Tagged
      then

         --  "The declaration of a specific descendant of an interface type
         --  freezes the interface type" (RM 13.14).

         declare
            Iface : Node_Id;
         begin
            if Is_Non_Empty_List (Interface_List (Type_Def)) then
               Iface := First (Interface_List (Type_Def));
               while Present (Iface) loop
                  Freeze_Before (N, Etype (Iface));
                  Next (Iface);
               end loop;
            end if;
         end;
      end if;

      --  STEP 1b : preliminary cleanup of the full view of private types

      --  If the type is already marked as having discriminants, then it's the
      --  completion of a private type or private extension and we need to
      --  retain the discriminants from the partial view if the current
      --  declaration has Discriminant_Specifications so that we can verify
      --  conformance. However, we must remove any existing components that
      --  were inherited from the parent (and attached in Copy_And_Swap)
      --  because the full type inherits all appropriate components anyway, and
      --  we do not want the partial view's components interfering.

      if Has_Discriminants (Derived_Type) and then Discriminant_Specs then
         Discrim := First_Discriminant (Derived_Type);
         loop
            Last_Discrim := Discrim;
            Next_Discriminant (Discrim);
            exit when No (Discrim);
         end loop;

         Set_Last_Entity (Derived_Type, Last_Discrim);

      --  In all other cases wipe out the list of inherited components (even
      --  inherited discriminants), it will be properly rebuilt here.

      else
         Set_First_Entity (Derived_Type, Empty);
         Set_Last_Entity  (Derived_Type, Empty);
      end if;

      --  STEP 1c: Initialize some flags for the Derived_Type

      --  The following flags must be initialized here so that
      --  Process_Discriminants can check that discriminants of tagged types
      --  do not have a default initial value and that access discriminants
      --  are only specified for limited records. For completeness, these
      --  flags are also initialized along with all the other flags below.

      Set_Is_Tagged_Type    (Derived_Type, Is_Tagged);
      Set_Is_Limited_Record (Derived_Type, Is_Limited_Record (Parent_Type));

      --  STEP 2a: process discriminants of derived type if any

      New_Scope (Derived_Type);

      if Discriminant_Specs then
         Set_Has_Unknown_Discriminants (Derived_Type, False);

         --  The following call initializes fields Has_Discriminants and
         --  Discriminant_Constraint, unless we are processing the completion
         --  of a private type declaration.

         Check_Or_Process_Discriminants (N, Derived_Type);

         --  For non-tagged types the constraint on the Parent_Type must be
         --  present and is used to rename the discriminants.

         if not Is_Tagged and then not Has_Discriminants (Parent_Type) then
            Error_Msg_N ("untagged parent must have discriminants", Indic);

         elsif not Is_Tagged and then not Constraint_Present then
            Error_Msg_N
              ("discriminant constraint needed for derived untagged records",
               Indic);

         --  Otherwise the parent subtype must be constrained unless we have a
         --  private extension.

         elsif not Constraint_Present
           and then not Private_Extension
           and then not Is_Constrained (Parent_Type)
         then
            Error_Msg_N
              ("unconstrained type not allowed in this context", Indic);

         elsif Constraint_Present then
            --  The following call sets the field Corresponding_Discriminant
            --  for the discriminants in the Derived_Type.

            Discs := Build_Discriminant_Constraints (Parent_Type, Indic, True);

            --  For untagged types all new discriminants must rename
            --  discriminants in the parent. For private extensions new
            --  discriminants cannot rename old ones (implied by [7.3(13)]).

            Discrim := First_Discriminant (Derived_Type);
            while Present (Discrim) loop
               if not Is_Tagged
                 and then not Present (Corresponding_Discriminant (Discrim))
               then
                  Error_Msg_N
                    ("new discriminants must constrain old ones", Discrim);

               elsif Private_Extension
                 and then Present (Corresponding_Discriminant (Discrim))
               then
                  Error_Msg_N
                    ("only static constraints allowed for parent"
                     & " discriminants in the partial view", Indic);
                  exit;
               end if;

               --  If a new discriminant is used in the constraint, then its
               --  subtype must be statically compatible with the parent
               --  discriminant's subtype (3.7(15)).

               if Present (Corresponding_Discriminant (Discrim))
                 and then
                   not Subtypes_Statically_Compatible
                         (Etype (Discrim),
                          Etype (Corresponding_Discriminant (Discrim)))
               then
                  Error_Msg_N
                    ("subtype must be compatible with parent discriminant",
                     Discrim);
               end if;

               Next_Discriminant (Discrim);
            end loop;

            --  Check whether the constraints of the full view statically
            --  match those imposed by the parent subtype [7.3(13)].

            if Present (Stored_Constraint (Derived_Type)) then
               declare
                  C1, C2 : Elmt_Id;

               begin
                  C1 := First_Elmt (Discs);
                  C2 := First_Elmt (Stored_Constraint (Derived_Type));
                  while Present (C1) and then Present (C2) loop
                     if not
                       Fully_Conformant_Expressions (Node (C1), Node (C2))
                     then
                        Error_Msg_N (
                          "not conformant with previous declaration",
                             Node (C1));
                     end if;

                     Next_Elmt (C1);
                     Next_Elmt (C2);
                  end loop;
               end;
            end if;
         end if;

      --  STEP 2b: No new discriminants, inherit discriminants if any

      else
         if Private_Extension then
            Set_Has_Unknown_Discriminants
              (Derived_Type,
               Has_Unknown_Discriminants (Parent_Type)
                 or else Unknown_Discriminants_Present (N));

         --  The partial view of the parent may have unknown discriminants,
         --  but if the full view has discriminants and the parent type is
         --  in scope they must be inherited.

         elsif Has_Unknown_Discriminants (Parent_Type)
           and then
            (not Has_Discriminants (Parent_Type)
              or else not In_Open_Scopes (Scope (Parent_Type)))
         then
            Set_Has_Unknown_Discriminants (Derived_Type);
         end if;

         if not Has_Unknown_Discriminants (Derived_Type)
           and then not Has_Unknown_Discriminants (Parent_Base)
           and then Has_Discriminants (Parent_Type)
         then
            Inherit_Discrims := True;
            Set_Has_Discriminants
              (Derived_Type, True);
            Set_Discriminant_Constraint
              (Derived_Type, Discriminant_Constraint (Parent_Base));
         end if;

         --  The following test is true for private types (remember
         --  transformation 5. is not applied to those) and in an error
         --  situation.

         if Constraint_Present then
            Discs := Build_Discriminant_Constraints (Parent_Type, Indic);
         end if;

         --  For now mark a new derived type as constrained only if it has no
         --  discriminants. At the end of Build_Derived_Record_Type we properly
         --  set this flag in the case of private extensions. See comments in
         --  point 9. just before body of Build_Derived_Record_Type.

         Set_Is_Constrained
           (Derived_Type,
            not (Inherit_Discrims
                   or else Has_Unknown_Discriminants (Derived_Type)));
      end if;

      --  STEP 3: initialize fields of derived type

      Set_Is_Tagged_Type    (Derived_Type, Is_Tagged);
      Set_Stored_Constraint (Derived_Type, No_Elist);

      --  Ada 2005 (AI-251): Private type-declarations can implement interfaces
      --  but cannot be interfaces

      if not Private_Extension
         and then Ekind (Derived_Type) /= E_Private_Type
         and then Ekind (Derived_Type) /= E_Limited_Private_Type
      then
         Set_Is_Interface (Derived_Type, Interface_Present (Type_Def));
         Set_Abstract_Interfaces (Derived_Type, No_Elist);
      end if;

      --  Fields inherited from the Parent_Type

      Set_Discard_Names
        (Derived_Type, Einfo.Discard_Names      (Parent_Type));
      Set_Has_Specified_Layout
        (Derived_Type, Has_Specified_Layout     (Parent_Type));
      Set_Is_Limited_Composite
        (Derived_Type, Is_Limited_Composite     (Parent_Type));
      Set_Is_Limited_Record
        (Derived_Type, Is_Limited_Record        (Parent_Type));
      Set_Is_Private_Composite
        (Derived_Type, Is_Private_Composite     (Parent_Type));

      --  Fields inherited from the Parent_Base

      Set_Has_Controlled_Component
        (Derived_Type, Has_Controlled_Component (Parent_Base));
      Set_Has_Non_Standard_Rep
        (Derived_Type, Has_Non_Standard_Rep     (Parent_Base));
      Set_Has_Primitive_Operations
        (Derived_Type, Has_Primitive_Operations (Parent_Base));

      --  Direct controlled types do not inherit Finalize_Storage_Only flag

      if not Is_Controlled  (Parent_Type) then
         Set_Finalize_Storage_Only
           (Derived_Type, Finalize_Storage_Only (Parent_Type));
      end if;

      --  Set fields for private derived types

      if Is_Private_Type (Derived_Type) then
         Set_Depends_On_Private (Derived_Type, True);
         Set_Private_Dependents (Derived_Type, New_Elmt_List);

      --  Inherit fields from non private record types. If this is the
      --  completion of a derivation from a private type, the parent itself
      --  is private, and the attributes come from its full view, which must
      --  be present.

      else
         if Is_Private_Type (Parent_Base)
           and then not Is_Record_Type (Parent_Base)
         then
            Set_Component_Alignment
              (Derived_Type, Component_Alignment (Full_View (Parent_Base)));
            Set_C_Pass_By_Copy
              (Derived_Type, C_Pass_By_Copy      (Full_View (Parent_Base)));
         else
            Set_Component_Alignment
              (Derived_Type, Component_Alignment (Parent_Base));

            Set_C_Pass_By_Copy
              (Derived_Type, C_Pass_By_Copy      (Parent_Base));
         end if;
      end if;

      --  Set fields for tagged types

      if Is_Tagged then
         Set_Primitive_Operations (Derived_Type, New_Elmt_List);

         --  All tagged types defined in Ada.Finalization are controlled

         if Chars (Scope (Derived_Type)) = Name_Finalization
           and then Chars (Scope (Scope (Derived_Type))) = Name_Ada
           and then Scope (Scope (Scope (Derived_Type))) = Standard_Standard
         then
            Set_Is_Controlled (Derived_Type);
         else
            Set_Is_Controlled (Derived_Type, Is_Controlled (Parent_Base));
         end if;

         Make_Class_Wide_Type (Derived_Type);
         Set_Is_Abstract      (Derived_Type, Abstract_Present (Type_Def));

         if Has_Discriminants (Derived_Type)
           and then Constraint_Present
         then
            Set_Stored_Constraint
              (Derived_Type, Expand_To_Stored_Constraint (Parent_Base, Discs));
         end if;

         --  Ada 2005 (AI-251): Look for the partial view of tagged types
         --  declared in the private part. This will be used 1) to check that
         --  the set of interfaces in both views is equal, and 2) to complete
         --  the derivation of subprograms covering interfaces.

         Tagged_Partial_View := Empty;

         if Has_Private_Declaration (Derived_Type) then
            Tagged_Partial_View := Next_Entity (Derived_Type);
            loop
               exit when Has_Private_Declaration (Tagged_Partial_View)
                 and then Full_View (Tagged_Partial_View) = Derived_Type;

               Next_Entity (Tagged_Partial_View);
            end loop;
         end if;

         --  Ada 2005 (AI-251): Collect the whole list of implemented
         --  interfaces.

         if Ada_Version >= Ada_05 then
            Set_Abstract_Interfaces (Derived_Type, New_Elmt_List);

            if Nkind (N) = N_Private_Extension_Declaration then
               Collect_Interfaces (N, Derived_Type);
            else
               Collect_Interfaces (Type_Definition (N), Derived_Type);
            end if;

            --  Ada 2005 (AI-251): The progenitor types specified in a private
            --  extension declaration and the progenitor types specified in the
            --  corresponding declaration of a record extension given in the
            --  private part need not be the same; the only requirement is that
            --  the private extension must be descended from each interface
            --  from which the record extension is descended (AARM 7.3, 20.1/2)

            if Has_Private_Declaration (Derived_Type) then
               declare
                  N_Partial : constant Node_Id := Parent (Tagged_Partial_View);
                  Iface_Partial : Entity_Id;

               begin
                  if Nkind (N_Partial) = N_Private_Extension_Declaration
                    and then not Is_Empty_List (Interface_List (N_Partial))
                  then
                     Iface_Partial := First (Interface_List (N_Partial));

                     while Present (Iface_Partial) loop
                        if not Interface_Present_In_Ancestor
                                 (Derived_Type, Etype (Iface_Partial))
                        then
                           Error_Msg_N
                             ("(Ada 2005) full type and private extension must"
                              & " have the same progenitors", Derived_Type);
                           exit;
                        end if;

                        Next (Iface_Partial);
                     end loop;
                  end if;
               end;
            end if;
         end if;

      else
         Set_Is_Packed (Derived_Type, Is_Packed (Parent_Base));
         Set_Has_Non_Standard_Rep
                       (Derived_Type, Has_Non_Standard_Rep (Parent_Base));
      end if;

      --  STEP 4: Inherit components from the parent base and constrain them.
      --          Apply the second transformation described in point 6. above.

      if (not Is_Empty_Elmt_List (Discs) or else Inherit_Discrims)
        or else not Has_Discriminants (Parent_Type)
        or else not Is_Constrained (Parent_Type)
      then
         Constrs := Discs;
      else
         Constrs := Discriminant_Constraint (Parent_Type);
      end if;

      Assoc_List := Inherit_Components (N,
        Parent_Base, Derived_Type, Is_Tagged, Inherit_Discrims, Constrs);

      --  STEP 5a: Copy the parent record declaration for untagged types

      if not Is_Tagged then

         --  Discriminant_Constraint (Derived_Type) has been properly
         --  constructed. Save it and temporarily set it to Empty because we
         --  do not want the call to New_Copy_Tree below to mess this list.

         if Has_Discriminants (Derived_Type) then
            Save_Discr_Constr := Discriminant_Constraint (Derived_Type);
            Set_Discriminant_Constraint (Derived_Type, No_Elist);
         else
            Save_Discr_Constr := No_Elist;
         end if;

         --  Save the Etype field of Derived_Type. It is correctly set now,
         --  but the call to New_Copy tree may remap it to point to itself,
         --  which is not what we want. Ditto for the Next_Entity field.

         Save_Etype       := Etype (Derived_Type);
         Save_Next_Entity := Next_Entity (Derived_Type);

         --  Assoc_List maps all stored discriminants in the Parent_Base to
         --  stored discriminants in the Derived_Type. It is fundamental that
         --  no types or itypes with discriminants other than the stored
         --  discriminants appear in the entities declared inside
         --  Derived_Type, since the back end cannot deal with it.

         New_Decl :=
           New_Copy_Tree
             (Parent (Parent_Base), Map => Assoc_List, New_Sloc => Loc);

         --  Restore the fields saved prior to the New_Copy_Tree call
         --  and compute the stored constraint.

         Set_Etype       (Derived_Type, Save_Etype);
         Set_Next_Entity (Derived_Type, Save_Next_Entity);

         if Has_Discriminants (Derived_Type) then
            Set_Discriminant_Constraint
              (Derived_Type, Save_Discr_Constr);
            Set_Stored_Constraint
              (Derived_Type, Expand_To_Stored_Constraint (Parent_Type, Discs));
            Replace_Components (Derived_Type, New_Decl);
         end if;

         --  Insert the new derived type declaration

         Rewrite (N, New_Decl);

      --  STEP 5b: Complete the processing for record extensions in generics

      --  There is no completion for record extensions declared in the
      --  parameter part of a generic, so we need to complete processing for
      --  these generic record extensions here. The Record_Type_Definition call
      --  will change the Ekind of the components from E_Void to E_Component.

      elsif Private_Extension and then Is_Generic_Type (Derived_Type) then
         Record_Type_Definition (Empty, Derived_Type);

      --  STEP 5c: Process the record extension for non private tagged types

      elsif not Private_Extension then

         --  Add the _parent field in the derived type

         Expand_Record_Extension (Derived_Type, Type_Def);

         --  Ada 2005 (AI-251): Addition of the Tag corresponding to all the
         --  implemented interfaces if we are in expansion mode

         if Expander_Active then
            Add_Interface_Tag_Components (N, Derived_Type);
         end if;

         --  Analyze the record extension

         Record_Type_Definition
           (Record_Extension_Part (Type_Def), Derived_Type);
      end if;

      End_Scope;

      if Etype (Derived_Type) = Any_Type then
         return;
      end if;

      --  Set delayed freeze and then derive subprograms, we need to do
      --  this in this order so that derived subprograms inherit the
      --  derived freeze if necessary.

      Set_Has_Delayed_Freeze (Derived_Type);

      if Derive_Subps then

         --  Ada 2005 (AI-251): Check if this tagged type implements abstract
         --  interfaces

         Has_Interfaces := False;

         if Is_Tagged_Type (Derived_Type) then
            declare
               E : Entity_Id;

            begin
               --  Handle private types

               if Present (Full_View (Derived_Type)) then
                  E := Full_View (Derived_Type);
               else
                  E := Derived_Type;
               end if;

               loop
                  if Is_Interface (E)
                    or else (Present (Abstract_Interfaces (E))
                               and then
                             not Is_Empty_Elmt_List (Abstract_Interfaces (E)))
                  then
                     Has_Interfaces := True;
                     exit;
                  end if;

                  exit when Etype (E) = E

                     --  Handle private types

                     or else (Present (Full_View (Etype (E)))
                               and then Full_View (Etype (E)) = E)

                     --  Protect the frontend against wrong source

                    or else Etype (E) = Derived_Type;

                  --  Climb to the ancestor type handling private types

                  if Present (Full_View (Etype (E))) then
                     E := Full_View (Etype (E));
                  else
                     E := Etype (E);
                  end if;
               end loop;
            end;
         end if;

         --  Ada 2005 (AI-251): Keep separate the management of tagged types
         --  implementing interfaces

         if not Is_Tagged_Type (Derived_Type)
           or else not Has_Interfaces
         then
            Derive_Subprograms (Parent_Type, Derived_Type);

         else
            --  Ada 2005 (AI-251): Complete the decoration of tagged private
            --  types that implement interfaces

            if Present (Tagged_Partial_View) then
               Derive_Subprograms
                 (Parent_Type, Derived_Type);

               Complete_Subprograms_Derivation
                 (Partial_View => Tagged_Partial_View,
                  Derived_Type => Derived_Type);

            --  Ada 2005 (AI-251): Derive the interface subprograms of all the
            --  implemented interfaces and check if some of the subprograms
            --  inherited from the ancestor cover some interface subprogram.

            else
               Derive_Subprograms (Parent_Type, Derived_Type);

               declare
                  Subp_Elmt         : Elmt_Id;
                  First_Iface_Elmt  : Elmt_Id;
                  Iface_Subp_Elmt   : Elmt_Id;
                  Subp              : Entity_Id;
                  Iface_Subp        : Entity_Id;
                  Is_Interface_Subp : Boolean;

               begin
                  --  Ada 2005 (AI-251): Remember the entity corresponding to
                  --  the last inherited primitive operation. This is required
                  --  to check if some of the inherited subprograms covers some
                  --  of the new interfaces.

                  Last_Inherited_Prim_Op := No_Elmt;

                  Subp_Elmt :=
                    First_Elmt (Primitive_Operations (Derived_Type));
                  while Present (Subp_Elmt) loop
                     Last_Inherited_Prim_Op := Subp_Elmt;
                     Next_Elmt (Subp_Elmt);
                  end loop;

                  --  Ada 2005 (AI-251): Derive subprograms in abstract
                  --  interfaces.

                  Derive_Interface_Subprograms (Derived_Type);

                  --  Ada 2005 (AI-251): Check if some of the inherited
                  --  subprograms cover some of the new interfaces.

                  if Present (Last_Inherited_Prim_Op) then
                     First_Iface_Elmt := Next_Elmt (Last_Inherited_Prim_Op);
                     Iface_Subp_Elmt  := First_Iface_Elmt;
                     while Present (Iface_Subp_Elmt) loop
                        Subp_Elmt := First_Elmt (Primitive_Operations
                                                  (Derived_Type));
                        while Subp_Elmt /= First_Iface_Elmt loop
                           Subp       := Node (Subp_Elmt);
                           Iface_Subp := Node (Iface_Subp_Elmt);

                           Is_Interface_Subp :=
                             Present (Alias (Subp))
                               and then Present (DTC_Entity (Alias (Subp)))
                               and then Is_Interface (Scope
                                                      (DTC_Entity
                                                       (Alias (Subp))));

                           if Chars (Subp) = Chars (Iface_Subp)
                             and then not Is_Interface_Subp
                             and then not Is_Abstract (Subp)
                             and then Type_Conformant (Iface_Subp, Subp)
                           then
                              Check_Dispatching_Operation
                                (Subp     => Subp,
                                 Old_Subp => Iface_Subp);

                              --  Traverse the list of aliased subprograms

                              declare
                                 E : Entity_Id;

                              begin
                                 E := Alias (Subp);
                                 while Present (Alias (E)) loop
                                    E := Alias (E);
                                 end loop;

                                 Set_Alias (Subp, E);
                              end;

                              Set_Has_Delayed_Freeze (Subp);
                              exit;
                           end if;

                           Next_Elmt (Subp_Elmt);
                        end loop;

                        Next_Elmt (Iface_Subp_Elmt);
                     end loop;
                  end if;
               end;
            end if;
         end if;
      end if;

      --  If we have a private extension which defines a constrained derived
      --  type mark as constrained here after we have derived subprograms. See
      --  comment on point 9. just above the body of Build_Derived_Record_Type.

      if Private_Extension and then Inherit_Discrims then
         if Constraint_Present and then not Is_Empty_Elmt_List (Discs) then
            Set_Is_Constrained          (Derived_Type, True);
            Set_Discriminant_Constraint (Derived_Type, Discs);

         elsif Is_Constrained (Parent_Type) then
            Set_Is_Constrained
              (Derived_Type, True);
            Set_Discriminant_Constraint
              (Derived_Type, Discriminant_Constraint (Parent_Type));
         end if;
      end if;

      --  Update the class_wide type, which shares the now-completed
      --  entity list with its specific type.

      if Is_Tagged then
         Set_First_Entity
           (Class_Wide_Type (Derived_Type), First_Entity (Derived_Type));
         Set_Last_Entity
           (Class_Wide_Type (Derived_Type), Last_Entity (Derived_Type));
      end if;

   end Build_Derived_Record_Type;

   ------------------------
   -- Build_Derived_Type --
   ------------------------

   procedure Build_Derived_Type
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id;
      Is_Completion : Boolean;
      Derive_Subps  : Boolean := True)
   is
      Parent_Base : constant Entity_Id := Base_Type (Parent_Type);

   begin
      --  Set common attributes

      Set_Scope          (Derived_Type, Current_Scope);

      Set_Ekind          (Derived_Type, Ekind     (Parent_Base));
      Set_Etype          (Derived_Type,            Parent_Base);
      Set_Has_Task       (Derived_Type, Has_Task  (Parent_Base));

      Set_Size_Info      (Derived_Type,                 Parent_Type);
      Set_RM_Size        (Derived_Type, RM_Size        (Parent_Type));
      Set_Convention     (Derived_Type, Convention     (Parent_Type));
      Set_Is_Controlled  (Derived_Type, Is_Controlled  (Parent_Type));

      --  The derived type inherits the representation clauses of the parent.
      --  However, for a private type that is completed by a derivation, there
      --  may be operation attributes that have been specified already (stream
      --  attributes and External_Tag) and those must be provided. Finally,
      --  if the partial view is a private extension, the representation items
      --  of the parent have been inherited already, and should not be chained
      --  twice to the derived type.

      if Is_Tagged_Type (Parent_Type)
        and then Present (First_Rep_Item (Derived_Type))
      then
         --  The existing items are either operational items or items inherited
         --  from a private extension declaration.

         declare
            Rep   : Node_Id;
            Found : Boolean := False;

         begin
            Rep := First_Rep_Item (Derived_Type);
            while Present (Rep) loop
               if Rep = First_Rep_Item (Parent_Type) then
                  Found := True;
                  exit;
               else
                  Rep := Next_Rep_Item (Rep);
               end if;
            end loop;

            if not Found then
               Set_Next_Rep_Item
                 (First_Rep_Item (Derived_Type), First_Rep_Item (Parent_Type));
            end if;
         end;

      else
         Set_First_Rep_Item (Derived_Type, First_Rep_Item (Parent_Type));
      end if;

      case Ekind (Parent_Type) is
         when Numeric_Kind =>
            Build_Derived_Numeric_Type (N, Parent_Type, Derived_Type);

         when Array_Kind =>
            Build_Derived_Array_Type (N, Parent_Type,  Derived_Type);

         when E_Record_Type
            | E_Record_Subtype
            | Class_Wide_Kind  =>
            Build_Derived_Record_Type
              (N, Parent_Type, Derived_Type, Derive_Subps);
            return;

         when Enumeration_Kind =>
            Build_Derived_Enumeration_Type (N, Parent_Type, Derived_Type);

         when Access_Kind =>
            Build_Derived_Access_Type (N, Parent_Type, Derived_Type);

         when Incomplete_Or_Private_Kind =>
            Build_Derived_Private_Type
              (N, Parent_Type, Derived_Type, Is_Completion, Derive_Subps);

            --  For discriminated types, the derivation includes deriving
            --  primitive operations. For others it is done below.

            if Is_Tagged_Type (Parent_Type)
              or else Has_Discriminants (Parent_Type)
              or else (Present (Full_View (Parent_Type))
                        and then Has_Discriminants (Full_View (Parent_Type)))
            then
               return;
            end if;

         when Concurrent_Kind =>
            Build_Derived_Concurrent_Type (N, Parent_Type, Derived_Type);

         when others =>
            raise Program_Error;
      end case;

      if Etype (Derived_Type) = Any_Type then
         return;
      end if;

      --  Set delayed freeze and then derive subprograms, we need to do this
      --  in this order so that derived subprograms inherit the derived freeze
      --  if necessary.

      Set_Has_Delayed_Freeze (Derived_Type);
      if Derive_Subps then
         Derive_Subprograms (Parent_Type, Derived_Type);
      end if;

      Set_Has_Primitive_Operations
        (Base_Type (Derived_Type), Has_Primitive_Operations (Parent_Type));
   end Build_Derived_Type;

   -----------------------
   -- Build_Discriminal --
   -----------------------

   procedure Build_Discriminal (Discrim : Entity_Id) is
      D_Minal : Entity_Id;
      CR_Disc : Entity_Id;

   begin
      --  A discriminal has the same name as the discriminant

      D_Minal :=
        Make_Defining_Identifier (Sloc (Discrim),
          Chars => Chars (Discrim));

      Set_Ekind     (D_Minal, E_In_Parameter);
      Set_Mechanism (D_Minal, Default_Mechanism);
      Set_Etype     (D_Minal, Etype (Discrim));

      Set_Discriminal (Discrim, D_Minal);
      Set_Discriminal_Link (D_Minal, Discrim);

      --  For task types, build at once the discriminants of the corresponding
      --  record, which are needed if discriminants are used in entry defaults
      --  and in family bounds.

      if Is_Concurrent_Type (Current_Scope)
        or else Is_Limited_Type (Current_Scope)
      then
         CR_Disc := Make_Defining_Identifier (Sloc (Discrim), Chars (Discrim));

         Set_Ekind            (CR_Disc, E_In_Parameter);
         Set_Mechanism        (CR_Disc, Default_Mechanism);
         Set_Etype            (CR_Disc, Etype (Discrim));
         Set_Discriminal_Link (CR_Disc, Discrim);
         Set_CR_Discriminant  (Discrim, CR_Disc);
      end if;
   end Build_Discriminal;

   ------------------------------------
   -- Build_Discriminant_Constraints --
   ------------------------------------

   function Build_Discriminant_Constraints
     (T           : Entity_Id;
      Def         : Node_Id;
      Derived_Def : Boolean := False) return Elist_Id
   is
      C        : constant Node_Id := Constraint (Def);
      Nb_Discr : constant Nat     := Number_Discriminants (T);

      Discr_Expr : array (1 .. Nb_Discr) of Node_Id := (others => Empty);
      --  Saves the expression corresponding to a given discriminant in T

      function Pos_Of_Discr (T : Entity_Id; D : Entity_Id) return Nat;
      --  Return the Position number within array Discr_Expr of a discriminant
      --  D within the discriminant list of the discriminated type T.

      ------------------
      -- Pos_Of_Discr --
      ------------------

      function Pos_Of_Discr (T : Entity_Id; D : Entity_Id) return Nat is
         Disc : Entity_Id;

      begin
         Disc := First_Discriminant (T);
         for J in Discr_Expr'Range loop
            if Disc = D then
               return J;
            end if;

            Next_Discriminant (Disc);
         end loop;

         --  Note: Since this function is called on discriminants that are
         --  known to belong to the discriminated type, falling through the
         --  loop with no match signals an internal compiler error.

         raise Program_Error;
      end Pos_Of_Discr;

      --  Declarations local to Build_Discriminant_Constraints

      Discr : Entity_Id;
      E     : Entity_Id;
      Elist : constant Elist_Id := New_Elmt_List;

      Constr   : Node_Id;
      Expr     : Node_Id;
      Id       : Node_Id;
      Position : Nat;
      Found    : Boolean;

      Discrim_Present : Boolean := False;

   --  Start of processing for Build_Discriminant_Constraints

   begin
      --  The following loop will process positional associations only.
      --  For a positional association, the (single) discriminant is
      --  implicitly specified by position, in textual order (RM 3.7.2).

      Discr  := First_Discriminant (T);
      Constr := First (Constraints (C));

      for D in Discr_Expr'Range loop
         exit when Nkind (Constr) = N_Discriminant_Association;

         if No (Constr) then
            Error_Msg_N ("too few discriminants given in constraint", C);
            return New_Elmt_List;

         elsif Nkind (Constr) = N_Range
           or else (Nkind (Constr) = N_Attribute_Reference
                     and then
                    Attribute_Name (Constr) = Name_Range)
         then
            Error_Msg_N
              ("a range is not a valid discriminant constraint", Constr);
            Discr_Expr (D) := Error;

         else
            Analyze_And_Resolve (Constr, Base_Type (Etype (Discr)));
            Discr_Expr (D) := Constr;
         end if;

         Next_Discriminant (Discr);
         Next (Constr);
      end loop;

      if No (Discr) and then Present (Constr) then
         Error_Msg_N ("too many discriminants given in constraint", Constr);
         return New_Elmt_List;
      end if;

      --  Named associations can be given in any order, but if both positional
      --  and named associations are used in the same discriminant constraint,
      --  then positional associations must occur first, at their normal
      --  position. Hence once a named association is used, the rest of the
      --  discriminant constraint must use only named associations.

      while Present (Constr) loop

         --  Positional association forbidden after a named association

         if Nkind (Constr) /= N_Discriminant_Association then
            Error_Msg_N ("positional association follows named one", Constr);
            return New_Elmt_List;

         --  Otherwise it is a named association

         else
            --  E records the type of the discriminants in the named
            --  association. All the discriminants specified in the same name
            --  association must have the same type.

            E := Empty;

            --  Search the list of discriminants in T to see if the simple name
            --  given in the constraint matches any of them.

            Id := First (Selector_Names (Constr));
            while Present (Id) loop
               Found := False;

               --  If Original_Discriminant is present, we are processing a
               --  generic instantiation and this is an instance node. We need
               --  to find the name of the corresponding discriminant in the
               --  actual record type T and not the name of the discriminant in
               --  the generic formal. Example:
               --
               --    generic
               --       type G (D : int) is private;
               --    package P is
               --       subtype W is G (D => 1);
               --    end package;
               --    type Rec (X : int) is record ... end record;
               --    package Q is new P (G => Rec);
               --
               --  At the point of the instantiation, formal type G is Rec
               --  and therefore when reanalyzing "subtype W is G (D => 1);"
               --  which really looks like "subtype W is Rec (D => 1);" at
               --  the point of instantiation, we want to find the discriminant
               --  that corresponds to D in Rec, ie X.

               if Present (Original_Discriminant (Id)) then
                  Discr := Find_Corresponding_Discriminant (Id, T);
                  Found := True;

               else
                  Discr := First_Discriminant (T);
                  while Present (Discr) loop
                     if Chars (Discr) = Chars (Id) then
                        Found := True;
                        exit;
                     end if;

                     Next_Discriminant (Discr);
                  end loop;

                  if not Found then
                     Error_Msg_N ("& does not match any discriminant", Id);
                     return New_Elmt_List;

                  --  The following is only useful for the benefit of generic
                  --  instances but it does not interfere with other
                  --  processing for the non-generic case so we do it in all
                  --  cases (for generics this statement is executed when
                  --  processing the generic definition, see comment at the
                  --  beginning of this if statement).

                  else
                     Set_Original_Discriminant (Id, Discr);
                  end if;
               end if;

               Position := Pos_Of_Discr (T, Discr);

               if Present (Discr_Expr (Position)) then
                  Error_Msg_N ("duplicate constraint for discriminant&", Id);

               else
                  --  Each discriminant specified in the same named association
                  --  must be associated with a separate copy of the
                  --  corresponding expression.

                  if Present (Next (Id)) then
                     Expr := New_Copy_Tree (Expression (Constr));
                     Set_Parent (Expr, Parent (Expression (Constr)));
                  else
                     Expr := Expression (Constr);
                  end if;

                  Discr_Expr (Position) := Expr;
                  Analyze_And_Resolve (Expr, Base_Type (Etype (Discr)));
               end if;

               --  A discriminant association with more than one discriminant
               --  name is only allowed if the named discriminants are all of
               --  the same type (RM 3.7.1(8)).

               if E = Empty then
                  E := Base_Type (Etype (Discr));

               elsif Base_Type (Etype (Discr)) /= E then
                  Error_Msg_N
                    ("all discriminants in an association " &
                     "must have the same type", Id);
               end if;

               Next (Id);
            end loop;
         end if;

         Next (Constr);
      end loop;

      --  A discriminant constraint must provide exactly one value for each
      --  discriminant of the type (RM 3.7.1(8)).

      for J in Discr_Expr'Range loop
         if No (Discr_Expr (J)) then
            Error_Msg_N ("too few discriminants given in constraint", C);
            return New_Elmt_List;
         end if;
      end loop;

      --  Determine if there are discriminant expressions in the constraint

      for J in Discr_Expr'Range loop
         if Denotes_Discriminant (Discr_Expr (J), Check_Protected => True) then
            Discrim_Present := True;
         end if;
      end loop;

      --  Build an element list consisting of the expressions given in the
      --  discriminant constraint and apply the appropriate checks. The list
      --  is constructed after resolving any named discriminant associations
      --  and therefore the expressions appear in the textual order of the
      --  discriminants.

      Discr := First_Discriminant (T);
      for J in Discr_Expr'Range loop
         if Discr_Expr (J) /= Error then

            Append_Elmt (Discr_Expr (J), Elist);

            --  If any of the discriminant constraints is given by a
            --  discriminant and we are in a derived type declaration we
            --  have a discriminant renaming. Establish link between new
            --  and old discriminant.

            if Denotes_Discriminant (Discr_Expr (J)) then
               if Derived_Def then
                  Set_Corresponding_Discriminant
                    (Entity (Discr_Expr (J)), Discr);
               end if;

            --  Force the evaluation of non-discriminant expressions.
            --  If we have found a discriminant in the constraint 3.4(26)
            --  and 3.8(18) demand that no range checks are performed are
            --  after evaluation. If the constraint is for a component
            --  definition that has a per-object constraint, expressions are
            --  evaluated but not checked either. In all other cases perform
            --  a range check.

            else
               if Discrim_Present then
                  null;

               elsif Nkind (Parent (Parent (Def))) = N_Component_Declaration
                 and then
                   Has_Per_Object_Constraint
                     (Defining_Identifier (Parent (Parent (Def))))
               then
                  null;

               elsif Is_Access_Type (Etype (Discr)) then
                  Apply_Constraint_Check (Discr_Expr (J), Etype (Discr));

               else
                  Apply_Range_Check (Discr_Expr (J), Etype (Discr));
               end if;

               Force_Evaluation (Discr_Expr (J));
            end if;

         --  Check that the designated type of an access discriminant's
         --  expression is not a class-wide type unless the discriminant's
         --  designated type is also class-wide.

            if Ekind (Etype (Discr)) = E_Anonymous_Access_Type
              and then not Is_Class_Wide_Type
                         (Designated_Type (Etype (Discr)))
              and then Etype (Discr_Expr (J)) /= Any_Type
              and then Is_Class_Wide_Type
                         (Designated_Type (Etype (Discr_Expr (J))))
            then
               Wrong_Type (Discr_Expr (J), Etype (Discr));
            end if;
         end if;

         Next_Discriminant (Discr);
      end loop;

      return Elist;
   end Build_Discriminant_Constraints;

   ---------------------------------
   -- Build_Discriminated_Subtype --
   ---------------------------------

   procedure Build_Discriminated_Subtype
     (T           : Entity_Id;
      Def_Id      : Entity_Id;
      Elist       : Elist_Id;
      Related_Nod : Node_Id;
      For_Access  : Boolean := False)
   is
      Has_Discrs  : constant Boolean := Has_Discriminants (T);
      Constrained : constant Boolean
                      := (Has_Discrs
                            and then not Is_Empty_Elmt_List (Elist)
                            and then not Is_Class_Wide_Type (T))
                           or else Is_Constrained (T);

   begin
      if Ekind (T) = E_Record_Type then
         if For_Access then
            Set_Ekind (Def_Id, E_Private_Subtype);
            Set_Is_For_Access_Subtype (Def_Id, True);
         else
            Set_Ekind (Def_Id, E_Record_Subtype);
         end if;

      elsif Ekind (T) = E_Task_Type then
         Set_Ekind (Def_Id, E_Task_Subtype);

      elsif Ekind (T) = E_Protected_Type then
         Set_Ekind (Def_Id, E_Protected_Subtype);

      elsif Is_Private_Type (T) then
         Set_Ekind (Def_Id, Subtype_Kind (Ekind (T)));

      elsif Is_Class_Wide_Type (T) then
         Set_Ekind (Def_Id, E_Class_Wide_Subtype);

      else
         --  Incomplete type.  attach subtype to list of dependents, to be
         --  completed with full view of parent type,  unless is it the
         --  designated subtype of a record component within an init_proc.
         --  This last case arises for a component of an access type whose
         --  designated type is incomplete (e.g. a Taft Amendment type).
         --  The designated subtype is within an inner scope, and needs no
         --  elaboration, because only the access type is needed in the
         --  initialization procedure.

         Set_Ekind (Def_Id, Ekind (T));

         if For_Access and then Within_Init_Proc then
            null;
         else
            Append_Elmt (Def_Id, Private_Dependents (T));
         end if;
      end if;

      Set_Etype             (Def_Id, T);
      Init_Size_Align       (Def_Id);
      Set_Has_Discriminants (Def_Id, Has_Discrs);
      Set_Is_Constrained    (Def_Id, Constrained);

      Set_First_Entity      (Def_Id, First_Entity   (T));
      Set_Last_Entity       (Def_Id, Last_Entity    (T));
      Set_First_Rep_Item    (Def_Id, First_Rep_Item (T));

      if Is_Tagged_Type (T) then
         Set_Is_Tagged_Type  (Def_Id);
         Make_Class_Wide_Type (Def_Id);
      end if;

      Set_Stored_Constraint (Def_Id, No_Elist);

      if Has_Discrs then
         Set_Discriminant_Constraint (Def_Id, Elist);
         Set_Stored_Constraint_From_Discriminant_Constraint (Def_Id);
      end if;

      if Is_Tagged_Type (T) then
         Set_Primitive_Operations (Def_Id, Primitive_Operations (T));
         Set_Is_Abstract (Def_Id, Is_Abstract (T));
      end if;

      --  Subtypes introduced by component declarations do not need to be
      --  marked as delayed, and do not get freeze nodes, because the semantics
      --  verifies that the parents of the subtypes are frozen before the
      --  enclosing record is frozen.

      if not Is_Type (Scope (Def_Id)) then
         Set_Depends_On_Private (Def_Id, Depends_On_Private (T));

         if Is_Private_Type (T)
           and then Present (Full_View (T))
         then
            Conditional_Delay (Def_Id, Full_View (T));
         else
            Conditional_Delay (Def_Id, T);
         end if;
      end if;

      if Is_Record_Type (T) then
         Set_Is_Limited_Record (Def_Id, Is_Limited_Record (T));

         if Has_Discrs
            and then not Is_Empty_Elmt_List (Elist)
            and then not For_Access
         then
            Create_Constrained_Components (Def_Id, Related_Nod, T, Elist);
         elsif not For_Access then
            Set_Cloned_Subtype (Def_Id, T);
         end if;
      end if;

   end Build_Discriminated_Subtype;

   ------------------------
   -- Build_Scalar_Bound --
   ------------------------

   function Build_Scalar_Bound
     (Bound : Node_Id;
      Par_T : Entity_Id;
      Der_T : Entity_Id) return Node_Id
   is
      New_Bound : Entity_Id;

   begin
      --  Note: not clear why this is needed, how can the original bound
      --  be unanalyzed at this point? and if it is, what business do we
      --  have messing around with it? and why is the base type of the
      --  parent type the right type for the resolution. It probably is
      --  not! It is OK for the new bound we are creating, but not for
      --  the old one??? Still if it never happens, no problem!

      Analyze_And_Resolve (Bound, Base_Type (Par_T));

      if Nkind (Bound) = N_Integer_Literal
        or else Nkind (Bound) = N_Real_Literal
      then
         New_Bound := New_Copy (Bound);
         Set_Etype (New_Bound, Der_T);
         Set_Analyzed (New_Bound);

      elsif Is_Entity_Name (Bound) then
         New_Bound := OK_Convert_To (Der_T, New_Copy (Bound));

      --  The following is almost certainly wrong. What business do we have
      --  relocating a node (Bound) that is presumably still attached to
      --  the tree elsewhere???

      else
         New_Bound := OK_Convert_To (Der_T, Relocate_Node (Bound));
      end if;

      Set_Etype (New_Bound, Der_T);
      return New_Bound;
   end Build_Scalar_Bound;

   --------------------------------
   -- Build_Underlying_Full_View --
   --------------------------------

   procedure Build_Underlying_Full_View
     (N   : Node_Id;
      Typ : Entity_Id;
      Par : Entity_Id)
   is
      Loc  : constant Source_Ptr := Sloc (N);
      Subt : constant Entity_Id :=
               Make_Defining_Identifier
                 (Loc, New_External_Name (Chars (Typ), 'S'));

      Constr : Node_Id;
      Indic  : Node_Id;
      C      : Node_Id;
      Id     : Node_Id;

      procedure Set_Discriminant_Name (Id : Node_Id);
      --  If the derived type has discriminants, they may rename discriminants
      --  of the parent. When building the full view of the parent, we need to
      --  recover the names of the original discriminants if the constraint is
      --  given by named associations.

      ---------------------------
      -- Set_Discriminant_Name --
      ---------------------------

      procedure Set_Discriminant_Name (Id : Node_Id) is
         Disc : Entity_Id;

      begin
         Set_Original_Discriminant (Id, Empty);

         if Has_Discriminants (Typ) then
            Disc := First_Discriminant (Typ);
            while Present (Disc) loop
               if Chars (Disc) = Chars (Id)
                 and then Present (Corresponding_Discriminant (Disc))
               then
                  Set_Chars (Id, Chars (Corresponding_Discriminant (Disc)));
               end if;
               Next_Discriminant (Disc);
            end loop;
         end if;
      end Set_Discriminant_Name;

   --  Start of processing for Build_Underlying_Full_View

   begin
      if Nkind (N) = N_Full_Type_Declaration then
         Constr := Constraint (Subtype_Indication (Type_Definition (N)));

      elsif Nkind (N) = N_Subtype_Declaration then
         Constr := New_Copy_Tree (Constraint (Subtype_Indication (N)));

      elsif Nkind (N) = N_Component_Declaration then
         Constr :=
           New_Copy_Tree
             (Constraint (Subtype_Indication (Component_Definition (N))));

      else
         raise Program_Error;
      end if;

      C := First (Constraints (Constr));
      while Present (C) loop
         if Nkind (C) = N_Discriminant_Association then
            Id := First (Selector_Names (C));
            while Present (Id) loop
               Set_Discriminant_Name (Id);
               Next (Id);
            end loop;
         end if;

         Next (C);
      end loop;

      Indic :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication  =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (Par, Loc),
              Constraint   => New_Copy_Tree (Constr)));

      --  If this is a component subtype for an outer itype, it is not
      --  a list member, so simply set the parent link for analysis: if
      --  the enclosing type does not need to be in a declarative list,
      --  neither do the components.

      if Is_List_Member (N)
        and then Nkind (N) /= N_Component_Declaration
      then
         Insert_Before (N, Indic);
      else
         Set_Parent (Indic, Parent (N));
      end if;

      Analyze (Indic);
      Set_Underlying_Full_View (Typ, Full_View (Subt));
   end Build_Underlying_Full_View;

   -------------------------------
   -- Check_Abstract_Overriding --
   -------------------------------

   procedure Check_Abstract_Overriding (T : Entity_Id) is
      Op_List  : Elist_Id;
      Elmt     : Elmt_Id;
      Subp     : Entity_Id;
      Type_Def : Node_Id;

   begin
      Op_List := Primitive_Operations (T);

      --  Loop to check primitive operations

      Elmt := First_Elmt (Op_List);
      while Present (Elmt) loop
         Subp := Node (Elmt);

         --  Special exception, do not complain about failure to override the
         --  stream routines _Input and _Output, as well as the primitive
         --  operations used in dispatching selects since we always provide
         --  automatic overridings for these subprograms.

         if Is_Abstract (Subp)
           and then not Is_TSS (Subp, TSS_Stream_Input)
           and then not Is_TSS (Subp, TSS_Stream_Output)
           and then not Is_Abstract (T)
           and then Chars (Subp) /= Name_uDisp_Asynchronous_Select
           and then Chars (Subp) /= Name_uDisp_Conditional_Select
           and then Chars (Subp) /= Name_uDisp_Get_Prim_Op_Kind
           and then Chars (Subp) /= Name_uDisp_Timed_Select
         then
            if Present (Alias (Subp)) then

               --  Only perform the check for a derived subprogram when
               --  the type has an explicit record extension. This avoids
               --  incorrectly flagging abstract subprograms for the case
               --  of a type without an extension derived from a formal type
               --  with a tagged actual (can occur within a private part).

               Type_Def := Type_Definition (Parent (T));
               if Nkind (Type_Def) = N_Derived_Type_Definition
                 and then Present (Record_Extension_Part (Type_Def))
               then
                  Error_Msg_NE
                    ("type must be declared abstract or & overridden",
                     T, Subp);

                  --  Traverse the whole chain of aliased subprograms to
                  --  complete the error notification. This is useful for
                  --  traceability of the chain of entities when the subprogram
                  --  corresponds with interface subprogram (that may be
                  --  defined in another package)

                  if Ada_Version >= Ada_05
                    and then Present (Alias (Subp))
                  then
                     declare
                        E : Entity_Id;

                     begin
                        E := Subp;
                        while Present (Alias (E)) loop
                           Error_Msg_Sloc := Sloc (E);
                           Error_Msg_NE ("\& has been inherited #", T, Subp);
                           E := Alias (E);
                        end loop;

                        Error_Msg_Sloc := Sloc (E);
                        Error_Msg_NE
                          ("\& has been inherited from subprogram #", T, Subp);
                     end;
                  end if;

               --  Ada 2005 (AI-345): Protected or task type implementing
               --  abstract interfaces.

               elsif Is_Concurrent_Record_Type (T)
                   and then Present (Abstract_Interfaces (T))
               then
                  Error_Msg_NE
                    ("interface subprogram & must be overridden",
                     T, Subp);
               end if;
            else
               Error_Msg_NE
                 ("abstract subprogram not allowed for type&",
                  Subp, T);
               Error_Msg_NE
                 ("nonabstract type has abstract subprogram&",
                  T, Subp);
            end if;
         end if;

         Next_Elmt (Elmt);
      end loop;
   end Check_Abstract_Overriding;

   ------------------------------------------------
   -- Check_Access_Discriminant_Requires_Limited --
   ------------------------------------------------

   procedure Check_Access_Discriminant_Requires_Limited
     (D   : Node_Id;
      Loc : Node_Id)
   is
   begin
      --  A discriminant_specification for an access discriminant shall appear
      --  only in the declaration for a task or protected type, or for a type
      --  with the reserved word 'limited' in its definition or in one of its
      --  ancestors. (RM 3.7(10))

      if Nkind (Discriminant_Type (D)) = N_Access_Definition
        and then not Is_Concurrent_Type (Current_Scope)
        and then not Is_Concurrent_Record_Type (Current_Scope)
        and then not Is_Limited_Record (Current_Scope)
        and then Ekind (Current_Scope) /= E_Limited_Private_Type
      then
         Error_Msg_N
           ("access discriminants allowed only for limited types", Loc);
      end if;
   end Check_Access_Discriminant_Requires_Limited;

   -----------------------------------
   -- Check_Aliased_Component_Types --
   -----------------------------------

   procedure Check_Aliased_Component_Types (T : Entity_Id) is
      C : Entity_Id;

   begin
      --  ??? Also need to check components of record extensions, but not
      --  components of protected types (which are always limited).

      --  Ada 2005: AI-363 relaxes this rule, to allow heap objects of such
      --  types to be unconstrained. This is safe because it is illegal to
      --  create access subtypes to such types with explicit discriminant
      --  constraints.

      if not Is_Limited_Type (T) then
         if Ekind (T) = E_Record_Type then
            C := First_Component (T);
            while Present (C) loop
               if Is_Aliased (C)
                 and then Has_Discriminants (Etype (C))
                 and then not Is_Constrained (Etype (C))
                 and then not In_Instance_Body
                 and then Ada_Version < Ada_05
               then
                  Error_Msg_N
                    ("aliased component must be constrained ('R'M 3.6(11))",
                      C);
               end if;

               Next_Component (C);
            end loop;

         elsif Ekind (T) = E_Array_Type then
            if Has_Aliased_Components (T)
              and then Has_Discriminants (Component_Type (T))
              and then not Is_Constrained (Component_Type (T))
              and then not In_Instance_Body
              and then Ada_Version < Ada_05
            then
               Error_Msg_N
                 ("aliased component type must be constrained ('R'M 3.6(11))",
                    T);
            end if;
         end if;
      end if;
   end Check_Aliased_Component_Types;

   ----------------------
   -- Check_Completion --
   ----------------------

   procedure Check_Completion (Body_Id : Node_Id := Empty) is
      E : Entity_Id;

      procedure Post_Error;
      --  Post error message for lack of completion for entity E

      ----------------
      -- Post_Error --
      ----------------

      procedure Post_Error is
      begin
         if not Comes_From_Source (E) then

            if Ekind (E) = E_Task_Type
              or else Ekind (E) = E_Protected_Type
            then
               --  It may be an anonymous protected type created for a
               --  single variable. Post error on variable, if present.

               declare
                  Var : Entity_Id;

               begin
                  Var := First_Entity (Current_Scope);
                  while Present (Var) loop
                     exit when Etype (Var) = E
                       and then Comes_From_Source (Var);

                     Next_Entity (Var);
                  end loop;

                  if Present (Var) then
                     E := Var;
                  end if;
               end;
            end if;
         end if;

         --  If a generated entity has no completion, then either previous
         --  semantic errors have disabled the expansion phase, or else we had
         --  missing subunits, or else we are compiling without expan- sion,
         --  or else something is very wrong.

         if not Comes_From_Source (E) then
            pragma Assert
              (Serious_Errors_Detected > 0
                or else Configurable_Run_Time_Violations > 0
                or else Subunits_Missing
                or else not Expander_Active);
            return;

         --  Here for source entity

         else
            --  Here if no body to post the error message, so we post the error
            --  on the declaration that has no completion. This is not really
            --  the right place to post it, think about this later ???

            if No (Body_Id) then
               if Is_Type (E) then
                  Error_Msg_NE
                    ("missing full declaration for }", Parent (E), E);
               else
                  Error_Msg_NE
                    ("missing body for &", Parent (E), E);
               end if;

            --  Package body has no completion for a declaration that appears
            --  in the corresponding spec. Post error on the body, with a
            --  reference to the non-completed declaration.

            else
               Error_Msg_Sloc := Sloc (E);

               if Is_Type (E) then
                  Error_Msg_NE
                    ("missing full declaration for }!", Body_Id, E);

               elsif Is_Overloadable (E)
                 and then Current_Entity_In_Scope (E) /= E
               then
                  --  It may be that the completion is mistyped and appears
                  --  as a  distinct overloading of the entity.

                  declare
                     Candidate : constant Entity_Id :=
                                   Current_Entity_In_Scope (E);
                     Decl      : constant Node_Id :=
                                   Unit_Declaration_Node (Candidate);

                  begin
                     if Is_Overloadable (Candidate)
                       and then Ekind (Candidate) = Ekind (E)
                       and then Nkind (Decl) = N_Subprogram_Body
                       and then Acts_As_Spec (Decl)
                     then
                        Check_Type_Conformant (Candidate, E);

                     else
                        Error_Msg_NE ("missing body for & declared#!",
                           Body_Id, E);
                     end if;
                  end;
               else
                  Error_Msg_NE ("missing body for & declared#!",
                     Body_Id, E);
               end if;
            end if;
         end if;
      end Post_Error;

   --  Start processing for Check_Completion

   begin
      E := First_Entity (Current_Scope);
      while Present (E) loop
         if Is_Intrinsic_Subprogram (E) then
            null;

         --  The following situation requires special handling: a child
         --  unit that appears in the context clause of the body of its
         --  parent:

         --    procedure Parent.Child (...);

         --    with Parent.Child;
         --    package body Parent is

         --  Here Parent.Child appears as a local entity, but should not
         --  be flagged as requiring completion, because it is a
         --  compilation unit.

         elsif     Ekind (E) = E_Function
           or else Ekind (E) = E_Procedure
           or else Ekind (E) = E_Generic_Function
           or else Ekind (E) = E_Generic_Procedure
         then
            if not Has_Completion (E)
              and then not Is_Abstract (E)
              and then Nkind (Parent (Unit_Declaration_Node (E))) /=
                                                       N_Compilation_Unit
              and then Chars (E) /= Name_uSize
            then
               Post_Error;
            end if;

         elsif Is_Entry (E) then
            if not Has_Completion (E) and then
              (Ekind (Scope (E)) = E_Protected_Object
                or else Ekind (Scope (E)) = E_Protected_Type)
            then
               Post_Error;
            end if;

         elsif Is_Package_Or_Generic_Package (E) then
            if Unit_Requires_Body (E) then
               if not Has_Completion (E)
                 and then Nkind (Parent (Unit_Declaration_Node (E))) /=
                                                       N_Compilation_Unit
               then
                  Post_Error;
               end if;

            elsif not Is_Child_Unit (E) then
               May_Need_Implicit_Body (E);
            end if;

         elsif Ekind (E) = E_Incomplete_Type
           and then No (Underlying_Type (E))
         then
            Post_Error;

         elsif (Ekind (E) = E_Task_Type or else
                Ekind (E) = E_Protected_Type)
           and then not Has_Completion (E)
         then
            Post_Error;

         --  A single task declared in the current scope is a constant, verify
         --  that the body of its anonymous type is in the same scope. If the
         --  task is defined elsewhere, this may be a renaming declaration for
         --  which no completion is needed.

         elsif Ekind (E) = E_Constant
           and then Ekind (Etype (E)) = E_Task_Type
           and then not Has_Completion (Etype (E))
           and then Scope (Etype (E)) = Current_Scope
         then
            Post_Error;

         elsif Ekind (E) = E_Protected_Object
           and then not Has_Completion (Etype (E))
         then
            Post_Error;

         elsif Ekind (E) = E_Record_Type then
            if Is_Tagged_Type (E) then
               Check_Abstract_Overriding (E);
            end if;

            Check_Aliased_Component_Types (E);

         elsif Ekind (E) = E_Array_Type then
            Check_Aliased_Component_Types (E);

         end if;

         Next_Entity (E);
      end loop;
   end Check_Completion;

   ----------------------------
   -- Check_Delta_Expression --
   ----------------------------

   procedure Check_Delta_Expression (E : Node_Id) is
   begin
      if not (Is_Real_Type (Etype (E))) then
         Wrong_Type (E, Any_Real);

      elsif not Is_OK_Static_Expression (E) then
         Flag_Non_Static_Expr
           ("non-static expression used for delta value!", E);

      elsif not UR_Is_Positive (Expr_Value_R (E)) then
         Error_Msg_N ("delta expression must be positive", E);

      else
         return;
      end if;

      --  If any of above errors occurred, then replace the incorrect
      --  expression by the real 0.1, which should prevent further errors.

      Rewrite (E,
        Make_Real_Literal (Sloc (E), Ureal_Tenth));
      Analyze_And_Resolve (E, Standard_Float);
   end Check_Delta_Expression;

   -----------------------------
   -- Check_Digits_Expression --
   -----------------------------

   procedure Check_Digits_Expression (E : Node_Id) is
   begin
      if not (Is_Integer_Type (Etype (E))) then
         Wrong_Type (E, Any_Integer);

      elsif not Is_OK_Static_Expression (E) then
         Flag_Non_Static_Expr
           ("non-static expression used for digits value!", E);

      elsif Expr_Value (E) <= 0 then
         Error_Msg_N ("digits value must be greater than zero", E);

      else
         return;
      end if;

      --  If any of above errors occurred, then replace the incorrect
      --  expression by the integer 1, which should prevent further errors.

      Rewrite (E, Make_Integer_Literal (Sloc (E), 1));
      Analyze_And_Resolve (E, Standard_Integer);

   end Check_Digits_Expression;

   --------------------------
   -- Check_Initialization --
   --------------------------

   procedure Check_Initialization (T : Entity_Id; Exp : Node_Id) is
   begin
      if (Is_Limited_Type (T)
           or else Is_Limited_Composite (T))
        and then not In_Instance
        and then not In_Inlined_Body
      then
         --  Ada 2005 (AI-287): Relax the strictness of the front-end in
         --  case of limited aggregates and extension aggregates.

         if Ada_Version >= Ada_05
           and then (Nkind (Exp) = N_Aggregate
                      or else Nkind (Exp) = N_Extension_Aggregate)
         then
            null;
         else
            Error_Msg_N
              ("cannot initialize entities of limited type", Exp);
            Explain_Limited_Type (T, Exp);
         end if;
      end if;
   end Check_Initialization;

   ------------------------------------
   -- Check_Or_Process_Discriminants --
   ------------------------------------

   --  If an incomplete or private type declaration was already given for the
   --  type, the discriminants may have already been processed if they were
   --  present on the incomplete declaration. In this case a full conformance
   --  check is performed otherwise just process them.

   procedure Check_Or_Process_Discriminants
     (N    : Node_Id;
      T    : Entity_Id;
      Prev : Entity_Id := Empty)
   is
   begin
      if Has_Discriminants (T) then

         --  Make the discriminants visible to component declarations

         declare
            D    : Entity_Id;
            Prev : Entity_Id;

         begin
            D := First_Discriminant (T);
            while Present (D) loop
               Prev := Current_Entity (D);
               Set_Current_Entity (D);
               Set_Is_Immediately_Visible (D);
               Set_Homonym (D, Prev);

               --  Ada 2005 (AI-230): Access discriminant allowed in
               --  non-limited record types.

               if Ada_Version < Ada_05 then

                  --  This restriction gets applied to the full type here. It
                  --  has already been applied earlier to the partial view.

                  Check_Access_Discriminant_Requires_Limited (Parent (D), N);
               end if;

               Next_Discriminant (D);
            end loop;
         end;

      elsif Present (Discriminant_Specifications (N)) then
         Process_Discriminants (N, Prev);
      end if;
   end Check_Or_Process_Discriminants;

   ----------------------
   -- Check_Real_Bound --
   ----------------------

   procedure Check_Real_Bound (Bound : Node_Id) is
   begin
      if not Is_Real_Type (Etype (Bound)) then
         Error_Msg_N
           ("bound in real type definition must be of real type", Bound);

      elsif not Is_OK_Static_Expression (Bound) then
         Flag_Non_Static_Expr
           ("non-static expression used for real type bound!", Bound);

      else
         return;
      end if;

      Rewrite
        (Bound, Make_Real_Literal (Sloc (Bound), Ureal_0));
      Analyze (Bound);
      Resolve (Bound, Standard_Float);
   end Check_Real_Bound;

   ------------------------
   -- Collect_Interfaces --
   ------------------------

   procedure Collect_Interfaces (N : Node_Id; Derived_Type : Entity_Id) is
      Intf : Node_Id;

      procedure Add_Interface (Iface : Entity_Id);
      --  Add one interface

      -------------------
      -- Add_Interface --
      -------------------

      procedure Add_Interface (Iface : Entity_Id) is
         Elmt : Elmt_Id;

      begin
         Elmt := First_Elmt (Abstract_Interfaces (Derived_Type));
         while Present (Elmt) and then Node (Elmt) /= Iface loop
            Next_Elmt (Elmt);
         end loop;

         if not Present (Elmt) then
            Append_Elmt (Node => Iface,
                         To   => Abstract_Interfaces (Derived_Type));
         end if;
      end Add_Interface;

   --  Start of processing for Add_Interface

   begin
      pragma Assert (False
         or else Nkind (N) = N_Derived_Type_Definition
         or else Nkind (N) = N_Record_Definition
         or else Nkind (N) = N_Private_Extension_Declaration);

      --  Traverse the graph of ancestor interfaces

      if Is_Non_Empty_List (Interface_List (N)) then
         Intf := First (Interface_List (N));
         while Present (Intf) loop

            --  Protect against wrong uses. For example:
            --    type I is interface;
            --    type O is tagged null record;
            --    type Wrong is new I and O with null record; -- ERROR

            if Is_Interface (Etype (Intf)) then

               --  Do not add the interface when the derived type already
               --  implements this interface

               if not Interface_Present_In_Ancestor (Derived_Type,
                                                     Etype (Intf))
               then
                  Collect_Interfaces
                     (Type_Definition (Parent (Etype (Intf))),
                      Derived_Type);
                  Add_Interface (Etype (Intf));
               end if;
            end if;

            Next (Intf);
         end loop;

      --  A type extension may be written as a derivation from an interface.
      --  The completion will have to implement the same, or derive from a
      --  type that implements it as well.

      elsif Nkind (N) = N_Private_Extension_Declaration
        and then Is_Interface (Etype (Derived_Type))
      then
         Add_Interface (Etype (Derived_Type));
      end if;

      --  Same for task and protected types, that can derive directly from
      --  an interface (and implement additional interfaces that will be
      --  present in the interface list of the declaration).

      if Nkind (N) = N_Task_Type_Declaration
        or else Nkind (N) = N_Protected_Type_Declaration
        or else Nkind (N) = N_Single_Protected_Declaration
        or else Nkind (N) = N_Single_Task_Declaration
      then
         if Is_Interface (Etype (Derived_Type)) then
            Add_Interface (Etype (Derived_Type));
         end if;
      end if;
   end Collect_Interfaces;

   ------------------------------
   -- Complete_Private_Subtype --
   ------------------------------

   procedure Complete_Private_Subtype
     (Priv        : Entity_Id;
      Full        : Entity_Id;
      Full_Base   : Entity_Id;
      Related_Nod : Node_Id)
   is
      Save_Next_Entity : Entity_Id;
      Save_Homonym     : Entity_Id;

   begin
      --  Set semantic attributes for (implicit) private subtype completion.
      --  If the full type has no discriminants, then it is a copy of the full
      --  view of the base. Otherwise, it is a subtype of the base with a
      --  possible discriminant constraint. Save and restore the original
      --  Next_Entity field of full to ensure that the calls to Copy_Node
      --  do not corrupt the entity chain.

      --  Note that the type of the full view is the same entity as the type of
      --  the partial view. In this fashion, the subtype has access to the
      --  correct view of the parent.

      Save_Next_Entity := Next_Entity (Full);
      Save_Homonym     := Homonym (Priv);

      case Ekind (Full_Base) is
         when E_Record_Type    |
              E_Record_Subtype |
              Class_Wide_Kind  |
              Private_Kind     |
              Task_Kind        |
              Protected_Kind   =>
            Copy_Node (Priv, Full);

            Set_Has_Discriminants  (Full, Has_Discriminants (Full_Base));
            Set_First_Entity       (Full, First_Entity (Full_Base));
            Set_Last_Entity        (Full, Last_Entity (Full_Base));

         when others =>
            Copy_Node (Full_Base, Full);
            Set_Chars          (Full, Chars (Priv));
            Conditional_Delay  (Full, Priv);
            Set_Sloc           (Full, Sloc (Priv));
      end case;

      Set_Next_Entity (Full, Save_Next_Entity);
      Set_Homonym     (Full, Save_Homonym);
      Set_Associated_Node_For_Itype (Full, Related_Nod);

      --  Set common attributes for all subtypes

      Set_Ekind (Full, Subtype_Kind (Ekind (Full_Base)));

      --  The Etype of the full view is inconsistent. Gigi needs to see the
      --  structural full view,  which is what the current scheme gives:
      --  the Etype of the full view is the etype of the full base. However,
      --  if the full base is a derived type, the full view then looks like
      --  a subtype of the parent, not a subtype of the full base. If instead
      --  we write:

      --       Set_Etype (Full, Full_Base);

      --  then we get inconsistencies in the front-end (confusion between
      --  views). Several outstanding bugs are related to this ???

      Set_Is_First_Subtype (Full, False);
      Set_Scope            (Full, Scope (Priv));
      Set_Size_Info        (Full, Full_Base);
      Set_RM_Size          (Full, RM_Size (Full_Base));
      Set_Is_Itype         (Full);

      --  A subtype of a private-type-without-discriminants, whose full-view
      --  has discriminants with default expressions, is not constrained!

      if not Has_Discriminants (Priv) then
         Set_Is_Constrained (Full, Is_Constrained (Full_Base));

         if Has_Discriminants (Full_Base) then
            Set_Discriminant_Constraint
              (Full, Discriminant_Constraint (Full_Base));

            --  The partial view may have been indefinite, the full view
            --  might not be.

            Set_Has_Unknown_Discriminants
              (Full, Has_Unknown_Discriminants (Full_Base));
         end if;
      end if;

      Set_First_Rep_Item     (Full, First_Rep_Item (Full_Base));
      Set_Depends_On_Private (Full, Has_Private_Component (Full));

      --  Freeze the private subtype entity if its parent is delayed, and not
      --  already frozen. We skip this processing if the type is an anonymous
      --  subtype of a record component, or is the corresponding record of a
      --  protected type, since ???

      if not Is_Type (Scope (Full)) then
         Set_Has_Delayed_Freeze (Full,
           Has_Delayed_Freeze (Full_Base)
             and then (not Is_Frozen (Full_Base)));
      end if;

      Set_Freeze_Node (Full, Empty);
      Set_Is_Frozen (Full, False);
      Set_Full_View (Priv, Full);

      if Has_Discriminants (Full) then
         Set_Stored_Constraint_From_Discriminant_Constraint (Full);
         Set_Stored_Constraint (Priv, Stored_Constraint (Full));

         if Has_Unknown_Discriminants (Full) then
            Set_Discriminant_Constraint (Full, No_Elist);
         end if;
      end if;

      if Ekind (Full_Base) = E_Record_Type
        and then Has_Discriminants (Full_Base)
        and then Has_Discriminants (Priv) -- might not, if errors
        and then not Has_Unknown_Discriminants (Priv)
        and then not Is_Empty_Elmt_List (Discriminant_Constraint (Priv))
      then
         Create_Constrained_Components
           (Full, Related_Nod, Full_Base, Discriminant_Constraint (Priv));

      --  If the full base is itself derived from private, build a congruent
      --  subtype of its underlying type, for use by the back end. For a
      --  constrained record component, the declaration cannot be placed on
      --  the component list, but it must nevertheless be built an analyzed, to
      --  supply enough information for Gigi to compute the size of component.

      elsif Ekind (Full_Base) in Private_Kind
        and then Is_Derived_Type (Full_Base)
        and then Has_Discriminants (Full_Base)
        and then (Ekind (Current_Scope) /= E_Record_Subtype)
      then
         if not Is_Itype (Priv)
           and then
             Nkind (Subtype_Indication (Parent (Priv))) = N_Subtype_Indication
         then
            Build_Underlying_Full_View
              (Parent (Priv), Full, Etype (Full_Base));

         elsif Nkind (Related_Nod) = N_Component_Declaration then
            Build_Underlying_Full_View (Related_Nod, Full, Etype (Full_Base));
         end if;

      elsif Is_Record_Type (Full_Base) then

         --  Show Full is simply a renaming of Full_Base

         Set_Cloned_Subtype (Full, Full_Base);
      end if;

      --  It is unsafe to share to bounds of a scalar type, because the Itype
      --  is elaborated on demand, and if a bound is non-static then different
      --  orders of elaboration in different units will lead to different
      --  external symbols.

      if Is_Scalar_Type (Full_Base) then
         Set_Scalar_Range (Full,
           Make_Range (Sloc (Related_Nod),
             Low_Bound  =>
               Duplicate_Subexpr_No_Checks (Type_Low_Bound  (Full_Base)),
             High_Bound =>
               Duplicate_Subexpr_No_Checks (Type_High_Bound (Full_Base))));

         --  This completion inherits the bounds of the full parent, but if
         --  the parent is an unconstrained floating point type, so is the
         --  completion.

         if Is_Floating_Point_Type (Full_Base) then
            Set_Includes_Infinities
             (Scalar_Range (Full), Has_Infinities (Full_Base));
         end if;
      end if;

      --  ??? It seems that a lot of fields are missing that should be copied
      --  from Full_Base to Full. Here are some that are introduced in a
      --  non-disruptive way but a cleanup is necessary.

      if Is_Tagged_Type (Full_Base) then
         Set_Is_Tagged_Type (Full);
         Set_Primitive_Operations (Full, Primitive_Operations (Full_Base));
         Set_Class_Wide_Type      (Full, Class_Wide_Type (Full_Base));

      --  If this is a subtype of a protected or task type, constrain its
      --  corresponding record, unless this is a subtype without constraints,
      --  i.e. a simple renaming as with an actual subtype in an instance.

      elsif Is_Concurrent_Type (Full_Base) then
         if Has_Discriminants (Full)
           and then Present (Corresponding_Record_Type (Full_Base))
           and then
             not Is_Empty_Elmt_List (Discriminant_Constraint (Full))
         then
            Set_Corresponding_Record_Type (Full,
              Constrain_Corresponding_Record
                (Full, Corresponding_Record_Type (Full_Base),
                  Related_Nod, Full_Base));

         else
            Set_Corresponding_Record_Type (Full,
              Corresponding_Record_Type (Full_Base));
         end if;
      end if;
   end Complete_Private_Subtype;

   -------------------------------------
   -- Complete_Subprograms_Derivation --
   -------------------------------------

   procedure Complete_Subprograms_Derivation
     (Partial_View : Entity_Id;
      Derived_Type : Entity_Id)
   is
      Result  : constant Elist_Id := New_Elmt_List;
      Elmt_P  : Elmt_Id;
      Elmt_D  : Elmt_Id;
      Found   : Boolean;
      Prim_Op : Entity_Id;
      E       : Entity_Id;

   begin
      --  Handle the case in which the full-view is a transitive
      --  derivation of the ancestor of the partial view.

      --   type I is interface;
      --   type T is new I with ...

      --   package H is
      --      type DT is new I with private;
      --   private
      --      type DT is new T with ...
      --   end;

      if Etype (Partial_View) /= Etype (Derived_Type)
        and then Is_Interface (Etype (Partial_View))
        and then Is_Ancestor (Etype (Partial_View), Etype (Derived_Type))
      then
         return;
      end if;

      if Is_Tagged_Type (Partial_View) then
         Elmt_P := First_Elmt (Primitive_Operations (Partial_View));
      else
         Elmt_P := No_Elmt;
      end if;

      --  Inherit primitives declared with the partial-view

      while Present (Elmt_P) loop
         Prim_Op := Node (Elmt_P);
         Found   := False;
         Elmt_D  := First_Elmt (Primitive_Operations (Derived_Type));
         while Present (Elmt_D) loop
            if Node (Elmt_D) = Prim_Op then
               Found := True;
               exit;
            end if;

            Next_Elmt (Elmt_D);
         end loop;

         if not Found then
            Append_Elmt (Prim_Op, Result);

            --  Search for entries associated with abstract interfaces that
            --  have been covered by this primitive

            Elmt_D := First_Elmt (Primitive_Operations (Derived_Type));
            while Present (Elmt_D) loop
               E := Node (Elmt_D);

               if Chars (E) = Chars (Prim_Op)
                 and then Is_Abstract (E)
                 and then Present (Alias (E))
                 and then Present (DTC_Entity (Alias (E)))
                 and then Is_Interface (Scope (DTC_Entity (Alias (E))))
               then
                  Remove_Elmt (Primitive_Operations (Derived_Type), Elmt_D);
               end if;

               Next_Elmt (Elmt_D);
            end loop;
         end if;

         Next_Elmt (Elmt_P);
      end loop;

      --  Append the entities of the full-view to the list of primitives
      --  of derived_type.

      Elmt_D := First_Elmt (Result);
      while Present (Elmt_D) loop
         Append_Elmt (Node (Elmt_D), Primitive_Operations (Derived_Type));
         Next_Elmt (Elmt_D);
      end loop;
   end Complete_Subprograms_Derivation;

   ----------------------------
   -- Constant_Redeclaration --
   ----------------------------

   procedure Constant_Redeclaration
     (Id : Entity_Id;
      N  : Node_Id;
      T  : out Entity_Id)
   is
      Prev    : constant Entity_Id := Current_Entity_In_Scope (Id);
      Obj_Def : constant Node_Id := Object_Definition (N);
      New_T   : Entity_Id;

      procedure Check_Recursive_Declaration (Typ : Entity_Id);
      --  If deferred constant is an access type initialized with an allocator,
      --  check whether there is an illegal recursion in the definition,
      --  through a default value of some record subcomponent. This is normally
      --  detected when generating init procs, but requires this additional
      --  mechanism when expansion is disabled.

      ---------------------------------
      -- Check_Recursive_Declaration --
      ---------------------------------

      procedure Check_Recursive_Declaration (Typ : Entity_Id) is
         Comp : Entity_Id;

      begin
         if Is_Record_Type (Typ) then
            Comp := First_Component (Typ);
            while Present (Comp) loop
               if Comes_From_Source (Comp) then
                  if Present (Expression (Parent (Comp)))
                    and then Is_Entity_Name (Expression (Parent (Comp)))
                    and then Entity (Expression (Parent (Comp))) = Prev
                  then
                     Error_Msg_Sloc := Sloc (Parent (Comp));
                     Error_Msg_NE
                       ("illegal circularity with declaration for&#",
                         N, Comp);
                     return;

                  elsif Is_Record_Type (Etype (Comp)) then
                     Check_Recursive_Declaration (Etype (Comp));
                  end if;
               end if;

               Next_Component (Comp);
            end loop;
         end if;
      end Check_Recursive_Declaration;

   --  Start of processing for Constant_Redeclaration

   begin
      if Nkind (Parent (Prev)) = N_Object_Declaration then
         if Nkind (Object_Definition
                     (Parent (Prev))) = N_Subtype_Indication
         then
            --  Find type of new declaration. The constraints of the two
            --  views must match statically, but there is no point in
            --  creating an itype for the full view.

            if Nkind (Obj_Def) = N_Subtype_Indication then
               Find_Type (Subtype_Mark (Obj_Def));
               New_T := Entity (Subtype_Mark (Obj_Def));

            else
               Find_Type (Obj_Def);
               New_T := Entity (Obj_Def);
            end if;

            T := Etype (Prev);

         else
            --  The full view may impose a constraint, even if the partial
            --  view does not, so construct the subtype.

            New_T := Find_Type_Of_Object (Obj_Def, N);
            T     := New_T;
         end if;

      else
         --  Current declaration is illegal, diagnosed below in Enter_Name

         T := Empty;
         New_T := Any_Type;
      end if;

      --  If previous full declaration exists, or if a homograph is present,
      --  let Enter_Name handle it, either with an error, or with the removal
      --  of an overridden implicit subprogram.

      if Ekind (Prev) /= E_Constant
        or else Present (Expression (Parent (Prev)))
        or else Present (Full_View (Prev))
      then
         Enter_Name (Id);

      --  Verify that types of both declarations match, or else that both types
      --  are anonymous access types whose designated subtypes statically match
      --  (as allowed in Ada 2005 by AI-385).

      elsif Base_Type (Etype (Prev)) /= Base_Type (New_T)
        and then
          (Ekind (Etype (Prev)) /= E_Anonymous_Access_Type
             or else Ekind (Etype (New_T)) /= E_Anonymous_Access_Type
             or else not Subtypes_Statically_Match
                           (Designated_Type (Etype (Prev)),
                            Designated_Type (Etype (New_T))))
      then
         Error_Msg_Sloc := Sloc (Prev);
         Error_Msg_N ("type does not match declaration#", N);
         Set_Full_View (Prev, Id);
         Set_Etype (Id, Any_Type);

      --  If so, process the full constant declaration

      else
         Set_Full_View (Prev, Id);
         Set_Is_Public (Id, Is_Public (Prev));
         Set_Is_Internal (Id);
         Append_Entity (Id, Current_Scope);

         --  Check ALIASED present if present before (RM 7.4(7))

         if Is_Aliased (Prev)
           and then not Aliased_Present (N)
         then
            Error_Msg_Sloc := Sloc (Prev);
            Error_Msg_N ("ALIASED required (see declaration#)", N);
         end if;

         --  Check that placement is in private part and that the incomplete
         --  declaration appeared in the visible part.

         if Ekind (Current_Scope) = E_Package
           and then not In_Private_Part (Current_Scope)
         then
            Error_Msg_Sloc := Sloc (Prev);
            Error_Msg_N ("full constant for declaration#"
                         & " must be in private part", N);

         elsif Ekind (Current_Scope) = E_Package
           and then List_Containing (Parent (Prev))
           /= Visible_Declarations
             (Specification (Unit_Declaration_Node (Current_Scope)))
         then
            Error_Msg_N
              ("deferred constant must be declared in visible part",
                 Parent (Prev));
         end if;

         if Is_Access_Type (T)
           and then Nkind (Expression (N)) = N_Allocator
         then
            Check_Recursive_Declaration (Designated_Type (T));
         end if;
      end if;
   end Constant_Redeclaration;

   ----------------------
   -- Constrain_Access --
   ----------------------

   procedure Constrain_Access
     (Def_Id      : in out Entity_Id;
      S           : Node_Id;
      Related_Nod : Node_Id)
   is
      T             : constant Entity_Id := Entity (Subtype_Mark (S));
      Desig_Type    : constant Entity_Id := Designated_Type (T);
      Desig_Subtype : Entity_Id := Create_Itype (E_Void, Related_Nod);
      Constraint_OK : Boolean := True;

      function Has_Defaulted_Discriminants (Typ : Entity_Id) return Boolean;
      --  Simple predicate to test for defaulted discriminants
      --  Shouldn't this be in sem_util???

      ---------------------------------
      -- Has_Defaulted_Discriminants --
      ---------------------------------

      function Has_Defaulted_Discriminants (Typ : Entity_Id) return Boolean is
      begin
         return Has_Discriminants (Typ)
          and then Present (First_Discriminant (Typ))
          and then Present
            (Discriminant_Default_Value (First_Discriminant (Typ)));
      end Has_Defaulted_Discriminants;

   --  Start of processing for Constrain_Access

   begin
      if Is_Array_Type (Desig_Type) then
         Constrain_Array (Desig_Subtype, S, Related_Nod, Def_Id, 'P');

      elsif (Is_Record_Type (Desig_Type)
              or else Is_Incomplete_Or_Private_Type (Desig_Type))
        and then not Is_Constrained (Desig_Type)
      then
         --  ??? The following code is a temporary kludge to ignore a
         --  discriminant constraint on access type if it is constraining
         --  the current record. Avoid creating the implicit subtype of the
         --  record we are currently compiling since right now, we cannot
         --  handle these. For now, just return the access type itself.

         if Desig_Type = Current_Scope
           and then No (Def_Id)
         then
            Set_Ekind (Desig_Subtype, E_Record_Subtype);
            Def_Id := Entity (Subtype_Mark (S));

            --  This call added to ensure that the constraint is analyzed
            --  (needed for a B test). Note that we still return early from
            --  this procedure to avoid recursive processing. ???

            Constrain_Discriminated_Type
              (Desig_Subtype, S, Related_Nod, For_Access => True);
            return;
         end if;

         if Ekind (T) = E_General_Access_Type
           and then Has_Private_Declaration (Desig_Type)
           and then In_Open_Scopes (Scope (Desig_Type))
         then
            --  Enforce rule that the constraint is illegal if there is
            --  an unconstrained view of the designated type. This means
            --  that the partial view (either a private type declaration or
            --  a derivation from a private type) has no discriminants.
            --  (Defect Report 8652/0008, Technical Corrigendum 1, checked
            --  by ACATS B371001).
            --  Rule updated for Ada 2005: the private type is said to have
            --  a constrained partial view, given that objects of the type
            --  can be declared.

            declare
               Pack  : constant Node_Id :=
                         Unit_Declaration_Node (Scope (Desig_Type));
               Decls : List_Id;
               Decl  : Node_Id;

            begin
               if Nkind (Pack) = N_Package_Declaration then
                  Decls := Visible_Declarations (Specification (Pack));
                  Decl := First (Decls);
                  while Present (Decl) loop
                     if (Nkind (Decl) = N_Private_Type_Declaration
                          and then
                            Chars (Defining_Identifier (Decl)) =
                                                     Chars (Desig_Type))

                       or else
                        (Nkind (Decl) = N_Full_Type_Declaration
                          and then
                            Chars (Defining_Identifier (Decl)) =
                                                     Chars (Desig_Type)
                          and then Is_Derived_Type (Desig_Type)
                          and then
                            Has_Private_Declaration (Etype (Desig_Type)))
                     then
                        if No (Discriminant_Specifications (Decl)) then
                           Error_Msg_N
                            ("cannot constrain general access type if " &
                               "designated type has constrained partial view",
                                S);
                        end if;

                        exit;
                     end if;

                     Next (Decl);
                  end loop;
               end if;
            end;
         end if;

         Constrain_Discriminated_Type (Desig_Subtype, S, Related_Nod,
           For_Access => True);

      elsif (Is_Task_Type (Desig_Type)
              or else Is_Protected_Type (Desig_Type))
        and then not Is_Constrained (Desig_Type)
      then
         Constrain_Concurrent
           (Desig_Subtype, S, Related_Nod, Desig_Type, ' ');

      else
         Error_Msg_N ("invalid constraint on access type", S);
         Desig_Subtype := Desig_Type; -- Ignore invalid constraint.
         Constraint_OK := False;
      end if;

      if No (Def_Id) then
         Def_Id := Create_Itype (E_Access_Subtype, Related_Nod);
      else
         Set_Ekind (Def_Id, E_Access_Subtype);
      end if;

      if Constraint_OK then
         Set_Etype (Def_Id, Base_Type (T));

         if Is_Private_Type (Desig_Type) then
            Prepare_Private_Subtype_Completion (Desig_Subtype, Related_Nod);
         end if;
      else
         Set_Etype (Def_Id, Any_Type);
      end if;

      Set_Size_Info                (Def_Id, T);
      Set_Is_Constrained           (Def_Id, Constraint_OK);
      Set_Directly_Designated_Type (Def_Id, Desig_Subtype);
      Set_Depends_On_Private       (Def_Id, Has_Private_Component (Def_Id));
      Set_Is_Access_Constant       (Def_Id, Is_Access_Constant (T));

      Conditional_Delay (Def_Id, T);

      --  AI-363 : Subtypes of general access types whose designated types have
      --  default discriminants are disallowed. In instances, the rule has to
      --  be checked against the actual, of which T is the subtype. In a
      --  generic body, the rule is checked assuming that the actual type has
      --  defaulted discriminants.

      if Ada_Version >=  Ada_05 then
         if Ekind (Base_Type (T)) = E_General_Access_Type
           and then Has_Defaulted_Discriminants (Desig_Type)
         then
            Error_Msg_N
              ("access subype of general access type not allowed", S);
            Error_Msg_N ("\ when discriminants have defaults", S);

         elsif Is_Access_Type (T)
           and then Is_Generic_Type (Desig_Type)
           and then Has_Discriminants (Desig_Type)
           and then In_Package_Body (Current_Scope)
         then
            Error_Msg_N ("access subtype not allowed in generic body", S);
            Error_Msg_N
              ("\ wben designated type is a discriminated formal", S);
         end if;
      end if;
   end Constrain_Access;

   ---------------------
   -- Constrain_Array --
   ---------------------

   procedure Constrain_Array
     (Def_Id      : in out Entity_Id;
      SI          : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id;
      Suffix      : Character)
   is
      C                     : constant Node_Id := Constraint (SI);
      Number_Of_Constraints : Nat := 0;
      Index                 : Node_Id;
      S, T                  : Entity_Id;
      Constraint_OK         : Boolean := True;

   begin
      T := Entity (Subtype_Mark (SI));

      if Ekind (T) in Access_Kind then
         T := Designated_Type (T);
      end if;

      --  If an index constraint follows a subtype mark in a subtype indication
      --  then the type or subtype denoted by the subtype mark must not already
      --  impose an index constraint. The subtype mark must denote either an
      --  unconstrained array type or an access type whose designated type
      --  is such an array type... (RM 3.6.1)

      if Is_Constrained (T) then
         Error_Msg_N
           ("array type is already constrained", Subtype_Mark (SI));
         Constraint_OK := False;

      else
         S := First (Constraints (C));
         while Present (S) loop
            Number_Of_Constraints := Number_Of_Constraints + 1;
            Next (S);
         end loop;

         --  In either case, the index constraint must provide a discrete
         --  range for each index of the array type and the type of each
         --  discrete range must be the same as that of the corresponding
         --  index. (RM 3.6.1)

         if Number_Of_Constraints /= Number_Dimensions (T) then
            Error_Msg_NE ("incorrect number of index constraints for }", C, T);
            Constraint_OK := False;

         else
            S := First (Constraints (C));
            Index := First_Index (T);
            Analyze (Index);

            --  Apply constraints to each index type

            for J in 1 .. Number_Of_Constraints loop
               Constrain_Index (Index, S, Related_Nod, Related_Id, Suffix, J);
               Next (Index);
               Next (S);
            end loop;

         end if;
      end if;

      if No (Def_Id) then
         Def_Id :=
           Create_Itype (E_Array_Subtype, Related_Nod, Related_Id, Suffix);
         Set_Parent (Def_Id, Related_Nod);

      else
         Set_Ekind (Def_Id, E_Array_Subtype);
      end if;

      Set_Size_Info      (Def_Id,                (T));
      Set_First_Rep_Item (Def_Id, First_Rep_Item (T));
      Set_Etype          (Def_Id, Base_Type      (T));

      if Constraint_OK then
         Set_First_Index (Def_Id, First (Constraints (C)));
      else
         Set_First_Index (Def_Id, First_Index (T));
      end if;

      Set_Is_Constrained     (Def_Id, True);
      Set_Is_Aliased         (Def_Id, Is_Aliased (T));
      Set_Depends_On_Private (Def_Id, Has_Private_Component (Def_Id));

      Set_Is_Private_Composite (Def_Id, Is_Private_Composite (T));
      Set_Is_Limited_Composite (Def_Id, Is_Limited_Composite (T));

      --  Build a freeze node if parent still needs one.  Also, make sure
      --  that the Depends_On_Private status is set (explanation ???)
      --  and also that a conditional delay is set.

      Set_Depends_On_Private (Def_Id, Depends_On_Private (T));
      Conditional_Delay (Def_Id, T);

   end Constrain_Array;

   ------------------------------
   -- Constrain_Component_Type --
   ------------------------------

   function Constrain_Component_Type
     (Comp            : Entity_Id;
      Constrained_Typ : Entity_Id;
      Related_Node    : Node_Id;
      Typ             : Entity_Id;
      Constraints     : Elist_Id) return Entity_Id
   is
      Loc         : constant Source_Ptr := Sloc (Constrained_Typ);
      Compon_Type : constant Entity_Id := Etype (Comp);

      function Build_Constrained_Array_Type
        (Old_Type : Entity_Id) return Entity_Id;
      --  If Old_Type is an array type, one of whose indices is constrained
      --  by a discriminant, build an Itype whose constraint replaces the
      --  discriminant with its value in the constraint.

      function Build_Constrained_Discriminated_Type
        (Old_Type : Entity_Id) return Entity_Id;
      --  Ditto for record components

      function Build_Constrained_Access_Type
        (Old_Type : Entity_Id) return Entity_Id;
      --  Ditto for access types. Makes use of previous two functions, to
      --  constrain designated type.

      function Build_Subtype (T : Entity_Id; C : List_Id) return Entity_Id;
      --  T is an array or discriminated type, C is a list of constraints
      --  that apply to T. This routine builds the constrained subtype.

      function Is_Discriminant (Expr : Node_Id) return Boolean;
      --  Returns True if Expr is a discriminant

      function Get_Discr_Value (Discrim : Entity_Id) return Node_Id;
      --  Find the value of discriminant Discrim in Constraint

      -----------------------------------
      -- Build_Constrained_Access_Type --
      -----------------------------------

      function Build_Constrained_Access_Type
        (Old_Type : Entity_Id) return Entity_Id
      is
         Desig_Type    : constant Entity_Id := Designated_Type (Old_Type);
         Itype         : Entity_Id;
         Desig_Subtype : Entity_Id;
         Scop          : Entity_Id;

      begin
         --  if the original access type was not embedded in the enclosing
         --  type definition, there is no need to produce a new access
         --  subtype. In fact every access type with an explicit constraint
         --  generates an itype whose scope is the enclosing record.

         if not Is_Type (Scope (Old_Type)) then
            return Old_Type;

         elsif Is_Array_Type (Desig_Type) then
            Desig_Subtype := Build_Constrained_Array_Type (Desig_Type);

         elsif Has_Discriminants (Desig_Type) then

            --  This may be an access type to an enclosing record type for
            --  which we are constructing the constrained components. Return
            --  the enclosing record subtype. This is not always correct,
            --  but avoids infinite recursion. ???

            Desig_Subtype := Any_Type;

            for J in reverse 0 .. Scope_Stack.Last loop
               Scop := Scope_Stack.Table (J).Entity;

               if Is_Type (Scop)
                 and then Base_Type (Scop) = Base_Type (Desig_Type)
               then
                  Desig_Subtype := Scop;
               end if;

               exit when not Is_Type (Scop);
            end loop;

            if Desig_Subtype = Any_Type then
               Desig_Subtype :=
                 Build_Constrained_Discriminated_Type (Desig_Type);
            end if;

         else
            return Old_Type;
         end if;

         if Desig_Subtype /= Desig_Type then

            --  The Related_Node better be here or else we won't be able
            --  to attach new itypes to a node in the tree.

            pragma Assert (Present (Related_Node));

            Itype := Create_Itype (E_Access_Subtype, Related_Node);

            Set_Etype                    (Itype, Base_Type      (Old_Type));
            Set_Size_Info                (Itype,                (Old_Type));
            Set_Directly_Designated_Type (Itype, Desig_Subtype);
            Set_Depends_On_Private       (Itype, Has_Private_Component
                                                                (Old_Type));
            Set_Is_Access_Constant       (Itype, Is_Access_Constant
                                                                (Old_Type));

            --  The new itype needs freezing when it depends on a not frozen
            --  type and the enclosing subtype needs freezing.

            if Has_Delayed_Freeze (Constrained_Typ)
              and then not Is_Frozen (Constrained_Typ)
            then
               Conditional_Delay (Itype, Base_Type (Old_Type));
            end if;

            return Itype;

         else
            return Old_Type;
         end if;
      end Build_Constrained_Access_Type;

      ----------------------------------
      -- Build_Constrained_Array_Type --
      ----------------------------------

      function Build_Constrained_Array_Type
        (Old_Type : Entity_Id) return Entity_Id
      is
         Lo_Expr     : Node_Id;
         Hi_Expr     : Node_Id;
         Old_Index   : Node_Id;
         Range_Node  : Node_Id;
         Constr_List : List_Id;

         Need_To_Create_Itype : Boolean := False;

      begin
         Old_Index := First_Index (Old_Type);
         while Present (Old_Index) loop
            Get_Index_Bounds (Old_Index, Lo_Expr, Hi_Expr);

            if Is_Discriminant (Lo_Expr)
              or else Is_Discriminant (Hi_Expr)
            then
               Need_To_Create_Itype := True;
            end if;

            Next_Index (Old_Index);
         end loop;

         if Need_To_Create_Itype then
            Constr_List := New_List;

            Old_Index := First_Index (Old_Type);
            while Present (Old_Index) loop
               Get_Index_Bounds (Old_Index, Lo_Expr, Hi_Expr);

               if Is_Discriminant (Lo_Expr) then
                  Lo_Expr := Get_Discr_Value (Lo_Expr);
               end if;

               if Is_Discriminant (Hi_Expr) then
                  Hi_Expr := Get_Discr_Value (Hi_Expr);
               end if;

               Range_Node :=
                 Make_Range
                   (Loc, New_Copy_Tree (Lo_Expr), New_Copy_Tree (Hi_Expr));

               Append (Range_Node, To => Constr_List);

               Next_Index (Old_Index);
            end loop;

            return Build_Subtype (Old_Type, Constr_List);

         else
            return Old_Type;
         end if;
      end Build_Constrained_Array_Type;

      ------------------------------------------
      -- Build_Constrained_Discriminated_Type --
      ------------------------------------------

      function Build_Constrained_Discriminated_Type
        (Old_Type : Entity_Id) return Entity_Id
      is
         Expr           : Node_Id;
         Constr_List    : List_Id;
         Old_Constraint : Elmt_Id;

         Need_To_Create_Itype : Boolean := False;

      begin
         Old_Constraint := First_Elmt (Discriminant_Constraint (Old_Type));
         while Present (Old_Constraint) loop
            Expr := Node (Old_Constraint);

            if Is_Discriminant (Expr) then
               Need_To_Create_Itype := True;
            end if;

            Next_Elmt (Old_Constraint);
         end loop;

         if Need_To_Create_Itype then
            Constr_List := New_List;

            Old_Constraint := First_Elmt (Discriminant_Constraint (Old_Type));
            while Present (Old_Constraint) loop
               Expr := Node (Old_Constraint);

               if Is_Discriminant (Expr) then
                  Expr := Get_Discr_Value (Expr);
               end if;

               Append (New_Copy_Tree (Expr), To => Constr_List);

               Next_Elmt (Old_Constraint);
            end loop;

            return Build_Subtype (Old_Type, Constr_List);

         else
            return Old_Type;
         end if;
      end Build_Constrained_Discriminated_Type;

      -------------------
      -- Build_Subtype --
      -------------------

      function Build_Subtype (T : Entity_Id; C : List_Id) return Entity_Id is
         Indic       : Node_Id;
         Subtyp_Decl : Node_Id;
         Def_Id      : Entity_Id;
         Btyp        : Entity_Id := Base_Type (T);

      begin
         --  The Related_Node better be here or else we won't be able to
         --  attach new itypes to a node in the tree.

         pragma Assert (Present (Related_Node));

         --  If the view of the component's type is incomplete or private
         --  with unknown discriminants, then the constraint must be applied
         --  to the full type.

         if Has_Unknown_Discriminants (Btyp)
           and then Present (Underlying_Type (Btyp))
         then
            Btyp := Underlying_Type (Btyp);
         end if;

         Indic :=
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Occurrence_Of (Btyp, Loc),
             Constraint   => Make_Index_Or_Discriminant_Constraint (Loc, C));

         Def_Id := Create_Itype (Ekind (T), Related_Node);

         Subtyp_Decl :=
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Subtype_Indication  => Indic);

         Set_Parent (Subtyp_Decl, Parent (Related_Node));

         --  Itypes must be analyzed with checks off (see package Itypes)

         Analyze (Subtyp_Decl, Suppress => All_Checks);

         return Def_Id;
      end Build_Subtype;

      ---------------------
      -- Get_Discr_Value --
      ---------------------

      function Get_Discr_Value (Discrim : Entity_Id) return Node_Id is
         D : Entity_Id;
         E : Elmt_Id;
         G : Elmt_Id;

      begin
         --  The discriminant may be declared for the type, in which case we
         --  find it by iterating over the list of discriminants. If the
         --  discriminant is inherited from a parent type, it appears as the
         --  corresponding discriminant of the current type. This will be the
         --  case when constraining an inherited component whose constraint is
         --  given by a discriminant of the parent.

         D := First_Discriminant (Typ);
         E := First_Elmt (Constraints);
         while Present (D) loop
            if D = Entity (Discrim)
              or else Corresponding_Discriminant (D) = Entity (Discrim)
            then
               return Node (E);
            end if;

            Next_Discriminant (D);
            Next_Elmt (E);
         end loop;

         --  The corresponding_Discriminant mechanism is incomplete, because
         --  the correspondence between new and old discriminants is not one
         --  to one: one new discriminant can constrain several old ones. In
         --  that case, scan sequentially the stored_constraint, the list of
         --  discriminants of the parents, and the constraints.

         if Is_Derived_Type (Typ)
           and then Present (Stored_Constraint (Typ))
           and then Scope (Entity (Discrim)) = Etype (Typ)
         then
            D := First_Discriminant (Etype (Typ));
            E := First_Elmt (Constraints);
            G := First_Elmt (Stored_Constraint (Typ));
            while Present (D) loop
               if D = Entity (Discrim) then
                  return Node (E);
               end if;

               Next_Discriminant (D);
               Next_Elmt (E);
               Next_Elmt (G);
            end loop;
         end if;

         --  Something is wrong if we did not find the value

         raise Program_Error;
      end Get_Discr_Value;

      ---------------------
      -- Is_Discriminant --
      ---------------------

      function Is_Discriminant (Expr : Node_Id) return Boolean is
         Discrim_Scope : Entity_Id;

      begin
         if Denotes_Discriminant (Expr) then
            Discrim_Scope := Scope (Entity (Expr));

            --  Either we have a reference to one of Typ's discriminants,

            pragma Assert (Discrim_Scope = Typ

               --  or to the discriminants of the parent type, in the case
               --  of a derivation of a tagged type with variants.

               or else Discrim_Scope = Etype (Typ)
               or else Full_View (Discrim_Scope) = Etype (Typ)

               --  or same as above for the case where the discriminants
               --  were declared in Typ's private view.

               or else (Is_Private_Type (Discrim_Scope)
                        and then Chars (Discrim_Scope) = Chars (Typ))

               --  or else we are deriving from the full view and the
               --  discriminant is declared in the private entity.

               or else (Is_Private_Type (Typ)
                        and then Chars (Discrim_Scope) = Chars (Typ))

               --  or we have a class-wide type, in which case make sure the
               --  discriminant found belongs to the root type.

               or else (Is_Class_Wide_Type (Typ)
                        and then Etype (Typ) = Discrim_Scope));

            return True;
         end if;

         --  In all other cases we have something wrong

         return False;
      end Is_Discriminant;

   --  Start of processing for Constrain_Component_Type

   begin
      if Nkind (Parent (Comp)) = N_Component_Declaration
        and then Comes_From_Source (Parent (Comp))
        and then Comes_From_Source
          (Subtype_Indication (Component_Definition (Parent (Comp))))
        and then
          Is_Entity_Name
            (Subtype_Indication (Component_Definition (Parent (Comp))))
      then
         return Compon_Type;

      elsif Is_Array_Type (Compon_Type) then
         return Build_Constrained_Array_Type (Compon_Type);

      elsif Has_Discriminants (Compon_Type) then
         return Build_Constrained_Discriminated_Type (Compon_Type);

      elsif Is_Access_Type (Compon_Type) then
         return Build_Constrained_Access_Type (Compon_Type);

      else
         return Compon_Type;
      end if;
   end Constrain_Component_Type;

   --------------------------
   -- Constrain_Concurrent --
   --------------------------

   --  For concurrent types, the associated record value type carries the same
   --  discriminants, so when we constrain a concurrent type, we must constrain
   --  the corresponding record type as well.

   procedure Constrain_Concurrent
     (Def_Id      : in out Entity_Id;
      SI          : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id;
      Suffix      : Character)
   is
      T_Ent : Entity_Id := Entity (Subtype_Mark (SI));
      T_Val : Entity_Id;

   begin
      if Ekind (T_Ent) in Access_Kind then
         T_Ent := Designated_Type (T_Ent);
      end if;

      T_Val := Corresponding_Record_Type (T_Ent);

      if Present (T_Val) then

         if No (Def_Id) then
            Def_Id := Create_Itype (E_Void, Related_Nod, Related_Id, Suffix);
         end if;

         Constrain_Discriminated_Type (Def_Id, SI, Related_Nod);

         Set_Depends_On_Private (Def_Id, Has_Private_Component (Def_Id));
         Set_Corresponding_Record_Type (Def_Id,
           Constrain_Corresponding_Record
             (Def_Id, T_Val, Related_Nod, Related_Id));

      else
         --  If there is no associated record, expansion is disabled and this
         --  is a generic context. Create a subtype in any case, so that
         --  semantic analysis can proceed.

         if No (Def_Id) then
            Def_Id := Create_Itype (E_Void, Related_Nod, Related_Id, Suffix);
         end if;

         Constrain_Discriminated_Type (Def_Id, SI, Related_Nod);
      end if;
   end Constrain_Concurrent;

   ------------------------------------
   -- Constrain_Corresponding_Record --
   ------------------------------------

   function Constrain_Corresponding_Record
     (Prot_Subt   : Entity_Id;
      Corr_Rec    : Entity_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id) return Entity_Id
   is
      T_Sub : constant Entity_Id :=
                Create_Itype (E_Record_Subtype, Related_Nod, Related_Id, 'V');

   begin
      Set_Etype             (T_Sub, Corr_Rec);
      Init_Size_Align       (T_Sub);
      Set_Has_Discriminants (T_Sub, Has_Discriminants (Prot_Subt));
      Set_Is_Constrained    (T_Sub, True);
      Set_First_Entity      (T_Sub, First_Entity (Corr_Rec));
      Set_Last_Entity       (T_Sub, Last_Entity  (Corr_Rec));

      Conditional_Delay (T_Sub, Corr_Rec);

      if Has_Discriminants (Prot_Subt) then -- False only if errors.
         Set_Discriminant_Constraint
           (T_Sub, Discriminant_Constraint (Prot_Subt));
         Set_Stored_Constraint_From_Discriminant_Constraint (T_Sub);
         Create_Constrained_Components
           (T_Sub, Related_Nod, Corr_Rec, Discriminant_Constraint (T_Sub));
      end if;

      Set_Depends_On_Private      (T_Sub, Has_Private_Component (T_Sub));

      return T_Sub;
   end Constrain_Corresponding_Record;

   -----------------------
   -- Constrain_Decimal --
   -----------------------

   procedure Constrain_Decimal (Def_Id : Node_Id; S : Node_Id) is
      T           : constant Entity_Id  := Entity (Subtype_Mark (S));
      C           : constant Node_Id    := Constraint (S);
      Loc         : constant Source_Ptr := Sloc (C);
      Range_Expr  : Node_Id;
      Digits_Expr : Node_Id;
      Digits_Val  : Uint;
      Bound_Val   : Ureal;

   begin
      Set_Ekind (Def_Id, E_Decimal_Fixed_Point_Subtype);

      if Nkind (C) = N_Range_Constraint then
         Range_Expr := Range_Expression (C);
         Digits_Val := Digits_Value (T);

      else
         pragma Assert (Nkind (C) = N_Digits_Constraint);
         Digits_Expr := Digits_Expression (C);
         Analyze_And_Resolve (Digits_Expr, Any_Integer);

         Check_Digits_Expression (Digits_Expr);
         Digits_Val := Expr_Value (Digits_Expr);

         if Digits_Val > Digits_Value (T) then
            Error_Msg_N
               ("digits expression is incompatible with subtype", C);
            Digits_Val := Digits_Value (T);
         end if;

         if Present (Range_Constraint (C)) then
            Range_Expr := Range_Expression (Range_Constraint (C));
         else
            Range_Expr := Empty;
         end if;
      end if;

      Set_Etype            (Def_Id, Base_Type        (T));
      Set_Size_Info        (Def_Id,                  (T));
      Set_First_Rep_Item   (Def_Id, First_Rep_Item   (T));
      Set_Delta_Value      (Def_Id, Delta_Value      (T));
      Set_Scale_Value      (Def_Id, Scale_Value      (T));
      Set_Small_Value      (Def_Id, Small_Value      (T));
      Set_Machine_Radix_10 (Def_Id, Machine_Radix_10 (T));
      Set_Digits_Value     (Def_Id, Digits_Val);

      --  Manufacture range from given digits value if no range present

      if No (Range_Expr) then
         Bound_Val := (Ureal_10 ** Digits_Val - Ureal_1) * Small_Value (T);
         Range_Expr :=
           Make_Range (Loc,
             Low_Bound =>
               Convert_To (T, Make_Real_Literal (Loc, (-Bound_Val))),
             High_Bound =>
               Convert_To (T, Make_Real_Literal (Loc, Bound_Val)));
      end if;

      Set_Scalar_Range_For_Subtype (Def_Id, Range_Expr, T);
      Set_Discrete_RM_Size (Def_Id);

      --  Unconditionally delay the freeze, since we cannot set size
      --  information in all cases correctly until the freeze point.

      Set_Has_Delayed_Freeze (Def_Id);
   end Constrain_Decimal;

   ----------------------------------
   -- Constrain_Discriminated_Type --
   ----------------------------------

   procedure Constrain_Discriminated_Type
     (Def_Id      : Entity_Id;
      S           : Node_Id;
      Related_Nod : Node_Id;
      For_Access  : Boolean := False)
   is
      E     : constant Entity_Id := Entity (Subtype_Mark (S));
      T     : Entity_Id;
      C     : Node_Id;
      Elist : Elist_Id := New_Elmt_List;

      procedure Fixup_Bad_Constraint;
      --  This is called after finding a bad constraint, and after having
      --  posted an appropriate error message. The mission is to leave the
      --  entity T in as reasonable state as possible!

      --------------------------
      -- Fixup_Bad_Constraint --
      --------------------------

      procedure Fixup_Bad_Constraint is
      begin
         --  Set a reasonable Ekind for the entity. For an incomplete type,
         --  we can't do much, but for other types, we can set the proper
         --  corresponding subtype kind.

         if Ekind (T) = E_Incomplete_Type then
            Set_Ekind (Def_Id, Ekind (T));
         else
            Set_Ekind (Def_Id, Subtype_Kind (Ekind (T)));
         end if;

         Set_Etype (Def_Id, Any_Type);
         Set_Error_Posted (Def_Id);
      end Fixup_Bad_Constraint;

   --  Start of processing for Constrain_Discriminated_Type

   begin
      C := Constraint (S);

      --  A discriminant constraint is only allowed in a subtype indication,
      --  after a subtype mark. This subtype mark must denote either a type
      --  with discriminants, or an access type whose designated type is a
      --  type with discriminants. A discriminant constraint specifies the
      --  values of these discriminants (RM 3.7.2(5)).

      T := Base_Type (Entity (Subtype_Mark (S)));

      if Ekind (T) in Access_Kind then
         T := Designated_Type (T);
      end if;

      --  Check that the type has visible discriminants. The type may be
      --  a private type with unknown discriminants whose full view has
      --  discriminants which are invisible.

      if not Has_Discriminants (T)
        or else
          (Has_Unknown_Discriminants (T)
             and then Is_Private_Type (T))
      then
         Error_Msg_N ("invalid constraint: type has no discriminant", C);
         Fixup_Bad_Constraint;
         return;

      elsif Is_Constrained (E)
        or else (Ekind (E) = E_Class_Wide_Subtype
                  and then Present (Discriminant_Constraint (E)))
      then
         Error_Msg_N ("type is already constrained", Subtype_Mark (S));
         Fixup_Bad_Constraint;
         return;
      end if;

      --  T may be an unconstrained subtype (e.g. a generic actual).
      --  Constraint applies to the base type.

      T := Base_Type (T);

      Elist := Build_Discriminant_Constraints (T, S);

      --  If the list returned was empty we had an error in building the
      --  discriminant constraint. We have also already signalled an error
      --  in the incomplete type case

      if Is_Empty_Elmt_List (Elist) then
         Fixup_Bad_Constraint;
         return;
      end if;

      Build_Discriminated_Subtype (T, Def_Id, Elist, Related_Nod, For_Access);
   end Constrain_Discriminated_Type;

   ---------------------------
   -- Constrain_Enumeration --
   ---------------------------

   procedure Constrain_Enumeration (Def_Id : Node_Id; S : Node_Id) is
      T : constant Entity_Id := Entity (Subtype_Mark (S));
      C : constant Node_Id   := Constraint (S);

   begin
      Set_Ekind (Def_Id, E_Enumeration_Subtype);

      Set_First_Literal     (Def_Id, First_Literal (Base_Type (T)));

      Set_Etype             (Def_Id, Base_Type         (T));
      Set_Size_Info         (Def_Id,                   (T));
      Set_First_Rep_Item    (Def_Id, First_Rep_Item    (T));
      Set_Is_Character_Type (Def_Id, Is_Character_Type (T));

      Set_Scalar_Range_For_Subtype (Def_Id, Range_Expression (C), T);

      Set_Discrete_RM_Size (Def_Id);
   end Constrain_Enumeration;

   ----------------------
   -- Constrain_Float --
   ----------------------

   procedure Constrain_Float (Def_Id : Node_Id; S : Node_Id) is
      T    : constant Entity_Id := Entity (Subtype_Mark (S));
      C    : Node_Id;
      D    : Node_Id;
      Rais : Node_Id;

   begin
      Set_Ekind (Def_Id, E_Floating_Point_Subtype);

      Set_Etype          (Def_Id, Base_Type      (T));
      Set_Size_Info      (Def_Id,                (T));
      Set_First_Rep_Item (Def_Id, First_Rep_Item (T));

      --  Process the constraint

      C := Constraint (S);

      --  Digits constraint present

      if Nkind (C) = N_Digits_Constraint then
         Check_Restriction (No_Obsolescent_Features, C);

         if Warn_On_Obsolescent_Feature then
            Error_Msg_N
              ("subtype digits constraint is an " &
               "obsolescent feature ('R'M 'J.3(8))?", C);
         end if;

         D := Digits_Expression (C);
         Analyze_And_Resolve (D, Any_Integer);
         Check_Digits_Expression (D);
         Set_Digits_Value (Def_Id, Expr_Value (D));

         --  Check that digits value is in range. Obviously we can do this
         --  at compile time, but it is strictly a runtime check, and of
         --  course there is an ACVC test that checks this!

         if Digits_Value (Def_Id) > Digits_Value (T) then
            Error_Msg_Uint_1 := Digits_Value (T);
            Error_Msg_N ("?digits value is too large, maximum is ^", D);
            Rais :=
              Make_Raise_Constraint_Error (Sloc (D),
                Reason => CE_Range_Check_Failed);
            Insert_Action (Declaration_Node (Def_Id), Rais);
         end if;

         C := Range_Constraint (C);

      --  No digits constraint present

      else
         Set_Digits_Value (Def_Id, Digits_Value (T));
      end if;

      --  Range constraint present

      if Nkind (C) = N_Range_Constraint then
         Set_Scalar_Range_For_Subtype (Def_Id, Range_Expression (C), T);

      --  No range constraint present

      else
         pragma Assert (No (C));
         Set_Scalar_Range (Def_Id, Scalar_Range (T));
      end if;

      Set_Is_Constrained (Def_Id);
   end Constrain_Float;

   ---------------------
   -- Constrain_Index --
   ---------------------

   procedure Constrain_Index
     (Index        : Node_Id;
      S            : Node_Id;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat)
   is
      Def_Id : Entity_Id;
      R      : Node_Id := Empty;
      T      : constant Entity_Id := Etype (Index);

   begin
      if Nkind (S) = N_Range
        or else
          (Nkind (S) = N_Attribute_Reference
            and then Attribute_Name (S) = Name_Range)
      then
         --  A Range attribute will transformed into N_Range by Resolve

         Analyze (S);
         Set_Etype (S, T);
         R := S;

         Process_Range_Expr_In_Decl (R, T, Empty_List);

         if not Error_Posted (S)
           and then
             (Nkind (S) /= N_Range
               or else not Covers (T, (Etype (Low_Bound (S))))
               or else not Covers (T, (Etype (High_Bound (S)))))
         then
            if Base_Type (T) /= Any_Type
              and then Etype (Low_Bound (S)) /= Any_Type
              and then Etype (High_Bound (S)) /= Any_Type
            then
               Error_Msg_N ("range expected", S);
            end if;
         end if;

      elsif Nkind (S) = N_Subtype_Indication then

         --  The parser has verified that this is a discrete indication

         Resolve_Discrete_Subtype_Indication (S, T);
         R := Range_Expression (Constraint (S));

      elsif Nkind (S) = N_Discriminant_Association then

         --  Syntactically valid in subtype indication

         Error_Msg_N ("invalid index constraint", S);
         Rewrite (S, New_Occurrence_Of (T, Sloc (S)));
         return;

      --  Subtype_Mark case, no anonymous subtypes to construct

      else
         Analyze (S);

         if Is_Entity_Name (S) then
            if not Is_Type (Entity (S)) then
               Error_Msg_N ("expect subtype mark for index constraint", S);

            elsif Base_Type (Entity (S)) /= Base_Type (T) then
               Wrong_Type (S, Base_Type (T));
            end if;

            return;

         else
            Error_Msg_N ("invalid index constraint", S);
            Rewrite (S, New_Occurrence_Of (T, Sloc (S)));
            return;
         end if;
      end if;

      Def_Id :=
        Create_Itype (E_Void, Related_Nod, Related_Id, Suffix, Suffix_Index);

      Set_Etype (Def_Id, Base_Type (T));

      if Is_Modular_Integer_Type (T) then
         Set_Ekind (Def_Id, E_Modular_Integer_Subtype);

      elsif Is_Integer_Type (T) then
         Set_Ekind (Def_Id, E_Signed_Integer_Subtype);

      else
         Set_Ekind (Def_Id, E_Enumeration_Subtype);
         Set_Is_Character_Type (Def_Id, Is_Character_Type (T));
      end if;

      Set_Size_Info      (Def_Id,                (T));
      Set_RM_Size        (Def_Id, RM_Size        (T));
      Set_First_Rep_Item (Def_Id, First_Rep_Item (T));

      Set_Scalar_Range   (Def_Id, R);

      Set_Etype (S, Def_Id);
      Set_Discrete_RM_Size (Def_Id);
   end Constrain_Index;

   -----------------------
   -- Constrain_Integer --
   -----------------------

   procedure Constrain_Integer (Def_Id : Node_Id; S : Node_Id) is
      T : constant Entity_Id := Entity (Subtype_Mark (S));
      C : constant Node_Id   := Constraint (S);

   begin
      Set_Scalar_Range_For_Subtype (Def_Id, Range_Expression (C), T);

      if Is_Modular_Integer_Type (T) then
         Set_Ekind (Def_Id, E_Modular_Integer_Subtype);
      else
         Set_Ekind (Def_Id, E_Signed_Integer_Subtype);
      end if;

      Set_Etype            (Def_Id, Base_Type        (T));
      Set_Size_Info        (Def_Id,                  (T));
      Set_First_Rep_Item   (Def_Id, First_Rep_Item   (T));
      Set_Discrete_RM_Size (Def_Id);
   end Constrain_Integer;

   ------------------------------
   -- Constrain_Ordinary_Fixed --
   ------------------------------

   procedure Constrain_Ordinary_Fixed (Def_Id : Node_Id; S : Node_Id) is
      T    : constant Entity_Id := Entity (Subtype_Mark (S));
      C    : Node_Id;
      D    : Node_Id;
      Rais : Node_Id;

   begin
      Set_Ekind          (Def_Id, E_Ordinary_Fixed_Point_Subtype);
      Set_Etype          (Def_Id, Base_Type        (T));
      Set_Size_Info      (Def_Id,                  (T));
      Set_First_Rep_Item (Def_Id, First_Rep_Item   (T));
      Set_Small_Value    (Def_Id, Small_Value      (T));

      --  Process the constraint

      C := Constraint (S);

      --  Delta constraint present

      if Nkind (C) = N_Delta_Constraint then
         Check_Restriction (No_Obsolescent_Features, C);

         if Warn_On_Obsolescent_Feature then
            Error_Msg_S
              ("subtype delta constraint is an " &
               "obsolescent feature ('R'M 'J.3(7))?");
         end if;

         D := Delta_Expression (C);
         Analyze_And_Resolve (D, Any_Real);
         Check_Delta_Expression (D);
         Set_Delta_Value (Def_Id, Expr_Value_R (D));

         --  Check that delta value is in range. Obviously we can do this
         --  at compile time, but it is strictly a runtime check, and of
         --  course there is an ACVC test that checks this!

         if Delta_Value (Def_Id) < Delta_Value (T) then
            Error_Msg_N ("?delta value is too small", D);
            Rais :=
              Make_Raise_Constraint_Error (Sloc (D),
                Reason => CE_Range_Check_Failed);
            Insert_Action (Declaration_Node (Def_Id), Rais);
         end if;

         C := Range_Constraint (C);

      --  No delta constraint present

      else
         Set_Delta_Value (Def_Id, Delta_Value (T));
      end if;

      --  Range constraint present

      if Nkind (C) = N_Range_Constraint then
         Set_Scalar_Range_For_Subtype (Def_Id, Range_Expression (C), T);

      --  No range constraint present

      else
         pragma Assert (No (C));
         Set_Scalar_Range (Def_Id, Scalar_Range (T));

      end if;

      Set_Discrete_RM_Size (Def_Id);

      --  Unconditionally delay the freeze, since we cannot set size
      --  information in all cases correctly until the freeze point.

      Set_Has_Delayed_Freeze (Def_Id);
   end Constrain_Ordinary_Fixed;

   ---------------------------
   -- Convert_Scalar_Bounds --
   ---------------------------

   procedure Convert_Scalar_Bounds
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Loc          : Source_Ptr)
   is
      Implicit_Base : constant Entity_Id := Base_Type (Derived_Type);

      Lo  : Node_Id;
      Hi  : Node_Id;
      Rng : Node_Id;

   begin
      Lo := Build_Scalar_Bound
              (Type_Low_Bound (Derived_Type),
               Parent_Type, Implicit_Base);

      Hi := Build_Scalar_Bound
              (Type_High_Bound (Derived_Type),
               Parent_Type, Implicit_Base);

      Rng :=
        Make_Range (Loc,
          Low_Bound  => Lo,
          High_Bound => Hi);

      Set_Includes_Infinities (Rng, Has_Infinities (Derived_Type));

      Set_Parent (Rng, N);
      Set_Scalar_Range (Derived_Type, Rng);

      --  Analyze the bounds

      Analyze_And_Resolve (Lo, Implicit_Base);
      Analyze_And_Resolve (Hi, Implicit_Base);

      --  Analyze the range itself, except that we do not analyze it if
      --  the bounds are real literals, and we have a fixed-point type.
      --  The reason for this is that we delay setting the bounds in this
      --  case till we know the final Small and Size values (see circuit
      --  in Freeze.Freeze_Fixed_Point_Type for further details).

      if Is_Fixed_Point_Type (Parent_Type)
        and then Nkind (Lo) = N_Real_Literal
        and then Nkind (Hi) = N_Real_Literal
      then
         return;

      --  Here we do the analysis of the range

      --  Note: we do this manually, since if we do a normal Analyze and
      --  Resolve call, there are problems with the conversions used for
      --  the derived type range.

      else
         Set_Etype    (Rng, Implicit_Base);
         Set_Analyzed (Rng, True);
      end if;
   end Convert_Scalar_Bounds;

   -------------------
   -- Copy_And_Swap --
   -------------------

   procedure Copy_And_Swap (Priv, Full : Entity_Id) is
   begin
      --  Initialize new full declaration entity by copying the pertinent
      --  fields of the corresponding private declaration entity.

      --  We temporarily set Ekind to a value appropriate for a type to
      --  avoid assert failures in Einfo from checking for setting type
      --  attributes on something that is not a type. Ekind (Priv) is an
      --  appropriate choice, since it allowed the attributes to be set
      --  in the first place. This Ekind value will be modified later.

      Set_Ekind (Full, Ekind (Priv));

      --  Also set Etype temporarily to Any_Type, again, in the absence
      --  of errors, it will be properly reset, and if there are errors,
      --  then we want a value of Any_Type to remain.

      Set_Etype (Full, Any_Type);

      --  Now start copying attributes

      Set_Has_Discriminants          (Full, Has_Discriminants       (Priv));

      if Has_Discriminants (Full) then
         Set_Discriminant_Constraint (Full, Discriminant_Constraint (Priv));
         Set_Stored_Constraint       (Full, Stored_Constraint       (Priv));
      end if;

      Set_First_Rep_Item             (Full, First_Rep_Item          (Priv));
      Set_Homonym                    (Full, Homonym                 (Priv));
      Set_Is_Immediately_Visible     (Full, Is_Immediately_Visible  (Priv));
      Set_Is_Public                  (Full, Is_Public               (Priv));
      Set_Is_Pure                    (Full, Is_Pure                 (Priv));
      Set_Is_Tagged_Type             (Full, Is_Tagged_Type          (Priv));

      Conditional_Delay              (Full,                          Priv);

      if Is_Tagged_Type (Full) then
         Set_Primitive_Operations    (Full, Primitive_Operations    (Priv));

         if Priv = Base_Type (Priv) then
            Set_Class_Wide_Type      (Full, Class_Wide_Type         (Priv));
         end if;
      end if;

      Set_Is_Volatile                (Full, Is_Volatile             (Priv));
      Set_Treat_As_Volatile          (Full, Treat_As_Volatile       (Priv));
      Set_Scope                      (Full, Scope                   (Priv));
      Set_Next_Entity                (Full, Next_Entity             (Priv));
      Set_First_Entity               (Full, First_Entity            (Priv));
      Set_Last_Entity                (Full, Last_Entity             (Priv));

      --  If access types have been recorded for later handling, keep them in
      --  the full view so that they get handled when the full view freeze
      --  node is expanded.

      if Present (Freeze_Node (Priv))
        and then Present (Access_Types_To_Process (Freeze_Node (Priv)))
      then
         Ensure_Freeze_Node (Full);
         Set_Access_Types_To_Process
           (Freeze_Node (Full),
            Access_Types_To_Process (Freeze_Node (Priv)));
      end if;

      --  Swap the two entities. Now Privat is the full type entity and
      --  Full is the private one. They will be swapped back at the end
      --  of the private part. This swapping ensures that the entity that
      --  is visible in the private part is the full declaration.

      Exchange_Entities (Priv, Full);
      Append_Entity (Full, Scope (Full));
   end Copy_And_Swap;

   -------------------------------------
   -- Copy_Array_Base_Type_Attributes --
   -------------------------------------

   procedure Copy_Array_Base_Type_Attributes (T1, T2 : Entity_Id) is
   begin
      Set_Component_Alignment      (T1, Component_Alignment      (T2));
      Set_Component_Type           (T1, Component_Type           (T2));
      Set_Component_Size           (T1, Component_Size           (T2));
      Set_Has_Controlled_Component (T1, Has_Controlled_Component (T2));
      Set_Finalize_Storage_Only    (T1, Finalize_Storage_Only    (T2));
      Set_Has_Non_Standard_Rep     (T1, Has_Non_Standard_Rep     (T2));
      Set_Has_Task                 (T1, Has_Task                 (T2));
      Set_Is_Packed                (T1, Is_Packed                (T2));
      Set_Has_Aliased_Components   (T1, Has_Aliased_Components   (T2));
      Set_Has_Atomic_Components    (T1, Has_Atomic_Components    (T2));
      Set_Has_Volatile_Components  (T1, Has_Volatile_Components  (T2));
   end Copy_Array_Base_Type_Attributes;

   -----------------------------------
   -- Copy_Array_Subtype_Attributes --
   -----------------------------------

   procedure Copy_Array_Subtype_Attributes (T1, T2 : Entity_Id) is
   begin
      Set_Size_Info (T1, T2);

      Set_First_Index          (T1, First_Index           (T2));
      Set_Is_Aliased           (T1, Is_Aliased            (T2));
      Set_Is_Atomic            (T1, Is_Atomic             (T2));
      Set_Is_Volatile          (T1, Is_Volatile           (T2));
      Set_Treat_As_Volatile    (T1, Treat_As_Volatile     (T2));
      Set_Is_Constrained       (T1, Is_Constrained        (T2));
      Set_Depends_On_Private   (T1, Has_Private_Component (T2));
      Set_First_Rep_Item       (T1, First_Rep_Item        (T2));
      Set_Convention           (T1, Convention            (T2));
      Set_Is_Limited_Composite (T1, Is_Limited_Composite  (T2));
      Set_Is_Private_Composite (T1, Is_Private_Composite  (T2));
   end Copy_Array_Subtype_Attributes;

   -----------------------------------
   -- Create_Constrained_Components --
   -----------------------------------

   procedure Create_Constrained_Components
     (Subt        : Entity_Id;
      Decl_Node   : Node_Id;
      Typ         : Entity_Id;
      Constraints : Elist_Id)
   is
      Loc         : constant Source_Ptr := Sloc (Subt);
      Comp_List   : constant Elist_Id   := New_Elmt_List;
      Parent_Type : constant Entity_Id  := Etype (Typ);
      Assoc_List  : constant List_Id    := New_List;
      Discr_Val   : Elmt_Id;
      Errors      : Boolean;
      New_C       : Entity_Id;
      Old_C       : Entity_Id;
      Is_Static   : Boolean := True;

      procedure Collect_Fixed_Components (Typ : Entity_Id);
      --  Collect parent type components that do not appear in a variant part

      procedure Create_All_Components;
      --  Iterate over Comp_List to create the components of the subtype

      function Create_Component (Old_Compon : Entity_Id) return Entity_Id;
      --  Creates a new component from Old_Compon, copying all the fields from
      --  it, including its Etype, inserts the new component in the Subt entity
      --  chain and returns the new component.

      function Is_Variant_Record (T : Entity_Id) return Boolean;
      --  If true, and discriminants are static, collect only components from
      --  variants selected by discriminant values.

      ------------------------------
      -- Collect_Fixed_Components --
      ------------------------------

      procedure Collect_Fixed_Components (Typ : Entity_Id) is
      begin
      --  Build association list for discriminants, and find components of the
      --  variant part selected by the values of the discriminants.

         Old_C := First_Discriminant (Typ);
         Discr_Val := First_Elmt (Constraints);
         while Present (Old_C) loop
            Append_To (Assoc_List,
              Make_Component_Association (Loc,
                 Choices    => New_List (New_Occurrence_Of (Old_C, Loc)),
                 Expression => New_Copy (Node (Discr_Val))));

            Next_Elmt (Discr_Val);
            Next_Discriminant (Old_C);
         end loop;

         --  The tag, and the possible parent and controller components
         --  are unconditionally in the subtype.

         if Is_Tagged_Type (Typ)
           or else Has_Controlled_Component (Typ)
         then
            Old_C := First_Component (Typ);
            while Present (Old_C) loop
               if Chars ((Old_C)) = Name_uTag
                 or else Chars ((Old_C)) = Name_uParent
                 or else Chars ((Old_C)) = Name_uController
               then
                  Append_Elmt (Old_C, Comp_List);
               end if;

               Next_Component (Old_C);
            end loop;
         end if;
      end Collect_Fixed_Components;

      ---------------------------
      -- Create_All_Components --
      ---------------------------

      procedure Create_All_Components is
         Comp : Elmt_Id;

      begin
         Comp := First_Elmt (Comp_List);
         while Present (Comp) loop
            Old_C := Node (Comp);
            New_C := Create_Component (Old_C);

            Set_Etype
              (New_C,
               Constrain_Component_Type
                 (Old_C, Subt, Decl_Node, Typ, Constraints));
            Set_Is_Public (New_C, Is_Public (Subt));

            Next_Elmt (Comp);
         end loop;
      end Create_All_Components;

      ----------------------
      -- Create_Component --
      ----------------------

      function Create_Component (Old_Compon : Entity_Id) return Entity_Id is
         New_Compon : constant Entity_Id := New_Copy (Old_Compon);

      begin
         --  Set the parent so we have a proper link for freezing etc. This
         --  is not a real parent pointer, since of course our parent does
         --  not own up to us and reference us, we are an illegitimate
         --  child of the original parent!

         Set_Parent (New_Compon, Parent (Old_Compon));

         --  We do not want this node marked as Comes_From_Source, since
         --  otherwise it would get first class status and a separate
         --  cross-reference line would be generated. Illegitimate
         --  children do not rate such recognition.

         Set_Comes_From_Source (New_Compon, False);

         --  But it is a real entity, and a birth certificate must be
         --  properly registered by entering it into the entity list.

         Enter_Name (New_Compon);
         return New_Compon;
      end Create_Component;

      -----------------------
      -- Is_Variant_Record --
      -----------------------

      function Is_Variant_Record (T : Entity_Id) return Boolean is
      begin
         return Nkind (Parent (T)) = N_Full_Type_Declaration
           and then Nkind (Type_Definition (Parent (T))) = N_Record_Definition
           and then Present (Component_List (Type_Definition (Parent (T))))
           and then Present (
             Variant_Part (Component_List (Type_Definition (Parent (T)))));
      end Is_Variant_Record;

   --  Start of processing for Create_Constrained_Components

   begin
      pragma Assert (Subt /= Base_Type (Subt));
      pragma Assert (Typ = Base_Type (Typ));

      Set_First_Entity (Subt, Empty);
      Set_Last_Entity  (Subt, Empty);

      --  Check whether constraint is fully static, in which case we can
      --  optimize the list of components.

      Discr_Val := First_Elmt (Constraints);
      while Present (Discr_Val) loop
         if not Is_OK_Static_Expression (Node (Discr_Val)) then
            Is_Static := False;
            exit;
         end if;

         Next_Elmt (Discr_Val);
      end loop;

      New_Scope (Subt);

      --  Inherit the discriminants of the parent type

      Old_C := First_Discriminant (Typ);
      while Present (Old_C) loop
         New_C := Create_Component (Old_C);
         Set_Is_Public (New_C, Is_Public (Subt));
         Next_Discriminant (Old_C);
      end loop;

      if Is_Static
        and then Is_Variant_Record (Typ)
      then
         Collect_Fixed_Components (Typ);

         Gather_Components (
           Typ,
           Component_List (Type_Definition (Parent (Typ))),
           Governed_By   => Assoc_List,
           Into          => Comp_List,
           Report_Errors => Errors);
         pragma Assert (not Errors);

         Create_All_Components;

      --  If the subtype declaration is created for a tagged type derivation
      --  with constraints, we retrieve the record definition of the parent
      --  type to select the components of the proper variant.

      elsif Is_Static
        and then Is_Tagged_Type (Typ)
        and then Nkind (Parent (Typ)) = N_Full_Type_Declaration
        and then
          Nkind (Type_Definition (Parent (Typ))) = N_Derived_Type_Definition
        and then Is_Variant_Record (Parent_Type)
      then
         Collect_Fixed_Components (Typ);

         Gather_Components (
           Typ,
           Component_List (Type_Definition (Parent (Parent_Type))),
           Governed_By   => Assoc_List,
           Into          => Comp_List,
           Report_Errors => Errors);
         pragma Assert (not Errors);

         --  If the tagged derivation has a type extension, collect all the
         --  new components therein.

         if Present
              (Record_Extension_Part (Type_Definition (Parent (Typ))))
         then
            Old_C := First_Component (Typ);
            while Present (Old_C) loop
               if Original_Record_Component (Old_C) = Old_C
                and then Chars (Old_C) /= Name_uTag
                and then Chars (Old_C) /= Name_uParent
                and then Chars (Old_C) /= Name_uController
               then
                  Append_Elmt (Old_C, Comp_List);
               end if;

               Next_Component (Old_C);
            end loop;
         end if;

         Create_All_Components;

      else
         --  If discriminants are not static, or if this is a multi-level type
         --  extension, we have to include all components of the parent type.

         Old_C := First_Component (Typ);
         while Present (Old_C) loop
            New_C := Create_Component (Old_C);

            Set_Etype
              (New_C,
               Constrain_Component_Type
                 (Old_C, Subt, Decl_Node, Typ, Constraints));
            Set_Is_Public (New_C, Is_Public (Subt));

            Next_Component (Old_C);
         end loop;
      end if;

      End_Scope;
   end Create_Constrained_Components;

   ------------------------------------------
   -- Decimal_Fixed_Point_Type_Declaration --
   ------------------------------------------

   procedure Decimal_Fixed_Point_Type_Declaration
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Loc           : constant Source_Ptr := Sloc (Def);
      Digs_Expr     : constant Node_Id    := Digits_Expression (Def);
      Delta_Expr    : constant Node_Id    := Delta_Expression (Def);
      Implicit_Base : Entity_Id;
      Digs_Val      : Uint;
      Delta_Val     : Ureal;
      Scale_Val     : Uint;
      Bound_Val     : Ureal;

   --  Start of processing for Decimal_Fixed_Point_Type_Declaration

   begin
      Check_Restriction (No_Fixed_Point, Def);

      --  Create implicit base type

      Implicit_Base :=
        Create_Itype (E_Decimal_Fixed_Point_Type, Parent (Def), T, 'B');
      Set_Etype (Implicit_Base, Implicit_Base);

      --  Analyze and process delta expression

      Analyze_And_Resolve (Delta_Expr, Universal_Real);

      Check_Delta_Expression (Delta_Expr);
      Delta_Val := Expr_Value_R (Delta_Expr);

      --  Check delta is power of 10, and determine scale value from it

      declare
         Val : Ureal;

      begin
         Scale_Val := Uint_0;
         Val := Delta_Val;

         if Val < Ureal_1 then
            while Val < Ureal_1 loop
               Val := Val * Ureal_10;
               Scale_Val := Scale_Val + 1;
            end loop;

            if Scale_Val > 18 then
               Error_Msg_N ("scale exceeds maximum value of 18", Def);
               Scale_Val := UI_From_Int (+18);
            end if;

         else
            while Val > Ureal_1 loop
               Val := Val / Ureal_10;
               Scale_Val := Scale_Val - 1;
            end loop;

            if Scale_Val < -18 then
               Error_Msg_N ("scale is less than minimum value of -18", Def);
               Scale_Val := UI_From_Int (-18);
            end if;
         end if;

         if Val /= Ureal_1 then
            Error_Msg_N ("delta expression must be a power of 10", Def);
            Delta_Val := Ureal_10 ** (-Scale_Val);
         end if;
      end;

      --  Set delta, scale and small (small = delta for decimal type)

      Set_Delta_Value (Implicit_Base, Delta_Val);
      Set_Scale_Value (Implicit_Base, Scale_Val);
      Set_Small_Value (Implicit_Base, Delta_Val);

      --  Analyze and process digits expression

      Analyze_And_Resolve (Digs_Expr, Any_Integer);
      Check_Digits_Expression (Digs_Expr);
      Digs_Val := Expr_Value (Digs_Expr);

      if Digs_Val > 18 then
         Digs_Val := UI_From_Int (+18);
         Error_Msg_N ("digits value out of range, maximum is 18", Digs_Expr);
      end if;

      Set_Digits_Value (Implicit_Base, Digs_Val);
      Bound_Val := UR_From_Uint (10 ** Digs_Val - 1) * Delta_Val;

      --  Set range of base type from digits value for now. This will be
      --  expanded to represent the true underlying base range by Freeze.

      Set_Fixed_Range (Implicit_Base, Loc, -Bound_Val, Bound_Val);

      --  Set size to zero for now, size will be set at freeze time. We have
      --  to do this for ordinary fixed-point, because the size depends on
      --  the specified small, and we might as well do the same for decimal
      --  fixed-point.

      Init_Size_Align (Implicit_Base);

      --  If there are bounds given in the declaration use them as the
      --  bounds of the first named subtype.

      if Present (Real_Range_Specification (Def)) then
         declare
            RRS      : constant Node_Id := Real_Range_Specification (Def);
            Low      : constant Node_Id := Low_Bound (RRS);
            High     : constant Node_Id := High_Bound (RRS);
            Low_Val  : Ureal;
            High_Val : Ureal;

         begin
            Analyze_And_Resolve (Low, Any_Real);
            Analyze_And_Resolve (High, Any_Real);
            Check_Real_Bound (Low);
            Check_Real_Bound (High);
            Low_Val := Expr_Value_R (Low);
            High_Val := Expr_Value_R (High);

            if Low_Val < (-Bound_Val) then
               Error_Msg_N
                 ("range low bound too small for digits value", Low);
               Low_Val := -Bound_Val;
            end if;

            if High_Val > Bound_Val then
               Error_Msg_N
                 ("range high bound too large for digits value", High);
               High_Val := Bound_Val;
            end if;

            Set_Fixed_Range (T, Loc, Low_Val, High_Val);
         end;

      --  If no explicit range, use range that corresponds to given
      --  digits value. This will end up as the final range for the
      --  first subtype.

      else
         Set_Fixed_Range (T, Loc, -Bound_Val, Bound_Val);
      end if;

      --  Complete entity for first subtype

      Set_Ekind          (T, E_Decimal_Fixed_Point_Subtype);
      Set_Etype          (T, Implicit_Base);
      Set_Size_Info      (T, Implicit_Base);
      Set_First_Rep_Item (T, First_Rep_Item (Implicit_Base));
      Set_Digits_Value   (T, Digs_Val);
      Set_Delta_Value    (T, Delta_Val);
      Set_Small_Value    (T, Delta_Val);
      Set_Scale_Value    (T, Scale_Val);
      Set_Is_Constrained (T);
   end Decimal_Fixed_Point_Type_Declaration;

   ---------------------------------
   -- Derive_Interface_Subprogram --
   ---------------------------------

   procedure Derive_Interface_Subprograms (Derived_Type : Entity_Id) is

      procedure Do_Derivation (T : Entity_Id);
      --  This inner subprograms is used to climb to the ancestors.
      --  It is needed to add the derivations to the Derived_Type.

      procedure Do_Derivation (T : Entity_Id) is
         Etyp : constant Entity_Id := Etype (T);
         AI   : Elmt_Id;

      begin
         if Etyp /= T
           and then Is_Interface (Etyp)
         then
            Do_Derivation (Etyp);
         end if;

         if Present (Abstract_Interfaces (T))
           and then not Is_Empty_Elmt_List (Abstract_Interfaces (T))
         then
            AI := First_Elmt (Abstract_Interfaces (T));
            while Present (AI) loop
               if not Is_Ancestor (Node (AI), Derived_Type) then
                  Derive_Subprograms
                    (Parent_Type         => Node (AI),
                     Derived_Type        => Derived_Type,
                     No_Predefined_Prims => True);
               end if;

               Next_Elmt (AI);
            end loop;
         end if;
      end Do_Derivation;

   begin
      Do_Derivation (Derived_Type);

      --  At this point the list of primitive operations of Derived_Type
      --  contains the entities corresponding to all the subprograms of all the
      --  implemented interfaces. If N interfaces have subprograms with the
      --  same profile we have N entities in this list because each one must be
      --  allocated in its corresponding virtual table.

      --  Its alias attribute references its original interface subprogram.
      --  When overridden, the alias attribute is later saved in the
      --  Abstract_Interface_Alias attribute.

   end Derive_Interface_Subprograms;

   -----------------------
   -- Derive_Subprogram --
   -----------------------

   procedure Derive_Subprogram
     (New_Subp     : in out Entity_Id;
      Parent_Subp  : Entity_Id;
      Derived_Type : Entity_Id;
      Parent_Type  : Entity_Id;
      Actual_Subp  : Entity_Id := Empty)
   is
      Formal       : Entity_Id;
      New_Formal   : Entity_Id;
      Visible_Subp : Entity_Id := Parent_Subp;

      function Is_Private_Overriding return Boolean;
      --  If Subp is a private overriding of a visible operation, the in-
      --  herited operation derives from the overridden op (even though
      --  its body is the overriding one) and the inherited operation is
      --  visible now. See sem_disp to see the details of the handling of
      --  the overridden subprogram, which is removed from the list of
      --  primitive operations of the type. The overridden subprogram is
      --  saved locally in Visible_Subp, and used to diagnose abstract
      --  operations that need overriding in the derived type.

      procedure Replace_Type (Id, New_Id : Entity_Id);
      --  When the type is an anonymous access type, create a new access type
      --  designating the derived type.

      procedure Set_Derived_Name;
      --  This procedure sets the appropriate Chars name for New_Subp. This
      --  is normally just a copy of the parent name. An exception arises for
      --  type support subprograms, where the name is changed to reflect the
      --  name of the derived type, e.g. if type foo is derived from type bar,
      --  then a procedure barDA is derived with a name fooDA.

      ---------------------------
      -- Is_Private_Overriding --
      ---------------------------

      function Is_Private_Overriding return Boolean is
         Prev : Entity_Id;

      begin
         --  The visible operation that is overridden is a homonym of the
         --  parent subprogram. We scan the homonym chain to find the one
         --  whose alias is the subprogram we are deriving.

         Prev := Current_Entity (Parent_Subp);
         while Present (Prev) loop
            if Is_Dispatching_Operation (Parent_Subp)
              and then Present (Prev)
              and then Ekind (Prev) = Ekind (Parent_Subp)
              and then Alias (Prev) = Parent_Subp
              and then Scope (Parent_Subp) = Scope (Prev)
              and then
                (not Is_Hidden (Prev)
                   or else

                  --  Ada 2005 (AI-251): Entities associated with overridden
                  --  interface subprograms are always marked as hidden; in
                  --  this case the field abstract_interface_alias references
                  --  the original entity (cf. override_dispatching_operation).

                 (Atree.Present (Abstract_Interface_Alias (Prev))
                    and then not Is_Hidden (Abstract_Interface_Alias (Prev))))
            then
               Visible_Subp := Prev;
               return True;
            end if;

            Prev := Homonym (Prev);
         end loop;

         return False;
      end Is_Private_Overriding;

      ------------------
      -- Replace_Type --
      ------------------

      procedure Replace_Type (Id, New_Id : Entity_Id) is
         Acc_Type : Entity_Id;
         IR       : Node_Id;
         Par      : constant Node_Id := Parent (Derived_Type);

      begin
         --  When the type is an anonymous access type, create a new access
         --  type designating the derived type. This itype must be elaborated
         --  at the point of the derivation, not on subsequent calls that may
         --  be out of the proper scope for Gigi, so we insert a reference to
         --  it after the derivation.

         if Ekind (Etype (Id)) = E_Anonymous_Access_Type then
            declare
               Desig_Typ : Entity_Id := Designated_Type (Etype (Id));

            begin
               if Ekind (Desig_Typ) = E_Record_Type_With_Private
                 and then Present (Full_View (Desig_Typ))
                 and then not Is_Private_Type (Parent_Type)
               then
                  Desig_Typ := Full_View (Desig_Typ);
               end if;

               if Base_Type (Desig_Typ) = Base_Type (Parent_Type) then
                  Acc_Type := New_Copy (Etype (Id));
                  Set_Etype (Acc_Type, Acc_Type);
                  Set_Scope (Acc_Type, New_Subp);

                  --  Compute size of anonymous access type

                  if Is_Array_Type (Desig_Typ)
                    and then not Is_Constrained (Desig_Typ)
                  then
                     Init_Size (Acc_Type, 2 * System_Address_Size);
                  else
                     Init_Size (Acc_Type, System_Address_Size);
                  end if;

                  Init_Alignment (Acc_Type);
                  Set_Directly_Designated_Type (Acc_Type, Derived_Type);

                  Set_Etype (New_Id, Acc_Type);
                  Set_Scope (New_Id, New_Subp);

                  --  Create a reference to it

                  IR := Make_Itype_Reference (Sloc (Parent (Derived_Type)));
                  Set_Itype (IR, Acc_Type);
                  Insert_After (Parent (Derived_Type), IR);

               else
                  Set_Etype (New_Id, Etype (Id));
               end if;
            end;

         elsif Base_Type (Etype (Id)) = Base_Type (Parent_Type)
           or else
             (Ekind (Etype (Id)) = E_Record_Type_With_Private
               and then Present (Full_View (Etype (Id)))
               and then
                 Base_Type (Full_View (Etype (Id))) = Base_Type (Parent_Type))
         then
            --  Constraint checks on formals are generated during expansion,
            --  based on the signature of the original subprogram. The bounds
            --  of the derived type are not relevant, and thus we can use
            --  the base type for the formals. However, the return type may be
            --  used in a context that requires that the proper static bounds
            --  be used (a case statement, for example)  and for those cases
            --  we must use the derived type (first subtype), not its base.

            --  If the derived_type_definition has no constraints, we know that
            --  the derived type has the same constraints as the first subtype
            --  of the parent, and we can also use it rather than its base,
            --  which can lead to more efficient code.

            if Etype (Id) = Parent_Type then
               if Is_Scalar_Type (Parent_Type)
                 and then
                   Subtypes_Statically_Compatible (Parent_Type, Derived_Type)
               then
                  Set_Etype (New_Id, Derived_Type);

               elsif Nkind (Par) = N_Full_Type_Declaration
                 and then
                   Nkind (Type_Definition (Par)) = N_Derived_Type_Definition
                 and then
                   Is_Entity_Name
                     (Subtype_Indication (Type_Definition (Par)))
               then
                  Set_Etype (New_Id, Derived_Type);

               else
                  Set_Etype (New_Id, Base_Type (Derived_Type));
               end if;

            else
               Set_Etype (New_Id, Base_Type (Derived_Type));
            end if;

         else
            Set_Etype (New_Id, Etype (Id));
         end if;
      end Replace_Type;

      ----------------------
      -- Set_Derived_Name --
      ----------------------

      procedure Set_Derived_Name is
         Nm : constant TSS_Name_Type := Get_TSS_Name (Parent_Subp);
      begin
         if Nm = TSS_Null then
            Set_Chars (New_Subp, Chars (Parent_Subp));
         else
            Set_Chars (New_Subp, Make_TSS_Name (Base_Type (Derived_Type), Nm));
         end if;
      end Set_Derived_Name;

   --  Start of processing for Derive_Subprogram

   begin
      New_Subp :=
         New_Entity (Nkind (Parent_Subp), Sloc (Derived_Type));
      Set_Ekind (New_Subp, Ekind (Parent_Subp));

      --  Check whether the inherited subprogram is a private operation that
      --  should be inherited but not yet made visible. Such subprograms can
      --  become visible at a later point (e.g., the private part of a public
      --  child unit) via Declare_Inherited_Private_Subprograms. If the
      --  following predicate is true, then this is not such a private
      --  operation and the subprogram simply inherits the name of the parent
      --  subprogram. Note the special check for the names of controlled
      --  operations, which are currently exempted from being inherited with
      --  a hidden name because they must be findable for generation of
      --  implicit run-time calls.

      if not Is_Hidden (Parent_Subp)
        or else Is_Internal (Parent_Subp)
        or else Is_Private_Overriding
        or else Is_Internal_Name (Chars (Parent_Subp))
        or else Chars (Parent_Subp) = Name_Initialize
        or else Chars (Parent_Subp) = Name_Adjust
        or else Chars (Parent_Subp) = Name_Finalize
      then
         Set_Derived_Name;

      --  If parent is hidden, this can be a regular derivation if the
      --  parent is immediately visible in a non-instantiating context,
      --  or if we are in the private part of an instance. This test
      --  should still be refined ???

      --  The test for In_Instance_Not_Visible avoids inheriting the derived
      --  operation as a non-visible operation in cases where the parent
      --  subprogram might not be visible now, but was visible within the
      --  original generic, so it would be wrong to make the inherited
      --  subprogram non-visible now. (Not clear if this test is fully
      --  correct; are there any cases where we should declare the inherited
      --  operation as not visible to avoid it being overridden, e.g., when
      --  the parent type is a generic actual with private primitives ???)

      --  (they should be treated the same as other private inherited
      --  subprograms, but it's not clear how to do this cleanly). ???

      elsif (In_Open_Scopes (Scope (Base_Type (Parent_Type)))
              and then Is_Immediately_Visible (Parent_Subp)
              and then not In_Instance)
        or else In_Instance_Not_Visible
      then
         Set_Derived_Name;

      --  The type is inheriting a private operation, so enter
      --  it with a special name so it can't be overridden.

      else
         Set_Chars (New_Subp, New_External_Name (Chars (Parent_Subp), 'P'));
      end if;

      Set_Parent (New_Subp, Parent (Derived_Type));
      Replace_Type (Parent_Subp, New_Subp);
      Conditional_Delay (New_Subp, Parent_Subp);

      Formal := First_Formal (Parent_Subp);
      while Present (Formal) loop
         New_Formal := New_Copy (Formal);

         --  Normally we do not go copying parents, but in the case of
         --  formals, we need to link up to the declaration (which is the
         --  parameter specification), and it is fine to link up to the
         --  original formal's parameter specification in this case.

         Set_Parent (New_Formal, Parent (Formal));

         Append_Entity (New_Formal, New_Subp);

         Replace_Type (Formal, New_Formal);
         Next_Formal (Formal);
      end loop;

      --  If this derivation corresponds to a tagged generic actual, then
      --  primitive operations rename those of the actual. Otherwise the
      --  primitive operations rename those of the parent type, If the
      --  parent renames an intrinsic operator, so does the new subprogram.
      --  We except concatenation, which is always properly typed, and does
      --  not get expanded as other intrinsic operations.

      if No (Actual_Subp) then
         if Is_Intrinsic_Subprogram (Parent_Subp) then
            Set_Is_Intrinsic_Subprogram (New_Subp);

            if Present (Alias (Parent_Subp))
              and then Chars (Parent_Subp) /= Name_Op_Concat
            then
               Set_Alias (New_Subp, Alias (Parent_Subp));
            else
               Set_Alias (New_Subp, Parent_Subp);
            end if;

         else
            Set_Alias (New_Subp, Parent_Subp);
         end if;

      else
         Set_Alias (New_Subp, Actual_Subp);
      end if;

      --  Derived subprograms of a tagged type must inherit the convention
      --  of the parent subprogram (a requirement of AI-117). Derived
      --  subprograms of untagged types simply get convention Ada by default.

      if Is_Tagged_Type (Derived_Type) then
         Set_Convention  (New_Subp, Convention  (Parent_Subp));
      end if;

      Set_Is_Imported (New_Subp, Is_Imported (Parent_Subp));
      Set_Is_Exported (New_Subp, Is_Exported (Parent_Subp));

      if Ekind (Parent_Subp) = E_Procedure then
         Set_Is_Valued_Procedure
           (New_Subp, Is_Valued_Procedure (Parent_Subp));
      end if;

      --  A derived function with a controlling result is abstract. If the
      --  Derived_Type is a nonabstract formal generic derived type, then
      --  inherited operations are not abstract: the required check is done at
      --  instantiation time. If the derivation is for a generic actual, the
      --  function is not abstract unless the actual is.

      if Is_Generic_Type (Derived_Type)
        and then not Is_Abstract (Derived_Type)
      then
         null;

      elsif Is_Abstract (Alias (New_Subp))
        or else (Is_Tagged_Type (Derived_Type)
                   and then Etype (New_Subp) = Derived_Type
                   and then No (Actual_Subp))
      then
         Set_Is_Abstract (New_Subp);

      --  Finally, if the parent type is abstract  we must verify that all
      --  inherited operations are either non-abstract or overridden, or
      --  that the derived type itself is abstract (this check is performed
      --  at the end of a package declaration, in Check_Abstract_Overriding).
      --  A private overriding in the parent type will not be visible in the
      --  derivation if we are not in an inner package or in a child unit of
      --  the parent type, in which case the abstractness of the inherited
      --  operation is carried to the new subprogram.

      elsif Is_Abstract (Parent_Type)
        and then not In_Open_Scopes (Scope (Parent_Type))
        and then Is_Private_Overriding
        and then Is_Abstract (Visible_Subp)
      then
         Set_Alias (New_Subp, Visible_Subp);
         Set_Is_Abstract (New_Subp);
      end if;

      New_Overloaded_Entity (New_Subp, Derived_Type);

      --  Check for case of a derived subprogram for the instantiation of a
      --  formal derived tagged type, if so mark the subprogram as dispatching
      --  and inherit the dispatching attributes of the parent subprogram. The
      --  derived subprogram is effectively renaming of the actual subprogram,
      --  so it needs to have the same attributes as the actual.

      if Present (Actual_Subp)
        and then Is_Dispatching_Operation (Parent_Subp)
      then
         Set_Is_Dispatching_Operation (New_Subp);
         if Present (DTC_Entity (Parent_Subp)) then
            Set_DTC_Entity (New_Subp, DTC_Entity (Parent_Subp));
            Set_DT_Position (New_Subp, DT_Position (Parent_Subp));
         end if;
      end if;

      --  Indicate that a derived subprogram does not require a body and that
      --  it does not require processing of default expressions.

      Set_Has_Completion (New_Subp);
      Set_Default_Expressions_Processed (New_Subp);

      if Ekind (New_Subp) = E_Function then
         Set_Mechanism (New_Subp, Mechanism (Parent_Subp));
      end if;
   end Derive_Subprogram;

   ------------------------
   -- Derive_Subprograms --
   ------------------------

   procedure Derive_Subprograms
     (Parent_Type           : Entity_Id;
      Derived_Type          : Entity_Id;
      Generic_Actual        : Entity_Id := Empty;
      No_Predefined_Prims   : Boolean   := False)
   is
      Op_List     : constant Elist_Id :=
                      Collect_Primitive_Operations (Parent_Type);
      Act_List    : Elist_Id;
      Act_Elmt    : Elmt_Id;
      Elmt        : Elmt_Id;
      Is_Predef   : Boolean;
      Subp        : Entity_Id;
      New_Subp    : Entity_Id := Empty;
      Parent_Base : Entity_Id;

   begin
      if Ekind (Parent_Type) = E_Record_Type_With_Private
        and then Has_Discriminants (Parent_Type)
        and then Present (Full_View (Parent_Type))
      then
         Parent_Base := Full_View (Parent_Type);
      else
         Parent_Base := Parent_Type;
      end if;

      if Present (Generic_Actual) then
         Act_List := Collect_Primitive_Operations (Generic_Actual);
         Act_Elmt := First_Elmt (Act_List);
      else
         Act_Elmt := No_Elmt;
      end if;

      --  Literals are derived earlier in the process of building the derived
      --  type, and are skipped here.

      Elmt := First_Elmt (Op_List);
      while Present (Elmt) loop
         Subp := Node (Elmt);

         if Ekind (Subp) /= E_Enumeration_Literal then
            Is_Predef :=
              Is_Dispatching_Operation (Subp)
                and then Is_Predefined_Dispatching_Operation (Subp);

            if No_Predefined_Prims and then Is_Predef then
               null;

            --  We don't need to derive alias entities associated with
            --  abstract interfaces

            elsif Is_Dispatching_Operation (Subp)
               and then Present (Alias (Subp))
               and then Present (Abstract_Interface_Alias (Subp))
            then
               null;

            elsif No (Generic_Actual) then
               Derive_Subprogram
                 (New_Subp, Subp, Derived_Type, Parent_Base);

            else
               Derive_Subprogram (New_Subp, Subp,
                 Derived_Type, Parent_Base, Node (Act_Elmt));
               Next_Elmt (Act_Elmt);
            end if;
         end if;

         Next_Elmt (Elmt);
      end loop;
   end Derive_Subprograms;

   --------------------------------
   -- Derived_Standard_Character --
   --------------------------------

   procedure Derived_Standard_Character
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Def           : constant Node_Id    := Type_Definition (N);
      Indic         : constant Node_Id    := Subtype_Indication (Def);
      Parent_Base   : constant Entity_Id  := Base_Type (Parent_Type);
      Implicit_Base : constant Entity_Id  :=
                        Create_Itype
                          (E_Enumeration_Type, N, Derived_Type, 'B');

      Lo : Node_Id;
      Hi : Node_Id;

   begin
      Discard_Node (Process_Subtype (Indic, N));

      Set_Etype     (Implicit_Base, Parent_Base);
      Set_Size_Info (Implicit_Base, Root_Type (Parent_Type));
      Set_RM_Size   (Implicit_Base, RM_Size (Root_Type (Parent_Type)));

      Set_Is_Character_Type  (Implicit_Base, True);
      Set_Has_Delayed_Freeze (Implicit_Base);

      --  The bounds of the implicit base are the bounds of the parent base.
      --  Note that their type is the parent base.

      Lo := New_Copy_Tree (Type_Low_Bound  (Parent_Base));
      Hi := New_Copy_Tree (Type_High_Bound (Parent_Base));

      Set_Scalar_Range (Implicit_Base,
        Make_Range (Loc,
          Low_Bound  => Lo,
          High_Bound => Hi));

      Conditional_Delay (Derived_Type, Parent_Type);

      Set_Ekind (Derived_Type, E_Enumeration_Subtype);
      Set_Etype (Derived_Type, Implicit_Base);
      Set_Size_Info         (Derived_Type, Parent_Type);

      if Unknown_RM_Size (Derived_Type) then
         Set_RM_Size (Derived_Type, RM_Size (Parent_Type));
      end if;

      Set_Is_Character_Type (Derived_Type, True);

      if Nkind (Indic) /= N_Subtype_Indication then

         --  If no explicit constraint, the bounds are those
         --  of the parent type.

         Lo := New_Copy_Tree (Type_Low_Bound  (Parent_Type));
         Hi := New_Copy_Tree (Type_High_Bound (Parent_Type));
         Set_Scalar_Range (Derived_Type, Make_Range (Loc, Lo, Hi));
      end if;

      Convert_Scalar_Bounds (N, Parent_Type, Derived_Type, Loc);

      --  Because the implicit base is used in the conversion of the bounds,
      --  we have to freeze it now. This is similar to what is done for
      --  numeric types, and it equally suspicious, but otherwise a non-
      --  static bound will have a reference to an unfrozen type, which is
      --  rejected by Gigi (???).

      Freeze_Before (N, Implicit_Base);
   end Derived_Standard_Character;

   ------------------------------
   -- Derived_Type_Declaration --
   ------------------------------

   procedure Derived_Type_Declaration
     (T             : Entity_Id;
      N             : Node_Id;
      Is_Completion : Boolean)
   is
      Def          : constant Node_Id := Type_Definition (N);
      Iface_Def    : Node_Id;
      Indic        : constant Node_Id := Subtype_Indication (Def);
      Extension    : constant Node_Id := Record_Extension_Part (Def);
      Parent_Type  : Entity_Id;
      Parent_Scope : Entity_Id;
      Taggd        : Boolean;

      function Comes_From_Generic (Typ : Entity_Id) return Boolean;
      --  Check whether the parent type is a generic formal, or derives
      --  directly or indirectly from one.

      ------------------------
      -- Comes_From_Generic --
      ------------------------

      function Comes_From_Generic (Typ : Entity_Id) return Boolean is
      begin
         if Is_Generic_Type (Typ) then
            return True;

         elsif Is_Generic_Type (Root_Type (Parent_Type)) then
            return True;

         elsif Is_Private_Type (Typ)
           and then Present (Full_View (Typ))
           and then Is_Generic_Type (Root_Type (Full_View (Typ)))
         then
            return True;

         elsif Is_Generic_Actual_Type (Typ) then
            return True;

         else
            return False;
         end if;
      end Comes_From_Generic;

   --  Start of processing for Derived_Type_Declaration

   begin
      Parent_Type := Find_Type_Of_Subtype_Indic (Indic);

      --  Ada 2005 (AI-251): In case of interface derivation check that the
      --  parent is also an interface.

      if Interface_Present (Def) then
         if not Is_Interface (Parent_Type) then
            Error_Msg_NE ("(Ada 2005) & must be an interface",
                          Indic, Parent_Type);

         else
            Iface_Def := Type_Definition (Parent (Parent_Type));

            --  Ada 2005 (AI-251): Limited interfaces can only inherit from
            --  other limited interfaces.

            if Limited_Present (Def) then
               if Limited_Present (Iface_Def) then
                  null;

               elsif Protected_Present (Iface_Def) then
                  Error_Msg_N ("(Ada 2005) limited interface cannot" &
                    " inherit from protected interface", Indic);

               elsif Synchronized_Present (Iface_Def) then
                  Error_Msg_N ("(Ada 2005) limited interface cannot" &
                    " inherit from synchronized interface", Indic);

               elsif Task_Present (Iface_Def) then
                  Error_Msg_N ("(Ada 2005) limited interface cannot" &
                    " inherit from task interface", Indic);

               else
                  Error_Msg_N ("(Ada 2005) limited interface cannot" &
                    " inherit from non-limited interface", Indic);
               end if;

            --  Ada 2005 (AI-345): Non-limited interfaces can only inherit
            --  from non-limited or limited interfaces.

            elsif not Protected_Present (Def)
              and then not Synchronized_Present (Def)
              and then not Task_Present (Def)
            then
               if Limited_Present (Iface_Def) then
                  null;

               elsif Protected_Present (Iface_Def) then
                  Error_Msg_N ("(Ada 2005) non-limited interface cannot" &
                    " inherit from protected interface", Indic);

               elsif Synchronized_Present (Iface_Def) then
                  Error_Msg_N ("(Ada 2005) non-limited interface cannot" &
                    " inherit from synchronized interface", Indic);

               elsif Task_Present (Iface_Def) then
                  Error_Msg_N ("(Ada 2005) non-limited interface cannot" &
                    " inherit from task interface", Indic);

               else
                  null;
               end if;
            end if;
         end if;
      end if;

      --  Ada 2005 (AI-251): Decorate all the names in the list of ancestor
      --  interfaces

      if Is_Tagged_Type (Parent_Type)
        and then Is_Non_Empty_List (Interface_List (Def))
      then
         declare
            Intf : Node_Id;
            T    : Entity_Id;

         begin
            Intf := First (Interface_List (Def));
            while Present (Intf) loop
               T := Find_Type_Of_Subtype_Indic (Intf);

               if not Is_Interface (T) then
                  Error_Msg_NE ("(Ada 2005) & must be an interface", Intf, T);
               end if;

               Next (Intf);
            end loop;
         end;
      end if;

      if Parent_Type = Any_Type
        or else Etype (Parent_Type) = Any_Type
        or else (Is_Class_Wide_Type (Parent_Type)
                   and then Etype (Parent_Type) = T)
      then
         --  If Parent_Type is undefined or illegal, make new type into a
         --  subtype of Any_Type, and set a few attributes to prevent cascaded
         --  errors. If this is a self-definition, emit error now.

         if T = Parent_Type
           or else T = Etype (Parent_Type)
         then
            Error_Msg_N ("type cannot be used in its own definition", Indic);
         end if;

         Set_Ekind        (T, Ekind (Parent_Type));
         Set_Etype        (T, Any_Type);
         Set_Scalar_Range (T, Scalar_Range (Any_Type));

         if Is_Tagged_Type (T) then
            Set_Primitive_Operations (T, New_Elmt_List);
         end if;

         return;
      end if;

      --  Only composite types other than array types are allowed to have
      --  discriminants.

      if Present (Discriminant_Specifications (N))
        and then (Is_Elementary_Type (Parent_Type)
                  or else Is_Array_Type (Parent_Type))
        and then not Error_Posted (N)
      then
         Error_Msg_N
           ("elementary or array type cannot have discriminants",
            Defining_Identifier (First (Discriminant_Specifications (N))));
         Set_Has_Discriminants (T, False);
      end if;

      --  In Ada 83, a derived type defined in a package specification cannot
      --  be used for further derivation until the end of its visible part.
      --  Note that derivation in the private part of the package is allowed.

      if Ada_Version = Ada_83
        and then Is_Derived_Type (Parent_Type)
        and then In_Visible_Part (Scope (Parent_Type))
      then
         if Ada_Version = Ada_83 and then Comes_From_Source (Indic) then
            Error_Msg_N
              ("(Ada 83): premature use of type for derivation", Indic);
         end if;
      end if;

      --  Check for early use of incomplete or private type

      if Ekind (Parent_Type) = E_Void
        or else Ekind (Parent_Type) = E_Incomplete_Type
      then
         Error_Msg_N ("premature derivation of incomplete type", Indic);
         return;

      elsif (Is_Incomplete_Or_Private_Type (Parent_Type)
              and then not Comes_From_Generic (Parent_Type))
        or else Has_Private_Component (Parent_Type)
      then
         --  The ancestor type of a formal type can be incomplete, in which
         --  case only the operations of the partial view are available in
         --  the generic. Subsequent checks may be required when the full
         --  view is analyzed, to verify that derivation from a tagged type
         --  has an extension.

         if Nkind (Original_Node (N)) = N_Formal_Type_Declaration then
            null;

         elsif No (Underlying_Type (Parent_Type))
           or else Has_Private_Component (Parent_Type)
         then
            Error_Msg_N
              ("premature derivation of derived or private type", Indic);

            --  Flag the type itself as being in error, this prevents some
            --  nasty problems with subsequent uses of the malformed type.

            Set_Error_Posted (T);

         --  Check that within the immediate scope of an untagged partial
         --  view it's illegal to derive from the partial view if the
         --  full view is tagged. (7.3(7))

         --  We verify that the Parent_Type is a partial view by checking
         --  that it is not a Full_Type_Declaration (i.e. a private type or
         --  private extension declaration), to distinguish a partial view
         --  from  a derivation from a private type which also appears as
         --  E_Private_Type.

         elsif Present (Full_View (Parent_Type))
           and then Nkind (Parent (Parent_Type)) /= N_Full_Type_Declaration
           and then not Is_Tagged_Type (Parent_Type)
           and then Is_Tagged_Type (Full_View (Parent_Type))
         then
            Parent_Scope := Scope (T);
            while Present (Parent_Scope)
              and then Parent_Scope /= Standard_Standard
            loop
               if Parent_Scope = Scope (Parent_Type) then
                  Error_Msg_N
                    ("premature derivation from type with tagged full view",
                     Indic);
               end if;

               Parent_Scope := Scope (Parent_Scope);
            end loop;
         end if;
      end if;

      --  Check that form of derivation is appropriate

      Taggd := Is_Tagged_Type (Parent_Type);

      --  Perhaps the parent type should be changed to the class-wide type's
      --  specific type in this case to prevent cascading errors ???

      if Present (Extension) and then Is_Class_Wide_Type (Parent_Type) then
         Error_Msg_N ("parent type must not be a class-wide type", Indic);
         return;
      end if;

      if Present (Extension) and then not Taggd then
         Error_Msg_N
           ("type derived from untagged type cannot have extension", Indic);

      elsif No (Extension) and then Taggd then

         --  If this declaration is within a private part (or body) of a
         --  generic instantiation then the derivation is allowed (the parent
         --  type can only appear tagged in this case if it's a generic actual
         --  type, since it would otherwise have been rejected in the analysis
         --  of the generic template).

         if not Is_Generic_Actual_Type (Parent_Type)
           or else In_Visible_Part (Scope (Parent_Type))
         then
            Error_Msg_N
              ("type derived from tagged type must have extension", Indic);
         end if;
      end if;

      Build_Derived_Type (N, Parent_Type, T, Is_Completion);
   end Derived_Type_Declaration;

   ----------------------------------
   -- Enumeration_Type_Declaration --
   ----------------------------------

   procedure Enumeration_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Ev     : Uint;
      L      : Node_Id;
      R_Node : Node_Id;
      B_Node : Node_Id;

   begin
      --  Create identifier node representing lower bound

      B_Node := New_Node (N_Identifier, Sloc (Def));
      L := First (Literals (Def));
      Set_Chars (B_Node, Chars (L));
      Set_Entity (B_Node,  L);
      Set_Etype (B_Node, T);
      Set_Is_Static_Expression (B_Node, True);

      R_Node := New_Node (N_Range, Sloc (Def));
      Set_Low_Bound  (R_Node, B_Node);

      Set_Ekind (T, E_Enumeration_Type);
      Set_First_Literal (T, L);
      Set_Etype (T, T);
      Set_Is_Constrained (T);

      Ev := Uint_0;

      --  Loop through literals of enumeration type setting pos and rep values
      --  except that if the Ekind is already set, then it means that the
      --  literal was already constructed (case of a derived type declaration
      --  and we should not disturb the Pos and Rep values.

      while Present (L) loop
         if Ekind (L) /= E_Enumeration_Literal then
            Set_Ekind (L, E_Enumeration_Literal);
            Set_Enumeration_Pos (L, Ev);
            Set_Enumeration_Rep (L, Ev);
            Set_Is_Known_Valid  (L, True);
         end if;

         Set_Etype (L, T);
         New_Overloaded_Entity (L);
         Generate_Definition (L);
         Set_Convention (L, Convention_Intrinsic);

         if Nkind (L) = N_Defining_Character_Literal then
            Set_Is_Character_Type (T, True);
         end if;

         Ev := Ev + 1;
         Next (L);
      end loop;

      --  Now create a node representing upper bound

      B_Node := New_Node (N_Identifier, Sloc (Def));
      Set_Chars (B_Node, Chars (Last (Literals (Def))));
      Set_Entity (B_Node,  Last (Literals (Def)));
      Set_Etype (B_Node, T);
      Set_Is_Static_Expression (B_Node, True);

      Set_High_Bound (R_Node, B_Node);
      Set_Scalar_Range (T, R_Node);
      Set_RM_Size (T, UI_From_Int (Minimum_Size (T)));
      Set_Enum_Esize (T);

      --  Set Discard_Names if configuration pragma set, or if there is
      --  a parameterless pragma in the current declarative region

      if Global_Discard_Names
        or else Discard_Names (Scope (T))
      then
         Set_Discard_Names (T);
      end if;

      --  Process end label if there is one

      if Present (Def) then
         Process_End_Label (Def, 'e', T);
      end if;
   end Enumeration_Type_Declaration;

   ---------------------------------
   -- Expand_To_Stored_Constraint --
   ---------------------------------

   function Expand_To_Stored_Constraint
     (Typ        : Entity_Id;
      Constraint : Elist_Id) return Elist_Id
   is
      Explicitly_Discriminated_Type : Entity_Id;
      Expansion    : Elist_Id;
      Discriminant : Entity_Id;

      function Type_With_Explicit_Discrims (Id : Entity_Id) return Entity_Id;
      --  Find the nearest type that actually specifies discriminants

      ---------------------------------
      -- Type_With_Explicit_Discrims --
      ---------------------------------

      function Type_With_Explicit_Discrims (Id : Entity_Id) return Entity_Id is
         Typ : constant E := Base_Type (Id);

      begin
         if Ekind (Typ) in Incomplete_Or_Private_Kind then
            if Present (Full_View (Typ)) then
               return Type_With_Explicit_Discrims (Full_View (Typ));
            end if;

         else
            if Has_Discriminants (Typ) then
               return Typ;
            end if;
         end if;

         if Etype (Typ) = Typ then
            return Empty;
         elsif Has_Discriminants (Typ) then
            return Typ;
         else
            return Type_With_Explicit_Discrims (Etype (Typ));
         end if;

      end Type_With_Explicit_Discrims;

   --  Start of processing for Expand_To_Stored_Constraint

   begin
      if No (Constraint)
        or else Is_Empty_Elmt_List (Constraint)
      then
         return No_Elist;
      end if;

      Explicitly_Discriminated_Type := Type_With_Explicit_Discrims (Typ);

      if No (Explicitly_Discriminated_Type) then
         return No_Elist;
      end if;

      Expansion := New_Elmt_List;

      Discriminant :=
         First_Stored_Discriminant (Explicitly_Discriminated_Type);
      while Present (Discriminant) loop
         Append_Elmt (
           Get_Discriminant_Value (
             Discriminant, Explicitly_Discriminated_Type, Constraint),
           Expansion);
         Next_Stored_Discriminant (Discriminant);
      end loop;

      return Expansion;
   end Expand_To_Stored_Constraint;

   --------------------
   -- Find_Type_Name --
   --------------------

   function Find_Type_Name (N : Node_Id) return Entity_Id is
      Id       : constant Entity_Id := Defining_Identifier (N);
      Prev     : Entity_Id;
      New_Id   : Entity_Id;
      Prev_Par : Node_Id;

   begin
      --  Find incomplete declaration, if one was given

      Prev := Current_Entity_In_Scope (Id);

      if Present (Prev) then

         --  Previous declaration exists. Error if not incomplete/private case
         --  except if previous declaration is implicit, etc. Enter_Name will
         --  emit error if appropriate.

         Prev_Par := Parent (Prev);

         if not Is_Incomplete_Or_Private_Type (Prev) then
            Enter_Name (Id);
            New_Id := Id;

         elsif Nkind (N) /= N_Full_Type_Declaration
           and then Nkind (N) /= N_Task_Type_Declaration
           and then Nkind (N) /= N_Protected_Type_Declaration
         then
            --  Completion must be a full type declarations (RM 7.3(4))

            Error_Msg_Sloc := Sloc (Prev);
            Error_Msg_NE ("invalid completion of }", Id, Prev);

            --  Set scope of Id to avoid cascaded errors. Entity is never
            --  examined again, except when saving globals in generics.

            Set_Scope (Id, Current_Scope);
            New_Id := Id;

         --  Case of full declaration of incomplete type

         elsif Ekind (Prev) = E_Incomplete_Type then

            --  Indicate that the incomplete declaration has a matching full
            --  declaration. The defining occurrence of the incomplete
            --  declaration remains the visible one, and the procedure
            --  Get_Full_View dereferences it whenever the type is used.

            if Present (Full_View (Prev)) then
               Error_Msg_NE ("invalid redeclaration of }", Id, Prev);
            end if;

            Set_Full_View (Prev,  Id);
            Append_Entity (Id, Current_Scope);
            Set_Is_Public (Id, Is_Public (Prev));
            Set_Is_Internal (Id);
            New_Id := Prev;

         --  Case of full declaration of private type

         else
            if Nkind (Parent (Prev)) /= N_Private_Extension_Declaration then
               if Etype (Prev) /= Prev then

                  --  Prev is a private subtype or a derived type, and needs
                  --  no completion.

                  Error_Msg_NE ("invalid redeclaration of }", Id, Prev);
                  New_Id := Id;

               elsif Ekind (Prev) = E_Private_Type
                 and then
                   (Nkind (N) = N_Task_Type_Declaration
                     or else Nkind (N) = N_Protected_Type_Declaration)
               then
                  Error_Msg_N
                   ("completion of nonlimited type cannot be limited", N);
               end if;

            --  Ada 2005 (AI-251): Private extension declaration of a
            --  task type. This case arises with tasks implementing interfaces

            elsif Nkind (N) = N_Task_Type_Declaration
              or else Nkind (N) = N_Protected_Type_Declaration
            then
               null;

            elsif Nkind (N) /= N_Full_Type_Declaration
              or else Nkind (Type_Definition (N)) /= N_Derived_Type_Definition
            then
               Error_Msg_N
                 ("full view of private extension must be an extension", N);

            elsif not (Abstract_Present (Parent (Prev)))
              and then Abstract_Present (Type_Definition (N))
            then
               Error_Msg_N
                 ("full view of non-abstract extension cannot be abstract", N);
            end if;

            if not In_Private_Part (Current_Scope) then
               Error_Msg_N
                 ("declaration of full view must appear in private part", N);
            end if;

            Copy_And_Swap (Prev, Id);
            Set_Has_Private_Declaration (Prev);
            Set_Has_Private_Declaration (Id);

            --  If no error, propagate freeze_node from private to full view.
            --  It may have been generated for an early operational item.

            if Present (Freeze_Node (Id))
              and then Serious_Errors_Detected = 0
              and then No (Full_View (Id))
            then
               Set_Freeze_Node (Prev, Freeze_Node (Id));
               Set_Freeze_Node (Id, Empty);
               Set_First_Rep_Item (Prev, First_Rep_Item (Id));
            end if;

            Set_Full_View (Id, Prev);
            New_Id := Prev;
         end if;

         --  Verify that full declaration conforms to incomplete one

         if Is_Incomplete_Or_Private_Type (Prev)
           and then Present (Discriminant_Specifications (Prev_Par))
         then
            if Present (Discriminant_Specifications (N)) then
               if Ekind (Prev) = E_Incomplete_Type then
                  Check_Discriminant_Conformance (N, Prev, Prev);
               else
                  Check_Discriminant_Conformance (N, Prev, Id);
               end if;

            else
               Error_Msg_N
                 ("missing discriminants in full type declaration", N);

               --  To avoid cascaded errors on subsequent use, share the
               --  discriminants of the partial view.

               Set_Discriminant_Specifications (N,
                 Discriminant_Specifications (Prev_Par));
            end if;
         end if;

         --  A prior untagged private type can have an associated class-wide
         --  type due to use of the class attribute, and in this case also the
         --  full type is required to be tagged.

         if Is_Type (Prev)
           and then (Is_Tagged_Type (Prev)
                      or else Present (Class_Wide_Type (Prev)))
           and then (Nkind (N) /= N_Task_Type_Declaration
                      and then Nkind (N) /= N_Protected_Type_Declaration)
         then
            --  The full declaration is either a tagged record or an
            --  extension otherwise this is an error

            if Nkind (Type_Definition (N)) = N_Record_Definition then
               if not Tagged_Present (Type_Definition (N)) then
                  Error_Msg_NE
                    ("full declaration of } must be tagged", Prev, Id);
                  Set_Is_Tagged_Type (Id);
                  Set_Primitive_Operations (Id, New_Elmt_List);
               end if;

            elsif Nkind (Type_Definition (N)) = N_Derived_Type_Definition then
               if No (Record_Extension_Part (Type_Definition (N))) then
                  Error_Msg_NE (
                    "full declaration of } must be a record extension",
                    Prev, Id);
                  Set_Is_Tagged_Type (Id);
                  Set_Primitive_Operations (Id, New_Elmt_List);
               end if;

            else
               Error_Msg_NE
                 ("full declaration of } must be a tagged type", Prev, Id);

            end if;
         end if;

         return New_Id;

      else
         --  New type declaration

         Enter_Name (Id);
         return Id;
      end if;
   end Find_Type_Name;

   -------------------------
   -- Find_Type_Of_Object --
   -------------------------

   function Find_Type_Of_Object
     (Obj_Def     : Node_Id;
      Related_Nod : Node_Id) return Entity_Id
   is
      Def_Kind : constant Node_Kind := Nkind (Obj_Def);
      P        : Node_Id := Parent (Obj_Def);
      T        : Entity_Id;
      Nam      : Name_Id;

   begin
      --  If the parent is a component_definition node we climb to the
      --  component_declaration node

      if Nkind (P) = N_Component_Definition then
         P := Parent (P);
      end if;

      --  Case of an anonymous array subtype

      if Def_Kind = N_Constrained_Array_Definition
        or else Def_Kind = N_Unconstrained_Array_Definition
      then
         T := Empty;
         Array_Type_Declaration (T, Obj_Def);

      --  Create an explicit subtype whenever possible

      elsif Nkind (P) /= N_Component_Declaration
        and then Def_Kind = N_Subtype_Indication
      then
         --  Base name of subtype on object name, which will be unique in
         --  the current scope.

         --  If this is a duplicate declaration, return base type, to avoid
         --  generating duplicate anonymous types.

         if Error_Posted (P) then
            Analyze (Subtype_Mark (Obj_Def));
            return Entity (Subtype_Mark (Obj_Def));
         end if;

         Nam :=
            New_External_Name
             (Chars (Defining_Identifier (Related_Nod)), 'S', 0, 'T');

         T := Make_Defining_Identifier (Sloc (P), Nam);

         Insert_Action (Obj_Def,
           Make_Subtype_Declaration (Sloc (P),
             Defining_Identifier => T,
             Subtype_Indication  => Relocate_Node (Obj_Def)));

         --  This subtype may need freezing, and this will not be done
         --  automatically if the object declaration is not in declarative
         --  part. Since this is an object declaration, the type cannot always
         --  be frozen here. Deferred constants do not freeze their type
         --  (which often enough will be private).

         if Nkind (P) = N_Object_Declaration
           and then Constant_Present (P)
           and then No (Expression (P))
         then
            null;
         else
            Insert_Actions (Obj_Def, Freeze_Entity (T, Sloc (P)));
         end if;

      --  Ada 2005 AI-406: the object definition in an object declaration
      --  can be an access definition.

      elsif Def_Kind = N_Access_Definition then
         T := Access_Definition (Related_Nod, Obj_Def);
         Set_Is_Local_Anonymous_Access (T);

      --  comment here, what cases ???

      else
         T := Process_Subtype (Obj_Def, Related_Nod);
      end if;

      return T;
   end Find_Type_Of_Object;

   --------------------------------
   -- Find_Type_Of_Subtype_Indic --
   --------------------------------

   function Find_Type_Of_Subtype_Indic (S : Node_Id) return Entity_Id is
      Typ : Entity_Id;

   begin
      --  Case of subtype mark with a constraint

      if Nkind (S) = N_Subtype_Indication then
         Find_Type (Subtype_Mark (S));
         Typ := Entity (Subtype_Mark (S));

         if not
           Is_Valid_Constraint_Kind (Ekind (Typ), Nkind (Constraint (S)))
         then
            Error_Msg_N
              ("incorrect constraint for this kind of type", Constraint (S));
            Rewrite (S, New_Copy_Tree (Subtype_Mark (S)));
         end if;

      --  Otherwise we have a subtype mark without a constraint

      elsif Error_Posted (S) then
         Rewrite (S, New_Occurrence_Of (Any_Id, Sloc (S)));
         return Any_Type;

      else
         Find_Type (S);
         Typ := Entity (S);
      end if;

      if Typ = Standard_Wide_Character
        or else Typ = Standard_Wide_Wide_Character
        or else Typ = Standard_Wide_String
        or else Typ = Standard_Wide_Wide_String
      then
         Check_Restriction (No_Wide_Characters, S);
      end if;

      return Typ;
   end Find_Type_Of_Subtype_Indic;

   -------------------------------------
   -- Floating_Point_Type_Declaration --
   -------------------------------------

   procedure Floating_Point_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Digs          : constant Node_Id := Digits_Expression (Def);
      Digs_Val      : Uint;
      Base_Typ      : Entity_Id;
      Implicit_Base : Entity_Id;
      Bound         : Node_Id;

      function Can_Derive_From (E : Entity_Id) return Boolean;
      --  Find if given digits value allows derivation from specified type

      ---------------------
      -- Can_Derive_From --
      ---------------------

      function Can_Derive_From (E : Entity_Id) return Boolean is
         Spec : constant Entity_Id := Real_Range_Specification (Def);

      begin
         if Digs_Val > Digits_Value (E) then
            return False;
         end if;

         if Present (Spec) then
            if Expr_Value_R (Type_Low_Bound (E)) >
               Expr_Value_R (Low_Bound (Spec))
            then
               return False;
            end if;

            if Expr_Value_R (Type_High_Bound (E)) <
               Expr_Value_R (High_Bound (Spec))
            then
               return False;
            end if;
         end if;

         return True;
      end Can_Derive_From;

   --  Start of processing for Floating_Point_Type_Declaration

   begin
      Check_Restriction (No_Floating_Point, Def);

      --  Create an implicit base type

      Implicit_Base :=
        Create_Itype (E_Floating_Point_Type, Parent (Def), T, 'B');

      --  Analyze and verify digits value

      Analyze_And_Resolve (Digs, Any_Integer);
      Check_Digits_Expression (Digs);
      Digs_Val := Expr_Value (Digs);

      --  Process possible range spec and find correct type to derive from

      Process_Real_Range_Specification (Def);

      if Can_Derive_From (Standard_Short_Float) then
         Base_Typ := Standard_Short_Float;
      elsif Can_Derive_From (Standard_Float) then
         Base_Typ := Standard_Float;
      elsif Can_Derive_From (Standard_Long_Float) then
         Base_Typ := Standard_Long_Float;
      elsif Can_Derive_From (Standard_Long_Long_Float) then
         Base_Typ := Standard_Long_Long_Float;

      --  If we can't derive from any existing type, use long_long_float
      --  and give appropriate message explaining the problem.

      else
         Base_Typ := Standard_Long_Long_Float;

         if Digs_Val >= Digits_Value (Standard_Long_Long_Float) then
            Error_Msg_Uint_1 := Digits_Value (Standard_Long_Long_Float);
            Error_Msg_N ("digits value out of range, maximum is ^", Digs);

         else
            Error_Msg_N
              ("range too large for any predefined type",
               Real_Range_Specification (Def));
         end if;
      end if;

      --  If there are bounds given in the declaration use them as the bounds
      --  of the type, otherwise use the bounds of the predefined base type
      --  that was chosen based on the Digits value.

      if Present (Real_Range_Specification (Def)) then
         Set_Scalar_Range (T, Real_Range_Specification (Def));
         Set_Is_Constrained (T);

         --  The bounds of this range must be converted to machine numbers
         --  in accordance with RM 4.9(38).

         Bound := Type_Low_Bound (T);

         if Nkind (Bound) = N_Real_Literal then
            Set_Realval
              (Bound, Machine (Base_Typ, Realval (Bound), Round, Bound));
            Set_Is_Machine_Number (Bound);
         end if;

         Bound := Type_High_Bound (T);

         if Nkind (Bound) = N_Real_Literal then
            Set_Realval
              (Bound, Machine (Base_Typ, Realval (Bound), Round, Bound));
            Set_Is_Machine_Number (Bound);
         end if;

      else
         Set_Scalar_Range (T, Scalar_Range (Base_Typ));
      end if;

      --  Complete definition of implicit base and declared first subtype

      Set_Etype          (Implicit_Base, Base_Typ);

      Set_Scalar_Range   (Implicit_Base, Scalar_Range   (Base_Typ));
      Set_Size_Info      (Implicit_Base,                (Base_Typ));
      Set_RM_Size        (Implicit_Base, RM_Size        (Base_Typ));
      Set_First_Rep_Item (Implicit_Base, First_Rep_Item (Base_Typ));
      Set_Digits_Value   (Implicit_Base, Digits_Value   (Base_Typ));
      Set_Vax_Float      (Implicit_Base, Vax_Float      (Base_Typ));

      Set_Ekind          (T, E_Floating_Point_Subtype);
      Set_Etype          (T, Implicit_Base);

      Set_Size_Info      (T,                (Implicit_Base));
      Set_RM_Size        (T, RM_Size        (Implicit_Base));
      Set_First_Rep_Item (T, First_Rep_Item (Implicit_Base));
      Set_Digits_Value   (T, Digs_Val);
   end Floating_Point_Type_Declaration;

   ----------------------------
   -- Get_Discriminant_Value --
   ----------------------------

   --  This is the situation:

   --  There is a non-derived type

   --       type T0 (Dx, Dy, Dz...)

   --  There are zero or more levels of derivation, with each derivation
   --  either purely inheriting the discriminants, or defining its own.

   --       type Ti      is new Ti-1
   --  or
   --       type Ti (Dw) is new Ti-1(Dw, 1, X+Y)
   --  or
   --       subtype Ti is ...

   --  The subtype issue is avoided by the use of Original_Record_Component,
   --  and the fact that derived subtypes also derive the constraints.

   --  This chain leads back from

   --       Typ_For_Constraint

   --  Typ_For_Constraint has discriminants, and the value for each
   --  discriminant is given by its corresponding Elmt of Constraints.

   --  Discriminant is some discriminant in this hierarchy

   --  We need to return its value

   --  We do this by recursively searching each level, and looking for
   --  Discriminant. Once we get to the bottom, we start backing up
   --  returning the value for it which may in turn be a discriminant
   --  further up, so on the backup we continue the substitution.

   function Get_Discriminant_Value
     (Discriminant       : Entity_Id;
      Typ_For_Constraint : Entity_Id;
      Constraint         : Elist_Id) return Node_Id
   is
      function Search_Derivation_Levels
        (Ti                    : Entity_Id;
         Discrim_Values        : Elist_Id;
         Stored_Discrim_Values : Boolean) return Node_Or_Entity_Id;
      --  This is the routine that performs the recursive search of levels
      --  as described above.

      ------------------------------
      -- Search_Derivation_Levels --
      ------------------------------

      function Search_Derivation_Levels
        (Ti                    : Entity_Id;
         Discrim_Values        : Elist_Id;
         Stored_Discrim_Values : Boolean) return Node_Or_Entity_Id
      is
         Assoc          : Elmt_Id;
         Disc           : Entity_Id;
         Result         : Node_Or_Entity_Id;
         Result_Entity  : Node_Id;

      begin
         --  If inappropriate type, return Error, this happens only in
         --  cascaded error situations, and we want to avoid a blow up.

         if not Is_Composite_Type (Ti) or else Is_Array_Type (Ti) then
            return Error;
         end if;

         --  Look deeper if possible. Use Stored_Constraints only for
         --  untagged types. For tagged types use the given constraint.
         --  This asymmetry needs explanation???

         if not Stored_Discrim_Values
           and then Present (Stored_Constraint (Ti))
           and then not Is_Tagged_Type (Ti)
         then
            Result :=
              Search_Derivation_Levels (Ti, Stored_Constraint (Ti), True);
         else
            declare
               Td : constant Entity_Id := Etype (Ti);

            begin
               if Td = Ti then
                  Result := Discriminant;

               else
                  if Present (Stored_Constraint (Ti)) then
                     Result :=
                        Search_Derivation_Levels
                          (Td, Stored_Constraint (Ti), True);
                  else
                     Result :=
                        Search_Derivation_Levels
                          (Td, Discrim_Values, Stored_Discrim_Values);
                  end if;
               end if;
            end;
         end if;

         --  Extra underlying places to search, if not found above. For
         --  concurrent types, the relevant discriminant appears in the
         --  corresponding record. For a type derived from a private type
         --  without discriminant, the full view inherits the discriminants
         --  of the full view of the parent.

         if Result = Discriminant then
            if Is_Concurrent_Type (Ti)
              and then Present (Corresponding_Record_Type (Ti))
            then
               Result :=
                 Search_Derivation_Levels (
                   Corresponding_Record_Type (Ti),
                   Discrim_Values,
                   Stored_Discrim_Values);

            elsif Is_Private_Type (Ti)
              and then not Has_Discriminants (Ti)
              and then Present (Full_View (Ti))
              and then Etype (Full_View (Ti)) /= Ti
            then
               Result :=
                 Search_Derivation_Levels (
                   Full_View (Ti),
                   Discrim_Values,
                   Stored_Discrim_Values);
            end if;
         end if;

         --  If Result is not a (reference to a) discriminant, return it,
         --  otherwise set Result_Entity to the discriminant.

         if Nkind (Result) = N_Defining_Identifier then
            pragma Assert (Result = Discriminant);
            Result_Entity := Result;

         else
            if not Denotes_Discriminant (Result) then
               return Result;
            end if;

            Result_Entity := Entity (Result);
         end if;

         --  See if this level of derivation actually has discriminants
         --  because tagged derivations can add them, hence the lower
         --  levels need not have any.

         if not Has_Discriminants (Ti) then
            return Result;
         end if;

         --  Scan Ti's discriminants for Result_Entity,
         --  and return its corresponding value, if any.

         Result_Entity := Original_Record_Component (Result_Entity);

         Assoc := First_Elmt (Discrim_Values);

         if Stored_Discrim_Values then
            Disc := First_Stored_Discriminant (Ti);
         else
            Disc := First_Discriminant (Ti);
         end if;

         while Present (Disc) loop
            pragma Assert (Present (Assoc));

            if Original_Record_Component (Disc) = Result_Entity then
               return Node (Assoc);
            end if;

            Next_Elmt (Assoc);

            if Stored_Discrim_Values then
               Next_Stored_Discriminant (Disc);
            else
               Next_Discriminant (Disc);
            end if;
         end loop;

         --  Could not find it
         --
         return Result;
      end Search_Derivation_Levels;

      Result : Node_Or_Entity_Id;

   --  Start of processing for Get_Discriminant_Value

   begin
      --  ??? This routine is a gigantic mess and will be deleted. For the
      --  time being just test for the trivial case before calling recurse.

      if Base_Type (Scope (Discriminant)) = Base_Type (Typ_For_Constraint) then
         declare
            D : Entity_Id;
            E : Elmt_Id;

         begin
            D := First_Discriminant (Typ_For_Constraint);
            E := First_Elmt (Constraint);
            while Present (D) loop
               if Chars (D) = Chars (Discriminant) then
                  return Node (E);
               end if;

               Next_Discriminant (D);
               Next_Elmt (E);
            end loop;
         end;
      end if;

      Result := Search_Derivation_Levels
        (Typ_For_Constraint, Constraint, False);

      --  ??? hack to disappear when this routine is gone

      if  Nkind (Result) = N_Defining_Identifier then
         declare
            D : Entity_Id;
            E : Elmt_Id;

         begin
            D := First_Discriminant (Typ_For_Constraint);
            E := First_Elmt (Constraint);
            while Present (D) loop
               if Corresponding_Discriminant (D) = Discriminant then
                  return Node (E);
               end if;

               Next_Discriminant (D);
               Next_Elmt (E);
            end loop;
         end;
      end if;

      pragma Assert (Nkind (Result) /= N_Defining_Identifier);
      return Result;
   end Get_Discriminant_Value;

   --------------------------
   -- Has_Range_Constraint --
   --------------------------

   function Has_Range_Constraint (N : Node_Id) return Boolean is
      C : constant Node_Id := Constraint (N);

   begin
      if Nkind (C) = N_Range_Constraint then
         return True;

      elsif Nkind (C) = N_Digits_Constraint then
         return
            Is_Decimal_Fixed_Point_Type (Entity (Subtype_Mark (N)))
              or else
            Present (Range_Constraint (C));

      elsif Nkind (C) = N_Delta_Constraint then
         return Present (Range_Constraint (C));

      else
         return False;
      end if;
   end Has_Range_Constraint;

   ------------------------
   -- Inherit_Components --
   ------------------------

   function Inherit_Components
     (N             : Node_Id;
      Parent_Base   : Entity_Id;
      Derived_Base  : Entity_Id;
      Is_Tagged     : Boolean;
      Inherit_Discr : Boolean;
      Discs         : Elist_Id) return Elist_Id
   is
      Assoc_List : constant Elist_Id := New_Elmt_List;

      procedure Inherit_Component
        (Old_C          : Entity_Id;
         Plain_Discrim  : Boolean := False;
         Stored_Discrim : Boolean := False);
      --  Inherits component Old_C from Parent_Base to the Derived_Base. If
      --  Plain_Discrim is True, Old_C is a discriminant. If Stored_Discrim is
      --  True, Old_C is a stored discriminant. If they are both false then
      --  Old_C is a regular component.

      -----------------------
      -- Inherit_Component --
      -----------------------

      procedure Inherit_Component
        (Old_C          : Entity_Id;
         Plain_Discrim  : Boolean := False;
         Stored_Discrim : Boolean := False)
      is
         New_C : constant Entity_Id := New_Copy (Old_C);

         Discrim      : Entity_Id;
         Corr_Discrim : Entity_Id;

      begin
         pragma Assert (not Is_Tagged or else not Stored_Discrim);

         Set_Parent (New_C, Parent (Old_C));

         --  Regular discriminants and components must be inserted
         --  in the scope of the Derived_Base. Do it here.

         if not Stored_Discrim then
            Enter_Name (New_C);
         end if;

         --  For tagged types the Original_Record_Component must point to
         --  whatever this field was pointing to in the parent type. This has
         --  already been achieved by the call to New_Copy above.

         if not Is_Tagged then
            Set_Original_Record_Component (New_C, New_C);
         end if;

         --  If we have inherited a component then see if its Etype contains
         --  references to Parent_Base discriminants. In this case, replace
         --  these references with the constraints given in Discs. We do not
         --  do this for the partial view of private types because this is
         --  not needed (only the components of the full view will be used
         --  for code generation) and cause problem. We also avoid this
         --  transformation in some error situations.

         if Ekind (New_C) = E_Component then
            if (Is_Private_Type (Derived_Base)
                  and then not Is_Generic_Type (Derived_Base))
              or else (Is_Empty_Elmt_List (Discs)
                         and then  not Expander_Active)
            then
               Set_Etype (New_C, Etype (Old_C));
            else
               Set_Etype
                 (New_C,
                  Constrain_Component_Type
                    (Old_C, Derived_Base, N, Parent_Base, Discs));
            end if;
         end if;

         --  In derived tagged types it is illegal to reference a non
         --  discriminant component in the parent type. To catch this, mark
         --  these components with an Ekind of E_Void. This will be reset in
         --  Record_Type_Definition after processing the record extension of
         --  the derived type.

         if Is_Tagged and then Ekind (New_C) = E_Component then
            Set_Ekind (New_C, E_Void);
         end if;

         if Plain_Discrim then
            Set_Corresponding_Discriminant (New_C, Old_C);
            Build_Discriminal (New_C);

         --  If we are explicitly inheriting a stored discriminant it will be
         --  completely hidden.

         elsif Stored_Discrim then
            Set_Corresponding_Discriminant (New_C, Empty);
            Set_Discriminal (New_C, Empty);
            Set_Is_Completely_Hidden (New_C);

            --  Set the Original_Record_Component of each discriminant in the
            --  derived base to point to the corresponding stored that we just
            --  created.

            Discrim := First_Discriminant (Derived_Base);
            while Present (Discrim) loop
               Corr_Discrim := Corresponding_Discriminant (Discrim);

               --  Corr_Discrim could be missing in an error situation

               if Present (Corr_Discrim)
                 and then Original_Record_Component (Corr_Discrim) = Old_C
               then
                  Set_Original_Record_Component (Discrim, New_C);
               end if;

               Next_Discriminant (Discrim);
            end loop;

            Append_Entity (New_C, Derived_Base);
         end if;

         if not Is_Tagged then
            Append_Elmt (Old_C, Assoc_List);
            Append_Elmt (New_C, Assoc_List);
         end if;
      end Inherit_Component;

      --  Variables local to Inherit_Component

      Loc : constant Source_Ptr := Sloc (N);

      Parent_Discrim : Entity_Id;
      Stored_Discrim : Entity_Id;
      D              : Entity_Id;
      Component      : Entity_Id;

   --  Start of processing for Inherit_Components

   begin
      if not Is_Tagged then
         Append_Elmt (Parent_Base,  Assoc_List);
         Append_Elmt (Derived_Base, Assoc_List);
      end if;

      --  Inherit parent discriminants if needed

      if Inherit_Discr then
         Parent_Discrim := First_Discriminant (Parent_Base);
         while Present (Parent_Discrim) loop
            Inherit_Component (Parent_Discrim, Plain_Discrim => True);
            Next_Discriminant (Parent_Discrim);
         end loop;
      end if;

      --  Create explicit stored discrims for untagged types when necessary

      if not Has_Unknown_Discriminants (Derived_Base)
        and then Has_Discriminants (Parent_Base)
        and then not Is_Tagged
        and then
          (not Inherit_Discr
             or else First_Discriminant (Parent_Base) /=
                     First_Stored_Discriminant (Parent_Base))
      then
         Stored_Discrim := First_Stored_Discriminant (Parent_Base);
         while Present (Stored_Discrim) loop
            Inherit_Component (Stored_Discrim, Stored_Discrim => True);
            Next_Stored_Discriminant (Stored_Discrim);
         end loop;
      end if;

      --  See if we can apply the second transformation for derived types, as
      --  explained in point 6. in the comments above Build_Derived_Record_Type
      --  This is achieved by appending Derived_Base discriminants into Discs,
      --  which has the side effect of returning a non empty Discs list to the
      --  caller of Inherit_Components, which is what we want. This must be
      --  done for private derived types if there are explicit stored
      --  discriminants, to ensure that we can retrieve the values of the
      --  constraints provided in the ancestors.

      if Inherit_Discr
        and then Is_Empty_Elmt_List (Discs)
        and then Present (First_Discriminant (Derived_Base))
        and then
          (not Is_Private_Type (Derived_Base)
             or else Is_Completely_Hidden
               (First_Stored_Discriminant (Derived_Base))
             or else Is_Generic_Type (Derived_Base))
      then
         D := First_Discriminant (Derived_Base);
         while Present (D) loop
            Append_Elmt (New_Reference_To (D, Loc), Discs);
            Next_Discriminant (D);
         end loop;
      end if;

      --  Finally, inherit non-discriminant components unless they are not
      --  visible because defined or inherited from the full view of the
      --  parent. Don't inherit the _parent field of the parent type.

      Component := First_Entity (Parent_Base);
      while Present (Component) loop

         --  Ada 2005 (AI-251): Do not inherit tags corresponding with the
         --  interfaces of the parent

         if Ekind (Component) = E_Component
           and then Is_Tag (Component)
           and then Etype  (Component) = RTE (RE_Interface_Tag)
         then
            null;

         elsif Ekind (Component) /= E_Component
           or else Chars (Component) = Name_uParent
         then
            null;

         --  If the derived type is within the parent type's declarative
         --  region, then the components can still be inherited even though
         --  they aren't visible at this point. This can occur for cases
         --  such as within public child units where the components must
         --  become visible upon entering the child unit's private part.

         elsif not Is_Visible_Component (Component)
           and then not In_Open_Scopes (Scope (Parent_Base))
         then
            null;

         elsif Ekind (Derived_Base) = E_Private_Type
           or else Ekind (Derived_Base) = E_Limited_Private_Type
         then
            null;

         else
            Inherit_Component (Component);
         end if;

         Next_Entity (Component);
      end loop;

      --  For tagged derived types, inherited discriminants cannot be used in
      --  component declarations of the record extension part. To achieve this
      --  we mark the inherited discriminants as not visible.

      if Is_Tagged and then Inherit_Discr then
         D := First_Discriminant (Derived_Base);
         while Present (D) loop
            Set_Is_Immediately_Visible (D, False);
            Next_Discriminant (D);
         end loop;
      end if;

      return Assoc_List;
   end Inherit_Components;

   ------------------------------
   -- Is_Valid_Constraint_Kind --
   ------------------------------

   function Is_Valid_Constraint_Kind
     (T_Kind          : Type_Kind;
      Constraint_Kind : Node_Kind) return Boolean
   is
   begin
      case T_Kind is
         when Enumeration_Kind |
              Integer_Kind =>
            return Constraint_Kind = N_Range_Constraint;

         when Decimal_Fixed_Point_Kind =>
            return
              Constraint_Kind = N_Digits_Constraint
                or else
              Constraint_Kind = N_Range_Constraint;

         when Ordinary_Fixed_Point_Kind =>
            return
              Constraint_Kind = N_Delta_Constraint
                or else
              Constraint_Kind = N_Range_Constraint;

         when Float_Kind =>
            return
              Constraint_Kind = N_Digits_Constraint
                or else
              Constraint_Kind = N_Range_Constraint;

         when Access_Kind       |
              Array_Kind        |
              E_Record_Type     |
              E_Record_Subtype  |
              Class_Wide_Kind   |
              E_Incomplete_Type |
              Private_Kind      |
              Concurrent_Kind  =>
            return Constraint_Kind = N_Index_Or_Discriminant_Constraint;

         when others =>
            return True; -- Error will be detected later
      end case;
   end Is_Valid_Constraint_Kind;

   --------------------------
   -- Is_Visible_Component --
   --------------------------

   function Is_Visible_Component (C : Entity_Id) return Boolean is
      Original_Comp  : Entity_Id := Empty;
      Original_Scope : Entity_Id;
      Type_Scope     : Entity_Id;

      function Is_Local_Type (Typ : Entity_Id) return Boolean;
      --  Check whether parent type of inherited component is declared locally,
      --  possibly within a nested package or instance. The current scope is
      --  the derived record itself.

      -------------------
      -- Is_Local_Type --
      -------------------

      function Is_Local_Type (Typ : Entity_Id) return Boolean is
         Scop : Entity_Id;

      begin
         Scop := Scope (Typ);
         while Present (Scop)
           and then Scop /= Standard_Standard
         loop
            if Scop = Scope (Current_Scope) then
               return True;
            end if;

            Scop := Scope (Scop);
         end loop;

         return False;
      end Is_Local_Type;

   --  Start of processing for Is_Visible_Component

   begin
      if Ekind (C) = E_Component
        or else Ekind (C) = E_Discriminant
      then
         Original_Comp := Original_Record_Component (C);
      end if;

      if No (Original_Comp) then

         --  Premature usage, or previous error

         return False;

      else
         Original_Scope := Scope (Original_Comp);
         Type_Scope     := Scope (Base_Type (Scope (C)));
      end if;

      --  This test only concerns tagged types

      if not Is_Tagged_Type (Original_Scope) then
         return True;

      --  If it is _Parent or _Tag, there is no visibility issue

      elsif not Comes_From_Source (Original_Comp) then
         return True;

      --  If we are in the body of an instantiation, the component is visible
      --  even when the parent type (possibly defined in an enclosing unit or
      --  in a parent unit) might not.

      elsif In_Instance_Body then
         return True;

      --  Discriminants are always visible

      elsif Ekind (Original_Comp) = E_Discriminant
        and then not Has_Unknown_Discriminants (Original_Scope)
      then
         return True;

      --  If the component has been declared in an ancestor which is currently
      --  a private type, then it is not visible. The same applies if the
      --  component's containing type is not in an open scope and the original
      --  component's enclosing type is a visible full type of a private type
      --  (which can occur in cases where an attempt is being made to reference
      --  a component in a sibling package that is inherited from a visible
      --  component of a type in an ancestor package; the component in the
      --  sibling package should not be visible even though the component it
      --  inherited from is visible). This does not apply however in the case
      --  where the scope of the type is a private child unit, or when the
      --  parent comes from a local package in which the ancestor is currently
      --  visible. The latter suppression of visibility is needed for cases
      --  that are tested in B730006.

      elsif Is_Private_Type (Original_Scope)
        or else
          (not Is_Private_Descendant (Type_Scope)
            and then not In_Open_Scopes (Type_Scope)
            and then Has_Private_Declaration (Original_Scope))
      then
         --  If the type derives from an entity in a formal package, there
         --  are no additional visible components.

         if Nkind (Original_Node (Unit_Declaration_Node (Type_Scope))) =
            N_Formal_Package_Declaration
         then
            return False;

         --  if we are not in the private part of the current package, there
         --  are no additional visible components.

         elsif Ekind (Scope (Current_Scope)) = E_Package
           and then not In_Private_Part (Scope (Current_Scope))
         then
            return False;
         else
            return
              Is_Child_Unit (Cunit_Entity (Current_Sem_Unit))
                and then Is_Local_Type (Type_Scope);
         end if;

      --  There is another weird way in which a component may be invisible
      --  when the private and the full view are not derived from the same
      --  ancestor. Here is an example :

      --       type A1 is tagged      record F1 : integer; end record;
      --       type A2 is new A1 with record F2 : integer; end record;
      --       type T is new A1 with private;
      --     private
      --       type T is new A2 with null record;

      --  In this case, the full view of T inherits F1 and F2 but the private
      --  view inherits only F1

      else
         declare
            Ancestor : Entity_Id := Scope (C);

         begin
            loop
               if Ancestor = Original_Scope then
                  return True;
               elsif Ancestor = Etype (Ancestor) then
                  return False;
               end if;

               Ancestor := Etype (Ancestor);
            end loop;

            return True;
         end;
      end if;
   end Is_Visible_Component;

   --------------------------
   -- Make_Class_Wide_Type --
   --------------------------

   procedure Make_Class_Wide_Type (T : Entity_Id) is
      CW_Type : Entity_Id;
      CW_Name : Name_Id;
      Next_E  : Entity_Id;

   begin
      --  The class wide type can have been defined by the partial view in
      --  which case everything is already done

      if Present (Class_Wide_Type (T)) then
         return;
      end if;

      CW_Type :=
        New_External_Entity (E_Void, Scope (T), Sloc (T), T, 'C', 0, 'T');

      --  Inherit root type characteristics

      CW_Name := Chars (CW_Type);
      Next_E  := Next_Entity (CW_Type);
      Copy_Node (T, CW_Type);
      Set_Comes_From_Source (CW_Type, False);
      Set_Chars (CW_Type, CW_Name);
      Set_Parent (CW_Type, Parent (T));
      Set_Next_Entity (CW_Type, Next_E);
      Set_Has_Delayed_Freeze (CW_Type);

      --  Customize the class-wide type: It has no prim. op., it cannot be
      --  abstract and its Etype points back to the specific root type.

      Set_Ekind                (CW_Type, E_Class_Wide_Type);
      Set_Is_Tagged_Type       (CW_Type, True);
      Set_Primitive_Operations (CW_Type, New_Elmt_List);
      Set_Is_Abstract          (CW_Type, False);
      Set_Is_Constrained       (CW_Type, False);
      Set_Is_First_Subtype     (CW_Type, Is_First_Subtype (T));
      Init_Size_Align          (CW_Type);

      if Ekind (T) = E_Class_Wide_Subtype then
         Set_Etype             (CW_Type, Etype (Base_Type (T)));
      else
         Set_Etype             (CW_Type, T);
      end if;

      --  If this is the class_wide type of a constrained subtype, it does
      --  not have discriminants.

      Set_Has_Discriminants (CW_Type,
        Has_Discriminants (T) and then not Is_Constrained (T));

      Set_Has_Unknown_Discriminants (CW_Type, True);
      Set_Class_Wide_Type (T, CW_Type);
      Set_Equivalent_Type (CW_Type, Empty);

      --  The class-wide type of a class-wide type is itself (RM 3.9(14))

      Set_Class_Wide_Type (CW_Type, CW_Type);
   end Make_Class_Wide_Type;

   ----------------
   -- Make_Index --
   ----------------

   procedure Make_Index
     (I            : Node_Id;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id := Empty;
      Suffix_Index : Nat := 1)
   is
      R      : Node_Id;
      T      : Entity_Id;
      Def_Id : Entity_Id := Empty;
      Found  : Boolean := False;

   begin
      --  For a discrete range used in a constrained array definition and
      --  defined by a range, an implicit conversion to the predefined type
      --  INTEGER is assumed if each bound is either a numeric literal, a named
      --  number, or an attribute, and the type of both bounds (prior to the
      --  implicit conversion) is the type universal_integer. Otherwise, both
      --  bounds must be of the same discrete type, other than universal
      --  integer; this type must be determinable independently of the
      --  context, but using the fact that the type must be discrete and that
      --  both bounds must have the same type.

      --  Character literals also have a universal type in the absence of
      --  of additional context,  and are resolved to Standard_Character.

      if Nkind (I) = N_Range then

         --  The index is given by a range constraint. The bounds are known
         --  to be of a consistent type.

         if not Is_Overloaded (I) then
            T := Etype (I);

            --  If the bounds are universal, choose the specific predefined
            --  type.

            if T = Universal_Integer then
               T := Standard_Integer;

            elsif T = Any_Character then

               if Ada_Version >= Ada_95 then
                  Error_Msg_N
                    ("ambiguous character literals (could be Wide_Character)",
                      I);
               end if;

               T := Standard_Character;
            end if;

         else
            T := Any_Type;

            declare
               Ind : Interp_Index;
               It  : Interp;

            begin
               Get_First_Interp (I, Ind, It);
               while Present (It.Typ) loop
                  if Is_Discrete_Type (It.Typ) then

                     if Found
                       and then not Covers (It.Typ, T)
                       and then not Covers (T, It.Typ)
                     then
                        Error_Msg_N ("ambiguous bounds in discrete range", I);
                        exit;
                     else
                        T := It.Typ;
                        Found := True;
                     end if;
                  end if;

                  Get_Next_Interp (Ind, It);
               end loop;

               if T = Any_Type then
                  Error_Msg_N ("discrete type required for range", I);
                  Set_Etype (I, Any_Type);
                  return;

               elsif T = Universal_Integer then
                  T := Standard_Integer;
               end if;
            end;
         end if;

         if not Is_Discrete_Type (T) then
            Error_Msg_N ("discrete type required for range", I);
            Set_Etype (I, Any_Type);
            return;
         end if;

         if Nkind (Low_Bound (I)) = N_Attribute_Reference
           and then Attribute_Name (Low_Bound (I)) = Name_First
           and then Is_Entity_Name (Prefix (Low_Bound (I)))
           and then Is_Type (Entity (Prefix (Low_Bound (I))))
           and then Is_Discrete_Type (Entity (Prefix (Low_Bound (I))))
         then
            --  The type of the index will be the type of the prefix, as long
            --  as the upper bound is 'Last of the same type.

            Def_Id := Entity (Prefix (Low_Bound (I)));

            if Nkind (High_Bound (I)) /= N_Attribute_Reference
              or else Attribute_Name (High_Bound (I)) /= Name_Last
              or else not Is_Entity_Name (Prefix (High_Bound (I)))
              or else Entity (Prefix (High_Bound (I))) /= Def_Id
            then
               Def_Id := Empty;
            end if;
         end if;

         R := I;
         Process_Range_Expr_In_Decl (R, T);

      elsif Nkind (I) = N_Subtype_Indication then

         --  The index is given by a subtype with a range constraint

         T :=  Base_Type (Entity (Subtype_Mark (I)));

         if not Is_Discrete_Type (T) then
            Error_Msg_N ("discrete type required for range", I);
            Set_Etype (I, Any_Type);
            return;
         end if;

         R := Range_Expression (Constraint (I));

         Resolve (R, T);
         Process_Range_Expr_In_Decl (R, Entity (Subtype_Mark (I)));

      elsif Nkind (I) = N_Attribute_Reference then

         --  The parser guarantees that the attribute is a RANGE attribute

         --  If the node denotes the range of a type mark, that is also the
         --  resulting type, and we do no need to create an Itype for it.

         if Is_Entity_Name (Prefix (I))
           and then Comes_From_Source (I)
           and then Is_Type (Entity (Prefix (I)))
           and then Is_Discrete_Type (Entity (Prefix (I)))
         then
            Def_Id := Entity (Prefix (I));
         end if;

         Analyze_And_Resolve (I);
         T := Etype (I);
         R := I;

      --  If none of the above, must be a subtype. We convert this to a
      --  range attribute reference because in the case of declared first
      --  named subtypes, the types in the range reference can be different
      --  from the type of the entity. A range attribute normalizes the
      --  reference and obtains the correct types for the bounds.

      --  This transformation is in the nature of an expansion, is only
      --  done if expansion is active. In particular, it is not done on
      --  formal generic types,  because we need to retain the name of the
      --  original index for instantiation purposes.

      else
         if not Is_Entity_Name (I) or else not Is_Type (Entity (I)) then
            Error_Msg_N ("invalid subtype mark in discrete range ", I);
            Set_Etype (I, Any_Integer);
            return;

         else
            --  The type mark may be that of an incomplete type. It is only
            --  now that we can get the full view, previous analysis does
            --  not look specifically for a type mark.

            Set_Entity (I, Get_Full_View (Entity (I)));
            Set_Etype  (I, Entity (I));
            Def_Id := Entity (I);

            if not Is_Discrete_Type (Def_Id) then
               Error_Msg_N ("discrete type required for index", I);
               Set_Etype (I, Any_Type);
               return;
            end if;
         end if;

         if Expander_Active then
            Rewrite (I,
              Make_Attribute_Reference (Sloc (I),
                Attribute_Name => Name_Range,
                Prefix         => Relocate_Node (I)));

            --  The original was a subtype mark that does not freeze. This
            --  means that the rewritten version must not freeze either.

            Set_Must_Not_Freeze (I);
            Set_Must_Not_Freeze (Prefix (I));

            --  Is order critical??? if so, document why, if not
            --  use Analyze_And_Resolve

            Analyze (I);
            T := Etype (I);
            Resolve (I);
            R := I;

         --  If expander is inactive, type is legal, nothing else to construct

         else
            return;
         end if;
      end if;

      if not Is_Discrete_Type (T) then
         Error_Msg_N ("discrete type required for range", I);
         Set_Etype (I, Any_Type);
         return;

      elsif T = Any_Type then
         Set_Etype (I, Any_Type);
         return;
      end if;

      --  We will now create the appropriate Itype to describe the range, but
      --  first a check. If we originally had a subtype, then we just label
      --  the range with this subtype. Not only is there no need to construct
      --  a new subtype, but it is wrong to do so for two reasons:

      --    1. A legality concern, if we have a subtype, it must not freeze,
      --       and the Itype would cause freezing incorrectly

      --    2. An efficiency concern, if we created an Itype, it would not be
      --       recognized as the same type for the purposes of eliminating
      --       checks in some circumstances.

      --  We signal this case by setting the subtype entity in Def_Id

      if No (Def_Id) then
         Def_Id :=
           Create_Itype (E_Void, Related_Nod, Related_Id, 'D', Suffix_Index);
         Set_Etype (Def_Id, Base_Type (T));

         if Is_Signed_Integer_Type (T) then
            Set_Ekind (Def_Id, E_Signed_Integer_Subtype);

         elsif Is_Modular_Integer_Type (T) then
            Set_Ekind (Def_Id, E_Modular_Integer_Subtype);

         else
            Set_Ekind             (Def_Id, E_Enumeration_Subtype);
            Set_Is_Character_Type (Def_Id, Is_Character_Type (T));
            Set_First_Literal     (Def_Id, First_Literal (T));
         end if;

         Set_Size_Info      (Def_Id,                  (T));
         Set_RM_Size        (Def_Id, RM_Size          (T));
         Set_First_Rep_Item (Def_Id, First_Rep_Item   (T));

         Set_Scalar_Range   (Def_Id, R);
         Conditional_Delay  (Def_Id, T);

         --  In the subtype indication case, if the immediate parent of the
         --  new subtype is non-static, then the subtype we create is non-
         --  static, even if its bounds are static.

         if Nkind (I) = N_Subtype_Indication
           and then not Is_Static_Subtype (Entity (Subtype_Mark (I)))
         then
            Set_Is_Non_Static_Subtype (Def_Id);
         end if;
      end if;

      --  Final step is to label the index with this constructed type

      Set_Etype (I, Def_Id);
   end Make_Index;

   ------------------------------
   -- Modular_Type_Declaration --
   ------------------------------

   procedure Modular_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Mod_Expr : constant Node_Id := Expression (Def);
      M_Val    : Uint;

      procedure Set_Modular_Size (Bits : Int);
      --  Sets RM_Size to Bits, and Esize to normal word size above this

      ----------------------
      -- Set_Modular_Size --
      ----------------------

      procedure Set_Modular_Size (Bits : Int) is
      begin
         Set_RM_Size (T, UI_From_Int (Bits));

         if Bits <= 8 then
            Init_Esize (T, 8);

         elsif Bits <= 16 then
            Init_Esize (T, 16);

         elsif Bits <= 32 then
            Init_Esize (T, 32);

         else
            Init_Esize (T, System_Max_Binary_Modulus_Power);
         end if;
      end Set_Modular_Size;

   --  Start of processing for Modular_Type_Declaration

   begin
      Analyze_And_Resolve (Mod_Expr, Any_Integer);
      Set_Etype (T, T);
      Set_Ekind (T, E_Modular_Integer_Type);
      Init_Alignment (T);
      Set_Is_Constrained (T);

      if not Is_OK_Static_Expression (Mod_Expr) then
         Flag_Non_Static_Expr
           ("non-static expression used for modular type bound!", Mod_Expr);
         M_Val := 2 ** System_Max_Binary_Modulus_Power;
      else
         M_Val := Expr_Value (Mod_Expr);
      end if;

      if M_Val < 1 then
         Error_Msg_N ("modulus value must be positive", Mod_Expr);
         M_Val := 2 ** System_Max_Binary_Modulus_Power;
      end if;

      Set_Modulus (T, M_Val);

      --   Create bounds for the modular type based on the modulus given in
      --   the type declaration and then analyze and resolve those bounds.

      Set_Scalar_Range (T,
        Make_Range (Sloc (Mod_Expr),
          Low_Bound  =>
            Make_Integer_Literal (Sloc (Mod_Expr), 0),
          High_Bound =>
            Make_Integer_Literal (Sloc (Mod_Expr), M_Val - 1)));

      --  Properly analyze the literals for the range. We do this manually
      --  because we can't go calling Resolve, since we are resolving these
      --  bounds with the type, and this type is certainly not complete yet!

      Set_Etype (Low_Bound  (Scalar_Range (T)), T);
      Set_Etype (High_Bound (Scalar_Range (T)), T);
      Set_Is_Static_Expression (Low_Bound  (Scalar_Range (T)));
      Set_Is_Static_Expression (High_Bound (Scalar_Range (T)));

      --  Loop through powers of two to find number of bits required

      for Bits in Int range 0 .. System_Max_Binary_Modulus_Power loop

         --  Binary case

         if M_Val = 2 ** Bits then
            Set_Modular_Size (Bits);
            return;

         --  Non-binary case

         elsif M_Val < 2 ** Bits then
            Set_Non_Binary_Modulus (T);

            if Bits > System_Max_Nonbinary_Modulus_Power then
               Error_Msg_Uint_1 :=
                 UI_From_Int (System_Max_Nonbinary_Modulus_Power);
               Error_Msg_N
                 ("nonbinary modulus exceeds limit (2 '*'*^ - 1)", Mod_Expr);
               Set_Modular_Size (System_Max_Binary_Modulus_Power);
               return;

            else
               --  In the non-binary case, set size as per RM 13.3(55)

               Set_Modular_Size (Bits);
               return;
            end if;
         end if;

      end loop;

      --  If we fall through, then the size exceed System.Max_Binary_Modulus
      --  so we just signal an error and set the maximum size.

      Error_Msg_Uint_1 := UI_From_Int (System_Max_Binary_Modulus_Power);
      Error_Msg_N ("modulus exceeds limit (2 '*'*^)", Mod_Expr);

      Set_Modular_Size (System_Max_Binary_Modulus_Power);
      Init_Alignment (T);

   end Modular_Type_Declaration;

   --------------------------
   -- New_Concatenation_Op --
   --------------------------

   procedure New_Concatenation_Op (Typ : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (Typ);
      Op  : Entity_Id;

      function Make_Op_Formal (Typ, Op : Entity_Id) return Entity_Id;
      --  Create abbreviated declaration for the formal of a predefined
      --  Operator 'Op' of type 'Typ'

      --------------------
      -- Make_Op_Formal --
      --------------------

      function Make_Op_Formal (Typ, Op : Entity_Id) return Entity_Id is
         Formal : Entity_Id;
      begin
         Formal := New_Internal_Entity (E_In_Parameter, Op, Loc, 'P');
         Set_Etype (Formal, Typ);
         Set_Mechanism (Formal, Default_Mechanism);
         return Formal;
      end Make_Op_Formal;

   --  Start of processing for New_Concatenation_Op

   begin
      Op := Make_Defining_Operator_Symbol (Loc, Name_Op_Concat);

      Set_Ekind                   (Op, E_Operator);
      Set_Scope                   (Op, Current_Scope);
      Set_Etype                   (Op, Typ);
      Set_Homonym                 (Op, Get_Name_Entity_Id (Name_Op_Concat));
      Set_Is_Immediately_Visible  (Op);
      Set_Is_Intrinsic_Subprogram (Op);
      Set_Has_Completion          (Op);
      Append_Entity               (Op, Current_Scope);

      Set_Name_Entity_Id (Name_Op_Concat, Op);

      Append_Entity (Make_Op_Formal (Typ, Op), Op);
      Append_Entity (Make_Op_Formal (Typ, Op), Op);
   end New_Concatenation_Op;

   -------------------------------------------
   -- Ordinary_Fixed_Point_Type_Declaration --
   -------------------------------------------

   procedure Ordinary_Fixed_Point_Type_Declaration
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Loc           : constant Source_Ptr := Sloc (Def);
      Delta_Expr    : constant Node_Id    := Delta_Expression (Def);
      RRS           : constant Node_Id    := Real_Range_Specification (Def);
      Implicit_Base : Entity_Id;
      Delta_Val     : Ureal;
      Small_Val     : Ureal;
      Low_Val       : Ureal;
      High_Val      : Ureal;

   begin
      Check_Restriction (No_Fixed_Point, Def);

      --  Create implicit base type

      Implicit_Base :=
        Create_Itype (E_Ordinary_Fixed_Point_Type, Parent (Def), T, 'B');
      Set_Etype (Implicit_Base, Implicit_Base);

      --  Analyze and process delta expression

      Analyze_And_Resolve (Delta_Expr, Any_Real);

      Check_Delta_Expression (Delta_Expr);
      Delta_Val := Expr_Value_R (Delta_Expr);

      Set_Delta_Value (Implicit_Base, Delta_Val);

      --  Compute default small from given delta, which is the largest power
      --  of two that does not exceed the given delta value.

      declare
         Tmp   : Ureal;
         Scale : Int;

      begin
         Tmp := Ureal_1;
         Scale := 0;

         if Delta_Val < Ureal_1 then
            while Delta_Val < Tmp loop
               Tmp := Tmp / Ureal_2;
               Scale := Scale + 1;
            end loop;

         else
            loop
               Tmp := Tmp * Ureal_2;
               exit when Tmp > Delta_Val;
               Scale := Scale - 1;
            end loop;
         end if;

         Small_Val := UR_From_Components (Uint_1, UI_From_Int (Scale), 2);
      end;

      Set_Small_Value (Implicit_Base, Small_Val);

      --  If no range was given, set a dummy range

      if RRS <= Empty_Or_Error then
         Low_Val  := -Small_Val;
         High_Val := Small_Val;

      --  Otherwise analyze and process given range

      else
         declare
            Low  : constant Node_Id := Low_Bound  (RRS);
            High : constant Node_Id := High_Bound (RRS);

         begin
            Analyze_And_Resolve (Low, Any_Real);
            Analyze_And_Resolve (High, Any_Real);
            Check_Real_Bound (Low);
            Check_Real_Bound (High);

            --  Obtain and set the range

            Low_Val  := Expr_Value_R (Low);
            High_Val := Expr_Value_R (High);

            if Low_Val > High_Val then
               Error_Msg_NE ("?fixed point type& has null range", Def, T);
            end if;
         end;
      end if;

      --  The range for both the implicit base and the declared first subtype
      --  cannot be set yet, so we use the special routine Set_Fixed_Range to
      --  set a temporary range in place. Note that the bounds of the base
      --  type will be widened to be symmetrical and to fill the available
      --  bits when the type is frozen.

      --  We could do this with all discrete types, and probably should, but
      --  we absolutely have to do it for fixed-point, since the end-points
      --  of the range and the size are determined by the small value, which
      --  could be reset before the freeze point.

      Set_Fixed_Range (Implicit_Base, Loc, Low_Val, High_Val);
      Set_Fixed_Range (T, Loc, Low_Val, High_Val);

      Init_Size_Align (Implicit_Base);

      --  Complete definition of first subtype

      Set_Ekind          (T, E_Ordinary_Fixed_Point_Subtype);
      Set_Etype          (T, Implicit_Base);
      Init_Size_Align    (T);
      Set_First_Rep_Item (T, First_Rep_Item (Implicit_Base));
      Set_Small_Value    (T, Small_Val);
      Set_Delta_Value    (T, Delta_Val);
      Set_Is_Constrained (T);

   end Ordinary_Fixed_Point_Type_Declaration;

   ----------------------------------------
   -- Prepare_Private_Subtype_Completion --
   ----------------------------------------

   procedure Prepare_Private_Subtype_Completion
     (Id          : Entity_Id;
      Related_Nod : Node_Id)
   is
      Id_B   : constant Entity_Id := Base_Type (Id);
      Full_B : constant Entity_Id := Full_View (Id_B);
      Full   : Entity_Id;

   begin
      if Present (Full_B) then

         --  The Base_Type is already completed, we can complete the subtype
         --  now. We have to create a new entity with the same name, Thus we
         --  can't use Create_Itype.

         --  This is messy, should be fixed ???

         Full := Make_Defining_Identifier (Sloc (Id), Chars (Id));
         Set_Is_Itype (Full);
         Set_Associated_Node_For_Itype (Full, Related_Nod);
         Complete_Private_Subtype (Id, Full, Full_B, Related_Nod);
      end if;

      --  The parent subtype may be private, but the base might not, in some
      --  nested instances. In that case, the subtype does not need to be
      --  exchanged. It would still be nice to make private subtypes and their
      --  bases consistent at all times ???

      if Is_Private_Type (Id_B) then
         Append_Elmt (Id, Private_Dependents (Id_B));
      end if;

   end Prepare_Private_Subtype_Completion;

   ---------------------------
   -- Process_Discriminants --
   ---------------------------

   procedure Process_Discriminants
     (N    : Node_Id;
      Prev : Entity_Id := Empty)
   is
      Elist               : constant Elist_Id := New_Elmt_List;
      Id                  : Node_Id;
      Discr               : Node_Id;
      Discr_Number        : Uint;
      Discr_Type          : Entity_Id;
      Default_Present     : Boolean := False;
      Default_Not_Present : Boolean := False;

   begin
      --  A composite type other than an array type can have discriminants.
      --  Discriminants of non-limited types must have a discrete type.
      --  On entry, the current scope is the composite type.

      --  The discriminants are initially entered into the scope of the type
      --  via Enter_Name with the default Ekind of E_Void to prevent premature
      --  use, as explained at the end of this procedure.

      Discr := First (Discriminant_Specifications (N));
      while Present (Discr) loop
         Enter_Name (Defining_Identifier (Discr));

         --  For navigation purposes we add a reference to the discriminant
         --  in the entity for the type. If the current declaration is a
         --  completion, place references on the partial view. Otherwise the
         --  type is the current scope.

         if Present (Prev) then

            --  The references go on the partial view, if present. If the
            --  partial view has discriminants, the references have been
            --  generated already.

            if not Has_Discriminants (Prev) then
               Generate_Reference (Prev, Defining_Identifier (Discr), 'd');
            end if;
         else
            Generate_Reference
              (Current_Scope, Defining_Identifier (Discr), 'd');
         end if;

         if Nkind (Discriminant_Type (Discr)) = N_Access_Definition then
            Discr_Type := Access_Definition (N, Discriminant_Type (Discr));

            --  Ada 2005 (AI-230): Access discriminants are now allowed for
            --  nonlimited types, and are treated like other components of
            --  anonymous access types in terms of accessibility.

            if not Is_Concurrent_Type (Current_Scope)
              and then not Is_Concurrent_Record_Type (Current_Scope)
              and then not Is_Limited_Record (Current_Scope)
              and then Ekind (Current_Scope) /= E_Limited_Private_Type
            then
               Set_Is_Local_Anonymous_Access (Discr_Type);
            end if;

            --  Ada 2005 (AI-254)

            if Present (Access_To_Subprogram_Definition
                         (Discriminant_Type (Discr)))
              and then Protected_Present (Access_To_Subprogram_Definition
                                           (Discriminant_Type (Discr)))
            then
               Discr_Type :=
                 Replace_Anonymous_Access_To_Protected_Subprogram
                   (Discr, Discr_Type);
            end if;

         else
            Find_Type (Discriminant_Type (Discr));
            Discr_Type := Etype (Discriminant_Type (Discr));

            if Error_Posted (Discriminant_Type (Discr)) then
               Discr_Type := Any_Type;
            end if;
         end if;

         if Is_Access_Type (Discr_Type) then

            --  Ada 2005 (AI-230): Access discriminant allowed in non-limited
            --  record types

            if Ada_Version < Ada_05 then
               Check_Access_Discriminant_Requires_Limited
                 (Discr, Discriminant_Type (Discr));
            end if;

            if Ada_Version = Ada_83 and then Comes_From_Source (Discr) then
               Error_Msg_N
                 ("(Ada 83) access discriminant not allowed", Discr);
            end if;

         elsif not Is_Discrete_Type (Discr_Type) then
            Error_Msg_N ("discriminants must have a discrete or access type",
              Discriminant_Type (Discr));
         end if;

         Set_Etype (Defining_Identifier (Discr), Discr_Type);

         --  If a discriminant specification includes the assignment compound
         --  delimiter followed by an expression, the expression is the default
         --  expression of the discriminant; the default expression must be of
         --  the type of the discriminant. (RM 3.7.1) Since this expression is
         --  a default expression, we do the special preanalysis, since this
         --  expression does not freeze (see "Handling of Default and Per-
         --  Object Expressions" in spec of package Sem).

         if Present (Expression (Discr)) then
            Analyze_Per_Use_Expression (Expression (Discr), Discr_Type);

            if Nkind (N) = N_Formal_Type_Declaration then
               Error_Msg_N
                 ("discriminant defaults not allowed for formal type",
                  Expression (Discr));

            --  Tagged types cannot have defaulted discriminants, but a
            --  non-tagged private type with defaulted discriminants
            --   can have a tagged completion.

            elsif Is_Tagged_Type (Current_Scope)
              and then Comes_From_Source (N)
            then
               Error_Msg_N
                 ("discriminants of tagged type cannot have defaults",
                  Expression (Discr));

            else
               Default_Present := True;
               Append_Elmt (Expression (Discr), Elist);

               --  Tag the defining identifiers for the discriminants with
               --  their corresponding default expressions from the tree.

               Set_Discriminant_Default_Value
                 (Defining_Identifier (Discr), Expression (Discr));
            end if;

         else
            Default_Not_Present := True;
         end if;

         --  Ada 2005 (AI-231): Create an Itype that is a duplicate of
         --  Discr_Type but with the null-exclusion attribute

         if Ada_Version >= Ada_05 then

            --  Ada 2005 (AI-231): Static checks

            if Can_Never_Be_Null (Discr_Type) then
               Null_Exclusion_Static_Checks (Discr);

            elsif Is_Access_Type (Discr_Type)
              and then Null_Exclusion_Present (Discr)

               --  No need to check itypes because in their case this check
               --  was done at their point of creation

              and then not Is_Itype (Discr_Type)
            then
               if Can_Never_Be_Null (Discr_Type) then
                  Error_Msg_N
                    ("(Ada 2005) already a null-excluding type", Discr);
               end if;

               Set_Etype (Defining_Identifier (Discr),
                 Create_Null_Excluding_Itype
                   (T           => Discr_Type,
                    Related_Nod => Discr));
            end if;

         end if;

         Next (Discr);
      end loop;

      --  An element list consisting of the default expressions of the
      --  discriminants is constructed in the above loop and used to set
      --  the Discriminant_Constraint attribute for the type. If an object
      --  is declared of this (record or task) type without any explicit
      --  discriminant constraint given, this element list will form the
      --  actual parameters for the corresponding initialization procedure
      --  for the type.

      Set_Discriminant_Constraint (Current_Scope, Elist);
      Set_Stored_Constraint (Current_Scope, No_Elist);

      --  Default expressions must be provided either for all or for none
      --  of the discriminants of a discriminant part. (RM 3.7.1)

      if Default_Present and then Default_Not_Present then
         Error_Msg_N
           ("incomplete specification of defaults for discriminants", N);
      end if;

      --  The use of the name of a discriminant is not allowed in default
      --  expressions of a discriminant part if the specification of the
      --  discriminant is itself given in the discriminant part. (RM 3.7.1)

      --  To detect this, the discriminant names are entered initially with an
      --  Ekind of E_Void (which is the default Ekind given by Enter_Name). Any
      --  attempt to use a void entity (for example in an expression that is
      --  type-checked) produces the error message: premature usage. Now after
      --  completing the semantic analysis of the discriminant part, we can set
      --  the Ekind of all the discriminants appropriately.

      Discr := First (Discriminant_Specifications (N));
      Discr_Number := Uint_1;
      while Present (Discr) loop
         Id := Defining_Identifier (Discr);
         Set_Ekind (Id, E_Discriminant);
         Init_Component_Location (Id);
         Init_Esize (Id);
         Set_Discriminant_Number (Id, Discr_Number);

         --  Make sure this is always set, even in illegal programs

         Set_Corresponding_Discriminant (Id, Empty);

         --  Initialize the Original_Record_Component to the entity itself.
         --  Inherit_Components will propagate the right value to
         --  discriminants in derived record types.

         Set_Original_Record_Component (Id, Id);

         --  Create the discriminal for the discriminant

         Build_Discriminal (Id);

         Next (Discr);
         Discr_Number := Discr_Number + 1;
      end loop;

      Set_Has_Discriminants (Current_Scope);
   end Process_Discriminants;

   -----------------------
   -- Process_Full_View --
   -----------------------

   procedure Process_Full_View (N : Node_Id; Full_T, Priv_T : Entity_Id) is
      Priv_Parent : Entity_Id;
      Full_Parent : Entity_Id;
      Full_Indic  : Node_Id;

      function Find_Ancestor_Interface
        (Typ : Entity_Id) return Entity_Id;
      --  Find an implemented interface in the derivation chain of Typ

      -----------------------------
      -- Find_Ancestor_Interface --
      -----------------------------

      function Find_Ancestor_Interface
        (Typ : Entity_Id) return Entity_Id
      is
         T : Entity_Id;

      begin
         T := Typ;
         while T /= Etype (T) loop
            if Is_Interface (Etype (T)) then
               return Etype (T);
            end if;

            T := Etype (T);

            --  Protect us against erroneous code that has a large
            --  chain of circularity dependencies

            exit when T = Typ;
         end loop;

         return Empty;
      end Find_Ancestor_Interface;

   --  Start of processing for Process_Full_View

   begin
      --  First some sanity checks that must be done after semantic
      --  decoration of the full view and thus cannot be placed with other
      --  similar checks in Find_Type_Name

      if not Is_Limited_Type (Priv_T)
        and then (Is_Limited_Type (Full_T)
                   or else Is_Limited_Composite (Full_T))
      then
         Error_Msg_N
           ("completion of nonlimited type cannot be limited", Full_T);
         Explain_Limited_Type (Full_T, Full_T);

      elsif Is_Abstract (Full_T) and then not Is_Abstract (Priv_T) then
         Error_Msg_N
           ("completion of nonabstract type cannot be abstract", Full_T);

      elsif Is_Tagged_Type (Priv_T)
        and then Is_Limited_Type (Priv_T)
        and then not Is_Limited_Type (Full_T)
      then
         --  GNAT allow its own definition of Limited_Controlled to disobey
         --  this rule in order in ease the implementation. The next test is
         --  safe because Root_Controlled is defined in a private system child

         if Etype (Full_T) = Full_View (RTE (RE_Root_Controlled)) then
            Set_Is_Limited_Composite (Full_T);
         else
            Error_Msg_N
              ("completion of limited tagged type must be limited", Full_T);
         end if;

      elsif Is_Generic_Type (Priv_T) then
         Error_Msg_N ("generic type cannot have a completion", Full_T);
      end if;

      --  Ada 2005 (AI-396): A full view shall be a descendant of an
      --  interface type if and only if the corresponding partial view
      --  (if any) is also a descendant of the interface type, or if
      --  the partial view is untagged.

      if Ada_Version >= Ada_05
        and then Is_Tagged_Type (Full_T)
      then
         declare
            Iface     : Entity_Id;
            Iface_Def : Node_Id;

         begin
            Iface := Find_Ancestor_Interface (Full_T);

            if Present (Iface) then
               Iface_Def := Type_Definition (Parent (Iface));

               --  The full view derives from an interface descendant, but the
               --  partial view does not share the same tagged type.

               if Is_Tagged_Type (Priv_T)
                 and then Etype (Priv_T) /= Etype (Full_T)
                 and then Etype (Priv_T) /= Iface
               then
                  Error_Msg_N ("(Ada 2005) tagged partial view cannot be " &
                               "completed by a type that implements an " &
                               "interface", Priv_T);
               end if;

               --  The full view derives from a limited, protected,
               --  synchronized or task interface descendant, but the
               --  partial view is not labeled as limited.

               if (Limited_Present               (Iface_Def)
                    or else Protected_Present    (Iface_Def)
                    or else Synchronized_Present (Iface_Def)
                    or else Task_Present         (Iface_Def))
                 and then not Limited_Present (Parent (Priv_T))
               then
                  Error_Msg_N ("(Ada 2005) non-limited private type cannot be "
                               & "completed by a limited type", Priv_T);
               end if;
            end if;
         end;
      end if;

      if Is_Tagged_Type (Priv_T)
        and then Nkind (Parent (Priv_T)) = N_Private_Extension_Declaration
        and then Is_Derived_Type (Full_T)
      then
         Priv_Parent := Etype (Priv_T);

         --  The full view of a private extension may have been transformed
         --  into an unconstrained derived type declaration and a subtype
         --  declaration (see build_derived_record_type for details).

         if Nkind (N) = N_Subtype_Declaration then
            Full_Indic  := Subtype_Indication (N);
            Full_Parent := Etype (Base_Type (Full_T));
         else
            Full_Indic  := Subtype_Indication (Type_Definition (N));
            Full_Parent := Etype (Full_T);
         end if;

         --  Check that the parent type of the full type is a descendant of
         --  the ancestor subtype given in the private extension. If either
         --  entity has an Etype equal to Any_Type then we had some previous
         --  error situation [7.3(8)].

         if Priv_Parent = Any_Type or else Full_Parent = Any_Type then
            return;

         elsif not Is_Ancestor (Base_Type (Priv_Parent), Full_Parent) then
            Error_Msg_N
              ("parent of full type must descend from parent"
                  & " of private extension", Full_Indic);

         --  Check the rules of 7.3(10): if the private extension inherits
         --  known discriminants, then the full type must also inherit those
         --  discriminants from the same (ancestor) type, and the parent
         --  subtype of the full type must be constrained if and only if
         --  the ancestor subtype of the private extension is constrained.

         elsif not Present (Discriminant_Specifications (Parent (Priv_T)))
           and then not Has_Unknown_Discriminants (Priv_T)
           and then Has_Discriminants (Base_Type (Priv_Parent))
         then
            declare
               Priv_Indic  : constant Node_Id :=
                               Subtype_Indication (Parent (Priv_T));

               Priv_Constr : constant Boolean :=
                               Is_Constrained (Priv_Parent)
                                 or else
                                   Nkind (Priv_Indic) = N_Subtype_Indication
                                 or else Is_Constrained (Entity (Priv_Indic));

               Full_Constr : constant Boolean :=
                               Is_Constrained (Full_Parent)
                                 or else
                                   Nkind (Full_Indic) = N_Subtype_Indication
                                 or else Is_Constrained (Entity (Full_Indic));

               Priv_Discr : Entity_Id;
               Full_Discr : Entity_Id;

            begin
               Priv_Discr := First_Discriminant (Priv_Parent);
               Full_Discr := First_Discriminant (Full_Parent);
               while Present (Priv_Discr) and then Present (Full_Discr) loop
                  if Original_Record_Component (Priv_Discr) =
                     Original_Record_Component (Full_Discr)
                    or else
                     Corresponding_Discriminant (Priv_Discr) =
                     Corresponding_Discriminant (Full_Discr)
                  then
                     null;
                  else
                     exit;
                  end if;

                  Next_Discriminant (Priv_Discr);
                  Next_Discriminant (Full_Discr);
               end loop;

               if Present (Priv_Discr) or else Present (Full_Discr) then
                  Error_Msg_N
                    ("full view must inherit discriminants of the parent type"
                     & " used in the private extension", Full_Indic);

               elsif Priv_Constr and then not Full_Constr then
                  Error_Msg_N
                    ("parent subtype of full type must be constrained",
                     Full_Indic);

               elsif Full_Constr and then not Priv_Constr then
                  Error_Msg_N
                    ("parent subtype of full type must be unconstrained",
                     Full_Indic);
               end if;
            end;

         --  Check the rules of 7.3(12): if a partial view has neither known
         --  or unknown discriminants, then the full type declaration shall
         --  define a definite subtype.

         elsif      not Has_Unknown_Discriminants (Priv_T)
           and then not Has_Discriminants (Priv_T)
           and then not Is_Constrained (Full_T)
         then
            Error_Msg_N
              ("full view must define a constrained type if partial view"
                & " has no discriminants", Full_T);
         end if;

         --  ??????? Do we implement the following properly ?????
         --  If the ancestor subtype of a private extension has constrained
         --  discriminants, then the parent subtype of the full view shall
         --  impose a statically matching constraint on those discriminants
         --  [7.3(13)].

      else
         --  For untagged types, verify that a type without discriminants
         --  is not completed with an unconstrained type.

         if not Is_Indefinite_Subtype (Priv_T)
           and then Is_Indefinite_Subtype (Full_T)
         then
            Error_Msg_N ("full view of type must be definite subtype", Full_T);
         end if;
      end if;

      --  Ada 2005 AI-363: if the full view has discriminants with
      --  defaults, it is illegal to declare constrained access subtypes
      --  whose designated type is the current type. This allows objects
      --  of the type that are declared in the heap to be unconstrained.

      if not Has_Unknown_Discriminants (Priv_T)
        and then not Has_Discriminants (Priv_T)
        and then Has_Discriminants (Full_T)
        and then
          Present
            (Discriminant_Default_Value (First_Discriminant (Full_T)))
      then
         Set_Has_Constrained_Partial_View (Full_T);
         Set_Has_Constrained_Partial_View (Priv_T);
      end if;

      --  Create a full declaration for all its subtypes recorded in
      --  Private_Dependents and swap them similarly to the base type. These
      --  are subtypes that have been define before the full declaration of
      --  the private type. We also swap the entry in Private_Dependents list
      --  so we can properly restore the private view on exit from the scope.

      declare
         Priv_Elmt : Elmt_Id;
         Priv      : Entity_Id;
         Full      : Entity_Id;

      begin
         Priv_Elmt := First_Elmt (Private_Dependents (Priv_T));
         while Present (Priv_Elmt) loop
            Priv := Node (Priv_Elmt);

            if Ekind (Priv) = E_Private_Subtype
              or else Ekind (Priv) = E_Limited_Private_Subtype
              or else Ekind (Priv) = E_Record_Subtype_With_Private
            then
               Full := Make_Defining_Identifier (Sloc (Priv), Chars (Priv));
               Set_Is_Itype (Full);
               Set_Parent (Full, Parent (Priv));
               Set_Associated_Node_For_Itype (Full, N);

               --  Now we need to complete the private subtype, but since the
               --  base type has already been swapped, we must also swap the
               --  subtypes (and thus, reverse the arguments in the call to
               --  Complete_Private_Subtype).

               Copy_And_Swap (Priv, Full);
               Complete_Private_Subtype (Full, Priv, Full_T, N);
               Replace_Elmt (Priv_Elmt, Full);
            end if;

            Next_Elmt (Priv_Elmt);
         end loop;
      end;

      --  If the private view was tagged, copy the new Primitive
      --  operations from the private view to the full view.

      if Is_Tagged_Type (Full_T) then
         declare
            Priv_List : Elist_Id;
            Full_List : constant Elist_Id := Primitive_Operations (Full_T);
            P1, P2    : Elmt_Id;
            Prim      : Entity_Id;
            D_Type    : Entity_Id;

         begin
            if Is_Tagged_Type (Priv_T) then
               Priv_List := Primitive_Operations (Priv_T);

               P1 := First_Elmt (Priv_List);
               while Present (P1) loop
                  Prim := Node (P1);

                  --  Transfer explicit primitives, not those inherited from
                  --  parent of partial view, which will be re-inherited on
                  --  the full view.

                  if Comes_From_Source (Prim) then
                     P2 := First_Elmt (Full_List);
                     while Present (P2) and then Node (P2) /= Prim loop
                        Next_Elmt (P2);
                     end loop;

                     --  If not found, that is a new one

                     if No (P2) then
                        Append_Elmt (Prim, Full_List);
                     end if;
                  end if;

                  Next_Elmt (P1);
               end loop;

            else
               --  In this case the partial view is untagged, so here we
               --  locate all of the earlier primitives that need to be
               --  treated as dispatching (those that appear between the two
               --  views). Note that these additional operations must all be
               --  new operations (any earlier operations that override
               --  inherited operations of the full view will already have
               --  been inserted in the primitives list and marked as
               --  dispatching by Check_Operation_From_Private_View. Note that
               --  implicit "/=" operators are excluded from being added to
               --  the primitives list since they shouldn't be treated as
               --  dispatching (tagged "/=" is handled specially).

               Prim := Next_Entity (Full_T);
               while Present (Prim) and then Prim /= Priv_T loop
                  if Ekind (Prim) = E_Procedure
                       or else
                     Ekind (Prim) = E_Function
                  then

                     D_Type := Find_Dispatching_Type (Prim);

                     if D_Type = Full_T
                       and then (Chars (Prim) /= Name_Op_Ne
                                  or else Comes_From_Source (Prim))
                     then
                        Check_Controlling_Formals (Full_T, Prim);

                        if not Is_Dispatching_Operation (Prim) then
                           Append_Elmt (Prim, Full_List);
                           Set_Is_Dispatching_Operation (Prim, True);
                           Set_DT_Position (Prim, No_Uint);
                        end if;

                     elsif Is_Dispatching_Operation (Prim)
                       and then D_Type  /= Full_T
                     then

                        --  Verify that it is not otherwise controlled by
                        --  a formal or a return value of type T.

                        Check_Controlling_Formals (D_Type, Prim);
                     end if;
                  end if;

                  Next_Entity (Prim);
               end loop;
            end if;

            --  For the tagged case, the two views can share the same
            --  Primitive Operation list and the same class wide type.
            --  Update attributes of the class-wide type which depend on
            --  the full declaration.

            if Is_Tagged_Type (Priv_T) then
               Set_Primitive_Operations (Priv_T, Full_List);
               Set_Class_Wide_Type
                 (Base_Type (Full_T), Class_Wide_Type (Priv_T));

               --  Any other attributes should be propagated to C_W ???

               Set_Has_Task (Class_Wide_Type (Priv_T), Has_Task (Full_T));

            end if;
         end;
      end if;
   end Process_Full_View;

   -----------------------------------
   -- Process_Incomplete_Dependents --
   -----------------------------------

   procedure Process_Incomplete_Dependents
     (N      : Node_Id;
      Full_T : Entity_Id;
      Inc_T  : Entity_Id)
   is
      Inc_Elmt : Elmt_Id;
      Priv_Dep : Entity_Id;
      New_Subt : Entity_Id;

      Disc_Constraint : Elist_Id;

   begin
      if No (Private_Dependents (Inc_T)) then
         return;
      end if;

      --  Itypes that may be generated by the completion of an incomplete
      --  subtype are not used by the back-end and not attached to the tree.
      --  They are created only for constraint-checking purposes.

      Inc_Elmt := First_Elmt (Private_Dependents (Inc_T));
      while Present (Inc_Elmt) loop
         Priv_Dep := Node (Inc_Elmt);

         if Ekind (Priv_Dep) = E_Subprogram_Type then

            --  An Access_To_Subprogram type may have a return type or a
            --  parameter type that is incomplete. Replace with the full view.

            if Etype (Priv_Dep) = Inc_T then
               Set_Etype (Priv_Dep, Full_T);
            end if;

            declare
               Formal : Entity_Id;

            begin
               Formal := First_Formal (Priv_Dep);
               while Present (Formal) loop
                  if Etype (Formal) = Inc_T then
                     Set_Etype (Formal, Full_T);
                  end if;

                  Next_Formal (Formal);
               end loop;
            end;

         elsif Is_Overloadable (Priv_Dep) then

            --  A protected operation is never dispatching: only its
            --  wrapper operation (which has convention Ada) is.

            if Is_Tagged_Type (Full_T)
              and then Convention (Priv_Dep) /= Convention_Protected
            then

               --  Subprogram has an access parameter whose designated type
               --  was incomplete. Reexamine declaration now, because it may
               --  be a primitive operation of the full type.

               Check_Operation_From_Incomplete_Type (Priv_Dep, Inc_T);
               Set_Is_Dispatching_Operation (Priv_Dep);
               Check_Controlling_Formals (Full_T, Priv_Dep);
            end if;

         elsif Ekind (Priv_Dep) = E_Subprogram_Body then

            --  Can happen during processing of a body before the completion
            --  of a TA type. Ignore, because spec is also on dependent list.

            return;

         --  Dependent is a subtype

         else
            --  We build a new subtype indication using the full view of the
            --  incomplete parent. The discriminant constraints have been
            --  elaborated already at the point of the subtype declaration.

            New_Subt := Create_Itype (E_Void, N);

            if Has_Discriminants (Full_T) then
               Disc_Constraint := Discriminant_Constraint (Priv_Dep);
            else
               Disc_Constraint := No_Elist;
            end if;

            Build_Discriminated_Subtype (Full_T, New_Subt, Disc_Constraint, N);
            Set_Full_View (Priv_Dep, New_Subt);
         end if;

         Next_Elmt (Inc_Elmt);
      end loop;
   end Process_Incomplete_Dependents;

   --------------------------------
   -- Process_Range_Expr_In_Decl --
   --------------------------------

   procedure Process_Range_Expr_In_Decl
     (R           : Node_Id;
      T           : Entity_Id;
      Check_List  : List_Id := Empty_List;
      R_Check_Off : Boolean := False)
   is
      Lo, Hi    : Node_Id;
      R_Checks  : Check_Result;
      Type_Decl : Node_Id;
      Def_Id    : Entity_Id;

   begin
      Analyze_And_Resolve (R, Base_Type (T));

      if Nkind (R) = N_Range then
         Lo := Low_Bound (R);
         Hi := High_Bound (R);

         --  If there were errors in the declaration, try and patch up some
         --  common mistakes in the bounds. The cases handled are literals
         --  which are Integer where the expected type is Real and vice versa.
         --  These corrections allow the compilation process to proceed further
         --  along since some basic assumptions of the format of the bounds
         --  are guaranteed.

         if Etype (R) = Any_Type then

            if Nkind (Lo) = N_Integer_Literal and then Is_Real_Type (T) then
               Rewrite (Lo,
                 Make_Real_Literal (Sloc (Lo), UR_From_Uint (Intval (Lo))));

            elsif Nkind (Hi) = N_Integer_Literal and then Is_Real_Type (T) then
               Rewrite (Hi,
                 Make_Real_Literal (Sloc (Hi), UR_From_Uint (Intval (Hi))));

            elsif Nkind (Lo) = N_Real_Literal and then Is_Integer_Type (T) then
               Rewrite (Lo,
                 Make_Integer_Literal (Sloc (Lo), UR_To_Uint (Realval (Lo))));

            elsif Nkind (Hi) = N_Real_Literal and then Is_Integer_Type (T) then
               Rewrite (Hi,
                 Make_Integer_Literal (Sloc (Hi), UR_To_Uint (Realval (Hi))));
            end if;

            Set_Etype (Lo, T);
            Set_Etype (Hi, T);
         end if;

         --  If the bounds of the range have been mistakenly given as string
         --  literals (perhaps in place of character literals), then an error
         --  has already been reported, but we rewrite the string literal as a
         --  bound of the range's type to avoid blowups in later processing
         --  that looks at static values.

         if Nkind (Lo) = N_String_Literal then
            Rewrite (Lo,
              Make_Attribute_Reference (Sloc (Lo),
                Attribute_Name => Name_First,
                Prefix => New_Reference_To (T, Sloc (Lo))));
            Analyze_And_Resolve (Lo);
         end if;

         if Nkind (Hi) = N_String_Literal then
            Rewrite (Hi,
              Make_Attribute_Reference (Sloc (Hi),
                Attribute_Name => Name_First,
                Prefix => New_Reference_To (T, Sloc (Hi))));
            Analyze_And_Resolve (Hi);
         end if;

         --  If bounds aren't scalar at this point then exit, avoiding
         --  problems with further processing of the range in this procedure.

         if not Is_Scalar_Type (Etype (Lo)) then
            return;
         end if;

         --  Resolve (actually Sem_Eval) has checked that the bounds are in
         --  then range of the base type. Here we check whether the bounds
         --  are in the range of the subtype itself. Note that if the bounds
         --  represent the null range the Constraint_Error exception should
         --  not be raised.

         --  ??? The following code should be cleaned up as follows

         --  1. The Is_Null_Range (Lo, Hi) test should disappear since it
         --     is done in the call to Range_Check (R, T); below

         --  2. The use of R_Check_Off should be investigated and possibly
         --     removed, this would clean up things a bit.

         if Is_Null_Range (Lo, Hi) then
            null;

         else
            --  Capture values of bounds and generate temporaries for them
            --  if needed, before applying checks, since checks may cause
            --  duplication of the expression without forcing evaluation.

            if Expander_Active then
               Force_Evaluation (Lo);
               Force_Evaluation (Hi);
            end if;

            --  We use a flag here instead of suppressing checks on the
            --  type because the type we check against isn't necessarily
            --  the place where we put the check.

            if not R_Check_Off then
               R_Checks := Range_Check (R, T);

               --  Look up tree to find an appropriate insertion point.
               --  This seems really junk code, and very brittle, couldn't
               --  we just use an insert actions call of some kind ???

               Type_Decl := Parent (R);
               while Present (Type_Decl) and then not
                 (Nkind (Type_Decl) = N_Full_Type_Declaration
                    or else
                  Nkind (Type_Decl) = N_Subtype_Declaration
                    or else
                  Nkind (Type_Decl) = N_Loop_Statement
                    or else
                  Nkind (Type_Decl) = N_Task_Type_Declaration
                    or else
                  Nkind (Type_Decl) = N_Single_Task_Declaration
                    or else
                  Nkind (Type_Decl) = N_Protected_Type_Declaration
                    or else
                  Nkind (Type_Decl) = N_Single_Protected_Declaration)
               loop
                  Type_Decl := Parent (Type_Decl);
               end loop;

               --  Why would Type_Decl not be present???  Without this test,
               --  short regression tests fail.

               if Present (Type_Decl) then

                  --  Case of loop statement (more comments ???)

                  if Nkind (Type_Decl) = N_Loop_Statement then
                     declare
                        Indic : Node_Id;

                     begin
                        Indic := Parent (R);
                        while Present (Indic) and then not
                          (Nkind (Indic) = N_Subtype_Indication)
                        loop
                           Indic := Parent (Indic);
                        end loop;

                        if Present (Indic) then
                           Def_Id := Etype (Subtype_Mark (Indic));

                           Insert_Range_Checks
                             (R_Checks,
                              Type_Decl,
                              Def_Id,
                              Sloc (Type_Decl),
                              R,
                              Do_Before => True);
                        end if;
                     end;

                  --  All other cases (more comments ???)

                  else
                     Def_Id := Defining_Identifier (Type_Decl);

                     if (Ekind (Def_Id) = E_Record_Type
                          and then Depends_On_Discriminant (R))
                       or else
                        (Ekind (Def_Id) = E_Protected_Type
                          and then Has_Discriminants (Def_Id))
                     then
                        Append_Range_Checks
                          (R_Checks, Check_List, Def_Id, Sloc (Type_Decl), R);

                     else
                        Insert_Range_Checks
                          (R_Checks, Type_Decl, Def_Id, Sloc (Type_Decl), R);

                     end if;
                  end if;
               end if;
            end if;
         end if;

      elsif Expander_Active then
         Get_Index_Bounds (R, Lo, Hi);
         Force_Evaluation (Lo);
         Force_Evaluation (Hi);
      end if;
   end Process_Range_Expr_In_Decl;

   --------------------------------------
   -- Process_Real_Range_Specification --
   --------------------------------------

   procedure Process_Real_Range_Specification (Def : Node_Id) is
      Spec : constant Node_Id := Real_Range_Specification (Def);
      Lo   : Node_Id;
      Hi   : Node_Id;
      Err  : Boolean := False;

      procedure Analyze_Bound (N : Node_Id);
      --  Analyze and check one bound

      -------------------
      -- Analyze_Bound --
      -------------------

      procedure Analyze_Bound (N : Node_Id) is
      begin
         Analyze_And_Resolve (N, Any_Real);

         if not Is_OK_Static_Expression (N) then
            Flag_Non_Static_Expr
              ("bound in real type definition is not static!", N);
            Err := True;
         end if;
      end Analyze_Bound;

   --  Start of processing for Process_Real_Range_Specification

   begin
      if Present (Spec) then
         Lo := Low_Bound (Spec);
         Hi := High_Bound (Spec);
         Analyze_Bound (Lo);
         Analyze_Bound (Hi);

         --  If error, clear away junk range specification

         if Err then
            Set_Real_Range_Specification (Def, Empty);
         end if;
      end if;
   end Process_Real_Range_Specification;

   ---------------------
   -- Process_Subtype --
   ---------------------

   function Process_Subtype
     (S           : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id := Empty;
      Suffix      : Character := ' ') return Entity_Id
   is
      P               : Node_Id;
      Def_Id          : Entity_Id;
      Error_Node      : Node_Id;
      Full_View_Id    : Entity_Id;
      Subtype_Mark_Id : Entity_Id;

      May_Have_Null_Exclusion : Boolean;

      procedure Check_Incomplete (T : Entity_Id);
      --  Called to verify that an incomplete type is not used prematurely

      ----------------------
      -- Check_Incomplete --
      ----------------------

      procedure Check_Incomplete (T : Entity_Id) is
      begin
         if Ekind (Root_Type (Entity (T))) = E_Incomplete_Type then
            Error_Msg_N ("invalid use of type before its full declaration", T);
         end if;
      end Check_Incomplete;

   --  Start of processing for Process_Subtype

   begin
      --  Case of no constraints present

      if Nkind (S) /= N_Subtype_Indication then

         Find_Type (S);
         Check_Incomplete (S);
         P := Parent (S);

         --  Ada 2005 (AI-231): Static check

         if Ada_Version >= Ada_05
           and then Present (P)
           and then Null_Exclusion_Present (P)
           and then Nkind (P) /= N_Access_To_Object_Definition
           and then not Is_Access_Type (Entity (S))
         then
            Error_Msg_N
              ("(Ada 2005) the null-exclusion part requires an access type",
               S);
         end if;

         May_Have_Null_Exclusion :=
           Nkind (P) = N_Access_Definition
           or else Nkind (P) = N_Access_Function_Definition
           or else Nkind (P) = N_Access_Procedure_Definition
           or else Nkind (P) = N_Access_To_Object_Definition
           or else Nkind (P) = N_Allocator
           or else Nkind (P) = N_Component_Definition
           or else Nkind (P) = N_Derived_Type_Definition
           or else Nkind (P) = N_Discriminant_Specification
           or else Nkind (P) = N_Object_Declaration
           or else Nkind (P) = N_Parameter_Specification
           or else Nkind (P) = N_Subtype_Declaration;

         --  Create an Itype that is a duplicate of Entity (S) but with the
         --  null-exclusion attribute

         if May_Have_Null_Exclusion
           and then Is_Access_Type (Entity (S))
           and then Null_Exclusion_Present (P)

            --  No need to check the case of an access to object definition.
            --  It is correct to define double not-null pointers.
            --  Example:
            --     type Not_Null_Int_Ptr is not null access Integer;
            --     type Acc is not null access Not_Null_Int_Ptr;

           and then Nkind (P) /= N_Access_To_Object_Definition
         then
            if Can_Never_Be_Null (Entity (S)) then
               case Nkind (Related_Nod) is
                  when N_Full_Type_Declaration =>
                     if Nkind (Type_Definition (Related_Nod))
                       in N_Array_Type_Definition
                     then
                        Error_Node :=
                          Subtype_Indication
                            (Component_Definition
                             (Type_Definition (Related_Nod)));
                     else
                        Error_Node :=
                          Subtype_Indication (Type_Definition (Related_Nod));
                     end if;

                  when N_Subtype_Declaration =>
                     Error_Node := Subtype_Indication (Related_Nod);

                  when N_Object_Declaration =>
                     Error_Node := Object_Definition (Related_Nod);

                  when N_Component_Declaration =>
                     Error_Node :=
                       Subtype_Indication (Component_Definition (Related_Nod));

                  when others =>
                     pragma Assert (False);
                     Error_Node := Related_Nod;
               end case;

               Error_Msg_N
                 ("(Ada 2005) already a null-excluding type", Error_Node);
            end if;

            Set_Etype  (S,
              Create_Null_Excluding_Itype
                (T           => Entity (S),
                 Related_Nod => P));
            Set_Entity (S, Etype (S));
         end if;

         return Entity (S);

      --  Case of constraint present, so that we have an N_Subtype_Indication
      --  node (this node is created only if constraints are present).

      else

         Find_Type (Subtype_Mark (S));

         if Nkind (Parent (S)) /= N_Access_To_Object_Definition
           and then not
            (Nkind (Parent (S)) = N_Subtype_Declaration
              and then
             Is_Itype (Defining_Identifier (Parent (S))))
         then
            Check_Incomplete (Subtype_Mark (S));
         end if;

         P := Parent (S);
         Subtype_Mark_Id := Entity (Subtype_Mark (S));

         --  Explicit subtype declaration case

         if Nkind (P) = N_Subtype_Declaration then
            Def_Id := Defining_Identifier (P);

         --  Explicit derived type definition case

         elsif Nkind (P) = N_Derived_Type_Definition then
            Def_Id := Defining_Identifier (Parent (P));

         --  Implicit case, the Def_Id must be created as an implicit type.
         --  The one exception arises in the case of concurrent types, array
         --  and access types, where other subsidiary implicit types may be
         --  created and must appear before the main implicit type. In these
         --  cases we leave Def_Id set to Empty as a signal that Create_Itype
         --  has not yet been called to create Def_Id.

         else
            if Is_Array_Type (Subtype_Mark_Id)
              or else Is_Concurrent_Type (Subtype_Mark_Id)
              or else Is_Access_Type (Subtype_Mark_Id)
            then
               Def_Id := Empty;

            --  For the other cases, we create a new unattached Itype,
            --  and set the indication to ensure it gets attached later.

            else
               Def_Id :=
                 Create_Itype (E_Void, Related_Nod, Related_Id, Suffix);
            end if;
         end if;

         --  If the kind of constraint is invalid for this kind of type,
         --  then give an error, and then pretend no constraint was given.

         if not Is_Valid_Constraint_Kind
                   (Ekind (Subtype_Mark_Id), Nkind (Constraint (S)))
         then
            Error_Msg_N
              ("incorrect constraint for this kind of type", Constraint (S));

            Rewrite (S, New_Copy_Tree (Subtype_Mark (S)));

            --  Set Ekind of orphan itype, to prevent cascaded errors

            if Present (Def_Id) then
               Set_Ekind (Def_Id, Ekind (Any_Type));
            end if;

            --  Make recursive call, having got rid of the bogus constraint

            return Process_Subtype (S, Related_Nod, Related_Id, Suffix);
         end if;

         --  Remaining processing depends on type

         case Ekind (Subtype_Mark_Id) is
            when Access_Kind =>
               Constrain_Access (Def_Id, S, Related_Nod);

            when Array_Kind =>
               Constrain_Array (Def_Id, S, Related_Nod, Related_Id, Suffix);

            when Decimal_Fixed_Point_Kind =>
               Constrain_Decimal (Def_Id, S);

            when Enumeration_Kind =>
               Constrain_Enumeration (Def_Id, S);

            when Ordinary_Fixed_Point_Kind =>
               Constrain_Ordinary_Fixed (Def_Id, S);

            when Float_Kind =>
               Constrain_Float (Def_Id, S);

            when Integer_Kind =>
               Constrain_Integer (Def_Id, S);

            when E_Record_Type     |
                 E_Record_Subtype  |
                 Class_Wide_Kind   |
                 E_Incomplete_Type =>
               Constrain_Discriminated_Type (Def_Id, S, Related_Nod);

            when Private_Kind =>
               Constrain_Discriminated_Type (Def_Id, S, Related_Nod);
               Set_Private_Dependents (Def_Id, New_Elmt_List);

               --  In case of an invalid constraint prevent further processing
               --  since the type constructed is missing expected fields.

               if Etype (Def_Id) = Any_Type then
                  return Def_Id;
               end if;

               --  If the full view is that of a task with discriminants,
               --  we must constrain both the concurrent type and its
               --  corresponding record type. Otherwise we will just propagate
               --  the constraint to the full view, if available.

               if Present (Full_View (Subtype_Mark_Id))
                 and then Has_Discriminants (Subtype_Mark_Id)
                 and then Is_Concurrent_Type (Full_View (Subtype_Mark_Id))
               then
                  Full_View_Id :=
                    Create_Itype (E_Void, Related_Nod, Related_Id, Suffix);

                  Set_Entity (Subtype_Mark (S), Full_View (Subtype_Mark_Id));
                  Constrain_Concurrent (Full_View_Id, S,
                    Related_Nod, Related_Id, Suffix);
                  Set_Entity (Subtype_Mark (S), Subtype_Mark_Id);
                  Set_Full_View (Def_Id, Full_View_Id);

               else
                  Prepare_Private_Subtype_Completion (Def_Id, Related_Nod);
               end if;

            when Concurrent_Kind  =>
               Constrain_Concurrent (Def_Id, S,
                 Related_Nod, Related_Id, Suffix);

            when others =>
               Error_Msg_N ("invalid subtype mark in subtype indication", S);
         end case;

         --  Size and Convention are always inherited from the base type

         Set_Size_Info  (Def_Id,            (Subtype_Mark_Id));
         Set_Convention (Def_Id, Convention (Subtype_Mark_Id));

         return Def_Id;
      end if;
   end Process_Subtype;

   -----------------------------
   -- Record_Type_Declaration --
   -----------------------------

   procedure Record_Type_Declaration
     (T    : Entity_Id;
      N    : Node_Id;
      Prev : Entity_Id)
   is
      Loc   : constant Source_Ptr := Sloc (N);
      Def   : constant Node_Id    := Type_Definition (N);
      Inc_T : Entity_Id := Empty;

      Is_Tagged : Boolean;
      Tag_Comp  : Entity_Id;

      procedure Check_Anonymous_Access_Types (Comp_List : Node_Id);
      --  Ada 2005 AI-382: an access component in a record declaration can
      --  refer to the enclosing record, in which case it denotes the type
      --  itself, and not the current instance of the type. We create an
      --  anonymous access type for the component, and flag it as an access
      --  to a component, so that accessibility checks are properly performed
      --  on it. The declaration of the access type is placed ahead of that
      --  of the record, to prevent circular order-of-elaboration issues in
      --  Gigi. We create an incomplete type for the record declaration, which
      --  is the designated type of the anonymous access.

      procedure Make_Incomplete_Type_Declaration;
      --  If the record type contains components that include an access to the
      --  current record, create an incomplete type declaration for the record,
      --  to be used as the designated type of the anonymous access. This is
      --  done only once, and only if there is no previous partial view of the
      --  type.

      ----------------------------------
      -- Check_Anonymous_Access_Types --
      ----------------------------------

      procedure Check_Anonymous_Access_Types (Comp_List : Node_Id) is
         Anon_Access : Entity_Id;
         Acc_Def     : Node_Id;
         Comp        : Node_Id;
         Decl        : Node_Id;
         Type_Def    : Node_Id;

         function Mentions_T (Acc_Def : Node_Id) return Boolean;
         --  Check whether an access definition includes a reference to
         --  the enclosing record type. The reference can be a subtype
         --  mark in the access definition itself, or a 'Class attribute
         --  reference, or recursively a reference appearing in a parameter
         --  type in an access_to_subprogram definition.

         ----------------
         -- Mentions_T --
         ----------------

         function Mentions_T (Acc_Def : Node_Id) return Boolean is
            Subt : Node_Id;

         begin
            if No (Access_To_Subprogram_Definition (Acc_Def)) then
               Subt := Subtype_Mark (Acc_Def);

               if Nkind (Subt) = N_Identifier then
                  return Chars (Subt) = Chars (T);
               elsif Nkind (Subt) = N_Attribute_Reference
                  and then Attribute_Name (Subt) = Name_Class
               then
                  return (Chars (Prefix (Subt))) = Chars (T);
               else
                  return False;
               end if;

            else
               --  Component is an access_to_subprogram: examine its formals

               declare
                  Param_Spec : Node_Id;

               begin
                  Param_Spec :=
                    First
                      (Parameter_Specifications
                        (Access_To_Subprogram_Definition (Acc_Def)));
                  while Present (Param_Spec) loop
                     if Nkind (Parameter_Type (Param_Spec))
                          = N_Access_Definition
                       and then Mentions_T (Parameter_Type (Param_Spec))
                     then
                        return True;
                     end if;

                     Next (Param_Spec);
                  end loop;

                  return False;
               end;
            end if;
         end Mentions_T;

      --  Start of processing for Check_Anonymous_Access_Types

      begin
         if No (Comp_List) then
            return;
         end if;

         Comp := First (Component_Items (Comp_List));
         while Present (Comp) loop
            if Nkind (Comp) = N_Component_Declaration
              and then
                Present (Access_Definition (Component_Definition (Comp)))
              and then
                Mentions_T (Access_Definition (Component_Definition (Comp)))
            then
               Acc_Def :=
                 Access_To_Subprogram_Definition
                   (Access_Definition (Component_Definition (Comp)));

               Make_Incomplete_Type_Declaration;
               Anon_Access :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_Internal_Name ('S'));

               --  Create a declaration for the anonymous access type: either
               --  an access_to_object or an access_to_subprogram.

               if Present (Acc_Def) then
                  if Nkind  (Acc_Def) = N_Access_Function_Definition then
                     Type_Def :=
                       Make_Access_Function_Definition (Loc,
                         Parameter_Specifications =>
                           Parameter_Specifications (Acc_Def),
                         Result_Definition => Result_Definition (Acc_Def));
                  else
                     Type_Def :=
                       Make_Access_Procedure_Definition (Loc,
                         Parameter_Specifications =>
                           Parameter_Specifications (Acc_Def));
                  end if;

               else
                  Type_Def :=
                    Make_Access_To_Object_Definition (Loc,
                      Subtype_Indication =>
                         Relocate_Node
                           (Subtype_Mark
                             (Access_Definition
                               (Component_Definition (Comp)))));
               end if;

               Decl := Make_Full_Type_Declaration (Loc,
                  Defining_Identifier => Anon_Access,
                  Type_Definition => Type_Def);

               Insert_Before (N, Decl);
               Analyze (Decl);

               Set_Access_Definition (Component_Definition (Comp), Empty);
               Set_Subtype_Indication (Component_Definition (Comp),
                  New_Occurrence_Of (Anon_Access, Loc));
               Set_Ekind (Anon_Access, E_Anonymous_Access_Type);
               Set_Is_Local_Anonymous_Access (Anon_Access);
            end if;

            Next (Comp);
         end loop;

         if Present (Variant_Part (Comp_List)) then
            declare
               V : Node_Id;
            begin
               V := First_Non_Pragma (Variants (Variant_Part (Comp_List)));
               while Present (V) loop
                  Check_Anonymous_Access_Types (Component_List (V));
                  Next_Non_Pragma (V);
               end loop;
            end;
         end if;
      end Check_Anonymous_Access_Types;

      --------------------------------------
      -- Make_Incomplete_Type_Declaration --
      --------------------------------------

      procedure Make_Incomplete_Type_Declaration is
         Decl : Node_Id;
         H    : Entity_Id;

      begin
         --  If there is a previous partial view, no need to create a new one

         if Prev /= T then
            return;

         elsif No (Inc_T) then
            Inc_T  := Make_Defining_Identifier (Loc, Chars (T));
            Decl   := Make_Incomplete_Type_Declaration (Loc, Inc_T);

            --  Type has already been inserted into the current scope.
            --  Remove it, and add incomplete declaration for type, so
            --  that subsequent anonymous access types can use it.

            H := Current_Entity (T);

            if H = T then
               Set_Name_Entity_Id (Chars (T), Empty);
            else
               while Present (H)
                 and then Homonym (H) /= T
               loop
                  H := Homonym (T);
               end loop;

               Set_Homonym (H, Homonym (T));
            end if;

            Insert_Before (N, Decl);
            Analyze (Decl);
            Set_Full_View (Inc_T, T);

            if Tagged_Present (Def) then
               Make_Class_Wide_Type (Inc_T);
               Set_Class_Wide_Type (T, Class_Wide_Type (Inc_T));
            end if;
         end if;
      end Make_Incomplete_Type_Declaration;

   --  Start of processing for Record_Type_Declaration

   begin
      --  These flags must be initialized before calling Process_Discriminants
      --  because this routine makes use of them.

      Set_Ekind               (T, E_Record_Type);
      Set_Etype               (T, T);
      Init_Size_Align         (T);
      Set_Abstract_Interfaces (T, No_Elist);
      Set_Stored_Constraint   (T, No_Elist);

      --  Normal case

      if Ada_Version < Ada_05
        or else not Interface_Present (Def)
      then
         --  The flag Is_Tagged_Type might have already been set by
         --  Find_Type_Name if it detected an error for declaration T. This
         --  arises in the case of private tagged types where the full view
         --  omits the word tagged.

         Is_Tagged :=
           Tagged_Present (Def)
             or else (Serious_Errors_Detected > 0 and then Is_Tagged_Type (T));

         Set_Is_Tagged_Type      (T, Is_Tagged);
         Set_Is_Limited_Record   (T, Limited_Present (Def));

         --  Type is abstract if full declaration carries keyword, or if
         --  previous partial view did.

         Set_Is_Abstract         (T, Is_Abstract (T)
                                      or else Abstract_Present (Def));

      else
         Is_Tagged := True;
         Analyze_Interface_Declaration (T, Def);
      end if;

      --  First pass: if there are self-referential access components,
      --  create the required anonymous access type declarations, and if
      --  need be an incomplete type declaration for T itself.

      Check_Anonymous_Access_Types (Component_List (Def));

      if Ada_Version >= Ada_05
        and then Present (Interface_List (Def))
      then
         declare
            Iface     : Node_Id;
            Iface_Def : Node_Id;
            Iface_Typ : Entity_Id;

         begin
            Iface := First (Interface_List (Def));
            while Present (Iface) loop
               Iface_Typ := Find_Type_Of_Subtype_Indic (Iface);
               Iface_Def := Type_Definition (Parent (Iface_Typ));

               if not Is_Interface (Iface_Typ) then
                  Error_Msg_NE ("(Ada 2005) & must be an interface",
                                Iface, Iface_Typ);

               else
                  --  "The declaration of a specific descendant of an
                  --  interface type freezes the interface type" RM 13.14

                  Freeze_Before (N, Iface_Typ);

                  --  Ada 2005 (AI-345): Protected interfaces can only
                  --  inherit from limited, synchronized or protected
                  --  interfaces.

                  if Protected_Present (Def) then
                     if Limited_Present (Iface_Def)
                       or else Synchronized_Present (Iface_Def)
                       or else Protected_Present (Iface_Def)
                     then
                        null;

                     elsif Task_Present (Iface_Def) then
                        Error_Msg_N ("(Ada 2005) protected interface cannot"
                          & " inherit from task interface", Iface);

                     else
                        Error_Msg_N ("(Ada 2005) protected interface cannot"
                          & " inherit from non-limited interface", Iface);
                     end if;

                  --  Ada 2005 (AI-345): Synchronized interfaces can only
                  --  inherit from limited and synchronized.

                  elsif Synchronized_Present (Def) then
                     if Limited_Present (Iface_Def)
                       or else Synchronized_Present (Iface_Def)
                     then
                        null;

                     elsif Protected_Present (Iface_Def) then
                        Error_Msg_N ("(Ada 2005) synchronized interface " &
                          "cannot inherit from protected interface", Iface);

                     elsif Task_Present (Iface_Def) then
                        Error_Msg_N ("(Ada 2005) synchronized interface " &
                          "cannot inherit from task interface", Iface);

                     else
                        Error_Msg_N ("(Ada 2005) synchronized interface " &
                          "cannot inherit from non-limited interface",
                          Iface);
                     end if;

                  --  Ada 2005 (AI-345): Task interfaces can only inherit
                  --  from limited, synchronized or task interfaces.

                  elsif Task_Present (Def) then
                     if Limited_Present (Iface_Def)
                       or else Synchronized_Present (Iface_Def)
                       or else Task_Present (Iface_Def)
                     then
                        null;

                     elsif Protected_Present (Iface_Def) then
                        Error_Msg_N ("(Ada 2005) task interface cannot" &
                          " inherit from protected interface", Iface);

                     else
                        Error_Msg_N ("(Ada 2005) task interface cannot" &
                          " inherit from non-limited interface", Iface);
                     end if;
                  end if;
               end if;

               Next (Iface);
            end loop;
            Set_Abstract_Interfaces (T, New_Elmt_List);
            Collect_Interfaces (Def, T);
         end;
      end if;

      --  Records constitute a scope for the component declarations within.
      --  The scope is created prior to the processing of these declarations.
      --  Discriminants are processed first, so that they are visible when
      --  processing the other components. The Ekind of the record type itself
      --  is set to E_Record_Type (subtypes appear as E_Record_Subtype).

      --  Enter record scope

      New_Scope (T);

      --  If an incomplete or private type declaration was already given for
      --  the type, then this scope already exists, and the discriminants have
      --  been declared within. We must verify that the full declaration
      --  matches the incomplete one.

      Check_Or_Process_Discriminants (N, T, Prev);

      Set_Is_Constrained     (T, not Has_Discriminants (T));
      Set_Has_Delayed_Freeze (T, True);

      --  For tagged types add a manually analyzed component corresponding
      --  to the component _tag, the corresponding piece of tree will be
      --  expanded as part of the freezing actions if it is not a CPP_Class.

      if Is_Tagged then

         --  Do not add the tag unless we are in expansion mode

         if Expander_Active then
            Tag_Comp := Make_Defining_Identifier (Sloc (Def), Name_uTag);
            Enter_Name (Tag_Comp);

            Set_Is_Tag                    (Tag_Comp);
            Set_Is_Aliased                (Tag_Comp);
            Set_Ekind                     (Tag_Comp, E_Component);
            Set_Etype                     (Tag_Comp, RTE (RE_Tag));
            Set_DT_Entry_Count            (Tag_Comp, No_Uint);
            Set_Original_Record_Component (Tag_Comp, Tag_Comp);
            Init_Component_Location       (Tag_Comp);

            --  Ada 2005 (AI-251): Addition of the Tag corresponding to all the
            --  implemented interfaces

            Add_Interface_Tag_Components (N, T);
         end if;

         Make_Class_Wide_Type (T);
         Set_Primitive_Operations (T, New_Elmt_List);
      end if;

      --  We must suppress range checks when processing the components
      --  of a record in the presence of discriminants, since we don't
      --  want spurious checks to be generated during their analysis, but
      --  must reset the Suppress_Range_Checks flags after having processed
      --  the record definition.

      if Has_Discriminants (T) and then not Range_Checks_Suppressed (T) then
         Set_Kill_Range_Checks (T, True);
         Record_Type_Definition (Def, Prev);
         Set_Kill_Range_Checks (T, False);
      else
         Record_Type_Definition (Def, Prev);
      end if;

      --  Exit from record scope

      End_Scope;

      if Expander_Active
        and then Is_Tagged
        and then not Is_Empty_List (Interface_List (Def))
      then
         --  Ada 2005 (AI-251): Derive the interface subprograms of all the
         --  implemented interfaces and check if some of the subprograms
         --  inherited from the ancestor cover some interface subprogram.

         Derive_Interface_Subprograms (T);
      end if;
   end Record_Type_Declaration;

   ----------------------------
   -- Record_Type_Definition --
   ----------------------------

   procedure Record_Type_Definition (Def : Node_Id; Prev_T : Entity_Id) is
      Component          : Entity_Id;
      Ctrl_Components    : Boolean := False;
      Final_Storage_Only : Boolean;
      T                  : Entity_Id;

   begin
      if Ekind (Prev_T) = E_Incomplete_Type then
         T := Full_View (Prev_T);
      else
         T := Prev_T;
      end if;

      Final_Storage_Only := not Is_Controlled (T);

      --  If the component list of a record type is defined by the reserved
      --  word null and there is no discriminant part, then the record type has
      --  no components and all records of the type are null records (RM 3.7)
      --  This procedure is also called to process the extension part of a
      --  record extension, in which case the current scope may have inherited
      --  components.

      if No (Def)
        or else No (Component_List (Def))
        or else Null_Present (Component_List (Def))
      then
         null;

      else
         Analyze_Declarations (Component_Items (Component_List (Def)));

         if Present (Variant_Part (Component_List (Def))) then
            Analyze (Variant_Part (Component_List (Def)));
         end if;
      end if;

      --  After completing the semantic analysis of the record definition,
      --  record components, both new and inherited, are accessible. Set
      --  their kind accordingly.

      Component := First_Entity (Current_Scope);
      while Present (Component) loop
         if Ekind (Component) = E_Void then
            Set_Ekind (Component, E_Component);
            Init_Component_Location (Component);
         end if;

         if Has_Task (Etype (Component)) then
            Set_Has_Task (T);
         end if;

         if Ekind (Component) /= E_Component then
            null;

         elsif Has_Controlled_Component (Etype (Component))
           or else (Chars (Component) /= Name_uParent
                    and then Is_Controlled (Etype (Component)))
         then
            Set_Has_Controlled_Component (T, True);
            Final_Storage_Only := Final_Storage_Only
              and then Finalize_Storage_Only (Etype (Component));
            Ctrl_Components := True;
         end if;

         Next_Entity (Component);
      end loop;

      --  A type is Finalize_Storage_Only only if all its controlled
      --  components are so.

      if Ctrl_Components then
         Set_Finalize_Storage_Only (T, Final_Storage_Only);
      end if;

      --  Place reference to end record on the proper entity, which may
      --  be a partial view.

      if Present (Def) then
         Process_End_Label (Def, 'e', Prev_T);
      end if;
   end Record_Type_Definition;

   ------------------------
   -- Replace_Components --
   ------------------------

   procedure Replace_Components (Typ : Entity_Id; Decl : Node_Id) is
      function Process (N : Node_Id) return Traverse_Result;

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
         Comp : Entity_Id;

      begin
         if Nkind (N) = N_Discriminant_Specification then
            Comp := First_Discriminant (Typ);
            while Present (Comp) loop
               if Chars (Comp) = Chars (Defining_Identifier (N)) then
                  Set_Defining_Identifier (N, Comp);
                  exit;
               end if;

               Next_Discriminant (Comp);
            end loop;

         elsif Nkind (N) = N_Component_Declaration then
            Comp := First_Component (Typ);
            while Present (Comp) loop
               if Chars (Comp) = Chars (Defining_Identifier (N)) then
                  Set_Defining_Identifier (N, Comp);
                  exit;
               end if;

               Next_Component (Comp);
            end loop;
         end if;

         return OK;
      end Process;

      procedure Replace is new Traverse_Proc (Process);

   --  Start of processing for Replace_Components

   begin
      Replace (Decl);
   end Replace_Components;

   -------------------------------
   -- Set_Completion_Referenced --
   -------------------------------

   procedure Set_Completion_Referenced (E : Entity_Id) is
   begin
      --  If in main unit, mark entity that is a completion as referenced,
      --  warnings go on the partial view when needed.

      if In_Extended_Main_Source_Unit (E) then
         Set_Referenced (E);
      end if;
   end Set_Completion_Referenced;

   ---------------------
   -- Set_Fixed_Range --
   ---------------------

   --  The range for fixed-point types is complicated by the fact that we
   --  do not know the exact end points at the time of the declaration. This
   --  is true for three reasons:

   --     A size clause may affect the fudging of the end-points
   --     A small clause may affect the values of the end-points
   --     We try to include the end-points if it does not affect the size

   --  This means that the actual end-points must be established at the point
   --  when the type is frozen. Meanwhile, we first narrow the range as
   --  permitted (so that it will fit if necessary in a small specified size),
   --  and then build a range subtree with these narrowed bounds.

   --  Set_Fixed_Range constructs the range from real literal values, and sets
   --  the range as the Scalar_Range of the given fixed-point type entity.

   --  The parent of this range is set to point to the entity so that it is
   --  properly hooked into the tree (unlike normal Scalar_Range entries for
   --  other scalar types, which are just pointers to the range in the
   --  original tree, this would otherwise be an orphan).

   --  The tree is left unanalyzed. When the type is frozen, the processing
   --  in Freeze.Freeze_Fixed_Point_Type notices that the range is not
   --  analyzed, and uses this as an indication that it should complete
   --  work on the range (it will know the final small and size values).

   procedure Set_Fixed_Range
     (E   : Entity_Id;
      Loc : Source_Ptr;
      Lo  : Ureal;
      Hi  : Ureal)
   is
      S : constant Node_Id :=
            Make_Range (Loc,
              Low_Bound  => Make_Real_Literal (Loc, Lo),
              High_Bound => Make_Real_Literal (Loc, Hi));

   begin
      Set_Scalar_Range (E, S);
      Set_Parent (S, E);
   end Set_Fixed_Range;

   ----------------------------------
   -- Set_Scalar_Range_For_Subtype --
   ----------------------------------

   procedure Set_Scalar_Range_For_Subtype
     (Def_Id : Entity_Id;
      R      : Node_Id;
      Subt   : Entity_Id)
   is
      Kind : constant Entity_Kind :=  Ekind (Def_Id);

   begin
      Set_Scalar_Range (Def_Id, R);

      --  We need to link the range into the tree before resolving it so
      --  that types that are referenced, including importantly the subtype
      --  itself, are properly frozen (Freeze_Expression requires that the
      --  expression be properly linked into the tree). Of course if it is
      --  already linked in, then we do not disturb the current link.

      if No (Parent (R)) then
         Set_Parent (R, Def_Id);
      end if;

      --  Reset the kind of the subtype during analysis of the range, to
      --  catch possible premature use in the bounds themselves.

      Set_Ekind (Def_Id, E_Void);
      Process_Range_Expr_In_Decl (R, Subt);
      Set_Ekind (Def_Id, Kind);

   end Set_Scalar_Range_For_Subtype;

   --------------------------------------------------------
   -- Set_Stored_Constraint_From_Discriminant_Constraint --
   --------------------------------------------------------

   procedure Set_Stored_Constraint_From_Discriminant_Constraint
     (E : Entity_Id)
   is
   begin
      --  Make sure set if encountered during Expand_To_Stored_Constraint

      Set_Stored_Constraint (E, No_Elist);

      --  Give it the right value

      if Is_Constrained (E) and then Has_Discriminants (E) then
         Set_Stored_Constraint (E,
           Expand_To_Stored_Constraint (E, Discriminant_Constraint (E)));
      end if;
   end Set_Stored_Constraint_From_Discriminant_Constraint;

   -------------------------------------
   -- Signed_Integer_Type_Declaration --
   -------------------------------------

   procedure Signed_Integer_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Implicit_Base : Entity_Id;
      Base_Typ      : Entity_Id;
      Lo_Val        : Uint;
      Hi_Val        : Uint;
      Errs          : Boolean := False;
      Lo            : Node_Id;
      Hi            : Node_Id;

      function Can_Derive_From (E : Entity_Id) return Boolean;
      --  Determine whether given bounds allow derivation from specified type

      procedure Check_Bound (Expr : Node_Id);
      --  Check bound to make sure it is integral and static. If not, post
      --  appropriate error message and set Errs flag

      ---------------------
      -- Can_Derive_From --
      ---------------------

      --  Note we check both bounds against both end values, to deal with
      --  strange types like ones with a range of 0 .. -12341234.

      function Can_Derive_From (E : Entity_Id) return Boolean is
         Lo : constant Uint := Expr_Value (Type_Low_Bound (E));
         Hi : constant Uint := Expr_Value (Type_High_Bound (E));
      begin
         return Lo <= Lo_Val and then Lo_Val <= Hi
                  and then
                Lo <= Hi_Val and then Hi_Val <= Hi;
      end Can_Derive_From;

      -----------------
      -- Check_Bound --
      -----------------

      procedure Check_Bound (Expr : Node_Id) is
      begin
         --  If a range constraint is used as an integer type definition, each
         --  bound of the range must be defined by a static expression of some
         --  integer type, but the two bounds need not have the same integer
         --  type (Negative bounds are allowed.) (RM 3.5.4)

         if not Is_Integer_Type (Etype (Expr)) then
            Error_Msg_N
              ("integer type definition bounds must be of integer type", Expr);
            Errs := True;

         elsif not Is_OK_Static_Expression (Expr) then
            Flag_Non_Static_Expr
              ("non-static expression used for integer type bound!", Expr);
            Errs := True;

         --  The bounds are folded into literals, and we set their type to be
         --  universal, to avoid typing difficulties: we cannot set the type
         --  of the literal to the new type, because this would be a forward
         --  reference for the back end,  and if the original type is user-
         --  defined this can lead to spurious semantic errors (e.g. 2928-003).

         else
            if Is_Entity_Name (Expr) then
               Fold_Uint (Expr, Expr_Value (Expr), True);
            end if;

            Set_Etype (Expr, Universal_Integer);
         end if;
      end Check_Bound;

   --  Start of processing for Signed_Integer_Type_Declaration

   begin
      --  Create an anonymous base type

      Implicit_Base :=
        Create_Itype (E_Signed_Integer_Type, Parent (Def), T, 'B');

      --  Analyze and check the bounds, they can be of any integer type

      Lo := Low_Bound (Def);
      Hi := High_Bound (Def);

      --  Arbitrarily use Integer as the type if either bound had an error

      if Hi = Error or else Lo = Error then
         Base_Typ := Any_Integer;
         Set_Error_Posted (T, True);

      --  Here both bounds are OK expressions

      else
         Analyze_And_Resolve (Lo, Any_Integer);
         Analyze_And_Resolve (Hi, Any_Integer);

         Check_Bound (Lo);
         Check_Bound (Hi);

         if Errs then
            Hi := Type_High_Bound (Standard_Long_Long_Integer);
            Lo := Type_Low_Bound (Standard_Long_Long_Integer);
         end if;

         --  Find type to derive from

         Lo_Val := Expr_Value (Lo);
         Hi_Val := Expr_Value (Hi);

         if Can_Derive_From (Standard_Short_Short_Integer) then
            Base_Typ := Base_Type (Standard_Short_Short_Integer);

         elsif Can_Derive_From (Standard_Short_Integer) then
            Base_Typ := Base_Type (Standard_Short_Integer);

         elsif Can_Derive_From (Standard_Integer) then
            Base_Typ := Base_Type (Standard_Integer);

         elsif Can_Derive_From (Standard_Long_Integer) then
            Base_Typ := Base_Type (Standard_Long_Integer);

         elsif Can_Derive_From (Standard_Long_Long_Integer) then
            Base_Typ := Base_Type (Standard_Long_Long_Integer);

         else
            Base_Typ := Base_Type (Standard_Long_Long_Integer);
            Error_Msg_N ("integer type definition bounds out of range", Def);
            Hi := Type_High_Bound (Standard_Long_Long_Integer);
            Lo := Type_Low_Bound (Standard_Long_Long_Integer);
         end if;
      end if;

      --  Complete both implicit base and declared first subtype entities

      Set_Etype          (Implicit_Base, Base_Typ);
      Set_Scalar_Range   (Implicit_Base, Scalar_Range   (Base_Typ));
      Set_Size_Info      (Implicit_Base,                (Base_Typ));
      Set_RM_Size        (Implicit_Base, RM_Size        (Base_Typ));
      Set_First_Rep_Item (Implicit_Base, First_Rep_Item (Base_Typ));

      Set_Ekind          (T, E_Signed_Integer_Subtype);
      Set_Etype          (T, Implicit_Base);

      Set_Size_Info      (T,                (Implicit_Base));
      Set_First_Rep_Item (T, First_Rep_Item (Implicit_Base));
      Set_Scalar_Range   (T, Def);
      Set_RM_Size        (T, UI_From_Int (Minimum_Size (T)));
      Set_Is_Constrained (T);
   end Signed_Integer_Type_Declaration;

end Sem_Ch3;
