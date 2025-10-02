------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 6                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Types; use Types;
package Sem_Ch6 is

   type Conformance_Type is
     (Type_Conformant, Mode_Conformant, Subtype_Conformant, Fully_Conformant);
   pragma Ordered (Conformance_Type);
   --  Conformance type used in conformance checks between specs and bodies,
   --  and for overriding. The literals match the RM definitions of the
   --  corresponding terms. This is an ordered type, since each conformance
   --  type is stronger than the ones preceding it.

   procedure Analyze_Abstract_Subprogram_Declaration (N : Node_Id);
   procedure Analyze_Expression_Function             (N : Node_Id);
   procedure Analyze_Extended_Return_Statement       (N : Node_Id);
   procedure Analyze_Function_Call                   (N : Node_Id);
   procedure Analyze_Operator_Symbol                 (N : Node_Id);
   procedure Analyze_Parameter_Association           (N : Node_Id);
   procedure Analyze_Procedure_Call                  (N : Node_Id);
   procedure Analyze_Return_When_Statement           (N : Node_Id);
   procedure Analyze_Simple_Return_Statement         (N : Node_Id);
   procedure Analyze_Subprogram_Declaration          (N : Node_Id);
   procedure Analyze_Subprogram_Body                 (N : Node_Id);

   function Analyze_Subprogram_Specification (N : Node_Id) return Entity_Id;
   --  Analyze subprogram specification in both subprogram declarations
   --  and body declarations. Returns the defining entity for the
   --  specification N.

   procedure Analyze_SPARK_Subprogram_Specification (N : Node_Id);
   --  Check SPARK legality rules that require that the specification has been
   --  analyzed already.

   function Can_Override_Operator (Subp : Entity_Id) return Boolean;
   --  Returns true if Subp can override a predefined operator

   procedure Check_Conventions (Typ : Entity_Id);
   --  Ada 2005 (AI-430): Check that the conventions of all inherited and
   --  overridden dispatching operations of type Typ are consistent with their
   --  respective counterparts.

   procedure Check_Delayed_Subprogram (Designator : Entity_Id);
   --  Designator can be a E_Subprogram_Type, E_Procedure or E_Function. Set
   --  Has_Delayed_Freeze on Designator if its freezing needs to be delayed.

   procedure Check_Formal_Subprogram_Conformance
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty);
   --  Check RM 6.3.1(17/3): the profile of a generic formal subprogram is not
   --  subtype conformant with any other profile and post an error message if
   --  either New_Id or Old_Id denotes a formal subprogram, with the flag being
   --  placed on the Err_Loc node if it is specified, and on New_Id if not. See
   --  also spec of Check_Fully_Conformant below for New_Id and Old_Id usage.

   procedure Check_Fully_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty);
   --  Check that two callable entities (subprograms, entries, literals)
   --  are fully conformant, post error message if not (RM 6.3.1(17)) with
   --  the flag being placed on the Err_Loc node if it is specified, and
   --  on the appropriate component of the New_Id construct if not. Note:
   --  when checking spec/body conformance, New_Id must be the body entity
   --  and Old_Id is the spec entity (the code in the implementation relies
   --  on this ordering, and in any case, this makes sense, since if flags
   --  are to be placed on the construct, they clearly belong on the body.

   procedure Check_Mode_Conformant
     (New_Id   : Entity_Id;
      Old_Id   : Entity_Id;
      Err_Loc  : Node_Id := Empty;
      Get_Inst : Boolean := False);
   --  Check that two callable entities (subprograms, entries, literals)
   --  are mode conformant, post error message if not (RM 6.3.1(15)) with
   --  the flag being placed on the Err_Loc node if it is specified, and
   --  on the appropriate component of the New_Id construct if not. The
   --  argument Get_Inst is set to True when this is a check against a
   --  formal access-to-subprogram type, indicating that mapping of types
   --  is needed.

   procedure Check_Overriding_Indicator
     (Subp            : Entity_Id;
      Overridden_Subp : Entity_Id;
      Is_Primitive    : Boolean);
   --  Verify the consistency of an overriding_indicator given for subprogram
   --  declaration, body, renaming, or instantiation. Overridden_Subp is set
   --  if the scope where we are introducing the subprogram contains a
   --  type-conformant subprogram that becomes hidden by the new subprogram.
   --  Is_Primitive indicates whether the subprogram is primitive.

   procedure Check_Subtype_Conformant
     (New_Id                   : Entity_Id;
      Old_Id                   : Entity_Id;
      Err_Loc                  : Node_Id := Empty;
      Skip_Controlling_Formals : Boolean := False;
      Get_Inst                 : Boolean := False);
   --  Check that two callable entities (subprograms, entries, literals)
   --  are subtype conformant, post error message if not (RM 6.3.1(16)),
   --  the flag being placed on the Err_Loc node if it is specified, and
   --  on the appropriate component of the New_Id construct if not.
   --  Skip_Controlling_Formals is True when checking the conformance of
   --  a subprogram that implements an interface operation. In that case,
   --  only the non-controlling formals can (and must) be examined. The
   --  argument Get_Inst is set to True when this is a check against a
   --  formal access-to-subprogram type, indicating that mapping of types
   --  is needed.

   procedure Check_Synchronized_Overriding
     (Def_Id          : Entity_Id;
      Overridden_Subp : out Entity_Id);
   --  First determine if Def_Id is an entry or a subprogram either defined in
   --  the scope of a task or protected type, or that is a primitive of such
   --  a type. Check whether Def_Id overrides a subprogram of an interface
   --  implemented by the synchronized type, returning the overridden entity
   --  or Empty.

   procedure Check_Type_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty);
   --  Check that two callable entities (subprograms, entries, literals)
   --  are type conformant, post error message if not (RM 6.3.1(14)) with
   --  the flag being placed on the Err_Loc node if it is specified, and
   --  on the appropriate component of the New_Id construct if not.

   function Conforming_Types
     (T1       : Entity_Id;
      T2       : Entity_Id;
      Ctype    : Conformance_Type;
      Get_Inst : Boolean := False) return Boolean;
   --  Check that the types of two formal parameters are conforming. In most
   --  cases this is just a name comparison, but within an instance it involves
   --  generic actual types, and in the presence of anonymous access types
   --  it must examine the designated types. The argument Get_Inst is set to
   --  True when this is a check against a formal access-to-subprogram type,
   --  indicating that mapping of types is needed.

   procedure Create_Extra_Formals
     (E           : Entity_Id;
      Related_Nod : Node_Id := Empty);
   --  For each parameter of a subprogram or entry that requires an additional
   --  formal (such as for access parameters and indefinite discriminated
   --  parameters), creates the appropriate formal and attach it to its
   --  associated parameter. Each extra formal will also be appended to
   --  the end of Subp's parameter list (with each subsequent extra formal
   --  being attached to the preceding extra formal).
   --
   --  Related_Nod is the node motivating the frontend call to create the
   --  extra formals; it is not passed when the node causing the call is E
   --  (for example, as part of freezing E). Related_Nod provides the context
   --  where the extra formals are created, and it is used to determine if
   --  the creation of the extra formals can be deferred when the underlying
   --  type of some formal (or its return type) is not available, and thus
   --  improve the support for AI05-0151-1/08.

   function Extra_Formals_Match_OK
     (E     : Entity_Id;
      Ref_E : Entity_Id) return Boolean;
   --  Return True if the extra formals of the given entities match. E is a
   --  subprogram, and Ref_E is the reference entity that will be used to check
   --  the extra formals of E: a subprogram type or another subprogram. For
   --  example, if E is a dispatching primitive of a tagged type then Ref_E
   --  may be the overridden primitive of its parent type or its ultimate
   --  renamed entity; however, if E is a subprogram to which 'Access is
   --  applied then Ref_E is its corresponding subprogram type. Used in
   --  assertions.

   function Extra_Formals_OK (E : Entity_Id) return Boolean;
   --  Return True if the decoration of the attributes associated with extra
   --  formals are properly set. Used in assertions.

   function Find_Corresponding_Spec
     (N          : Node_Id;
      Post_Error : Boolean := True) return Entity_Id;
   --  Use the subprogram specification in the body to retrieve the previous
   --  subprogram declaration, if any.

   procedure Freeze_Extra_Formals (E : Entity_Id);
   --  Given a subprogram, subprogram type, or entry, flag E to indicate that
   --  its extra formals (if any) are known (by setting Extra_Formals_Known).
   --  This subprogram serves three purposes: (1) Document the places where
   --  the extra formals are known, (2) Ensure that extra formals are added
   --  only once, and (3) Provide a convenient place for setting a debugger
   --  breakpoint to locate when extra formals are known.

   function Fully_Conformant (New_Id, Old_Id : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are fully conformant (RM 6.3.1(17))

   function Fully_Conformant_Expressions
     (Given_E1 : Node_Id;
      Given_E2 : Node_Id;
      Report   : Boolean := False) return Boolean;
   --  Determines if two (non-empty) expressions are fully conformant
   --  as defined by (RM 6.3.1(18-21))

   function Fully_Conformant_Discrete_Subtypes
      (Given_S1 : Node_Id;
       Given_S2 : Node_Id) return Boolean;
   --  Determines if two subtype definitions are fully conformant. Used
   --  for entry family conformance checks (RM 6.3.1 (24)).

   function Has_BIP_Formals (E : Entity_Id) return Boolean;
   --  Determines if a given entity has build-in-place formals

   procedure Install_Entity (E : Entity_Id);
   --  Place a single entity on the visibility chain

   procedure Install_Formals (Id : Entity_Id);
   --  On entry to a subprogram body, make the formals visible. Note that
   --  simply placing the subprogram on the scope stack is not sufficient:
   --  the formals must become the current entities for their names. This
   --  procedure is also used to get visibility to the formals when analyzing
   --  preconditions and postconditions appearing in the spec.

   function Is_Interface_Conformant
     (Tagged_Type : Entity_Id;
      Iface_Prim  : Entity_Id;
      Prim        : Entity_Id) return Boolean;
   --  Returns true if both primitives have a matching name (including support
   --  for names of inherited private primitives --which have suffix 'P'), they
   --  are type conformant, and Prim is defined in the scope of Tagged_Type.
   --  Special management is done for functions returning interfaces.

   procedure List_Inherited_Pre_Post_Aspects (E : Entity_Id);
   --  E is the entity for a subprogram or generic subprogram spec. This call
   --  lists all inherited Pre/Post aspects if List_Inherited_Pre_Post is True.

   procedure May_Need_Actuals (Fun : Entity_Id);
   --  Flag functions that can be called without parameters, i.e. those that
   --  have no parameters, or those for which defaults exist for all parameters
   --  Used for subprogram declarations and for access subprogram declarations,
   --  where they apply to the anonymous designated type. On return the flag
   --  Set_Needs_No_Actuals is set appropriately in Fun.

   function Mode_Conformant (New_Id, Old_Id : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are mode conformant (RM 6.3.1(15))

   procedure New_Overloaded_Entity
     (S            : Entity_Id;
      Derived_Type : Entity_Id := Empty);
   --  Process new overloaded entity. Overloaded entities are created by
   --  enumeration type declarations, subprogram specifications, entry
   --  declarations, and (implicitly) by type derivations. If Derived_Type
   --  is non-empty then this is a subprogram derived for that type.

   procedure Process_Formals (T : List_Id; Related_Nod : Node_Id);
   --  Enter the formals in the scope of the subprogram or entry, and
   --  analyze default expressions if any. The implicit types created for
   --  access parameter are attached to the Related_Nod which comes from the
   --  context.

   procedure Reference_Body_Formals (Spec : Entity_Id; Bod : Entity_Id);
   --  If there is a separate spec for a subprogram or generic subprogram, the
   --  formals of the body are treated as references to the corresponding
   --  formals of the spec. This reference does not count as an actual use of
   --  the formal, in order to diagnose formals that are unused in the body.
   --  This procedure is also used in renaming_as_body declarations, where
   --  the formals of the specification must be treated as body formals that
   --  correspond to the previous subprogram declaration, and not as new
   --  entities with their defining entry in the cross-reference information.

   procedure Set_Actual_Subtypes (N : Node_Id; Subp : Entity_Id);
   --  If the formals of a subprogram are unconstrained, build a subtype
   --  declaration that uses the bounds or discriminants of the actual to
   --  construct an actual subtype for them. This is an optimization that
   --  is done only in some cases where the actual subtype cannot change
   --  during execution of the subprogram. By setting the actual subtype
   --  once, we avoid recomputing it unnecessarily.

   function Subtype_Conformant
     (New_Id                   : Entity_Id;
      Old_Id                   : Entity_Id;
      Skip_Controlling_Formals : Boolean := False) return Boolean;
   --  Determine whether two callable entities (subprograms, entries, literals)
   --  are subtype conformant (RM 6.3.1(16)). Skip_Controlling_Formals is True
   --  when checking the conformance of a subprogram that implements an
   --  interface operation. In that case, only the non-controlling formals
   --  can (and must) be examined.

   function Type_Conformant
     (New_Id                   : Entity_Id;
      Old_Id                   : Entity_Id;
      Skip_Controlling_Formals : Boolean := False) return Boolean;
   --  Determine whether two callable entities (subprograms, entries, literals)
   --  are type conformant (RM 6.3.1(14)). Skip_Controlling_Formals is True
   --  when checking the conformance of a subprogram that implements an
   --  interface operation. In that case, only the non-controlling formals
   --  can (and must) be examined.

   procedure Valid_Operator_Definition (Designator : Entity_Id);
   --  Verify that an operator definition has the proper number of formals

   ------------------------------------
   -- Deferred_Extra_Formals_Support --
   ------------------------------------

   --  This package provides support for deferring the addition of extra
   --  formals to subprograms, entries, and subprogram types; it also provides
   --  support for deferring the addition of extra actuals to direct calls to
   --  subprograms and entries, and indirect calls through subprogram types.
   --  The addition of the extra formals and actuals is deferred until the
   --  underlying type of all the parameters and result types of registered
   --  subprograms, entries, and subprogram types is known.

   --  Functional Description
   --  ----------------------
   --
   --  When Create_Extra_Formals identifies that the underlying type of
   --  some parameter or result type of an entity E is not available, E is
   --  registered by this package, and the addition of its extra formals is
   --  deferred. As part of this registration, the types of all the params
   --  and result types of E with no underlying type are also registered.
   --
   --  When Expand_Call_Helper identifies that the underlying type of some
   --  parameter or result type of a called entity is not available, the call
   --  is registered by Register_Deferred_Extra_Formals_Call, and the addition
   --  of its extra actuals is deferred.
   --
   --  When the full type declaration of some registered type T is analyzed,
   --  the subprogram Add_Deferred_Extra_Params is invoked; this subprogram
   --  does the following actions:
   --    1) Check all the registered entities (subprograms, entries, and
   --       subprogram types); for each registered entity that has all its
   --       underlying types available, call Create_Extra_Formals, and
   --       unregister the entity.
   --    2) Check all the registered calls; for each registered call that
   --       has available the underlying type of all the parameters and result
   --       types of the called entity, call Create_Extra_Actuals, and
   --       unregister the call.
   --    3) Unregister T.
   --
   --  Example 1
   --  ---------
   --  A package spec has a private type declaration T, and declarations of
   --  expression functions and/or primitives with class-wide conditions
   --  invoking primitives of type T before the full view of T is defined.
   --
   --  As part of processing the early freezing of the called subprograms
   --  (and as part of processing the calls) the functions are registered as
   --  subprograms with deferred extra formals, and the calls are registered
   --  as calls with deferred extra actuals.
   --
   --  When the full type declaration of T is analyzed, extra formals are
   --  added to all the registered subprograms, and extra actuals are added
   --  to all the registered calls with deferred extra actuals.
   --
   --  Example 2
   --  ---------
   --  The specification of package P has a limited_with_clause on package Q,
   --  and the type of the formals of subprograms defined in P are types
   --  defined in Q.
   --
   --  When compiling the spec of P, similarly to the previous example,
   --  subprograms with incomplete formals are registered as subprograms
   --  with deferred extra formals; if the spec of P has calls to these
   --  subprograms, then these calls are registered as calls with deferred
   --  extra actuals. That is, when the analysis of package P completes,
   --  deferred extra formals and actuals have not been added.
   --
   --  When another compilation unit is analyzed (including the body of
   --  package P), and a regular with-clause on Q is processed, when the
   --  full type declaration of deferred entities is analyzed, deferred
   --  extra formals and deferred extra actuals are added.
   --
   --  This machinery relies on the GNAT Compilation Model; that is, when
   --  we analyze the spec of P (for which we generally don't generate code),
   --  it is safe to complete the compilation and still have entities with
   --  deferred extra formals, and calls with deferred extra actuals.
   --
   --  The body package P generally has a regular with-clause on package Q.
   --  Hence, when we compile the body of package P, the implicit dependence
   --  on its package spec causes the analysis of the spec of P (thus
   --  registering deferred entities), followed by the analysis of context
   --  clauses in the body of P. When the regular with-clause on package Q
   --  is analyzed, we add the extra formals and extra actuals to deferred
   --  entities. Thus, the generated code will have all the needed formals.
   --
   --  The (still) unsupported case is when the body of package P does not
   --  have a regular with-clause on package Q (AI05-0151-1/08). This case
   --  is left documented in the front-end sources by means of calls to
   --  the following subprograms: Is_Unsupported_Extra_Formals_Entity, and
   --  Is_Unsupported_Extra_Actuals_Call.

   package Deferred_Extra_Formals_Support is

      procedure Add_Deferred_Extra_Params (Typ : Entity_Id);
      --  Check all the registered subprograms, entries, and subprogram types
      --  with deferred addition of their extra formals; if the underlying
      --  types of all their formals is available then add their extra formals.
      --  Check also all the registered calls with deferred addition of their
      --  extra actuals; add their extra actuals if the underlying types of all
      --  their parameters and result types are available. Finally unregister
      --  Typ from the list of types used for the deferral of extra formals/
      --  actuals.

      procedure Register_Deferred_Extra_Formals_Entity (Id : Entity_Id);
      --  Register the given subprogram, entry, or subprogram type to defer the
      --  addition of its extra formals.

      procedure Register_Deferred_Extra_Formals_Call
        (Call_Node : Node_Id;
         Scope_Id  : Entity_Id);
      --  Register the given call, performed from the given scope, to defer the
      --  addition of its extra actuals.

      function Has_Deferred_Extra_Formals (Typ : Entity_Id) return Boolean;
      --  Return True if there some registered subprogram, subprogram type, or
      --  entry with deferred extra formals that has some formal type or
      --  result type of type Typ (i.e. which depends on the given type to
      --  add its extra formals).

      function Is_Deferred_Extra_Formals_Entity
        (Id : Entity_Id) return Boolean;
      --  Return True if Id is a subprogram, subprogram type, or entry that has
      --  been registered to defer the addition of its extra formals.

      function Is_Unsupported_Extra_Formals_Entity
        (Id          : Entity_Id;
         Related_Nod : Node_Id := Empty) return Boolean;
      --  Id is a subprogram, subprogram type, or entry. Return True if Id is
      --  unsupported for deferring the addition of its extra formals; that is,
      --  it is defined in a compilation unit that is a package body or a
      --  subprogram body, and the underlying type of some of its parameters
      --  or result type is not available. Related_Nod is the node where this
      --  check is performed (it is generally a subprogram call); if it is not
      --  available then the location of entity Id is used as its related node.
      --
      --  The context for this case is an unsupported case of AI05-0151-1/08
      --  that allows incomplete tagged types as parameter and result types.
      --  More concretely, a type T is visible in a package spec through a
      --  limited_with_clause, and the body of the package has no regular
      --  with_clause. In such a case, the machinery for deferring the
      --  addition of extra formals does not work because the underlying
      --  type of the type is not seen during the compilation of the
      --  package body.
      --
      --  The purpose of this function is to facilitate locating in the sources
      --  the places where the front end performs the current (incomplete)
      --  management of such case (to facilitate further work) ???

      function Is_Unsupported_Extra_Actuals_Call
        (Call_Node : Node_Id; Id : Entity_Id) return Boolean;
      --  Same as previous function but applicable to a call to the given
      --  entity Id.

   end Deferred_Extra_Formals_Support;

end Sem_Ch6;
