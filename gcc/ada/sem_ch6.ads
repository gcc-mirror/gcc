------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 6                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
   --  pragma Ordered (Conformance_Type);
   --  Why is above line commented out ???
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
   procedure Analyze_Simple_Return_Statement         (N : Node_Id);
   procedure Analyze_Subprogram_Declaration          (N : Node_Id);
   procedure Analyze_Subprogram_Body                 (N : Node_Id);

   procedure Analyze_Subprogram_Contract (Subp : Entity_Id);
   --  Analyze all delayed aspects chained on the contract of subprogram Subp
   --  as if they appeared at the end of a declarative region.

   function Analyze_Subprogram_Specification (N : Node_Id) return Entity_Id;
   --  Analyze subprogram specification in both subprogram declarations
   --  and body declarations. Returns the defining entity for the
   --  specification N.

   procedure Cannot_Inline
     (Msg        : String;
      N          : Node_Id;
      Subp       : Entity_Id;
      Is_Serious : Boolean := False);
   --  This procedure is called if the node N, an instance of a call to
   --  subprogram Subp, cannot be inlined. Msg is the message to be issued,
   --  which ends with ? (it does not end with ?p?, this routine takes care of
   --  the need to change ? to ?p?). Temporarily the behavior of this routine
   --  depends on the value of -gnatd.k:
   --
   --    * If -gnatd.k is not set (ie. old inlining model) then if Subp has
   --      a pragma Always_Inlined, then an error message is issued (by
   --      removing the last character of Msg). If Subp is not Always_Inlined,
   --      then a warning is issued if the flag Ineffective_Inline_Warnings
   --      is set, adding ?p to the msg, and if not, the call has no effect.
   --
   --    * If -gnatd.k is set (ie. new inlining model) then:
   --      - If Is_Serious is true, then an error is reported (by removing the
   --        last character of Msg);
   --
   --      - otherwise:
   --
   --        * Compiling without optimizations if Subp has a pragma
   --          Always_Inlined, then an error message is issued; if Subp is
   --          not Always_Inlined, then a warning is issued if the flag
   --          Ineffective_Inline_Warnings is set (adding p?), and if not,
   --          the call has no effect.
   --
   --        * Compiling with optimizations then a warning is issued if the
   --          flag Ineffective_Inline_Warnings is set (adding p?); otherwise
   --          no effect since inlining may be performed by the backend.

   procedure Check_Conventions (Typ : Entity_Id);
   --  Ada 2005 (AI-430): Check that the conventions of all inherited and
   --  overridden dispatching operations of type Typ are consistent with their
   --  respective counterparts.

   procedure Check_Delayed_Subprogram (Designator : Entity_Id);
   --  Designator can be a E_Subprogram_Type, E_Procedure or E_Function. If a
   --  type in its profile depends on a private type without a full
   --  declaration, indicate that the subprogram or type is delayed.

   procedure Check_Discriminant_Conformance
     (N        : Node_Id;
      Prev     : Entity_Id;
      Prev_Loc : Node_Id);
   --  Check that the discriminants of a full type N fully conform to the
   --  discriminants of the corresponding partial view Prev. Prev_Loc indicates
   --  the source location of the partial view, which may be different than
   --  Prev in the case of private types.

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
   --  declaration, body, renaming, or instantiation.  Overridden_Subp is set
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

   procedure Create_Extra_Formals (E : Entity_Id);
   --  For each parameter of a subprogram or entry that requires an additional
   --  formal (such as for access parameters and indefinite discriminated
   --  parameters), creates the appropriate formal and attach it to its
   --  associated parameter. Each extra formal will also be appended to
   --  the end of Subp's parameter list (with each subsequent extra formal
   --  being attached to the preceding extra formal).

   function Find_Corresponding_Spec
     (N          : Node_Id;
      Post_Error : Boolean := True) return Entity_Id;
   --  Use the subprogram specification in the body to retrieve the previous
   --  subprogram declaration, if any.

   function Fully_Conformant (New_Id, Old_Id : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are fully conformant (RM 6.3.1(17))

   function Fully_Conformant_Expressions
     (Given_E1 : Node_Id;
      Given_E2 : Node_Id) return Boolean;
   --  Determines if two (non-empty) expressions are fully conformant
   --  as defined by (RM 6.3.1(18-21))

   function Fully_Conformant_Discrete_Subtypes
      (Given_S1 : Node_Id;
       Given_S2 : Node_Id) return Boolean;
   --  Determines if two subtype definitions are fully conformant. Used
   --  for entry family conformance checks (RM 6.3.1 (24)).

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

   procedure Set_Formal_Mode (Formal_Id : Entity_Id);
   --  Set proper Ekind to reflect formal mode (in, out, in out)

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

end Sem_Ch6;
