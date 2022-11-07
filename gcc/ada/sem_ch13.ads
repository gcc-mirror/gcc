------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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
with Sem_Disp; use Sem_Disp;
with Uintp; use Uintp;

package Sem_Ch13 is
   function All_Membership_Choices_Static (Expr : Node_Id) return Boolean;
   --  Given a membership test, returns True iff all choices are static.

   procedure Analyze_At_Clause                          (N : Node_Id);
   procedure Analyze_Attribute_Definition_Clause        (N : Node_Id);
   procedure Analyze_Enumeration_Representation_Clause  (N : Node_Id);
   procedure Analyze_Free_Statement                     (N : Node_Id);
   procedure Analyze_Freeze_Entity                      (N : Node_Id);
   procedure Analyze_Freeze_Generic_Entity              (N : Node_Id);
   procedure Analyze_Record_Representation_Clause       (N : Node_Id);
   procedure Analyze_Code_Statement                     (N : Node_Id);

   procedure Analyze_Aspect_Specifications (N : Node_Id; E : Entity_Id);
   --  This procedure is called to analyze aspect specifications for node N. E
   --  is the corresponding entity declared by the declaration node N. Callers
   --  should check that Has_Aspects (N) is True before calling this routine.

   procedure Analyze_Aspects_On_Subprogram_Body_Or_Stub (N : Node_Id);
   --  Analyze the aspect specifications of [generic] subprogram body or stub
   --  N. Callers should check that Has_Aspects (N) is True before calling the
   --  routine. This routine diagnoses misplaced aspects that should appear on
   --  the initial declaration of N and offers suggestions for replacements.

   procedure Adjust_Record_For_Reverse_Bit_Order (R : Entity_Id);
   --  Called from Freeze where R is a record entity for which reverse bit
   --  order is specified and there is at least one component clause. Note:
   --  component positions are normally adjusted as per AI95-0133, unless
   --  -gnatd.p is used to restore original Ada 95 mode.

   procedure Check_Record_Representation_Clause (N : Node_Id);
   --  This procedure completes the analysis of a record representation clause
   --  N. It is called at freeze time after adjustment of component clause bit
   --  positions for possible non-standard bit order. In the case of Ada 2005
   --  (machine scalar) mode, this adjustment can make substantial changes, so
   --  some checks, in particular for component overlaps cannot be done at the
   --  time the record representation clause is first seen, but must be delayed
   --  till freeze time, and in particular is called after calling the above
   --  procedure for adjusting record bit positions for reverse bit order.

   procedure Initialize;
   --  Initialize internal tables for new compilation

   procedure Kill_Rep_Clause (N : Node_Id);
   --  This procedure is called for a rep clause N when we are in -gnatI mode
   --  (Ignore_Rep_Clauses). It replaces the node N with a null statement. This
   --  is only called if Ignore_Rep_Clauses is True.

   procedure Set_Enum_Esize (T : Entity_Id);
   --  This routine sets the Esize field for an enumeration type T, based
   --  on the current representation information available for T. Note that
   --  the setting of the RM_Size field is not affected. This routine also
   --  initializes the alignment field to zero.

   Unknown_Minimum_Size : constant Nonzero_Int := -1;

   function Minimum_Size
     (T      : Entity_Id;
      Biased : Boolean := False) return Int;
   --  Given an elementary type, determines the minimum number of bits required
   --  to represent all values of the type. This function may not be called
   --  with any other types. If the flag Biased is set True, then the minimum
   --  size calculation that biased representation is used in the case of a
   --  discrete type, e.g. the range 7..8 gives a minimum size of 4 with
   --  Biased set to False, and 1 with Biased set to True. Note that the
   --  biased parameter only has an effect if the type is not biased, it
   --  causes Minimum_Size to indicate the minimum size of an object with
   --  the given type, of the size the type would have if it were biased. If
   --  the type is already biased, then Minimum_Size returns the biased size,
   --  regardless of the setting of Biased. Also, fixed-point types are never
   --  biased in the current implementation. If the size is not known at
   --  compile time, this function returns Unknown_Minimum_Size.

   procedure Check_Constant_Address_Clause (Expr : Node_Id; U_Ent : Entity_Id);
   --  Expr is an expression for an address clause. This procedure checks
   --  that the expression is constant, in the limited sense that it is safe
   --  to evaluate it at the point the object U_Ent is declared, rather than
   --  at the point of the address clause. The condition for this to be true
   --  is that the expression has no variables, no constants declared after
   --  U_Ent, and no calls to non-pure functions. If this condition is not
   --  met, then an appropriate error message is posted. This check is applied
   --  at the point an object with an address clause is frozen, as well as for
   --  address clauses for tasks and entries.

   procedure Check_Size
     (N      : Node_Id;
      T      : Entity_Id;
      Siz    : Uint;
      Biased : out Boolean);
   --  Called when size Siz is specified for subtype T. This subprogram checks
   --  that the size is appropriate, posting errors on node N as required. This
   --  check is effective for elementary types and bit-packed arrays. For
   --  composite types, a check is only made if an explicit size has been given
   --  for the type (and the specified size must match).  The parameter Biased
   --  is set False if the size specified did not require the use of biased
   --  representation, and True if biased representation was required to meet
   --  the size requirement. Note that Biased is only set if the type is not
   --  currently biased, but biasing it is the only way to meet the
   --  requirement. If the type is currently biased, then this biased size is
   --  used in the initial check, and Biased is False. For a Component_Size
   --  clause, T is the component type.

   function Has_Compatible_Representation
     (Target_Typ, Operand_Typ : Entity_Id) return Boolean;
   --  Given an explicit or implicit conversion from Operand_Typ to Target_Typ,
   --  determine whether the types have compatible or different representation,
   --  thus requiring special processing for the conversion in the latter case.
   --  A False result is possible only for array, enumeration and record types.

   procedure Parse_Aspect_Aggregate
     (N                   : Node_Id;
      Empty_Subp          : in out Node_Id;
      Add_Named_Subp      : in out Node_Id;
      Add_Unnamed_Subp    : in out Node_Id;
      New_Indexed_Subp    : in out Node_Id;
      Assign_Indexed_Subp : in out Node_Id);
   --  Utility to unpack the subprograms in an occurrence of aspect Aggregate;
   --  used to verify the structure of the aspect, and resolve and expand an
   --  aggregate for a container type that carries the aspect.

   function Parse_Aspect_Stable_Properties
     (Aspect_Spec : Node_Id; Negated : out Boolean) return Subprogram_List;
   --  Utility to unpack the subprograms in a Stable_Properties list;
   --  in the case of the aspect of a type, Negated will always be False.

   function Rep_Item_Too_Early (T : Entity_Id; N : Node_Id) return Boolean;
   --  Called at start of processing a representation clause/pragma. Used to
   --  check that the representation item is not being applied to an incomplete
   --  type or to a generic formal type or a type derived from a generic formal
   --  type. Returns False if no such error occurs. If this error does occur,
   --  appropriate error messages are posted on node N, and True is returned.

   generic
      with procedure Replace_Type_Reference (N : Node_Id);
   procedure Replace_Type_References_Generic (N : Node_Id; T : Entity_Id);
   --  This is used to scan an expression for a predicate or invariant aspect
   --  replacing occurrences of the name of the subtype to which the aspect
   --  applies with appropriate references to the parameter of the predicate
   --  function or invariant procedure. The procedure passed as a generic
   --  parameter does the actual replacement of node N, which is either a
   --  simple direct reference to T, or a selected component that represents
   --  an appropriately qualified occurrence of T.
   --
   --  This also replaces each reference to a component, entry, or protected
   --  procedure with a selected component whose prefix is the parameter.
   --  For example, Component_Name becomes Parameter.Component_Name, where
   --  Parameter is the parameter, which is of type T.

   function Rep_Item_Too_Late
     (T     : Entity_Id;
      N     : Node_Id;
      FOnly : Boolean := False) return Boolean;
   --  Called at the start of processing a representation clause or a
   --  representation pragma. Used to check that a representation item for
   --  entity T does not appear too late (according to the rules in RM 13.1(9)
   --  and RM 13.1(10)). N is the associated node, which in the pragma case
   --  is the pragma or representation clause itself, used for placing error
   --  messages if the item is too late.
   --
   --  FOnly is a flag that causes only the freezing rule (para 9) to be
   --  applied, and the tests of para 10 are skipped. This is appropriate for
   --  both subtype related attributes (Alignment and Size) and for stream
   --  attributes, which, although certainly not subtype related attributes,
   --  clearly should not be subject to the para 10 restrictions (see
   --  AI95-00137). Similarly, we also skip the para 10 restrictions for
   --  the Storage_Size case where they also clearly do not apply, and for
   --  Stream_Convert which is in the same category as the stream attributes.
   --
   --  If the rep item is too late, an appropriate message is output and True
   --  is returned, which is a signal that the caller should abandon processing
   --  for the item. If the item is not too late, then False is returned, and
   --  the caller can continue processing the item.
   --
   --  If no error is detected, this call also as a side effect links the
   --  representation item onto the head of the representation item chain
   --  (referenced by the First_Rep_Item field of the entity).
   --
   --  Note: Rep_Item_Too_Late must be called with the underlying type in the
   --  case of a private or incomplete type. The protocol is to first check for
   --  Rep_Item_Too_Early using the initial entity, then take the underlying
   --  type, then call Rep_Item_Too_Late on the result.
   --
   --  Note: Calls to Rep_Item_Too_Late are ignored for the case of attribute
   --  definition clauses which have From_Aspect_Specification set. This is
   --  because such clauses are linked on to the Rep_Item chain in procedure
   --  Sem_Ch13.Analyze_Aspect_Specifications. See that procedure for details.

   procedure Validate_Unchecked_Conversion
     (N        : Node_Id;
      Act_Unit : Entity_Id);
   --  Validate a call to unchecked conversion. N is the node for the actual
   --  instantiation, which is used only for error messages. Act_Unit is the
   --  entity for the instantiation, from which the actual types etc. for this
   --  instantiation can be determined. This procedure makes an entry in a
   --  table and/or generates an N_Validate_Unchecked_Conversion node. The
   --  actual checking is done in Validate_Unchecked_Conversions or in the
   --  back end as required.

   procedure Validate_Unchecked_Conversions;
   --  This routine is called after calling the back end to validate unchecked
   --  conversions for size and alignment appropriateness. The reason it is
   --  called that late is to take advantage of any back-annotation of size
   --  and alignment performed by the back end.

   procedure Validate_Address_Clauses;
   --  This is called after the back end has been called (and thus after the
   --  alignments of objects have been back annotated). It goes through the
   --  table of saved address clauses checking for suspicious alignments and
   --  if necessary issuing warnings.

   -----------------------------------
   -- Handling of Aspect Visibility --
   -----------------------------------

   --  The visibility of aspects is tricky. First, the visibility is delayed
   --  to the freeze point. This is not too complicated, what we do is simply
   --  to leave the aspect "laying in wait" for the freeze point, and at that
   --  point materialize and analyze the corresponding attribute definition
   --  clause or pragma. There is some special processing for preconditions
   --  and postonditions, where the pragmas themselves deal with the required
   --  delay, but basically the approach is the same, delay analysis of the
   --  expression to the freeze point.

   --  Much harder is the requirement for diagnosing cases in which an early
   --  freeze causes a change in visibility. Consider:

   --    package AspectVis is
   --       R_Size : constant Integer := 32;
   --
   --       package Inner is
   --          type R is new Integer with
   --            Size => R_Size;
   --          F : R; -- freezes
   --          R_Size : constant Integer := 64;
   --          S : constant Integer := R'Size; -- 32 not 64
   --       end Inner;
   --    end AspectVis;

   --  Here the 32 not 64 shows what would be expected if this program were
   --  legal, since the evaluation of R_Size has to be done at the freeze
   --  point and gets the outer definition not the inner one.

   --  But the language rule requires this program to be diagnosed as illegal
   --  because the visibility changes between the freeze point and the end of
   --  the declarative region.

   --  To meet this requirement, we first note that the Expression field of the
   --  N_Aspect_Specification node holds the raw unanalyzed expression, which
   --  will get used in processing the aspect. At the time of analyzing the
   --  N_Aspect_Specification node, we create a complete copy of the expression
   --  and store it in the entity field of the Identifier (an odd usage, but
   --  the identifier is not used except to identify the aspect, so its Entity
   --  field is otherwise unused, and we are short of room in the node).

   --  This copy stays unanalyzed up to the freeze point, where we analyze the
   --  resulting pragma or attribute definition clause, except that in the
   --  case of invariants and predicates, we mark occurrences of the subtype
   --  name as having the entity of the subprogram parameter, so that they
   --  will not cause trouble in the following steps.

   --  Then at the freeze point, we create another copy of this unanalyzed
   --  expression. By this time we no longer need the Expression field for
   --  other purposes, so we can store it there. Now we have two copies of
   --  the original unanalyzed expression. One of them gets preanalyzed at
   --  the freeze point to capture the visibility at the freeze point.

   --  Now when we hit the freeze all at the end of the declarative part, if
   --  we come across a frozen entity with delayed aspects, we still have one
   --  copy of the unanalyzed expression available in the node, and we again
   --  do a preanalysis using that copy and the visibility at the end of the
   --  declarative part. Now we have two preanalyzed expression (preanalysis
   --  is good enough, since we are only interested in referenced entities).
   --  One captures the visibility at the freeze point, the other captures the
   --  visibility at the end of the declarative part. We see if the entities
   --  in these two expressions are the same, by seeing if the two expressions
   --  are fully conformant, and if not, issue appropriate error messages.

   --  Quite an awkward approach, but this is an awkard requirement

   procedure Analyze_Aspects_At_Freeze_Point (E : Entity_Id);
   --  Analyze all the delayed aspects for entity E at freezing point. This
   --  includes dealing with inheriting delayed aspects from the parent type
   --  in the case where a derived type is frozen.

   procedure Check_Aspect_At_Freeze_Point (ASN : Node_Id);
   --  Performs the processing described above at the freeze point, ASN is the
   --  N_Aspect_Specification node for the aspect.

   procedure Check_Aspect_At_End_Of_Declarations (ASN : Node_Id);
   --  Performs the processing described above at the freeze all point, and
   --  issues appropriate error messages if the visibility has indeed changed.
   --  Again, ASN is the N_Aspect_Specification node for the aspect.

   procedure Inherit_Aspects_At_Freeze_Point (Typ : Entity_Id);
   --  Given an entity Typ that denotes a derived type or a subtype, this
   --  routine performs the inheritance of aspects at the freeze point.

   --  ??? Note that, for now, just a limited number of representation aspects
   --  have been inherited here so far. Many of them are still inherited in
   --  Sem_Ch3 and need to be dealt with. Here is a non-exhaustive list of
   --  aspects that likely also need to be moved to this routine: Alignment,
   --  Component_Alignment, Component_Size, Machine_Radix, Object_Size, Pack,
   --  Predicates, Preelaborable_Initialization, Size and Small.

   procedure Inherit_Delayed_Rep_Aspects (Typ : Entity_Id);
   --  As discussed in the spec of Aspects (see Aspect_Delay declaration),
   --  a derived type can inherit aspects from its parent which have been
   --  specified at the time of the derivation using an aspect, as in:
   --
   --    type A is range 1 .. 10
   --      with Size => Not_Defined_Yet;
   --    ..
   --    type B is new A;
   --    ..
   --    Not_Defined_Yet : constant := 64;
   --
   --  In this example, the Size of A is considered to be specified prior
   --  to the derivation, and thus inherited, even though the value is not
   --  known at the time of derivation. To deal with this, we use two entity
   --  flags. The flag Has_Derived_Rep_Aspects is set in the parent type (A
   --  here), and then the flag May_Inherit_Delayed_Rep_Aspects is set in
   --  the derived type (B here). If this flag is set when the derived type
   --  is frozen, then this procedure is called to ensure proper inheritance
   --  of all delayed aspects from the parent type.

   --  ??? Obviously we ought not to have two mechanisms to do the same thing

   procedure Resolve_Aspect_Expressions (E : Entity_Id);
   --  Name resolution of an aspect expression happens at the end of the
   --  current declarative part or at the freeze point for the entity,
   --  whichever comes first. For declarations in the visible part of a
   --  package, name resolution takes place before analysis of the private
   --  part even though the freeze point of the entity may appear later.

   procedure Validate_Iterable_Aspect (Typ : Entity_Id; ASN : Node_Id);
   --  For SPARK 2014 formal containers. The expression has the form of an
   --  aggregate, and each entry must denote a function with the proper syntax
   --  for First, Next, and Has_Element. Optionally an Element primitive may
   --  also be defined.

   procedure Validate_Literal_Aspect (Typ : Entity_Id; ASN : Node_Id);
   --  Check legality of Integer_Literal, Real_Literal, and String_Literal
   --  aspect specifications.

   procedure Install_Discriminants (E : Entity_Id);
   --  Make visible the discriminants of type entity E

   procedure Uninstall_Discriminants (E : Entity_Id);
   --  Remove visibility to the discriminants of type entity E

end Sem_Ch13;
