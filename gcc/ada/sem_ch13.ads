------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
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

with Table;
with Types; use Types;
with Uintp; use Uintp;

package Sem_Ch13 is
   procedure Analyze_At_Clause                          (N : Node_Id);
   procedure Analyze_Attribute_Definition_Clause        (N : Node_Id);
   procedure Analyze_Enumeration_Representation_Clause  (N : Node_Id);
   procedure Analyze_Free_Statement                     (N : Node_Id);
   procedure Analyze_Freeze_Entity                      (N : Node_Id);
   procedure Analyze_Record_Representation_Clause       (N : Node_Id);
   procedure Analyze_Code_Statement                     (N : Node_Id);

   procedure Analyze_Aspect_Specifications
     (N : Node_Id;
      E : Entity_Id;
      L : List_Id);
   --  This procedure is called to analyze aspect specifications for node N. E
   --  is the corresponding entity declared by the declaration node N, and L is
   --  the list of aspect specifications for this node. If L is No_List, the
   --  call is ignored. Note that we can't use a simpler interface of just
   --  passing the node N, since the analysis of the node may cause it to be
   --  rewritten to a node not permitting aspect specifications.

   procedure Adjust_Record_For_Reverse_Bit_Order (R : Entity_Id);
   --  Called from Freeze where R is a record entity for which reverse bit
   --  order is specified and there is at least one component clause. Adjusts
   --  component positions according to either Ada 95 or Ada 2005 (AI-133).

   procedure Build_Invariant_Procedure (Typ : Entity_Id; N : Node_Id);
   --  Typ is a private type with invariants (indicated by Has_Invariants being
   --  set for Typ, indicating the presence of pragma Invariant entries on the
   --  rep chain, note that Invariant aspects have already been converted to
   --  pragma Invariant), then this procedure builds the spec and body for the
   --  corresponding Invariant procedure, inserting them at appropriate points
   --  in the package specification N. Invariant_Procedure is set for Typ. Note
   --  that this procedure is called at the end of processing the declarations
   --  in the visible part (i.e. the right point for visibility analysis of
   --  the invariant expression).

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

   procedure Set_Enum_Esize (T : Entity_Id);
   --  This routine sets the Esize field for an enumeration type T, based
   --  on the current representation information available for T. Note that
   --  the setting of the RM_Size field is not affected. This routine also
   --  initializes the alignment field to zero.

   function Minimum_Size
     (T      : Entity_Id;
      Biased : Boolean := False) return Nat;
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
   --  compile time, this function returns 0.

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
   --  that the size is appropriate, posting errors on node N as required.
   --  This check is effective for elementary types and bit-packed arrays.
   --  For other non-elementary types, a check is only made if an explicit
   --  size has been given for the type (and the specified size must match).
   --  The parameter Biased is set False if the size specified did not require
   --  the use of biased representation, and True if biased representation
   --  was required to meet the size requirement. Note that Biased is only
   --  set if the type is not currently biased, but biasing it is the only
   --  way to meet the requirement. If the type is currently biased, then
   --  this biased size is used in the initial check, and Biased is False.
   --  If the size is too small, and an error message is given, then both
   --  Esize and RM_Size are reset to the allowed minimum value in T.

   function Rep_Item_Too_Early (T : Entity_Id; N : Node_Id) return Boolean;
   --  Called at the start of processing a representation clause or a
   --  representation pragma. Used to check that the representation item
   --  is not being applied to an incomplete type or to a generic formal
   --  type or a type derived from a generic formal type. Returns False if
   --  no such error occurs. If this error does occur, appropriate error
   --  messages are posted on node N, and True is returned.

   function Rep_Item_Too_Late
     (T     : Entity_Id;
      N     : Node_Id;
      FOnly : Boolean := False) return Boolean;
   --  Called at the start of processing a representation clause or a
   --  representation pragma. Used to check that a representation item
   --  for entity T does not appear too late (according to the rules in
   --  RM 13.1(9) and RM 13.1(10)). N is the associated node, which in
   --  the pragma case is the pragma or representation clause itself, used
   --  for placing error messages if the item is too late.
   --
   --  Fonly is a flag that causes only the freezing rule (para 9) to be
   --  applied, and the tests of para 10 are skipped. This is appropriate
   --  for both subtype related attributes (Alignment and Size) and for
   --  stream attributes, which, although certainly not subtype related
   --  attributes, clearly should not be subject to the para 10 restrictions
   --  (see AI95-00137). Similarly, we also skip the para 10 restrictions for
   --  the Storage_Size case where they also clearly do not apply, and for
   --  Stream_Convert which is in the same category as the stream attributes.
   --
   --  If the rep item is too late, an appropriate message is output and
   --  True is returned, which is a signal that the caller should abandon
   --  processing for the item. If the item is not too late, then False
   --  is returned, and the caller can continue processing the item.
   --
   --  If no error is detected, this call also as a side effect links the
   --  representation item onto the head of the representation item chain
   --  (referenced by the First_Rep_Item field of the entity).
   --
   --  Note: Rep_Item_Too_Late must be called with the underlying type in
   --  the case of a private or incomplete type. The protocol is to first
   --  check for Rep_Item_Too_Early using the initial entity, then take the
   --  underlying type, then call Rep_Item_Too_Late on the result.
   --
   --  Note: Calls to Rep_Item_Too_Late are ignored for the case of attribute
   --  definition clauses which have From_Aspect_Specification set. This is
   --  because such clauses are linked on to the Rep_Item chain in procedure
   --  Sem_Ch13.Analyze_Aspect_Specifications. See that procedure for details.

   function Same_Representation (Typ1, Typ2 : Entity_Id) return Boolean;
   --  Given two types, where the two types are related by possible derivation,
   --  determines if the two types have the same representation, or different
   --  representations, requiring the special processing for representation
   --  change. A False result is possible only for array, enumeration or
   --  record types.

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
   --  This routine is called after calling the backend to validate unchecked
   --  conversions for size and alignment appropriateness. The reason it is
   --  called that late is to take advantage of any back-annotation of size
   --  and alignment performed by the backend.

   procedure Validate_Address_Clauses;
   --  This is called after the back end has been called (and thus after the
   --  alignments of objects have been back annotated). It goes through the
   --  table of saved address clauses checking for suspicious alignments and
   --  if necessary issuing warnings.

   procedure Validate_Independence;
   --  This is called after the back end has been called (and thus after the
   --  layout of components has been back annotated). It goes through the
   --  table of saved pragma Independent[_Component] entries, checking that
   --  independence can be achieved, and if necessary issuing error messages.

   -------------------------------------
   -- Table for Validate_Independence --
   -------------------------------------

   --  If a legal pragma Independent or Independent_Components is given for
   --  an entity, then an entry is made in this table, to be checked by a
   --  call to Validate_Independence after back annotation of layout is done.

   type Independence_Check_Record is record
      N : Node_Id;
      --  The pragma Independent or Independent_Components

      E : Entity_Id;
      --  The entity to which it applies
   end record;

   package Independence_Checks is new Table.Table (
     Table_Component_Type => Independence_Check_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "Independence_Checks");

end Sem_Ch13;
