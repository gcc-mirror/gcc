------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ T Y P E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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

--  This unit contains the routines used to handle type determination,
--  including the routine used to support overload resolution.

with Types; use Types;

package Sem_Type is

   ---------------------------------------------
   -- Data Structures for Overload Resolution --
   ---------------------------------------------

   --  To determine the unique meaning of an identifier, overload resolution
   --  may have to be performed if the visibility rules alone identify more
   --  than one possible entity as the denotation of a given identifier. When
   --  the visibility rules find such a potential ambiguity, the set of
   --  possible interpretations must be attached to the identifier, and
   --  overload resolution must be performed over the innermost enclosing
   --  complete context. At the end of the resolution,  either a single
   --  interpretation is found for all identifiers in the context, or else a
   --  type error (invalid type or ambiguous reference) must be signalled.

   --  The set of interpretations of a given name is stored in a data structure
   --  that is separate from the syntax tree, because it corresponds to
   --  transient information.  The interpretations themselves are stored in
   --  table All_Interp. A mapping from tree nodes to sets of interpretations
   --  called Interp_Map, is maintained by the overload resolution routines.
   --  Both these structures are initialized at the beginning of every complete
   --  context.

   --  Corresponding to the set of interpretation for a given overloadable
   --  identifier, there is a set of possible types corresponding to the types
   --  that the overloaded call may return. We keep a 1-to-1 correspondence
   --  between interpretations and types: for user-defined subprograms the
   --  type is the declared return type. For operators, the type is determined
   --  by the type of the arguments. If the arguments themselves are
   --  overloaded, we enter the operator name in the names table for each
   --  possible result type. In most cases, arguments are not overloaded and
   --  only one interpretation is present anyway.

   type Interp is record
      Nam : Entity_Id;
      Typ : Entity_Id;
   end record;

   No_Interp : constant Interp := (Empty, Empty);

   subtype Interp_Index is Int;

   ----------------------
   --  Error Reporting --
   ----------------------

   --  A common error is the use of an operator in infix notation on arguments
   --  of a type that is not directly visible. Rather than diagnosing a type
   --  mismatch, it is better to indicate that the type can be made use-visible
   --  with the appropriate use clause. The global variable Candidate_Type is
   --  set in Add_One_Interp whenever an interpretation might be legal for an
   --  operator if the type were directly visible. This variable is used in
   --  sem_ch4 when no legal interpretation is found.

   Candidate_Type : Entity_Id;

   -----------------
   -- Subprograms --
   -----------------

   procedure Init_Interp_Tables;
   --  Invoked by gnatf when processing multiple files.

   procedure Collect_Interps (N : Node_Id);
   --  Invoked when the name N has more than one visible interpretation.
   --  This is the high level routine which accumulates the possible
   --  interpretations of the node. The first meaning and type of N have
   --  already been stored in N. If the name is an expanded name, the homonyms
   --  are only those that belong to the same scope.

   function Is_Invisible_Operator
     (N    : Node_Id;
      T    : Entity_Id)
      return Boolean;
   --  Check whether a predefined operation with universal operands appears
   --  in a context in which the operators of the expected type are not
   --  visible.

   procedure List_Interps (Nam : Node_Id; Err : Node_Id);
   --  List candidate interpretations of an overloaded name. Used for
   --  various error reports.

   procedure Add_One_Interp
     (N         : Node_Id;
      E         : Entity_Id;
      T         : Entity_Id;
      Opnd_Type : Entity_Id := Empty);
   --  Add (E, T) to the list of interpretations of the node being resolved.
   --  For calls and operators, i.e. for nodes that have a name field,
   --  E is an overloadable entity, and T is its type. For constructs such
   --  as indexed expressions, the caller sets E equal to T, because the
   --  overloading comes from other fields, and the node itself has no name
   --  to resolve. Add_One_Interp includes the semantic processing to deal
   --  with adding entries that hide one another etc.

   --  For operators, the legality of the operation depends on the visibility
   --  of T and its scope. If the operator is an equality or comparison, T is
   --  always Boolean, and we use Opnd_Type, which is a candidate type for one
   --  of the operands of N, to check visibility.

   procedure End_Interp_List;
   --  End the list of interpretations of current node.

   procedure Get_First_Interp
     (N  : Node_Id;
      I  : out Interp_Index;
      It : out Interp);
   --  Initialize iteration over set of interpretations for Node N. The first
   --  interpretation is placed in It, and I is initialized for subsequent
   --  calls to Get_Next_Interp.

   procedure Get_Next_Interp (I : in out Interp_Index; It : out Interp);
   --  Iteration step over set of interpretations. Using the value in I, which
   --  was set by a previous call to Get_First_Interp or Get_Next_Interp, the
   --  next interpretation is placed in It, and I is updated for the next call.
   --  The end of the list of interpretations is signalled by It.Nam = Empty.

   procedure Remove_Interp (I : in out Interp_Index);
   --  Remove an interpretation that his hidden by another, or that does not
   --  match the context. The value of I on input was set by a call to either
   --  Get_First_Interp or Get_Next_Interp and references the interpretation
   --  to be removed. The only allowed use of the exit value of I is as input
   --  to a subsequent call to Get_Next_Interp, which yields the interpretation
   --  following the removed one.

   procedure Save_Interps (Old_N : Node_Id; New_N : Node_Id);
   --  If an overloaded node is rewritten during semantic analysis, its
   --  possible interpretations must be linked to the copy. This procedure
   --  transfers the overload information from Old_N, the old node, to
   --  New_N, its new copy. It has no effect in the non-overloaded case.

   function Covers (T1, T2 : Entity_Id) return Boolean;
   --  This is the basic type compatibility routine. T1 is the expected
   --  type, imposed by context, and T2 is the actual type. The processing
   --  reflects both the definition of type coverage and the rules
   --  for operand matching.

   function Disambiguate
     (N      : Node_Id;
      I1, I2 : Interp_Index;
      Typ    : Entity_Id)
      return   Interp;
   --  If more than one interpretation  of a name in a call is legal, apply
   --  preference rules (universal types first) and operator visibility in
   --  order to remove ambiguity. I1 and I2 are the first two interpretations
   --  that are compatible with the context, but there may be others.

   function Entity_Matches_Spec (Old_S,  New_S : Entity_Id) return Boolean;
   --  To resolve subprogram renaming and default formal subprograms in generic
   --  definitions. Old_S is a possible interpretation of the entity being
   --  renamed, New_S has an explicit signature. If Old_S is a subprogram, as
   --  opposed to an operator, type and mode conformance are required.

   function Find_Unique_Type (L : Node_Id; R : Node_Id) return Entity_Id;
   --  Used in second pass of resolution,  for equality and comparison nodes.
   --  L is the left operand, whose type is known to be correct, and R is
   --  the right operand,  which has one interpretation compatible with that
   --  of L. Return the type intersection of the two.

   function Has_Compatible_Type
     (N    : Node_Id;
      Typ  : Entity_Id)
      return Boolean;
   --  Verify that some interpretation of the node N has a type compatible
   --  with Typ. If N is not overloaded, then its unique type must be
   --  compatible with Typ. Otherwise iterate through the interpretations
   --  of N looking for a compatible one.

   function Hides_Op (F : Entity_Id; Op : Entity_Id) return Boolean;
   --  A user-defined function hides a predefined operator if it is
   --  matches the signature of the operator, and is declared in an
   --  open scope, or in the scope of the result type.

   function Intersect_Types (L, R : Node_Id) return Entity_Id;
   --  Find the common interpretation to two analyzed nodes. If one of the
   --  interpretations is universal, choose the non-universal one. If either
   --  node is overloaded, find single common interpretation.

   function Is_Subtype_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean;
   --  Checks whether T1 is any subtype of T2 directly or indirectly. Applies
   --  only to scalar subtypes ???

   function Is_Ancestor (T1, T2 : Entity_Id) return Boolean;
   --  T1 is a tagged type (not class-wide). Verify that it is one of the
   --  ancestors of type T2 (which may or not be class-wide)

   function Operator_Matches_Spec (Op,  New_S : Entity_Id) return Boolean;
   --  Used to resolve subprograms renaming operators, and calls to user
   --  defined operators. Determines whether a given operator Op, matches
   --  a specification, New_S.

   function Valid_Comparison_Arg (T : Entity_Id) return Boolean;
   --  A valid argument to an ordering operator must be a discrete type, a
   --  real type, or a one dimensional array with a discrete component type.

   function Valid_Boolean_Arg (T : Entity_Id) return Boolean;
   --  A valid argument of a boolean operator is either some boolean type,
   --  or a one-dimensional array of boolean type.

   procedure Write_Interp_Ref (Map_Ptr : Int);
   --  Debugging procedure to display entry in Interp_Map. Would not be
   --  needed if it were possible to debug instantiations of Table.

   procedure Write_Overloads (N : Node_Id);
   --  Debugging procedure to output info on possibly overloaded entities
   --  for specified node.

end Sem_Type;
