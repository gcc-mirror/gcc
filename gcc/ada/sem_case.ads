------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C A S E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2024, Free Software Foundation, Inc.         --
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

--  Package containing the routines to process a list of discrete choices.
--  Such lists can occur in two different constructs: case statements and
--  record variants. We have factorized what used to be two very similar
--  sets of routines in one place. These are not currently used for the
--  aggregate case, since issues with nested aggregates make that case
--  substantially different.

--  The following processing is required for such cases:

--    1. Analysis of names of subtypes, constants, expressions appearing within
--    the choices. This must be done when the construct is encountered to get
--    proper visibility of names.

--    2. Checking for semantic correctness of the choices. A lot of this could
--    be done at the time when the construct is encountered, but not all, since
--    in the case of variants, statically predicated subtypes won't be frozen
--    (and the choice sets known) till the enclosing record type is frozen. So
--    at least the check for no overlaps and covering the range must be delayed
--    till the freeze point in this case.

--    3. Set the Others_Discrete_Choices list for an others choice. This is
--    used in various ways, e.g. to construct the discriminant checking
--    function for the case of a variant with an others choice.

--    4. In the case of static predicates, we need to expand out choices that
--    correspond to the predicate for the back end. This expansion destroys
--    the list of choices, so it should be delayed to expansion time.

--  Step 1 is performed by the generic procedure Analyze_Choices, which is
--  called when the variant record or case statement/expression is first
--  encountered.

--  Step 2 is performed by the generic procedure Check_Choices. We decide to
--  do all semantic checking in that step, since as noted above some of this
--  has to be deferred to the freeze point in any case for variants. For case
--  statements and expressions, this procedure can be called at the time the
--  case construct is encountered (after calling Analyze_Choices).

--  Step 3 is also performed by Check_Choices, since we need the static ranges
--  for predicated subtypes to accurately construct this.

--  Step 4 is performed by the procedure Expand_Static_Predicates_In_Choices.
--  For case statements, this call only happens during expansion. The reason
--  we do the expansion unconditionally for variants is that other processing,
--  for example for aggregates, relies on having a complete list of choices.

--  Historical note: We used to perform all four of these functions at once in
--  a single procedure called Analyze_Choices. This routine was called at the
--  time the construct was first encountered. That seemed to work OK up to Ada
--  2005, but the introduction of statically predicated subtypes with delayed
--  evaluation of the static ranges made this completely wrong, both because
--  the ASIS tree got destroyed by step 4, and steps 2 and 3 were too early
--  in the variant record case.

with Types; use Types;

package Sem_Case is

   procedure No_OP (C : Node_Id);
   --  The no-operation routine. Does mostly nothing. Can be used
   --  in the following generics for the parameters Process_Empty_Choice,
   --  or Process_Associated_Node. In the case of an empty range choice,
   --  routine emits a warning when Warn_On_Redundant_Constructs is enabled.

   generic
      with procedure Process_Associated_Node (A : Node_Id);
      --  Associated with each case alternative or record variant A there is
      --  a node or list of nodes that need additional processing. This routine
      --  implements that processing.

   package Generic_Analyze_Choices is

      procedure Analyze_Choices
        (Alternatives : List_Id;
         Subtyp       : Entity_Id);
      --  From a case expression, case statement, or record variant, this
      --  routine analyzes the corresponding list of discrete choices which
      --  appear in each element of the list Alternatives (for the variant
      --  part case, this is the variants, for a case expression or statement,
      --  this is the Alternatives).
      --
      --  Subtyp is the subtype of the discrete choices. The type against which
      --  the discrete choices must be resolved is its base type.

   end Generic_Analyze_Choices;

   generic
      with procedure Process_Empty_Choice (Choice : Node_Id);
      --  Processing to carry out for an empty Choice. Set to No_Op (declared
      --  above) if no such processing is required.

      with procedure Process_Non_Static_Choice (Choice : Node_Id);
      --  Processing to carry out for a non static Choice (gives an error msg)

      with procedure Process_Associated_Node (A : Node_Id);
      --  Associated with each case alternative or record variant A there is
      --  a node or list of nodes that need semantic processing. This routine
      --  implements that processing.

   package Generic_Check_Choices is

      procedure Check_Choices
        (N              : Node_Id;
         Alternatives   : List_Id;
         Subtyp         : Entity_Id;
         Others_Present : out Boolean);
      --  From a case expression, case statement, or record variant N, this
      --  routine analyzes the corresponding list of discrete choices which
      --  appear in each element of the list Alternatives (for the variant
      --  part case, this is the variants, for a case expression or statement,
      --  this is the Alternatives).
      --
      --  Subtyp is the subtype of the discrete choices. The type against which
      --  the discrete choices must be resolved is its base type.
      --
      --  Others_Present is set to True if an Others choice is present in the
      --  list of choices, and in this case Others_Discrete_Choices is set in
      --  the N_Others_Choice node.
      --
      --  If a Discrete_Choice list contains at least one instance of a subtype
      --  with a static predicate, then the Has_SP_Choice flag is set true in
      --  the parent node (N_Variant, N_Case_Expression/Statement_Alternative).

   end Generic_Check_Choices;

   function Is_Case_Choice_Pattern (Expr : Node_Id) return Boolean;
   --  GNAT language extensions allow casing on a non-discrete value, with
   --  patterns as case choices. Return True iff Expr is such a pattern, or
   --  a subexpression thereof.

end Sem_Case;
