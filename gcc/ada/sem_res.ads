------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ R E S                               --
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

--  Resolution processing for all subexpression nodes. Note that the separate
--  package Sem_Aggr contains the actual resolution routines for aggregates,
--  which are separated off since aggregate processing is complex.

with Types; use Types;

package Sem_Res is

   --  As described in Sem_Type, the type resolution proceeds in two phases.
   --  The first phase is a bottom up pass that is achieved during the
   --  recursive traversal performed by the Analyze procedures. This phase
   --  determines unambiguous types, and collects sets of possible types
   --  where the interpretation is potentially ambiguous.

   --  On completing this bottom up pass, which corresponds to a call to
   --  Analyze on a complete context, the Resolve routine is called which
   --  performs a top down resolution with recursive calls to itself to
   --  resolve operands.

   --  Since in practice a lot of semantic analysis has to be postponed until
   --  types are known (e.g. static folding, setting of suppress flags), the
   --  Resolve routines also complete the semantic analysis, and call the
   --  expander for possible expansion of the completely type resolved node.

   procedure Ambiguous_Character (C : Node_Id);
   --  Give list of candidate interpretations when a character literal cannot
   --  be resolved, for example in a (useless) comparison such as 'A' = 'B'.
   --  In Ada 95 the literals in question can be of type Character or Wide_
   --  Character. In Ada 2005 Wide_Wide_Character is also a candidate. The
   --  node may also be overloaded with user-defined character types.

   procedure Analyze_And_Resolve (N : Node_Id);
   procedure Analyze_And_Resolve (N : Node_Id; Typ : Entity_Id);
   procedure Analyze_And_Resolve
     (N        : Node_Id;
      Typ      : Entity_Id;
      Suppress : Check_Id);
   procedure Analyze_And_Resolve
     (N        : Node_Id;
      Suppress : Check_Id);
   --  These routines combine the effect of Analyze and Resolve. If a Suppress
   --  argument is present, then the analysis is done with the specified check
   --  suppressed (can be All_Checks to suppress all checks). These checks are
   --  suppressed for both the analysis and resolution. If the type argument
   --  is not present, then the Etype of the expression after the Analyze
   --  call is used for the Resolve.

   procedure Check_Parameterless_Call (N : Node_Id);
   --  Several forms of names can denote calls to entities without parameters.
   --  The context determines whether the name denotes the entity or a call to
   --  it. When it is a call, the node must be rebuilt accordingly and
   --  reanalyzed to obtain possible interpretations.
   --
   --  The name may be that of an overloadable construct, or it can be an
   --  explicit dereference of a prefix that denotes an access to subprogram.
   --  In that case, we want to convert the name into a call only if the
   --  context requires the return type of the subprogram. Finally, a
   --  parameterless protected subprogram appears as a selected component.
   --
   --  The parameter T is the Typ for the corresponding resolve call.

   function Is_Ambiguous_Operand
     (Operand        : Node_Id;
      In_Interp_Expr : Boolean := False;
      Report_Errors  : Boolean := True) return Boolean;
   --  Examine the interpretations of the given overloaded operand in a type
   --  conversion or interpolated expression. Returns True if the call is
   --  ambiguous; reports errors for ambiguous calls unless Report_Errors is
   --  set to False. In_Interp_Expr is True when the operand is an
   --  interpolated expression; used to improve the clarity of reported
   --  error messages.

   procedure Preanalyze_And_Resolve (N : Node_Id; T : Entity_Id);
   --  Performs a preanalysis of expression node N. During preanalysis, N is
   --  analyzed and then resolved against type T, but no expansion is carried
   --  out for N or its children. For more info on preanalysis read the spec
   --  of Sem.

   procedure Preanalyze_And_Resolve (N : Node_Id);
   --  Same, but use type of node because context does not impose a single type

   procedure Preanalyze_With_Freezing_And_Resolve (N : Node_Id; T : Entity_Id);
   --  Same, but perform freezing of static expressions of N or its children.

   procedure Resolve (N : Node_Id; Typ : Entity_Id);
   procedure Resolve (N : Node_Id; Typ : Entity_Id; Suppress : Check_Id);
   --  Top-level type-checking procedure, called in a complete context. The
   --  construct N, which is a subexpression, has already been analyzed, and
   --  is required to be of type Typ given the analysis of the context (which
   --  uses the information gathered on the bottom-up phase in Analyze). The
   --  resolve routines do various other processing, e.g. static evaluation.
   --  If a Suppress argument is present, then the resolution is done with the
   --  specified check suppressed (can be All_Checks to suppress all checks).

   procedure Resolve (N : Node_Id);
   --  A version of Resolve where the type to be used for resolution is taken
   --  from the Etype (N). This is commonly used in cases where the context
   --  does not add anything and the first pass of analysis found the correct
   --  expected type.

   procedure Resolve_Discrete_Subtype_Indication
     (N   : Node_Id;
      Typ : Entity_Id);
   --  Resolve subtype indications in choices (case statements and aggregates)
   --  and in index constraints. Note that the resulting Etype of the subtype_
   --  indication node is set to the Etype of the contained range (i.e. an
   --  Itype is not constructed for the actual subtype).

   procedure Resolve_Entry (Entry_Name : Node_Id);
   --  Find name of entry being called, and resolve prefix of name with its
   --  own type. For now we assume that the prefix cannot be overloaded and
   --  the name of the entry plays no role in the resolution.

   procedure Resolve_Membership_Equality (N : Node_Id; Typ : Entity_Id);
   --  Resolve the equality operator in an individual membership test

   function Valid_Conversion
     (N           : Node_Id;
      Target      : Entity_Id;
      Operand     : Node_Id;
      Report_Errs : Boolean := True) return Boolean;
   --  Verify legality rules given in 4.6 (8-23). Target is the target type
   --  of the conversion, which may be an implicit conversion of an actual
   --  parameter to an anonymous access type (in which case N denotes the
   --  actual parameter and N = Operand). Returns a Boolean result indicating
   --  whether the conversion is legal. Reports errors in the case of illegal
   --  conversions, unless Report_Errs is False.

private
   procedure Resolve_Implicit_Type (N : Node_Id) renames Resolve;
   pragma Inline (Resolve_Implicit_Type);
   --  We use this renaming to make the application of Inline very explicit to
   --  this version, since other versions of Resolve are not inlined.

end Sem_Res;
