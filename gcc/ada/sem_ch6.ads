------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 6                               --
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
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;
package Sem_Ch6 is

   procedure Analyze_Abstract_Subprogram_Declaration (N : Node_Id);
   procedure Analyze_Function_Call                   (N : Node_Id);
   procedure Analyze_Operator_Symbol                 (N : Node_Id);
   procedure Analyze_Parameter_Association           (N : Node_Id);
   procedure Analyze_Procedure_Call                  (N : Node_Id);
   procedure Analyze_Return_Statement                (N : Node_Id);
   procedure Analyze_Subprogram_Declaration          (N : Node_Id);
   procedure Analyze_Subprogram_Body                 (N : Node_Id);

   function Analyze_Subprogram_Specification (N : Node_Id) return Entity_Id;
   --  Analyze subprogram specification in both subprogram declarations
   --  and body declarations. Returns the defining entity for the spec.

   procedure Cannot_Inline (Msg : String; N : Node_Id; Subp : Entity_Id);
   --  This procedure is called if the node N, an instance of a call to
   --  subprogram Subp, cannot be inlined. Msg is the message to be issued,
   --  and has a ? as the last character. If Subp has a pragma Always_Inlined,
   --  then an error message is issued (by removing the last character of Msg).
   --  If Subp is not Always_Inlined, then a warning is issued if the flag
   --  Ineffective_Inline_Warnings is set, and if not, the call has no effect.

   procedure Check_Delayed_Subprogram (Designator : Entity_Id);
   --  Designator can be a E_Subrpgram_Type, E_Procedure or E_Function. If a
   --  type in its profile depends on a private type without a full
   --  declaration, indicate that the subprogram is delayed.

   procedure Check_Discriminant_Conformance
     (N        : Node_Id;
      Prev     : Entity_Id;
      Prev_Loc : Node_Id);
   --  Check that the discriminants of a full type N fully conform to
   --  the discriminants of the corresponding partial view Prev.
   --  Prev_Loc indicates the source location of the partial view,
   --  which may be different than Prev in the case of private types.

   procedure Check_Fully_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty);
   --  Check that two callable entitites (subprograms, entries, literals)
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
   --  Check that two callable entitites (subprograms, entries, literals)
   --  are mode conformant, post error message if not (RM 6.3.1(15)) with
   --  the flag being placed on the Err_Loc node if it is specified, and
   --  on the appropriate component of the New_Id construct if not. The
   --  argument Get_Inst is set to True when this is a check against a
   --  formal access-to-subprogram type, indicating that mapping of types
   --  is needed.

   procedure Check_Subtype_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty);
   --  Check that two callable entitites (subprograms, entries, literals)
   --  are subtype conformant, post error message if not (RM 6.3.1(16))
   --  the flag being placed on the Err_Loc node if it is specified, and
   --  on the appropriate component of the New_Id construct if not.

   procedure Check_Type_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty);
   --  Check that two callable entitites (subprograms, entries, literals)
   --  are type conformant, post error message if not (RM 6.3.1(14)) with
   --  the flag being placed on the Err_Loc node if it is specified, and
   --  on the appropriate component of the New_Id construct if not.

   procedure Create_Extra_Formals (E : Entity_Id);
   --  For each parameter of a subprogram or entry that requires an additional
   --  formal (such as for access parameters and indefinite discriminated
   --  parameters), creates the appropriate formal and attach it to its
   --  associated parameter. Each extra formal will also be appended to
   --  the end of Subp's parameter list (with each subsequent extra formal
   --  being attached to the preceding extra formal).

   function Find_Corresponding_Spec (N : Node_Id) return Entity_Id;
   --  Use the subprogram specification in the body to retrieve the previous
   --  subprogram declaration, if any.

   function Fully_Conformant (New_Id, Old_Id : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are fully conformant (RM 6.3.1(17))

   function Fully_Conformant_Expressions
     (Given_E1 : Node_Id;
      Given_E2 : Node_Id)
      return     Boolean;
   --  Determines if two (non-empty) expressions are fully conformant
   --  as defined by (RM 6.3.1(18-21))

   function Fully_Conformant_Discrete_Subtypes
      (Given_S1 : Node_Id;
       Given_S2 : Node_Id)
       return Boolean;
   --  Determines if two subtype definitions are fully conformant. Used
   --  for entry family conformance checks (RM 6.3.1 (24)).

   function Mode_Conformant (New_Id, Old_Id : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are mode conformant (RM 6.3.1(15))

   procedure New_Overloaded_Entity
     (S            : Entity_Id;
      Derived_Type : Entity_Id := Empty);
   --  Process new overloaded entity. Overloaded entities are created
   --  by enumeration type declarations, subprogram specifications,
   --  entry declarations, and (implicitly) by type derivations.
   --  If Derived_Type is not Empty, then it indicates that this
   --  is subprogram derived for that type.

   procedure Process_Formals (T : List_Id; Related_Nod : Node_Id);
   --  Enter the formals in the scope of the subprogram or entry, and
   --  analyze default expressions if any. The implicit types created for
   --  access parameter are attached to the Related_Nod which comes from the
   --  context.

   procedure Set_Actual_Subtypes (N : Node_Id; Subp : Entity_Id);
   --  If the formals of a subprogram are unconstrained, build a subtype
   --  declaration that uses the bounds or discriminants of the actual to
   --  construct an actual subtype for them. This is an optimization that
   --  is done only in some cases where the actual subtype cannot change
   --  during execution of the subprogram. By setting the actual subtype
   --  once, we avoid recomputing it unnecessarily.

   procedure Set_Formal_Mode (Formal_Id : Entity_Id);
   --  Set proper Ekind to reflect formal mode (in, out, in out)

   function Subtype_Conformant (New_Id, Old_Id : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are subtype conformant (RM6.3.1(16))

   function Type_Conformant (New_Id, Old_Id : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are type conformant (RM6.3.1(14))

   procedure Valid_Operator_Definition (Designator : Entity_Id);
   --  Verify that an operator definition has the proper number of formals

end Sem_Ch6;
