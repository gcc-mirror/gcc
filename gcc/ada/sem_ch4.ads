------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 4                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

package Sem_Ch4  is
   procedure Analyze_Aggregate                 (N : Node_Id);
   procedure Analyze_Allocator                 (N : Node_Id);
   procedure Analyze_Arithmetic_Op             (N : Node_Id);
   procedure Analyze_Call                      (N : Node_Id);
   procedure Analyze_Case_Expression           (N : Node_Id);
   procedure Analyze_Comparison_Equality_Op    (N : Node_Id);
   procedure Analyze_Concatenation             (N : Node_Id);
   procedure Analyze_Explicit_Dereference      (N : Node_Id);
   procedure Analyze_Expression_With_Actions   (N : Node_Id);
   procedure Analyze_If_Expression             (N : Node_Id);
   procedure Analyze_Logical_Op                (N : Node_Id);
   procedure Analyze_Membership_Op             (N : Node_Id);
   procedure Analyze_Mod                       (N : Node_Id);
   procedure Analyze_Negation                  (N : Node_Id);
   procedure Analyze_Null                      (N : Node_Id);
   procedure Analyze_Qualified_Expression      (N : Node_Id);
   procedure Analyze_Quantified_Expression     (N : Node_Id);
   procedure Analyze_Range                     (N : Node_Id);
   procedure Analyze_Reference                 (N : Node_Id);
   procedure Analyze_Selected_Component        (N : Node_Id);
   procedure Analyze_Short_Circuit             (N : Node_Id);
   procedure Analyze_Slice                     (N : Node_Id);
   procedure Analyze_Type_Conversion           (N : Node_Id);
   procedure Analyze_Unary_Op                  (N : Node_Id);
   procedure Analyze_Unchecked_Expression      (N : Node_Id);
   procedure Analyze_Unchecked_Type_Conversion (N : Node_Id);

   procedure Ambiguous_Operands (N : Node_Id);
   --  Give an error for comparison, equality and membership operators with
   --  ambiguous operands, and list possible interpretations.

   procedure Analyze_Indexed_Component_Form    (N : Node_Id);
   --  Prior to semantic analysis, an indexed component node can denote any
   --  of the following syntactic constructs:
   --    a) An indexed component of an array
   --    b) A function call
   --    c) A conversion
   --    d) A slice
   --  The resolution of the construct requires some semantic information
   --  on the prefix and the indexes.

   procedure Nondispatching_Call_To_Abstract_Operation
     (N           : Node_Id;
      Abstract_Op : Entity_Id);
   --  Give an error, or a warning and rewrite N to raise Program_Error because
   --  it is a nondispatching call to an abstract operation.

   function Try_Object_Operation
     (N                : Node_Id;
      CW_Test_Only     : Boolean := False;
      Allow_Extensions : Boolean := False) return Boolean;
   --  Ada 2005 (AI-252): Support the object.operation notation. If node N is
   --  a call in this notation, it is transformed into a normal subprogram call
   --  where the prefix is a parameter, and True is returned. If node N is not
   --  of this form, it is unchanged, and False is returned. If CW_Test_Only is
   --  true then N is an N_Selected_Component node which is part of a call to
   --  an entry or procedure of a tagged concurrent type and this routine is
   --  invoked to search for class-wide subprograms conflicting with the target
   --  entity. If Allow_Extensions is True, then a prefixed call of a primitive
   --  of a non-tagged type is allowed as if Extensions_Allowed returned True.
   --  This is used to issue better error messages.

end Sem_Ch4;
