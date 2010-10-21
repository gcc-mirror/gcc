------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 4                               --
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

with Types; use Types;

package Sem_Ch4  is
   procedure Analyze_Aggregate                          (N : Node_Id);
   procedure Analyze_Allocator                          (N : Node_Id);
   procedure Analyze_Arithmetic_Op                      (N : Node_Id);
   procedure Analyze_Call                               (N : Node_Id);
   procedure Analyze_Case_Expression                    (N : Node_Id);
   procedure Analyze_Comparison_Op                      (N : Node_Id);
   procedure Analyze_Concatenation                      (N : Node_Id);
   procedure Analyze_Conditional_Expression             (N : Node_Id);
   procedure Analyze_Equality_Op                        (N : Node_Id);
   procedure Analyze_Explicit_Dereference               (N : Node_Id);
   procedure Analyze_Expression_With_Actions            (N : Node_Id);
   procedure Analyze_Logical_Op                         (N : Node_Id);
   procedure Analyze_Membership_Op                      (N : Node_Id);
   procedure Analyze_Negation                           (N : Node_Id);
   procedure Analyze_Null                               (N : Node_Id);
   procedure Analyze_Qualified_Expression               (N : Node_Id);
   procedure Analyze_Quantified_Expression              (N : Node_Id);
   procedure Analyze_Range                              (N : Node_Id);
   procedure Analyze_Reference                          (N : Node_Id);
   procedure Analyze_Selected_Component                 (N : Node_Id);
   procedure Analyze_Short_Circuit                      (N : Node_Id);
   procedure Analyze_Slice                              (N : Node_Id);
   procedure Analyze_Type_Conversion                    (N : Node_Id);
   procedure Analyze_Unary_Op                           (N : Node_Id);
   procedure Analyze_Unchecked_Expression               (N : Node_Id);
   procedure Analyze_Unchecked_Type_Conversion          (N : Node_Id);

   procedure Analyze_Indexed_Component_Form (N : Node_Id);
   --  Prior to semantic analysis, an indexed component node can denote any
   --  of the following syntactic constructs:
   --    a) An indexed component of an array
   --    b) A function call
   --    c) A conversion
   --    d) A slice
   --  The resolution of the construct requires some semantic information
   --  on the prefix and the indexes.

end Sem_Ch4;
