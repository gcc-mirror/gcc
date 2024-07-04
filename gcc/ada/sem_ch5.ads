------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 5                               --
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

with Types; use Types;

package Sem_Ch5 is

   procedure Analyze_Assignment                   (N : Node_Id);
   procedure Analyze_Block_Statement              (N : Node_Id);
   procedure Analyze_Case_Statement               (N : Node_Id);
   procedure Analyze_Compound_Statement           (N : Node_Id);
   procedure Analyze_Exit_Statement               (N : Node_Id);
   procedure Analyze_Goto_Statement               (N : Node_Id);
   procedure Analyze_Goto_When_Statement          (N : Node_Id);
   procedure Analyze_If_Statement                 (N : Node_Id);
   procedure Analyze_Implicit_Label_Declaration   (N : Node_Id);
   procedure Analyze_Iterator_Specification       (N : Node_Id);
   procedure Analyze_Iteration_Scheme             (N : Node_Id);
   procedure Analyze_Label                        (N : Node_Id);
   procedure Analyze_Loop_Parameter_Specification (N : Node_Id);
   procedure Analyze_Loop_Statement               (N : Node_Id);
   procedure Analyze_Null_Statement               (N : Node_Id);
   procedure Analyze_Statements                   (L : List_Id);
   procedure Analyze_Target_Name                  (N : Node_Id);

   procedure Check_Unreachable_Code (N : Node_Id);
   --  This procedure is called with N being the node for a statement that is
   --  an unconditional transfer of control or an apparent infinite loop. It
   --  checks to see if the statement is followed by some other statement, and
   --  if so generates an appropriate warning for unreachable code.

end Sem_Ch5;
