------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 5                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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

--  Expand routines for chapter 5 constructs

with Types; use Types;

package Exp_Ch5 is
   procedure Expand_N_Assignment_Statement      (N : Node_Id);
   procedure Expand_N_Block_Statement           (N : Node_Id);
   procedure Expand_N_Case_Statement            (N : Node_Id);
   procedure Expand_N_Exit_Statement            (N : Node_Id);
   procedure Expand_N_Goto_Statement            (N : Node_Id);
   procedure Expand_N_If_Statement              (N : Node_Id);
   procedure Expand_N_Loop_Statement            (N : Node_Id);

   procedure Expand_Iterator_Loop_Over_Array (N : Node_Id);
   --  Expand loop over arrays that uses the form "for X of C"

end Exp_Ch5;
