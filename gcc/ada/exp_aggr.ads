------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A G G R                              --
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

package Exp_Aggr is

   procedure Expand_N_Aggregate           (N : Node_Id);
   procedure Expand_N_Delta_Aggregate     (N : Node_Id);
   procedure Expand_N_Extension_Aggregate (N : Node_Id);

   procedure Convert_Aggr_In_Allocator (N : Node_Id; Temp : Entity_Id);
   --  N is an N_Allocator whose (ultimate) expression must be an N_Aggregate
   --  or N_Extension_Aggregate with Expansion_Delayed.
   --  This procedure performs an in-place aggregate assignment into an object
   --  allocated with the subtype of the aggregate and designated by Temp, so
   --  that N can be rewritten as a mere occurrence of Temp.

   procedure Convert_Aggr_In_Assignment (N : Node_Id);
   --  If the right-hand side of an assignment is an aggregate, expand the
   --  statement into a series of individual component assignments. This is
   --  done if there are non-static values involved in either the bounds or
   --  the components, and the aggregate cannot be handled as a whole by the
   --  backend.

   procedure Convert_Aggr_In_Object_Decl (N : Node_Id);
   --  N is an N_Object_Declaration whose expression must be an N_Aggregate or
   --  N_Extension_Aggregate with Expansion_Delayed.
   --  This procedure performs an in-place aggregate assignment.

   function Is_Delayed_Aggregate (N : Node_Id) return Boolean;
   --  Returns True if N is an aggregate of some kind whose Expansion_Delayed
   --  flag is set (see sinfo for meaning of flag).

   function Is_Two_Pass_Aggregate (N : Node_Id) return Boolean;
   --  Return True if N is an aggregate that is to be expanded in two passes.
   --  This is the case if it consists only of iterated component associations
   --  with iterator specifications, see RM 4.3.3(20.2/5).

   function Static_Array_Aggregate (N : Node_Id) return Boolean;
   --  N is an array aggregate that may have a component association with
   --  an others clause and a range. If bounds are static and the expressions
   --  are compile-time known constants, rewrite N as a purely positional
   --  aggregate, to be used to initialize variables and components of the type
   --  without generating elaboration code.

end Exp_Aggr;
