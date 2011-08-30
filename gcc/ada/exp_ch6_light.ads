------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         E X P _ C H 6 _ L I G H T                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--              Copyright (C) 2011, Free Software Foundation, Inc.          --
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

--  Light expand routines for chapter 6 constructs

with Types; use Types;

package Exp_Ch6_Light is

   procedure Expand_Light_Call (N : Node_Id);
   --  This procedure contains common processing for function and procedure
   --  calls:
   --  * expansion of actuals to introduce necessary temporaries
   --  * replacement of renaming by subprogram renamed

   procedure Expand_Light_N_Simple_Return_Statement (N : Node_Id);
   --  Insert conversion on function return if necessary

   procedure Expand_Light_N_Subprogram_Body (N : Node_Id);
   --  Fully qualify names of enclosed entities

end Exp_Ch6_Light;
