------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E X P _ C G                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2010-2020, Free Software Foundation, Inc.      --
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

--  This package contains routines used to store and handle nodes required
--  to generate call graph information of dispatching calls.

with Types; use Types;

package Exp_CG is

   procedure Generate_CG_Output;
   --  Generate in the standard output the information associated with tagged
   --  types declaration and dispatching calls

   procedure Initialize;
   --  Called at the start of compilation to initialize the table that stores
   --  the tree nodes used by Generate_Output. This table is required because
   --  the format of the output requires fully qualified names (and hence the
   --  output must be generated after the source program has been compiled).

   procedure Register_CG_Node (N : Node_Id);
   --  Register a dispatching call node or the defining entity of a tagged
   --  type declaration

end Exp_CG;
