------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A L F A                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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

--  This package implements a light expansion which is used in formal
--  verification mode. Instead of a complete expansion of nodes for code
--  generation, this Alfa expansion targets generation of intermediate code
--  for formal verification.

--  Expand_Alfa is called directly by Expander.Expand.

--  Alfa expansion has three main objectives:

--    1. Perform limited expansion to explicit some Ada rules and constructs
--       (translate 'Old and 'Result, replace renamings by renamed, insert
--        conversions, expand actuals in calls to introduce temporaries)

--    2. Facilitate treatment for the formal verification back-end (fully
--       qualify names, set membership)

--    3. Avoid the introduction of low-level code that is difficult to analyze
--       formally, as typically done in the full expansion for high-level
--       constructs (tasking, dispatching)

with Types; use Types;

package Exp_Alfa is

   procedure Expand_Alfa (N : Node_Id);

end Exp_Alfa;
