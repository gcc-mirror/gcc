------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A L F A                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2012, Free Software Foundation, Inc.         --
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
--  verification mode (Alfa_Mode = True). Instead of a complete expansion
--  of nodes for code generation, this Alfa expansion targets generation
--  of intermediate code for formal verification.

--  Expand_Alfa is called directly by Expander.Expand.

--  Alfa expansion has three main objectives:

--    1. Perform limited expansion to explicit some Ada rules and constructs
--       (translate 'Old and 'Result, replace renamings by renamed, insert
--        conversions, expand actuals in calls to introduce temporaries, expand
--        generics instantiations)

--    2. Facilitate treatment for the formal verification back-end (fully
--       qualify names, expand set membership, compute data dependences)

--    3. Avoid the introduction of low-level code that is difficult to analyze
--       formally, as typically done in the full expansion for high-level
--       constructs (tasking, dispatching)

--  To fulfill objective 1, Expand_Alfa selectively expands some constructs.

--  To fulfill objective 2, the tree after Alfa expansion should be fully
--  analyzed semantically. In particular, all expression must have their proper
--  type, and semantic links should be set between tree nodes (partial to full
--  view, etc.) Some kinds of nodes should be either absent, or can be ignored
--  by the formal verification backend:

--      N_Object_Renaming_Declaration: can be ignored safely
--      N_Expression_Function:         absent (rewitten)
--      N_Expression_With_Actions:     absent (not generated)

--  Alfa cross-references are generated from the regular cross-references (used
--  for browsing and code understanding) and additional references collected
--  during semantic analysis, in particular on all dereferences. These Alfa
--  cross-references are output in a separate section of ALI files, as
--  described in alfa.adb. They are the basis for the computation of data
--  dependences in the formal verification backend. This implies that all
--  cross-references should be generated in this mode, even those that would
--  not make sense from a user point-of-view, and that cross-references that do
--  not lead to data dependences for subprograms can be safely ignored.

--  To support the formal verification of units parameterized by data, the
--  value of deferred constants should not be considered as a compile-time
--  constant at program locations where the full view is not visible.

--  To fulfill objective 3, Expand_Alfa does not expand features that are not
--  formally analyzed (tasking), or for which formal analysis relies on the
--  source level representation (dispatching, aspects, pragmas). However, these
--  should be semantically analyzed, which sometimes requires the insertion of
--  semantic pre-analysis, for example for subprogram contracts and pragma
--  check/assert.

with Types; use Types;

package Exp_Alfa is

   procedure Expand_Alfa (N : Node_Id);

end Exp_Alfa;
