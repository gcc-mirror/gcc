------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      B I N D O . A U G M E N T O R S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2019-2025, Free Software Foundation, Inc.      --
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

--  For full architecture, see unit Bindo.

--  The following unit contains facilities to enhance the library graph, which
--  reflects source dependencies between units, with information obtained from
--  the invocation graph, which reflects all activations of tasks, calls, and
--  instantiations within units.

with Bindo.Graphs;
use  Bindo.Graphs;
use  Bindo.Graphs.Invocation_Graphs;
use  Bindo.Graphs.Library_Graphs;

package Bindo.Augmentors is

   ------------------------------
   -- Library_Graph_Augmentors --
   ------------------------------

   package Library_Graph_Augmentors is
      procedure Augment_Library_Graph (Inv_Graph : Invocation_Graph);
      --  Augment the library graph of Inv_Graph with information from
      --  invocation graph Inv_Graph as follows:
      --
      --    1) Traverse the invocation graph starting from each elaboration
      --       procedure of unit Root.
      --
      --    2) Each time the traversal transitions from one unit into another
      --       unit Curr, add an invocation edge between predecessor Curr and
      --       successor Root in the library graph.
      --
      --    3) Do the above steps for all units with an elaboration procedure.

   end Library_Graph_Augmentors;

end Bindo.Augmentors;
