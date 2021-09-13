------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        B I N D O . B U I L D E R S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2019-2021, Free Software Foundation, Inc.      --
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

--  The following unit contains facilities to create various graphs that
--  reflect dependencies between units, as well as activations of tasks,
--  calls, and instantiations within them.

with Bindo.Graphs;
use  Bindo.Graphs;
use  Bindo.Graphs.Invocation_Graphs;
use  Bindo.Graphs.Library_Graphs;

package Bindo.Builders is

   -------------------------------
   -- Invocation_Graph_Builders --
   -------------------------------

   package Invocation_Graph_Builders is
      function Build_Invocation_Graph
        (Lib_G : Library_Graph) return Invocation_Graph;
      --  Return a new invocation graph that reflects the activations of
      --  tasks, calls, and instantiations in all units of the bind. Each
      --  invocation graph vertex is linked with the corresponding vertex
      --  of library graph Lib_G, which contains the body of the activated
      --  task, invoked subprogram, or instantiated generic.

   end Invocation_Graph_Builders;

   ----------------------------
   -- Library_Graph_Builders --
   ----------------------------

   package Library_Graph_Builders is
      function Build_Library_Graph return Library_Graph;
      --  Return a new library graph that reflects the dependencies between
      --  all units of the bind.

   end Library_Graph_Builders;

end Bindo.Builders;
