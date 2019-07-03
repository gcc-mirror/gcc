------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     B I N D O . D I A G N O S T I C S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019, Free Software Foundation, Inc.           --
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

package body Bindo.Diagnostics is

   -----------------------
   -- Cycle_Diagnostics --
   -----------------------

   package body Cycle_Diagnostics is

      -----------------------------
      -- Has_Elaborate_All_Cycle --
      -----------------------------

      function Has_Elaborate_All_Cycle (G : Library_Graph) return Boolean is
         Has_Cycle : Boolean;
         Iter      : All_Edge_Iterator;
         LGE_Id    : Library_Graph_Edge_Id;

      begin
         pragma Assert (Present (G));

         --  Assume that the graph lacks a cycle

         Has_Cycle := False;

         --  The library graph has an Elaborate_All cycle when one of its edges
         --  represents a with clause for a unit with pragma Elaborate_All, and
         --  both the predecessor and successor reside in the same component.
         --  Note that the iteration must run to completion in order to unlock
         --  the graph.

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, LGE_Id);
            pragma Assert (Present (LGE_Id));

            if Kind (G, LGE_Id) = Elaborate_All_Edge
              and then Links_Vertices_In_Same_Component (G, LGE_Id)
            then
               Has_Cycle := True;
            end if;
         end loop;

         return Has_Cycle;
      end Has_Elaborate_All_Cycle;
   end Cycle_Diagnostics;

end Bindo.Diagnostics;
