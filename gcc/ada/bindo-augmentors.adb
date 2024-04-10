------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      B I N D O . A U G M E N T O R S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019-2024, Free Software Foundation, Inc.      --
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

with Debug;  use Debug;
with Output; use Output;
with Types;  use Types;

with Bindo.Writers;
use  Bindo.Writers;
use  Bindo.Writers.Phase_Writers;

package body Bindo.Augmentors is

   ------------------------------
   -- Library_Graph_Augmentors --
   ------------------------------

   package body Library_Graph_Augmentors is

      ----------------
      -- Statistics --
      ----------------

      Longest_Path : Natural := 0;
      --  The length of the longest path found during the traversal of the
      --  invocation graph.

      Total_Visited : Natural := 0;
      --  The number of visited invocation graph vertices during the process
      --  of augmentation.

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Visit_Elaboration_Root
        (Inv_Graph : Invocation_Graph;
         Root      : Invocation_Graph_Vertex_Id);
      pragma Inline (Visit_Elaboration_Root);
      --  Start a DFS traversal from elaboration root Root to:
      --
      --    * Detect transitions between units.
      --
      --    * Create invocation edges for each such transition where the
      --      successor is Root.

      procedure Visit_Elaboration_Roots (Inv_Graph : Invocation_Graph);
      pragma Inline (Visit_Elaboration_Roots);
      --  Start a DFS traversal from all elaboration roots to:
      --
      --    * Detect transitions between units.
      --
      --    * Create invocation edges for each such transition where the
      --      successor is the current root.

      procedure Visit_Vertex
        (Inv_Graph                  : Invocation_Graph;
         Invoker                    : Invocation_Graph_Vertex_Id;
         Last_Vertex                : Library_Graph_Vertex_Id;
         Root_Vertex                : Library_Graph_Vertex_Id;
         Visited_Invokers           : IGV_Sets.Membership_Set;
         Activates_Task             : Boolean;
         Internal_Controlled_Action : Boolean;
         Path                       : Natural);
      pragma Inline (Visit_Vertex);
      --  Visit invocation graph vertex Invoker to:
      --
      --    * Detect a transition from the last library graph vertex denoted by
      --      Last_Vertex to the library graph vertex of Invoker.
      --
      --    * Create an invocation edge in library graph Lib_Graph to reflect
      --      the transition, where the predecessor is the library graph vertex
      --      or Invoker, and the successor is Root_Vertex.
      --
      --    * Visit the neighbours of Invoker.
      --
      --  Flag Internal_Controlled_Action should be set when the DFS traversal
      --  visited an internal controlled invocation edge. Path is the length of
      --  the path.

      procedure Write_Statistics;
      pragma Inline (Write_Statistics);
      --  Write the statistical information of the augmentation to standard
      --  output.

      ---------------------------
      -- Augment_Library_Graph --
      ---------------------------

      procedure Augment_Library_Graph (Inv_Graph : Invocation_Graph) is
         Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);
      begin
         pragma Assert (Present (Lib_Graph));

         --  Nothing to do when there is no invocation graph

         if not Present (Inv_Graph) then
            return;
         end if;

         Start_Phase (Library_Graph_Augmentation);

         --  Prepare the statistics data

         Longest_Path  := 0;
         Total_Visited := 0;

         Visit_Elaboration_Roots (Inv_Graph);
         Write_Statistics;

         End_Phase (Library_Graph_Augmentation);
      end Augment_Library_Graph;

      ----------------------------
      -- Visit_Elaboration_Root --
      ----------------------------

      procedure Visit_Elaboration_Root
        (Inv_Graph : Invocation_Graph;
         Root      : Invocation_Graph_Vertex_Id)
      is
         Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (Root));

         Root_Vertex : constant Library_Graph_Vertex_Id :=
                         Body_Vertex (Inv_Graph, Root);

         Visited : IGV_Sets.Membership_Set;

      begin
         --  Nothing to do when the unit where the elaboration root resides
         --  lacks elaboration code. This implies that any invocation edges
         --  going out of the unit are unwanted. This behavior emulates the
         --  old elaboration order mechanism.

         if Has_No_Elaboration_Code (Lib_Graph, Root_Vertex) then
            return;
         end if;

         --  Prepare the global data

         Visited := IGV_Sets.Create (Number_Of_Vertices (Inv_Graph));

         Visit_Vertex
           (Inv_Graph                  => Inv_Graph,
            Invoker                    => Root,
            Last_Vertex                => Root_Vertex,
            Root_Vertex                => Root_Vertex,
            Visited_Invokers           => Visited,
            Activates_Task             => False,
            Internal_Controlled_Action => False,
            Path                       => 0);

         IGV_Sets.Destroy (Visited);
      end Visit_Elaboration_Root;

      -----------------------------
      -- Visit_Elaboration_Roots --
      -----------------------------

      procedure Visit_Elaboration_Roots (Inv_Graph : Invocation_Graph) is
         Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Lib_Graph));

         Iter : Elaboration_Root_Iterator;
         Root : Invocation_Graph_Vertex_Id;

      begin
         Iter := Iterate_Elaboration_Roots (Inv_Graph);
         while Has_Next (Iter) loop
            Next (Iter, Root);

            Visit_Elaboration_Root (Inv_Graph => Inv_Graph, Root => Root);
         end loop;
      end Visit_Elaboration_Roots;

      ------------------
      -- Visit_Vertex --
      ------------------

      procedure Visit_Vertex
        (Inv_Graph                  : Invocation_Graph;
         Invoker                    : Invocation_Graph_Vertex_Id;
         Last_Vertex                : Library_Graph_Vertex_Id;
         Root_Vertex                : Library_Graph_Vertex_Id;
         Visited_Invokers           : IGV_Sets.Membership_Set;
         Activates_Task             : Boolean;
         Internal_Controlled_Action : Boolean;
         Path                       : Natural)
      is
         Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

         New_Path : constant Natural := Path + 1;

         Edge           : Invocation_Graph_Edge_Id;
         Edge_Kind      : Invocation_Kind;
         Invoker_Vertex : Library_Graph_Vertex_Id;
         Iter           : Edges_To_Targets_Iterator;

      begin
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (Invoker));
         pragma Assert (Present (Last_Vertex));
         pragma Assert (Present (Root_Vertex));
         pragma Assert (IGV_Sets.Present (Visited_Invokers));

         --  Nothing to do when the current invocation graph vertex has already
         --  been visited.

         if IGV_Sets.Contains (Visited_Invokers, Invoker) then
            return;
         end if;

         IGV_Sets.Insert (Visited_Invokers, Invoker);

         --  Update the statistics

         Longest_Path  := Natural'Max (Longest_Path, New_Path);
         Total_Visited := Total_Visited + 1;

         --  The library graph vertex of the current invocation graph vertex
         --  differs from that of the previous invocation graph vertex. This
         --  indicates that elaboration is transitioning from one unit to
         --  another. Add a library graph edge to capture this dependency.

         Invoker_Vertex := Body_Vertex (Inv_Graph, Invoker);
         pragma Assert (Present (Invoker_Vertex));

         if Invoker_Vertex /= Last_Vertex then

            --  The path ultimately reaches back into the unit where the root
            --  resides, resulting in a self dependency. In most cases this is
            --  a valid circularity, except when the path went through one of
            --  the Deep_xxx finalization-related routines. Do not create a
            --  library graph edge because the circularity is the result of
            --  expansion and thus spurious.

            if Invoker_Vertex = Root_Vertex
              and then Internal_Controlled_Action
            then
               null;

            --  Otherwise create the library graph edge, even if this results
            --  in a self dependency.

            else
               Add_Edge
                 (G              => Lib_Graph,
                  Pred           => Invoker_Vertex,
                  Succ           => Root_Vertex,
                  Kind           => Invocation_Edge,
                  Activates_Task => Activates_Task);
            end if;
         end if;

         --  Extend the DFS traversal to all targets of the invocation graph
         --  vertex.

         Iter := Iterate_Edges_To_Targets (Inv_Graph, Invoker);
         while Has_Next (Iter) loop
            Next (Iter, Edge);
            Edge_Kind := Kind (Inv_Graph, Edge);

            Visit_Vertex
              (Inv_Graph                  => Inv_Graph,
               Invoker                    => Target (Inv_Graph, Edge),
               Last_Vertex                => Invoker_Vertex,
               Root_Vertex                => Root_Vertex,
               Visited_Invokers           => Visited_Invokers,
               Activates_Task             =>
                 Activates_Task
                   or else Edge_Kind = Task_Activation,
               Internal_Controlled_Action =>
                 Internal_Controlled_Action
                   or else Edge_Kind in Internal_Controlled_Invocation_Kind,
               Path                       => New_Path);
         end loop;
      end Visit_Vertex;

      ----------------------
      -- Write_Statistics --
      ----------------------

      procedure Write_Statistics is
      begin
         --  Nothing to do when switch -d_L (output library item graph) is not
         --  in effect.

         if not Debug_Flag_Underscore_LL then
            return;
         end if;

         Write_Str ("Library Graph Augmentation");
         Write_Eol;
         Write_Eol;

         Write_Str ("Vertices visited   : ");
         Write_Num (Int (Total_Visited));
         Write_Eol;

         Write_Str ("Longest path length: ");
         Write_Num (Int (Longest_Path));
         Write_Eol;
         Write_Eol;

         Write_Str ("Library Graph Augmentation end");
         Write_Eol;
         Write_Eol;
      end Write_Statistics;
   end Library_Graph_Augmentors;

end Bindo.Augmentors;
