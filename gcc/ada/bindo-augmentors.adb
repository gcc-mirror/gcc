------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      B I N D O . A U G M E N T O R S                     --
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

with Debug;  use Debug;
with Output; use Output;
with Types;  use Types;

with Bindo.Writers; use Bindo.Writers;

with GNAT;      use GNAT;
with GNAT.Sets; use GNAT.Sets;

package body Bindo.Augmentors is

   ------------------------------
   -- Library_Graph_Augmentors --
   ------------------------------

   package body Library_Graph_Augmentors is

      -----------------
      -- Visited set --
      -----------------

      package VS is new Membership_Sets
        (Element_Type => Invocation_Graph_Vertex_Id,
         "="          => "=",
         Hash         => Hash_Invocation_Graph_Vertex);
      use VS;

      -----------------
      -- Global data --
      -----------------

      Inv_Graph : Invocation_Graph := Invocation_Graphs.Nil;
      Lib_Graph : Library_Graph    := Library_Graphs.Nil;
      Visited   : Membership_Set   := VS.Nil;

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

      function Is_Visited
        (IGV_Id : Invocation_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Visited);
      --  Determine whether invocation graph vertex IGV_Id has been visited
      --  during the traversal.

      procedure Set_Is_Visited
        (IGV_Id : Invocation_Graph_Vertex_Id;
         Val    : Boolean := True);
      pragma Inline (Set_Is_Visited);
      --  Mark invocation graph vertex IGV_Id as visited during the traversal
      --  depending on value Val.

      procedure Visit_Elaboration_Root (Root : Invocation_Graph_Vertex_Id);
      pragma Inline (Visit_Elaboration_Root);
      --  Start a DFS traversal from elaboration root Root to:
      --
      --    * Detect transitions between units.
      --
      --    * Create invocation edges for each such transition where the
      --      successor is Root.

      procedure Visit_Elaboration_Roots;
      pragma Inline (Visit_Elaboration_Roots);
      --  Start a DFS traversal from all elaboration roots to:
      --
      --    * Detect transitions between units.
      --
      --    * Create invocation edges for each such transition where the
      --      successor is the current root.

      procedure Visit_Vertex
        (Curr_IGV_Id   : Invocation_Graph_Vertex_Id;
         Last_LGV_Id   : Library_Graph_Vertex_Id;
         Root_LGV_Id   : Library_Graph_Vertex_Id;
         Internal_Ctrl : Boolean;
         Path          : Natural);
      pragma Inline (Visit_Vertex);
      --  Visit invocation graph vertex Curr_IGV_Id to:
      --
      --    * Detect a transition from the last library graph vertex denoted by
      --      Last_LGV_Id to the library graph vertex of Curr_IGV_Id.
      --
      --    * Create an invocation edge in library graph Lib_Graph to reflect
      --      the transition, where the predecessor is the library graph vertex
      --      or Curr_IGV_Id, and the successor is Root_LGV_Id.
      --
      --    * Visit the neighbours of Curr_IGV_Id.
      --
      --  Flag Internal_Ctrl should be set when the DFS traversal visited an
      --  internal controlled invocation edge. Path denotes the length of the
      --  path.

      procedure Write_Statistics;
      pragma Inline (Write_Statistics);
      --  Write the statistical information of the augmentation to standard
      --  output.

      ---------------------------
      -- Augment_Library_Graph --
      ---------------------------

      procedure Augment_Library_Graph
        (Inv_G : Invocation_Graph;
         Lib_G : Library_Graph)
      is
      begin
         pragma Assert (Present (Lib_G));

         --  Nothing to do when there is no invocation graph

         if not Present (Inv_G) then
            return;
         end if;

         --  Prepare the global data. Note that Visited is initialized for each
         --  elaboration root.

         Inv_Graph     := Inv_G;
         Lib_Graph     := Lib_G;
         Longest_Path  := 0;
         Total_Visited := 0;

         Visit_Elaboration_Roots;
         Write_Statistics;
      end Augment_Library_Graph;

      ----------------
      -- Is_Visited --
      ----------------

      function Is_Visited
        (IGV_Id : Invocation_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (Visited));
         pragma Assert (Present (IGV_Id));

         return Contains (Visited, IGV_Id);
      end Is_Visited;

      --------------------
      -- Set_Is_Visited --
      --------------------

      procedure Set_Is_Visited
        (IGV_Id : Invocation_Graph_Vertex_Id;
         Val    : Boolean := True)
      is
      begin
         pragma Assert (Present (Visited));
         pragma Assert (Present (IGV_Id));

         if Val then
            Insert (Visited, IGV_Id);
         else
            Delete (Visited, IGV_Id);
         end if;
      end Set_Is_Visited;

      ----------------------------
      -- Visit_Elaboration_Root --
      ----------------------------

      procedure Visit_Elaboration_Root (Root : Invocation_Graph_Vertex_Id) is
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Root));
         pragma Assert (Present (Lib_Graph));

         Root_LGV_Id : constant Library_Graph_Vertex_Id :=
                         Lib_Vertex (Inv_Graph, Root);

         pragma Assert (Present (Root_LGV_Id));

      begin
         --  Prepare the global data

         Visited := Create (Number_Of_Vertices (Inv_Graph));

         Visit_Vertex
           (Curr_IGV_Id   => Root,
            Last_LGV_Id   => Root_LGV_Id,
            Root_LGV_Id   => Root_LGV_Id,
            Internal_Ctrl => False,
            Path          => 0);

         Destroy (Visited);
      end Visit_Elaboration_Root;

      -----------------------------
      -- Visit_Elaboration_Roots --
      -----------------------------

      procedure Visit_Elaboration_Roots is
         Iter : Elaboration_Root_Iterator;
         Root : Invocation_Graph_Vertex_Id;

      begin
         pragma Assert (Present (Inv_Graph));

         Iter := Iterate_Elaboration_Roots (Inv_Graph);
         while Has_Next (Iter) loop
            Next (Iter, Root);
            pragma Assert (Present (Root));

            Visit_Elaboration_Root (Root);
         end loop;
      end Visit_Elaboration_Roots;

      ------------------
      -- Visit_Vertex --
      ------------------

      procedure Visit_Vertex
        (Curr_IGV_Id   : Invocation_Graph_Vertex_Id;
         Last_LGV_Id   : Library_Graph_Vertex_Id;
         Root_LGV_Id   : Library_Graph_Vertex_Id;
         Internal_Ctrl : Boolean;
         Path          : Natural)
      is
         New_Path : constant Natural := Path + 1;

         Curr_LGV_Id : Library_Graph_Vertex_Id;
         IGE_Id      : Invocation_Graph_Edge_Id;
         Iter        : Edges_To_Targets_Iterator;
         Targ        : Invocation_Graph_Vertex_Id;

      begin
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Curr_IGV_Id));
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (Last_LGV_Id));
         pragma Assert (Present (Root_LGV_Id));

         --  Nothing to do when the current invocation graph vertex has already
         --  been visited.

         if Is_Visited (Curr_IGV_Id) then
            return;
         end if;

         Set_Is_Visited (Curr_IGV_Id);

         --  Update the statistics

         Longest_Path  := Natural'Max (Longest_Path, New_Path);
         Total_Visited := Total_Visited + 1;

         --  The library graph vertex of the current invocation graph vertex
         --  differs from that of the previous invocation graph vertex. This
         --  indicates that elaboration is transitioning from one unit to
         --  another. Add a library graph edge to capture this dependency.

         Curr_LGV_Id := Lib_Vertex (Inv_Graph, Curr_IGV_Id);
         pragma Assert (Present (Curr_LGV_Id));

         if Curr_LGV_Id /= Last_LGV_Id then

            --  The path ultimately reaches back into the unit where the root
            --  resides, resulting in a self dependency. In most cases this is
            --  a valid circularity, except when the path went through one of
            --  the Deep_xxx finalization-related routines. Do not create a
            --  library graph edge because the circularity is the result of
            --  expansion and thus spurious.

            if Curr_LGV_Id = Root_LGV_Id and then Internal_Ctrl then
               null;

            --  Otherwise create the library graph edge, even if this results
            --  in a self dependency.

            else
               Add_Edge
                 (G    => Lib_Graph,
                  Pred => Curr_LGV_Id,
                  Succ => Root_LGV_Id,
                  Kind => Invocation_Edge);
            end if;
         end if;

         --  Extend the DFS traversal to all targets of the invocation graph
         --  vertex.

         Iter := Iterate_Edges_To_Targets (Inv_Graph, Curr_IGV_Id);
         while Has_Next (Iter) loop
            Next (Iter, IGE_Id);
            pragma Assert (Present (IGE_Id));

            Targ := Target (Inv_Graph, IGE_Id);
            pragma Assert (Present (Targ));

            Visit_Vertex
              (Curr_IGV_Id   => Targ,
               Last_LGV_Id   => Curr_LGV_Id,
               Root_LGV_Id   => Root_LGV_Id,
               Internal_Ctrl =>
                 Internal_Ctrl
                   or else Kind (Inv_Graph, IGE_Id) in
                             Internal_Controlled_Invocation_Kind,
               Path          => New_Path);
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
