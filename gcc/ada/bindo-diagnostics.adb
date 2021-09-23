------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     B I N D O . D I A G N O S T I C S                    --
--                                                                          --
--                                 B o d y                                  --
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

with Binderr;  use Binderr;
with Debug;    use Debug;
with Rident;   use Rident;
with Types;    use Types;

with Bindo.Validators;
use  Bindo.Validators;
use  Bindo.Validators.Cycle_Validators;

with Bindo.Writers;
use  Bindo.Writers;
use  Bindo.Writers.Cycle_Writers;
use  Bindo.Writers.Phase_Writers;

package body Bindo.Diagnostics is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Diagnose_All_Cycles (Inv_Graph : Invocation_Graph);
   pragma Inline (Diagnose_All_Cycles);
   --  Emit diagnostics for all cycles of library graph G

   procedure Diagnose_Cycle
     (Inv_Graph : Invocation_Graph;
      Cycle     : Library_Graph_Cycle_Id);
   pragma Inline (Diagnose_Cycle);
   --  Emit diagnostics for cycle Cycle of library graph G

   procedure Find_And_Output_Invocation_Paths
     (Inv_Graph   : Invocation_Graph;
      Source      : Library_Graph_Vertex_Id;
      Destination : Library_Graph_Vertex_Id);
   pragma Inline (Find_And_Output_Invocation_Paths);
   --  Find all paths in invocation graph Inv_Graph that originate from vertex
   --  Source and reach vertex Destination of library graph Lib_Graph. Output
   --  the transitions of each such path.

   function Find_Elaboration_Root
     (Inv_Graph : Invocation_Graph;
      Vertex : Library_Graph_Vertex_Id) return Invocation_Graph_Vertex_Id;
   pragma Inline (Find_Elaboration_Root);
   --  Find the elaboration root in invocation graph Inv_Graph that corresponds
   --  to vertex Vertex of library graph Lib_Graph.

   procedure Output_All_Cycles_Suggestions (G : Library_Graph);
   pragma Inline (Output_All_Cycles_Suggestions);
   --  Suggest the diagnostic of all cycles in library graph G if circumstances
   --  allow it.

   procedure Output_Elaborate_All_Suggestions
     (G    : Library_Graph;
      Pred : Library_Graph_Vertex_Id;
      Succ : Library_Graph_Vertex_Id);
   pragma Inline (Output_Elaborate_All_Suggestions);
   --  Suggest ways to break a cycle that involves an Elaborate_All edge that
   --  links predecessor Pred and successor Succ of library graph G.

   procedure Output_Elaborate_All_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id);
   pragma Inline (Output_Elaborate_All_Transition);
   --  Output a transition through an Elaborate_All edge of library graph G
   --  with successor Source and predecessor Actual_Destination. Parameter
   --  Expected_Destination denotes the predecessor as specified by the next
   --  edge in a cycle.

   procedure Output_Elaborate_Body_Suggestions
     (G    : Library_Graph;
      Succ : Library_Graph_Vertex_Id);
   pragma Inline (Output_Elaborate_Body_Suggestions);
   --  Suggest ways to break a cycle that involves an edge where successor Succ
   --  is either a spec subject to pragma Elaborate_Body or the body of such a
   --  spec.

   procedure Output_Elaborate_Body_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id;
      Elaborate_All_Active : Boolean);
   pragma Inline (Output_Elaborate_Body_Transition);
   --  Output a transition through an edge of library graph G with successor
   --  Source and predecessor Actual_Destination. Vertex Source is either
   --  a spec subject to pragma Elaborate_Body or denotes the body of such
   --  a spec. Expected_Destination denotes the predecessor as specified by
   --  the next edge in a cycle. Elaborate_All_Active should be set when the
   --  transition occurs within a cycle that involves an Elaborate_All edge.

   procedure Output_Elaborate_Suggestions
     (G    : Library_Graph;
      Pred : Library_Graph_Vertex_Id;
      Succ : Library_Graph_Vertex_Id);
   pragma Inline (Output_Elaborate_Suggestions);
   --  Suggest ways to break a cycle that involves an Elaborate edge that links
   --  predecessor Pred and successor Succ of library graph G.

   procedure Output_Elaborate_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id);
   pragma Inline (Output_Elaborate_Transition);
   --  Output a transition through an Elaborate edge of library graph G
   --  with successor Source and predecessor Actual_Destination. Parameter
   --  Expected_Destination denotes the predecessor as specified by the next
   --  edge in a cycle.

   procedure Output_Forced_Suggestions
     (G    : Library_Graph;
      Pred : Library_Graph_Vertex_Id;
      Succ : Library_Graph_Vertex_Id);
   pragma Inline (Output_Forced_Suggestions);
   --  Suggest ways to break a cycle that involves a Forced edge that links
   --  predecessor Pred with successor Succ of library graph G.

   procedure Output_Forced_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id;
      Elaborate_All_Active : Boolean);
   pragma Inline (Output_Forced_Transition);
   --  Output a transition through a Forced edge of library graph G with
   --  successor Source and predecessor Actual_Destination. Parameter
   --  Expected_Destination denotes the predecessor as specified by the
   --  next edge in a cycle. Elaborate_All_Active should be set when the
   --  transition occurs within a cycle that involves an Elaborate_All edge.

   procedure Output_Full_Encoding_Suggestions
     (G          : Library_Graph;
      Cycle      : Library_Graph_Cycle_Id;
      First_Edge : Library_Graph_Edge_Id);
   pragma Inline (Output_Full_Encoding_Suggestions);
   --  Suggest the use of the full path invocation graph encoding to break
   --  cycle Cycle with initial edge First_Edge of library graph G.

   procedure Output_Invocation_Path
     (Inv_Graph         : Invocation_Graph;
      Elaborated_Vertex : Library_Graph_Vertex_Id;
      Path              : IGE_Lists.Doubly_Linked_List;
      Path_Id           : in out Nat);
   pragma Inline (Output_Invocation_Path);
   --  Output path Path, which consists of invocation graph Inv_Graph edges.
   --  Elaborated_Vertex is the vertex of library graph Lib_Graph whose
   --  elaboration initiated the path. Path_Id is the unique id of the path.

   procedure Output_Invocation_Path_Transition
     (Inv_Graph : Invocation_Graph;
      Edge      : Invocation_Graph_Edge_Id);
   pragma Inline (Output_Invocation_Path_Transition);
   --  Output a transition through edge Edge of invocation graph G, which is
   --  part of an invocation path.

   procedure Output_Invocation_Related_Suggestions
     (G     : Library_Graph;
      Cycle : Library_Graph_Cycle_Id);
   pragma Inline (Output_Invocation_Related_Suggestions);
   --  Suggest ways to break cycle Cycle of library graph G that involves at
   --  least one invocation edge.

   procedure Output_Invocation_Transition
     (Inv_Graph   : Invocation_Graph;
      Source      : Library_Graph_Vertex_Id;
      Destination : Library_Graph_Vertex_Id);
   pragma Inline (Output_Invocation_Transition);
   --  Output a transition through an invocation edge of library graph G with
   --  successor Source and predecessor Destination. Inv_Graph is the related
   --  invocation graph.

   procedure Output_Reason_And_Circularity_Header
     (G          : Library_Graph;
      First_Edge : Library_Graph_Edge_Id);
   pragma Inline (Output_Reason_And_Circularity_Header);
   --  Output the reason and circularity header for a circularity of library
   --  graph G with initial edge First_Edge.

   procedure Output_Suggestions
     (G          : Library_Graph;
      Cycle      : Library_Graph_Cycle_Id;
      First_Edge : Library_Graph_Edge_Id);
   pragma Inline (Output_Suggestions);
   --  Suggest various ways to break cycle Cycle with initial edge First_Edge
   --  of library graph G.

   procedure Output_Transition
     (Inv_Graph            : Invocation_Graph;
      Current_Edge         : Library_Graph_Edge_Id;
      Next_Edge            : Library_Graph_Edge_Id;
      Elaborate_All_Active : Boolean);
   pragma Inline (Output_Transition);
   --  Output a transition described by edge Current_Edge, which is followed by
   --  edge Next_Edge of library graph Lib_Graph. Inv_Graph denotes the related
   --  invocation graph. Elaborate_All_Active should be set when the transition
   --  occurs within a cycle that involves an Elaborate_All edge.

   procedure Output_With_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id;
      Elaborate_All_Active : Boolean);
   pragma Inline (Output_With_Transition);
   --  Output a transition through a regular with edge of library graph G
   --  with successor Source and predecessor Actual_Destination. Parameter
   --  Expected_Destination denotes the predecessor as specified by the next
   --  edge in a cycle. Elaborate_All_Active should be set when the transition
   --  occurs within a cycle that involves an Elaborate_All edge.

   procedure Visit_Vertex
     (Inv_Graph         : Invocation_Graph;
      Invoker           : Invocation_Graph_Vertex_Id;
      Invoker_Vertex    : Library_Graph_Vertex_Id;
      Last_Vertex       : Library_Graph_Vertex_Id;
      Elaborated_Vertex : Library_Graph_Vertex_Id;
      End_Vertex        : Library_Graph_Vertex_Id;
      Visited_Invokers  : IGV_Sets.Membership_Set;
      Path              : IGE_Lists.Doubly_Linked_List;
      Path_Id           : in out Nat);
   pragma Inline (Visit_Vertex);
   --  Visit invocation graph vertex Invoker that resides in library graph
   --  vertex Invoker_Vertex as part of a DFS traversal. Last_Vertex denotes
   --  the previous vertex in the traversal. Elaborated_Vertex is the vertex
   --  whose elaboration started the traversal. End_Vertex is the vertex that
   --  terminates the traversal. Visited_Invoker is the set of all invokers
   --  visited so far. All edges along the path are recorded in Path. Path_Id
   --  is the id of the path.

   -------------------------
   -- Diagnose_All_Cycles --
   -------------------------

   procedure Diagnose_All_Cycles (Inv_Graph : Invocation_Graph) is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

      Cycle : Library_Graph_Cycle_Id;
      Iter  : All_Cycle_Iterator;

   begin
      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));

      Iter := Iterate_All_Cycles (Lib_Graph);
      while Has_Next (Iter) loop
         Next (Iter, Cycle);

         Diagnose_Cycle (Inv_Graph => Inv_Graph, Cycle => Cycle);
      end loop;
   end Diagnose_All_Cycles;

   ----------------------------
   -- Diagnose_Circularities --
   ----------------------------

   procedure Diagnose_Circularities (Inv_Graph : Invocation_Graph) is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);
   begin
      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));

      --  Find, validate, and output all cycles of the library graph

      Find_Cycles     (Lib_Graph);
      Validate_Cycles (Lib_Graph);
      Write_Cycles    (Lib_Graph);

      --  Diagnose all cycles in the graph regardless of their importance when
      --  switch -d_C (diagnose all cycles) is in effect.

      if Debug_Flag_Underscore_CC then
         Diagnose_All_Cycles (Inv_Graph);

      --  Otherwise diagnose the most important cycle in the graph

      else
         Diagnose_Cycle
           (Inv_Graph => Inv_Graph,
            Cycle     => Highest_Precedence_Cycle (Lib_Graph));
      end if;
   end Diagnose_Circularities;

   --------------------
   -- Diagnose_Cycle --
   --------------------

   procedure Diagnose_Cycle
     (Inv_Graph : Invocation_Graph;
      Cycle     : Library_Graph_Cycle_Id)
   is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));
      pragma Assert (Present (Cycle));

      Elaborate_All_Active : constant Boolean :=
                               Contains_Elaborate_All_Edge
                                 (G     => Lib_Graph,
                                  Cycle => Cycle);

      Current_Edge : Library_Graph_Edge_Id := No_Library_Graph_Edge;
      First_Edge   : Library_Graph_Edge_Id;
      Iter         : Edges_Of_Cycle_Iterator;
      Next_Edge    : Library_Graph_Edge_Id;

   begin
      Start_Phase (Cycle_Diagnostics);

      First_Edge := No_Library_Graph_Edge;

      --  Inspect the edges of the cycle in pairs, emitting diagnostics based
      --  on their successors and predecessors.

      Iter := Iterate_Edges_Of_Cycle (Lib_Graph, Cycle);
      while Has_Next (Iter) loop

         --  Emit the reason for the cycle using the initial edge, which is the
         --  most important edge in the cycle.

         if not Present (First_Edge) then
            Next (Iter, Current_Edge);

            First_Edge := Current_Edge;
            Output_Reason_And_Circularity_Header
              (G          => Lib_Graph,
               First_Edge => First_Edge);
         end if;

         --  Obtain the other edge of the pair

         exit when not Has_Next (Iter);
         Next (Iter, Next_Edge);

         --  Describe the transition from the current edge to the next edge by
         --  taking into account the predecessors and successors involved, as
         --  well as the nature of the edge.

         Output_Transition
           (Inv_Graph            => Inv_Graph,
            Current_Edge         => Current_Edge,
            Next_Edge            => Next_Edge,
            Elaborate_All_Active => Elaborate_All_Active);

         Current_Edge := Next_Edge;
      end loop;

      --  Describe the transition from the last edge to the first edge

      Output_Transition
        (Inv_Graph            => Inv_Graph,
         Current_Edge         => Current_Edge,
         Next_Edge            => First_Edge,
         Elaborate_All_Active => Elaborate_All_Active);

      --  Suggest various alternatives for breaking the cycle

      Output_Suggestions
        (G          => Lib_Graph,
         Cycle      => Cycle,
         First_Edge => First_Edge);

      End_Phase (Cycle_Diagnostics);
   end Diagnose_Cycle;

   --------------------------------------
   -- Find_And_Output_Invocation_Paths --
   --------------------------------------

   procedure Find_And_Output_Invocation_Paths
     (Inv_Graph   : Invocation_Graph;
      Source      : Library_Graph_Vertex_Id;
      Destination : Library_Graph_Vertex_Id)
   is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

      Path    : IGE_Lists.Doubly_Linked_List;
      Path_Id : Nat;
      Visited : IGV_Sets.Membership_Set;

   begin
      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));
      pragma Assert (Present (Source));
      pragma Assert (Present (Destination));

      --  Nothing to do when the invocation graph encoding format of the source
      --  vertex does not contain detailed information about invocation paths.

      if Invocation_Graph_Encoding (Lib_Graph, Source) /=
           Full_Path_Encoding
      then
         return;
      end if;

      Path    := IGE_Lists.Create;
      Path_Id := 1;
      Visited := IGV_Sets.Create (Number_Of_Vertices (Inv_Graph));

      --  Start a DFS traversal over the invocation graph, in an attempt to
      --  reach Destination from Source. The actual start of the path is the
      --  elaboration root invocation vertex that corresponds to the Source.
      --  Each unique path is emitted as part of the current cycle diagnostic.

      Visit_Vertex
        (Inv_Graph         => Inv_Graph,
         Invoker           =>
           Find_Elaboration_Root
             (Inv_Graph => Inv_Graph,
              Vertex    => Source),
         Invoker_Vertex    => Source,
         Last_Vertex       => Source,
         Elaborated_Vertex => Source,
         End_Vertex        => Destination,
         Visited_Invokers  => Visited,
         Path              => Path,
         Path_Id           => Path_Id);

      IGE_Lists.Destroy (Path);
      IGV_Sets.Destroy  (Visited);
   end Find_And_Output_Invocation_Paths;

   ---------------------------
   -- Find_Elaboration_Root --
   ---------------------------

   function Find_Elaboration_Root
     (Inv_Graph : Invocation_Graph;
      Vertex    : Library_Graph_Vertex_Id) return Invocation_Graph_Vertex_Id
   is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

      Current_Vertex : Invocation_Graph_Vertex_Id;
      Iter           : Elaboration_Root_Iterator;
      Root_Vertex    : Invocation_Graph_Vertex_Id;

   begin
      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));
      pragma Assert (Present (Vertex));

      --  Assume that the vertex does not have a corresponding elaboration root

      Root_Vertex := No_Invocation_Graph_Vertex;

      --  Inspect all elaboration roots trying to find the one that resides in
      --  the input vertex.
      --
      --  IMPORTANT:
      --
      --    * The iterator must run to completion in order to unlock the
      --      invocation graph.

      Iter := Iterate_Elaboration_Roots (Inv_Graph);
      while Has_Next (Iter) loop
         Next (Iter, Current_Vertex);

         if not Present (Root_Vertex)
           and then Body_Vertex (Inv_Graph, Current_Vertex) = Vertex
         then
            Root_Vertex := Current_Vertex;
         end if;
      end loop;

      return Root_Vertex;
   end Find_Elaboration_Root;

   -----------------------------------
   -- Output_All_Cycles_Suggestions --
   -----------------------------------

   procedure Output_All_Cycles_Suggestions (G : Library_Graph) is
   begin
      pragma Assert (Present (G));

      --  The library graph contains at least one cycle and only the highest
      --  priority cycle was diagnosed. Diagnosing all cycles may yield extra
      --  information for decision making.

      if Number_Of_Cycles (G) > 1 and then not Debug_Flag_Underscore_CC then
         Error_Msg_Info
           ("    diagnose all circularities (binder switch -d_C)");
      end if;
   end Output_All_Cycles_Suggestions;

   --------------------------------------
   -- Output_Elaborate_All_Suggestions --
   --------------------------------------

   procedure Output_Elaborate_All_Suggestions
     (G    : Library_Graph;
      Pred : Library_Graph_Vertex_Id;
      Succ : Library_Graph_Vertex_Id)
   is
   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Pred));
      pragma Assert (Present (Succ));

      Error_Msg_Unit_1 := Name (G, Pred);
      Error_Msg_Unit_2 := Name (G, Succ);
      Error_Msg_Info
        ("    change pragma Elaborate_All for unit $ to Elaborate in unit $");
      Error_Msg_Info
        ("    remove pragma Elaborate_All for unit $ in unit $");
   end Output_Elaborate_All_Suggestions;

   -------------------------------------
   -- Output_Elaborate_All_Transition --
   -------------------------------------

   procedure Output_Elaborate_All_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id)
   is
   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Source));
      pragma Assert (Present (Actual_Destination));
      pragma Assert (Present (Expected_Destination));

      --  The actual and expected destination vertices match, and denote the
      --  initial declaration of a unit.
      --
      --            Elaborate_All   Actual_Destination
      --    Source ---------------> spec -->
      --                            Expected_Destination
      --
      --            Elaborate_All   Actual_Destination
      --    Source ---------------> stand-alone body -->
      --                            Expected_Destination

      if Actual_Destination = Expected_Destination then
         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause and pragma Elaborate_All for unit $");

      --  Otherwise the actual destination vertex denotes the spec of a unit,
      --  while the expected destination is the corresponding body.
      --
      --            Elaborate_All   Actual_Destination
      --    Source ---------------> spec
      --
      --                            body -->
      --                            Expected_Destination

      else
         pragma Assert (Is_Spec_With_Body (G, Actual_Destination));
         pragma Assert (Is_Body_With_Spec (G, Expected_Destination));
         pragma Assert
           (Proper_Body (G, Actual_Destination) = Expected_Destination);

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause and pragma Elaborate_All for unit $");

         Error_Msg_Unit_1 := Name (G, Expected_Destination);
         Error_Msg_Info
           ("    unit $ is in the closure of pragma Elaborate_All");
      end if;
   end Output_Elaborate_All_Transition;

   ---------------------------------------
   -- Output_Elaborate_Body_Suggestions --
   ---------------------------------------

   procedure Output_Elaborate_Body_Suggestions
     (G    : Library_Graph;
      Succ : Library_Graph_Vertex_Id)
   is
      Spec : Library_Graph_Vertex_Id;

   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Succ));

      --  Find the initial declaration of the unit because it is the one
      --  subject to pragma Elaborate_Body.

      if Is_Body_With_Spec (G, Succ) then
         Spec := Proper_Spec (G, Succ);
      else
         Spec := Succ;
      end if;

      Error_Msg_Unit_1 := Name (G, Spec);
      Error_Msg_Info
        ("    remove pragma Elaborate_Body in unit $");
   end Output_Elaborate_Body_Suggestions;

   --------------------------------------
   -- Output_Elaborate_Body_Transition --
   --------------------------------------

   procedure Output_Elaborate_Body_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id;
      Elaborate_All_Active : Boolean)
   is
   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Source));
      pragma Assert (Present (Actual_Destination));
      pragma Assert (Present (Expected_Destination));

      --  The actual and expected destination vertices match
      --
      --                     Actual_Destination
      --    Source --------> spec -->
      --    Elaborate_Body   Expected_Destination
      --
      --                     spec
      --
      --                     Actual_Destination
      --    Source --------> body -->
      --    Elaborate_Body   Expected_Destination

      if Actual_Destination = Expected_Destination then
         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause for unit $");

      --  The actual destination vertex denotes the spec of a unit while the
      --  expected destination is the corresponding body, and the unit is in
      --  the closure of an earlier Elaborate_All pragma.
      --
      --                     Actual_Destination
      --    Source --------> spec
      --    Elaborate_Body
      --                     body -->
      --                     Expected_Destination

      elsif Elaborate_All_Active then
         pragma Assert (Is_Spec_With_Body (G, Actual_Destination));
         pragma Assert (Is_Body_With_Spec (G, Expected_Destination));
         pragma Assert
           (Proper_Body (G, Actual_Destination) = Expected_Destination);

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause for unit $");

         Error_Msg_Unit_1 := Name (G, Expected_Destination);
         Error_Msg_Info
           ("    unit $ is in the closure of pragma Elaborate_All");

      --  Otherwise the actual destination vertex is the spec of a unit subject
      --  to pragma Elaborate_Body and the expected destination vertex is the
      --  completion body.
      --
      --                     Actual_Destination
      --    Source --------> spec Elaborate_Body
      --    Elaborate_Body
      --                     body -->
      --                     Expected_Destination

      else
         pragma Assert
           (Is_Elaborate_Body_Pair
             (G           => G,
              Spec_Vertex => Actual_Destination,
              Body_Vertex => Expected_Destination));

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause for unit $");

         Error_Msg_Unit_1 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ is subject to pragma Elaborate_Body");

         Error_Msg_Unit_1 := Name (G, Expected_Destination);
         Error_Msg_Info
           ("    unit $ is in the closure of pragma Elaborate_Body");
      end if;
   end Output_Elaborate_Body_Transition;

   ----------------------------------
   -- Output_Elaborate_Suggestions --
   ----------------------------------

   procedure Output_Elaborate_Suggestions
     (G    : Library_Graph;
      Pred : Library_Graph_Vertex_Id;
      Succ : Library_Graph_Vertex_Id)
   is
   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Pred));
      pragma Assert (Present (Succ));

      Error_Msg_Unit_1 := Name (G, Pred);
      Error_Msg_Unit_2 := Name (G, Succ);
      Error_Msg_Info
        ("    remove pragma Elaborate for unit $ in unit $");
   end Output_Elaborate_Suggestions;

   ---------------------------------
   -- Output_Elaborate_Transition --
   ---------------------------------

   procedure Output_Elaborate_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id)
   is
      Spec : Library_Graph_Vertex_Id;

   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Source));
      pragma Assert (Present (Actual_Destination));
      pragma Assert (Present (Expected_Destination));

      --  The actual and expected destination vertices match, and denote the
      --  initial declaration of a unit.
      --
      --            Elaborate   Actual_Destination
      --    Source -----------> spec -->
      --                        Expected_Destination
      --
      --            Elaborate   Actual_Destination
      --    Source -----------> stand-alone body -->
      --                        Expected_Destination
      --
      --  The processing of pragma Elaborate body generates an edge between a
      --  successor and predecessor body.
      --
      --                        spec
      --
      --            Elaborate   Actual_Destination
      --    Source -----------> body -->
      --                        Expected_Destination

      if Actual_Destination = Expected_Destination then

         --  Find the initial declaration of the unit because it is the one
         --  subject to pragma Elaborate.

         if Is_Body_With_Spec (G, Actual_Destination) then
            Spec := Proper_Spec (G, Actual_Destination);
         else
            Spec := Actual_Destination;
         end if;

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Spec);
         Error_Msg_Info
           ("    unit $ has with clause and pragma Elaborate for unit $");

         if Actual_Destination /= Spec then
            Error_Msg_Unit_1 := Name (G, Actual_Destination);
            Error_Msg_Info
              ("    unit $ is in the closure of pragma Elaborate");
         end if;

      --  Otherwise the actual destination vertex denotes the spec of a unit
      --  while the expected destination vertex is the corresponding body.
      --
      --            Elaborate   Actual_Destination
      --    Source -----------> spec
      --
      --                        body -->
      --                        Expected_Destination

      else
         pragma Assert (Is_Spec_With_Body (G, Actual_Destination));
         pragma Assert (Is_Body_With_Spec (G, Expected_Destination));
         pragma Assert
           (Proper_Body (G, Actual_Destination) = Expected_Destination);

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause and pragma Elaborate for unit $");

         Error_Msg_Unit_1 := Name (G, Expected_Destination);
         Error_Msg_Info
           ("    unit $ is in the closure of pragma Elaborate");
      end if;
   end Output_Elaborate_Transition;

   -------------------------------
   -- Output_Forced_Suggestions --
   -------------------------------

   procedure Output_Forced_Suggestions
     (G    : Library_Graph;
      Pred : Library_Graph_Vertex_Id;
      Succ : Library_Graph_Vertex_Id)
   is
   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Pred));
      pragma Assert (Present (Succ));

      Error_Msg_Unit_1 := Name (G, Succ);
      Error_Msg_Unit_2 := Name (G, Pred);
      Error_Msg_Info
        ("    remove the dependency of unit $ on unit $ from the argument of "
         & "switch -f");
      Error_Msg_Info
        ("    remove switch -f");
   end Output_Forced_Suggestions;

   ------------------------------
   -- Output_Forced_Transition --
   ------------------------------

   procedure Output_Forced_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id;
      Elaborate_All_Active : Boolean)
   is
   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Source));
      pragma Assert (Present (Actual_Destination));
      pragma Assert (Present (Expected_Destination));

      --  The actual and expected destination vertices match
      --
      --            Forced   Actual_Destination
      --    Source --------> spec -->
      --                     Expected_Destination
      --
      --            Forced   Actual_Destination
      --    Source --------> body -->
      --                     Expected_Destination

      if Actual_Destination = Expected_Destination then
         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has a dependency on unit $ forced by -f switch");

      --  The actual destination vertex denotes the spec of a unit while the
      --  expected destination is the corresponding body, and the unit is in
      --  the closure of an earlier Elaborate_All pragma.
      --
      --            Forced   Actual_Destination
      --    Source --------> spec
      --
      --                     body -->
      --                     Expected_Destination

      elsif Elaborate_All_Active then
         pragma Assert (Is_Spec_With_Body (G, Actual_Destination));
         pragma Assert (Is_Body_With_Spec (G, Expected_Destination));
         pragma Assert
           (Proper_Body (G, Actual_Destination) = Expected_Destination);

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has a dependency on unit $ forced by -f switch");

         Error_Msg_Unit_1 := Name (G, Expected_Destination);
         Error_Msg_Info
           ("    unit $ is in the closure of pragma Elaborate_All");

      --  Otherwise the actual destination vertex denotes a spec subject to
      --  pragma Elaborate_Body while the expected destination denotes the
      --  corresponding body.
      --
      --            Forced   Actual_Destination
      --    Source --------> spec Elaborate_Body
      --
      --                     body -->
      --                     Expected_Destination

      else
         pragma Assert
           (Is_Elaborate_Body_Pair
             (G           => G,
              Spec_Vertex => Actual_Destination,
              Body_Vertex => Expected_Destination));

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has a dependency on unit $ forced by -f switch");

         Error_Msg_Unit_1 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ is subject to pragma Elaborate_Body");

         Error_Msg_Unit_1 := Name (G, Expected_Destination);
         Error_Msg_Info
           ("    unit $ is in the closure of pragma Elaborate_Body");
      end if;
   end Output_Forced_Transition;

   --------------------------------------
   -- Output_Full_Encoding_Suggestions --
   --------------------------------------

   procedure Output_Full_Encoding_Suggestions
     (G          : Library_Graph;
      Cycle      : Library_Graph_Cycle_Id;
      First_Edge : Library_Graph_Edge_Id)
   is
      Succ : Library_Graph_Vertex_Id;

   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Cycle));
      pragma Assert (Present (First_Edge));

      if Is_Invocation_Edge (G, First_Edge) then
         Succ := Successor (G, First_Edge);

         if Invocation_Graph_Encoding (G, Succ) /= Full_Path_Encoding then
            Error_Msg_Info
              ("    use detailed invocation information (compiler switch "
               & "-gnatd_F)");
         end if;
      end if;
   end Output_Full_Encoding_Suggestions;

   ----------------------------
   -- Output_Invocation_Path --
   -----------------------------

   procedure Output_Invocation_Path
     (Inv_Graph         : Invocation_Graph;
      Elaborated_Vertex : Library_Graph_Vertex_Id;
      Path              : IGE_Lists.Doubly_Linked_List;
      Path_Id           : in out Nat)
   is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

      Edge : Invocation_Graph_Edge_Id;
      Iter : IGE_Lists.Iterator;

   begin
      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));
      pragma Assert (Present (Elaborated_Vertex));
      pragma Assert (IGE_Lists.Present (Path));

      Error_Msg_Nat_1 := Path_Id;
      Error_Msg_Info ("      path #:");

      Error_Msg_Unit_1 := Name (Lib_Graph, Elaborated_Vertex);
      Error_Msg_Info ("        elaboration of unit $");

      Iter := IGE_Lists.Iterate (Path);
      while IGE_Lists.Has_Next (Iter) loop
         IGE_Lists.Next (Iter, Edge);

         Output_Invocation_Path_Transition
           (Inv_Graph => Inv_Graph, Edge => Edge);
      end loop;

      Path_Id := Path_Id + 1;
   end Output_Invocation_Path;

   ---------------------------------------
   -- Output_Invocation_Path_Transition --
   ---------------------------------------

   procedure Output_Invocation_Path_Transition
     (Inv_Graph : Invocation_Graph;
      Edge      : Invocation_Graph_Edge_Id)
   is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));
      pragma Assert (Present (Edge));

      Declared : constant String := "declared at {:#:#";

      Targ        : constant Invocation_Graph_Vertex_Id :=
                      Target (Inv_Graph, Edge);
      Targ_Extra  : constant Name_Id                    :=
                      Extra (Inv_Graph, Edge);
      Targ_Vertex : constant Library_Graph_Vertex_Id    :=
                      Spec_Vertex (Inv_Graph, Targ);

   begin
      Error_Msg_Name_1 := Name   (Inv_Graph, Targ);
      Error_Msg_Nat_1  := Line   (Inv_Graph, Targ);
      Error_Msg_Nat_2  := Column (Inv_Graph, Targ);
      Error_Msg_File_1 := File_Name (Lib_Graph, Targ_Vertex);

      case Kind (Inv_Graph, Edge) is
         when Accept_Alternative =>
            Error_Msg_Info
              ("        selection of entry % "
               & Declared);

         when Access_Taken =>
            Error_Msg_Info
              ("        aliasing of subprogram % "
               & Declared);

         when Call =>
            Error_Msg_Info
              ("        call to subprogram % "
               & Declared);

         when Controlled_Adjustment
            | Internal_Controlled_Adjustment
         =>
            Error_Msg_Name_1 := Targ_Extra;
            Error_Msg_Info
              ("        adjustment actions for type % "
               & Declared);

         when Controlled_Finalization
            | Internal_Controlled_Finalization
         =>
            Error_Msg_Name_1 := Targ_Extra;
            Error_Msg_Info
              ("        finalization actions for type % "
               & Declared);

         when Controlled_Initialization
            | Internal_Controlled_Initialization
            | Type_Initialization
         =>
            Error_Msg_Name_1 := Targ_Extra;
            Error_Msg_Info
              ("        initialization actions for type % "
               & Declared);

         when Default_Initial_Condition_Verification =>
            Error_Msg_Name_1 := Targ_Extra;
            Error_Msg_Info
              ("        verification of Default_Initial_Condition for type % "
               & Declared);

         when Initial_Condition_Verification =>
            Error_Msg_Info
              ("        verification of Initial_Condition "
               & Declared);

         when Instantiation =>
            Error_Msg_Info
              ("        instantiation % "
               & Declared);

         when Invariant_Verification =>
            Error_Msg_Name_1 := Targ_Extra;
            Error_Msg_Info
              ("        verification of invariant for type % "
               & Declared);

         when Postcondition_Verification =>
            Error_Msg_Name_1 := Targ_Extra;
            Error_Msg_Info
              ("        verification of postcondition for subprogram % "
               & Declared);

         when Protected_Entry_Call =>
            Error_Msg_Info
              ("        call to protected entry % "
               & Declared);

         when Protected_Subprogram_Call =>
            Error_Msg_Info
              ("        call to protected subprogram % "
               & Declared);

         when Task_Activation =>
            Error_Msg_Info
              ("        activation of local task "
               & Declared);

         when Task_Entry_Call =>
            Error_Msg_Info
              ("        call to task entry % "
               & Declared);

         when others =>
            pragma Assert (False);
            null;
      end case;
   end Output_Invocation_Path_Transition;

   -------------------------------------------
   -- Output_Invocation_Related_Suggestions --
   -------------------------------------------

   procedure Output_Invocation_Related_Suggestions
     (G     : Library_Graph;
      Cycle : Library_Graph_Cycle_Id)
   is
   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Cycle));

      --  Nothing to do when the cycle does not contain an invocation edge

      if Invocation_Edge_Count (G, Cycle) = 0 then
         return;
      end if;

      --  The cycle contains at least one invocation edge, where at least
      --  one of the paths the edge represents activates a task. The use of
      --  restriction No_Entry_Calls_In_Elaboration_Code may halt the flow
      --  within the task body on a select or accept statement, eliminating
      --  subsequent invocation edges, thus breaking the cycle.

      if not Cumulative_Restrictions.Set (No_Entry_Calls_In_Elaboration_Code)
        and then Contains_Task_Activation (G, Cycle)
      then
         Error_Msg_Info
           ("    use pragma Restrictions "
            & "(No_Entry_Calls_In_Elaboration_Code)");
      end if;

      --  The cycle contains at least one invocation edge where the successor
      --  was statically elaborated. The use of the dynamic model may remove
      --  one of the invocation edges in the cycle, thus breaking the cycle.

      if Contains_Static_Successor_Edge (G, Cycle) then
         Error_Msg_Info
           ("    use the dynamic elaboration model (compiler switch -gnatE)");
      end if;
   end Output_Invocation_Related_Suggestions;

   ----------------------------------
   -- Output_Invocation_Transition --
   ----------------------------------

   procedure Output_Invocation_Transition
     (Inv_Graph   : Invocation_Graph;
      Source      : Library_Graph_Vertex_Id;
      Destination : Library_Graph_Vertex_Id)
   is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);
   begin
      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));
      pragma Assert (Present (Source));
      pragma Assert (Present (Destination));

      Error_Msg_Unit_1 := Name (Lib_Graph, Source);
      Error_Msg_Unit_2 := Name (Lib_Graph, Destination);
      Error_Msg_Info
        ("    unit $ invokes a construct of unit $ at elaboration time");

      Find_And_Output_Invocation_Paths
        (Inv_Graph   => Inv_Graph,
         Source      => Source,
         Destination => Destination);
   end Output_Invocation_Transition;

   ------------------------------------------
   -- Output_Reason_And_Circularity_Header --
   ------------------------------------------

   procedure Output_Reason_And_Circularity_Header
     (G          : Library_Graph;
      First_Edge : Library_Graph_Edge_Id)
   is
      pragma Assert (Present (G));
      pragma Assert (Present (First_Edge));

      Succ : constant Library_Graph_Vertex_Id := Successor (G, First_Edge);

   begin
      Error_Msg_Unit_1 := Name (G, Succ);
      Error_Msg      ("Elaboration circularity detected");
      Error_Msg_Info ("");
      Error_Msg_Info ("  Reason:");
      Error_Msg_Info ("");
      Error_Msg_Info ("    unit $ depends on its own elaboration");
      Error_Msg_Info ("");
      Error_Msg_Info ("  Circularity:");
      Error_Msg_Info ("");
   end Output_Reason_And_Circularity_Header;

   ------------------------
   -- Output_Suggestions --
   ------------------------

   procedure Output_Suggestions
     (G          : Library_Graph;
      Cycle      : Library_Graph_Cycle_Id;
      First_Edge : Library_Graph_Edge_Id)
   is
      pragma Assert (Present (G));
      pragma Assert (Present (Cycle));
      pragma Assert (Present (First_Edge));

      Pred : constant Library_Graph_Vertex_Id := Predecessor (G, First_Edge);
      Succ : constant Library_Graph_Vertex_Id := Successor   (G, First_Edge);

   begin
      Error_Msg_Info ("");
      Error_Msg_Info ("  Suggestions:");
      Error_Msg_Info ("");

      --  Output edge-specific suggestions

      if Is_Elaborate_All_Edge (G, First_Edge) then
         Output_Elaborate_All_Suggestions
           (G    => G,
            Pred => Pred,
            Succ => Succ);

      elsif Is_Elaborate_Body_Edge (G, First_Edge) then
         Output_Elaborate_Body_Suggestions
           (G    => G,
            Succ => Succ);

      elsif Is_Elaborate_Edge (G, First_Edge) then
         Output_Elaborate_Suggestions
           (G    => G,
            Pred => Pred,
            Succ => Succ);

      elsif Is_Forced_Edge (G, First_Edge) then
         Output_Forced_Suggestions
           (G    => G,
            Pred => Pred,
            Succ => Succ);
      end if;

      --  Output general purpose suggestions

      Output_Invocation_Related_Suggestions
        (G     => G,
         Cycle => Cycle);

      Output_Full_Encoding_Suggestions
        (G          => G,
         Cycle      => Cycle,
         First_Edge => First_Edge);

      Output_All_Cycles_Suggestions (G);

      Error_Msg_Info ("");
   end Output_Suggestions;

   -----------------------
   -- Output_Transition --
   -----------------------

   procedure Output_Transition
     (Inv_Graph            : Invocation_Graph;
      Current_Edge         : Library_Graph_Edge_Id;
      Next_Edge            : Library_Graph_Edge_Id;
      Elaborate_All_Active : Boolean)
   is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));
      pragma Assert (Present (Current_Edge));
      pragma Assert (Present (Next_Edge));

      Actual_Destination   : constant Library_Graph_Vertex_Id :=
                               Predecessor (Lib_Graph, Current_Edge);
      Expected_Destination : constant Library_Graph_Vertex_Id :=
                               Successor   (Lib_Graph, Next_Edge);
      Source               : constant Library_Graph_Vertex_Id :=
                               Successor   (Lib_Graph, Current_Edge);

   begin
      if Is_Elaborate_All_Edge (Lib_Graph, Current_Edge) then
         Output_Elaborate_All_Transition
           (G                    => Lib_Graph,
            Source               => Source,
            Actual_Destination   => Actual_Destination,
            Expected_Destination => Expected_Destination);

      elsif Is_Elaborate_Body_Edge (Lib_Graph, Current_Edge) then
         Output_Elaborate_Body_Transition
           (G                    => Lib_Graph,
            Source               => Source,
            Actual_Destination   => Actual_Destination,
            Expected_Destination => Expected_Destination,
            Elaborate_All_Active => Elaborate_All_Active);

      elsif Is_Elaborate_Edge (Lib_Graph, Current_Edge) then
         Output_Elaborate_Transition
           (G                    => Lib_Graph,
            Source               => Source,
            Actual_Destination   => Actual_Destination,
            Expected_Destination => Expected_Destination);

      elsif Is_Forced_Edge (Lib_Graph, Current_Edge) then
         Output_Forced_Transition
           (G                    => Lib_Graph,
            Source               => Source,
            Actual_Destination   => Actual_Destination,
            Expected_Destination => Expected_Destination,
            Elaborate_All_Active => Elaborate_All_Active);

      elsif Is_Invocation_Edge (Lib_Graph, Current_Edge) then
         Output_Invocation_Transition
           (Inv_Graph   => Inv_Graph,
            Source      => Source,
            Destination => Expected_Destination);

      else
         pragma Assert (Is_With_Edge (Lib_Graph, Current_Edge));

         Output_With_Transition
           (G                    => Lib_Graph,
            Source               => Source,
            Actual_Destination   => Actual_Destination,
            Expected_Destination => Expected_Destination,
            Elaborate_All_Active => Elaborate_All_Active);
      end if;
   end Output_Transition;

   ----------------------------
   -- Output_With_Transition --
   ----------------------------

   procedure Output_With_Transition
     (G                    : Library_Graph;
      Source               : Library_Graph_Vertex_Id;
      Actual_Destination   : Library_Graph_Vertex_Id;
      Expected_Destination : Library_Graph_Vertex_Id;
      Elaborate_All_Active : Boolean)
   is
   begin
      pragma Assert (Present (G));
      pragma Assert (Present (Source));
      pragma Assert (Present (Actual_Destination));
      pragma Assert (Present (Expected_Destination));

      --  The actual and expected destination vertices match, and denote the
      --  initial declaration of a unit.
      --
      --            with   Actual_Destination
      --    Source ------> spec -->
      --                   Expected_Destination
      --
      --            with   Actual_Destination
      --    Source ------> stand-alone body -->
      --                   Expected_Destination

      if Actual_Destination = Expected_Destination then
         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause for unit $");

      --  The actual destination vertex denotes the spec of a unit while the
      --  expected destination is the corresponding body, and the unit is in
      --  the closure of an earlier Elaborate_All pragma.
      --
      --            with   Actual_Destination
      --    Source ------> spec
      --
      --                   body -->
      --                   Expected_Destination

      elsif Elaborate_All_Active then
         pragma Assert (Is_Spec_With_Body (G, Actual_Destination));
         pragma Assert (Is_Body_With_Spec (G, Expected_Destination));
         pragma Assert
           (Proper_Body (G, Actual_Destination) = Expected_Destination);

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause for unit $");

         Error_Msg_Unit_1 := Name (G, Expected_Destination);
         Error_Msg_Info
           ("    unit $ is in the closure of pragma Elaborate_All");

      --  Otherwise the actual destination vertex denotes a spec subject to
      --  pragma Elaborate_Body while the expected destination denotes the
      --  corresponding body.
      --
      --            with   Actual_Destination
      --    Source ------> spec Elaborate_Body
      --
      --                   body -->
      --                   Expected_Destination

      else
         pragma Assert
           (Is_Elaborate_Body_Pair
             (G           => G,
              Spec_Vertex => Actual_Destination,
              Body_Vertex => Expected_Destination));

         Error_Msg_Unit_1 := Name (G, Source);
         Error_Msg_Unit_2 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ has with clause for unit $");

         Error_Msg_Unit_1 := Name (G, Actual_Destination);
         Error_Msg_Info
           ("    unit $ is subject to pragma Elaborate_Body");

         Error_Msg_Unit_1 := Name (G, Expected_Destination);
         Error_Msg_Info
           ("    unit $ is in the closure of pragma Elaborate_Body");
      end if;
   end Output_With_Transition;

   ------------------
   -- Visit_Vertex --
   ------------------

   procedure Visit_Vertex
     (Inv_Graph         : Invocation_Graph;
      Invoker           : Invocation_Graph_Vertex_Id;
      Invoker_Vertex    : Library_Graph_Vertex_Id;
      Last_Vertex       : Library_Graph_Vertex_Id;
      Elaborated_Vertex : Library_Graph_Vertex_Id;
      End_Vertex        : Library_Graph_Vertex_Id;
      Visited_Invokers  : IGV_Sets.Membership_Set;
      Path              : IGE_Lists.Doubly_Linked_List;
      Path_Id           : in out Nat)
   is
      Lib_Graph : constant Library_Graph := Get_Lib_Graph (Inv_Graph);

      Edge : Invocation_Graph_Edge_Id;
      Iter : Edges_To_Targets_Iterator;
      Targ : Invocation_Graph_Vertex_Id;

   begin
      pragma Assert (Present (Inv_Graph));
      pragma Assert (Present (Lib_Graph));
      pragma Assert (Present (Invoker));
      pragma Assert (Present (Invoker_Vertex));
      pragma Assert (Present (Last_Vertex));
      pragma Assert (Present (Elaborated_Vertex));
      pragma Assert (Present (End_Vertex));
      pragma Assert (IGV_Sets.Present (Visited_Invokers));
      pragma Assert (IGE_Lists.Present (Path));

      --  The current invocation vertex resides within the end library vertex.
      --  Emit the path that started from some elaboration root and ultimately
      --  reached the desired library vertex.

      if Body_Vertex (Inv_Graph, Invoker) = End_Vertex
        and then Invoker_Vertex /= Last_Vertex
      then
         Output_Invocation_Path
           (Inv_Graph         => Inv_Graph,
            Elaborated_Vertex => Elaborated_Vertex,
            Path              => Path,
            Path_Id           => Path_Id);

      --  Otherwise extend the search for the end library vertex via all edges
      --  to targets.

      elsif not IGV_Sets.Contains (Visited_Invokers, Invoker) then

         --  Prepare for invoker backtracking

         IGV_Sets.Insert (Visited_Invokers, Invoker);

         --  Extend the search via all edges to targets

         Iter := Iterate_Edges_To_Targets (Inv_Graph, Invoker);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            --  Prepare for edge backtracking

            IGE_Lists.Append (Path, Edge);

            --  The traversal proceeds through the library vertex that houses
            --  the body of the target.

            Targ := Target (Inv_Graph, Edge);

            Visit_Vertex
              (Inv_Graph         => Inv_Graph,
               Invoker           => Targ,
               Invoker_Vertex    => Body_Vertex (Inv_Graph, Targ),
               Last_Vertex       => Invoker_Vertex,
               Elaborated_Vertex => Elaborated_Vertex,
               End_Vertex        => End_Vertex,
               Visited_Invokers  => Visited_Invokers,
               Path              => Path,
               Path_Id           => Path_Id);

            --  Backtrack the edge

            IGE_Lists.Delete_Last (Path);
         end loop;

         --  Backtrack the invoker

         IGV_Sets.Delete (Visited_Invokers, Invoker);
      end if;
   end Visit_Vertex;

end Bindo.Diagnostics;
