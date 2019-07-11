------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     B I N D O . E L A B O R A T O R S                    --
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

with Butil;  use Butil;
with Debug;  use Debug;
with Output; use Output;
with Types;  use Types;

with Bindo.Augmentors;
use  Bindo.Augmentors;
use  Bindo.Augmentors.Library_Graph_Augmentors;

with Bindo.Builders;
use  Bindo.Builders;
use  Bindo.Builders.Invocation_Graph_Builders;
use  Bindo.Builders.Library_Graph_Builders;

with Bindo.Diagnostics;
use  Bindo.Diagnostics;

with Bindo.Units;
use  Bindo.Units;

with Bindo.Validators;
use  Bindo.Validators;
use  Bindo.Validators.Elaboration_Order_Validators;

with Bindo.Writers;
use  Bindo.Writers;
use  Bindo.Writers.ALI_Writers;
use  Bindo.Writers.Dependency_Writers;
use  Bindo.Writers.Elaboration_Order_Writers;
use  Bindo.Writers.Invocation_Graph_Writers;
use  Bindo.Writers.Library_Graph_Writers;
use  Bindo.Writers.Phase_Writers;
use  Bindo.Writers.Unit_Closure_Writers;

with GNAT;        use GNAT;
with GNAT.Graphs; use GNAT.Graphs;

package body Bindo.Elaborators is

   --  The following type defines the advancement of the elaboration order
   --  algorithm in terms of steps.

   type Elaboration_Order_Step is new Natural;

   Initial_Step : constant Elaboration_Order_Step :=
                    Elaboration_Order_Step'First;

   ----------------------------------------------
   -- Invocation_And_Library_Graph_Elaborators --
   ----------------------------------------------

   package body Invocation_And_Library_Graph_Elaborators is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Create_Component_Vertex_Sets
        (G                   : Library_Graph;
         Comp                : Component_Id;
         Elaborable_Vertices : out LGV_Sets.Membership_Set;
         Waiting_Vertices    : out LGV_Sets.Membership_Set;
         Step                : Elaboration_Order_Step);
      pragma Inline (Create_Component_Vertex_Sets);
      --  Split all vertices of component Comp of library graph G as follows:
      --
      --    * Elaborable vertices are added to set Elaborable_Vertices.
      --
      --    * Vertices that are still waiting on their predecessors to be
      --      elaborated are added to set Waiting_Vertices.
      --
      --  Step is the current step in the elaboration order.

      procedure Create_Vertex_Sets
        (G                   : Library_Graph;
         Elaborable_Vertices : out LGV_Sets.Membership_Set;
         Waiting_Vertices    : out LGV_Sets.Membership_Set;
         Step                : Elaboration_Order_Step);
      pragma Inline (Create_Vertex_Sets);
      --  Split all vertices of library graph G as follows:
      --
      --    * Elaborable vertices are added to set Elaborable_Vertices.
      --
      --    * Vertices that are still waiting on their predecessors to be
      --      elaborated are added to set Waiting_Vertices.
      --
      --  Step is the current step in the elaboration order.

      procedure Elaborate_Component
        (G                       : Library_Graph;
         Comp                    : Component_Id;
         All_Elaborable_Vertices : LGV_Sets.Membership_Set;
         All_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Order                   : in out Unit_Id_Table;
         Step                    : Elaboration_Order_Step);
      pragma Inline (Elaborate_Component);
      --  Elaborate as many vertices as possible that appear in component Comp
      --  of library graph G. The sets contain vertices arranged as follows:
      --
      --    * All_Elaborable_Vertices - all elaborable vertices in the library
      --      graph.
      --
      --    * All_Waiting_Vertices - all vertices in the library graph that are
      --      waiting on predecessors to be elaborated.
      --
      --  Order is the elaboration order. Step denotes the current step in the
      --  elaboration order.

      procedure Elaborate_Library_Graph
        (G      : Library_Graph;
         Order  : out Unit_Id_Table;
         Status : out Elaboration_Order_Status);
      pragma Inline (Elaborate_Library_Graph);
      --  Elaborate as many vertices as possible of library graph G. Order is
      --  the elaboration order. Status is the condition of the elaboration
      --  order.

      procedure Elaborate_Vertex
        (G                        : Library_Graph;
         Vertex                   : Library_Graph_Vertex_Id;
         All_Elaborable_Vertices  : LGV_Sets.Membership_Set;
         All_Waiting_Vertices     : LGV_Sets.Membership_Set;
         Comp_Elaborable_Vertices : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Order                    : in out Unit_Id_Table;
         Step                     : Elaboration_Order_Step;
         Indent                   : Indentation_Level);
      pragma Inline (Elaborate_Vertex);
      --  Elaborate vertex Vertex of library graph G by adding its unit to
      --  elaboration order Order. The routine updates awaiting successors
      --  where applicable. The sets contain vertices arranged as follows:
      --
      --    * All_Elaborable_Vertices - all elaborable vertices in the library
      --      graph.
      --
      --    * All_Waiting_Vertices - all vertices in the library graph that are
      --      waiting on predecessors to be elaborated.
      --
      --    * Comp_Elaborable_Vertices - all elaborable vertices found in the
      --      component of Vertex.
      --
      --    * Comp_Waiting_Vertices - all vertices found in the component of
      --      Vertex that are still waiting on predecessors to be elaborated.
      --
      --  Order denotes the elaboration order. Step is the current step in the
      --  elaboration order. Indent denotes the desired indentation level for
      --  tracing.

      function Find_Best_Elaborable_Vertex
        (G      : Library_Graph;
         Set    : LGV_Sets.Membership_Set;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level) return Library_Graph_Vertex_Id;
      pragma Inline (Find_Best_Elaborable_Vertex);
      --  Find the best vertex of library graph G from membership set S that
      --  can be elaborated. Step is the current step in the elaboration order.
      --  Indent is the desired indentation level for tracing.

      function Find_Best_Vertex
        (G                   : Library_Graph;
         Set                 : LGV_Sets.Membership_Set;
         Is_Suitable_Vertex  : LGV_Predicate_Ptr;
         Compare_Vertices    : LGV_Comparator_Ptr;
         Initial_Best_Msg    : String;
         Subsequent_Best_Msg : String;
         Step                : Elaboration_Order_Step;
         Indent              : Indentation_Level)
         return Library_Graph_Vertex_Id;
      pragma Inline (Find_Best_Vertex);
      --  Find the best vertex of library graph G from membership set S which
      --  satisfies predicate Is_Suitable_Vertex and is preferred by comparator
      --  Compare_Vertices. Initial_Best_Msg is emitted on the first candidate
      --  vertex. Subsequent_Best_Msg is emitted whenever a better vertex is
      --  discovered. Step is the current step in the elaboration order. Indent
      --  is the desired indentation level for tracing.

      function Find_Best_Weakly_Elaborable_Vertex
        (G      : Library_Graph;
         Set    : LGV_Sets.Membership_Set;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level) return Library_Graph_Vertex_Id;
      pragma Inline (Find_Best_Weakly_Elaborable_Vertex);
      --  Find the best vertex of library graph G from membership set S that
      --  can be weakly elaborated. Step is the current step in the elaboration
      --  order. Indent is the desired indentation level for tracing.

      function Has_Elaborable_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Has_Elaborable_Body);
      --  Determine whether vertex Vertex of library graph G has a body that is
      --  elaborable. It is assumed that the vertex has been elaborated.

      procedure Insert_Elaborable_Successor
        (G                     : Library_Graph;
         Vertex                : Library_Graph_Vertex_Id;
         Elaborable_Vertices   : LGV_Sets.Membership_Set;
         All_Waiting_Vertices  : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices : LGV_Sets.Membership_Set;
         Msg                   : String;
         Step                  : Elaboration_Order_Step;
         Indent                : Indentation_Level);
      pragma Inline (Insert_Elaborable_Successor);
      --  Add elaborable successor Vertex of library graph G to membership set
      --  Elaborable_Vertices and remove it from both All_Waiting_Vertices and
      --  Comp_Waiting_Vertices. Msg is a message emitted for tracing purposes.
      --  Step is the current step in the elaboration order. Indent denotes the
      --  desired indentation level for tracing.

      procedure Insert_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Set    : LGV_Sets.Membership_Set;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level);
      pragma Inline (Insert_Vertex);
      --  Add vertex Vertex of library graph G to membership set Set. Msg is
      --  a message emitted for tracing purposes. Step is the current step in
      --  the elaboration order. Indent is the desired indentation level for
      --  tracing.

      function Is_Better_Elaborable_Vertex
        (G           : Library_Graph;
         Vertex      : Library_Graph_Vertex_Id;
         Compared_To : Library_Graph_Vertex_Id) return Precedence_Kind;
      pragma Inline (Is_Better_Elaborable_Vertex);
      --  Determine whether vertex Vertex of library graph G is a better choice
      --  for elaboration compared to vertex Compared_To.

      function Is_Better_Weakly_Elaborable_Vertex
        (G           : Library_Graph;
         Vertex      : Library_Graph_Vertex_Id;
         Compared_To : Library_Graph_Vertex_Id) return Precedence_Kind;
      pragma Inline (Is_Better_Weakly_Elaborable_Vertex);
      --  Determine whether vertex Vertex of library graph G is a better choice
      --  for weak elaboration compared to vertex Compared_To.

      function Is_Suitable_Elaborable_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Suitable_Elaborable_Vertex);
      --  Determine whether vertex Vertex of library graph G is suitable for
      --  elaboration.

      function Is_Suitable_Weakly_Elaborable_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Suitable_Weakly_Elaborable_Vertex);
      --  Determine whether vertex Vertex of library graph G is suitable for
      --  weak elaboration.

      procedure Set_Unit_Elaboration_Positions (Order : Unit_Id_Table);
      pragma Inline (Set_Unit_Elaboration_Positions);
      --  Set the ALI.Units positions of all elaboration units in order Order

      procedure Trace_Component
        (G    : Library_Graph;
         Comp : Component_Id;
         Msg  : String;
         Step : Elaboration_Order_Step);
      pragma Inline (Trace_Component);
      --  Write elaboration-related information for component Comp of library
      --  graph G to standard output, starting with message Msg. Step is the
      --  current step in the elaboration order.

      procedure Trace_Step (Step : Elaboration_Order_Step);
      pragma Inline (Trace_Step);
      --  Write current step Step of the elaboration order to standard output

      procedure Trace_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level);
      pragma Inline (Trace_Vertex);
      --  Write elaboration-related information for vertex Vertex of library
      --  graph G to standard output, starting with message Msg. Step is the
      --  current step in the elaboration order. Indent denotes the desired
      --  indentation level for tracing.

      procedure Trace_Vertices
        (G          : Library_Graph;
         Set        : LGV_Sets.Membership_Set;
         Set_Msg    : String;
         Vertex_Msg : String;
         Step       : Elaboration_Order_Step;
         Indent     : Indentation_Level);
      pragma Inline (Trace_Vertices);
      --  Write the candidate vertices of library graph G present in membership
      --  set Set to standard output, starting with message Set_Msg. Vertex_Msg
      --  is the message emitted prior to each vertex. Step denotes the current
      --  step in the elaboration order. Indent denotes the desired indentation
      --  level for tracing.

      procedure Update_Successor
        (G                        : Library_Graph;
         Edge                     : Library_Graph_Edge_Id;
         All_Elaborable_Vertices  : LGV_Sets.Membership_Set;
         All_Waiting_Vertices     : LGV_Sets.Membership_Set;
         Comp_Elaborable_Vertices : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Step                     : Elaboration_Order_Step;
         Indent                   : Indentation_Level);
      pragma Inline (Update_Successor);
      --  Notify the successor of edge Edge of library graph G along with its
      --  component that their predecessor has just been elaborated. This may
      --  cause new vertices to become elaborable. The sets contain vertices
      --  arranged as follows:
      --
      --    * All_Elaborable_Vertices - all elaborable vertices in the library
      --      graph.
      --
      --    * All_Waiting_Vertices - all vertices in the library graph that are
      --      waiting on predecessors to be elaborated.
      --
      --    * Comp_Elaborable_Vertices - all elaborable vertices found in the
      --      component of Vertex.
      --
      --    * Comp_Waiting_Vertices - all vertices found in the component of
      --      Vertex that are still waiting on predecessors to be elaborated.
      --
      --  Step is the current step in the elaboration order. Indent denotes the
      --  desired indentation level for tracing.

      procedure Update_Successors
        (G                        : Library_Graph;
         Vertex                   : Library_Graph_Vertex_Id;
         All_Elaborable_Vertices  : LGV_Sets.Membership_Set;
         All_Waiting_Vertices     : LGV_Sets.Membership_Set;
         Comp_Elaborable_Vertices : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Step                     : Elaboration_Order_Step;
         Indent                   : Indentation_Level);
      pragma Inline (Update_Successors);
      --  Notify all successors of vertex Vertex of library graph G along with
      --  their components that their predecessor has just been elaborated.
      --  This may cause new vertices to become elaborable. The sets contain
      --  vertices arranged as follows:
      --
      --    * All_Elaborable_Vertices - all elaborable vertices in the library
      --      graph.
      --
      --    * All_Waiting_Vertices - all vertices in the library graph that are
      --      waiting on predecessors to be elaborated.
      --
      --    * Comp_Elaborable_Vertices - all elaborable vertices found in the
      --      component of Vertex.
      --
      --    * Comp_Waiting_Vertices - all vertices found in the component of
      --      Vertex that are still waiting on predecessors to be elaborated.
      --
      --  Step is the current step in the elaboration order. Indent denotes the
      --  desired indentation level for tracing.

      ----------------------------------
      -- Create_Component_Vertex_Sets --
      ----------------------------------

      procedure Create_Component_Vertex_Sets
        (G                   : Library_Graph;
         Comp                : Component_Id;
         Elaborable_Vertices : out LGV_Sets.Membership_Set;
         Waiting_Vertices    : out LGV_Sets.Membership_Set;
         Step                : Elaboration_Order_Step)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Num_Of_Vertices : constant Natural :=
                             Number_Of_Component_Vertices (G, Comp);

         Iter   : Component_Vertex_Iterator;
         Vertex : Library_Graph_Vertex_Id;

      begin
         Elaborable_Vertices := LGV_Sets.Create (Num_Of_Vertices);
         Waiting_Vertices    := LGV_Sets.Create (Num_Of_Vertices);

         Iter := Iterate_Component_Vertices (G, Comp);
         while Has_Next (Iter) loop
            Next (Iter, Vertex);

            --  Add the vertex to the proper set depending on whether it can be
            --  elaborated.

            if Is_Elaborable_Vertex (G, Vertex) then
               Insert_Vertex
                 (G      => G,
                  Vertex => Vertex,
                  Set    => Elaborable_Vertices,
                  Msg    => "add elaborable component vertex",
                  Step   => Step,
                  Indent => No_Indentation);

            else
               Insert_Vertex
                 (G      => G,
                  Vertex => Vertex,
                  Set    => Waiting_Vertices,
                  Msg    => "add waiting component vertex",
                  Step   => Step,
                  Indent => No_Indentation);
            end if;
         end loop;
      end Create_Component_Vertex_Sets;

      ------------------------
      -- Create_Vertex_Sets --
      ------------------------

      procedure Create_Vertex_Sets
        (G                   : Library_Graph;
         Elaborable_Vertices : out LGV_Sets.Membership_Set;
         Waiting_Vertices    : out LGV_Sets.Membership_Set;
         Step                : Elaboration_Order_Step)
      is
         pragma Assert (Present (G));

         Num_Of_Vertices : constant Natural := Number_Of_Vertices (G);

         Iter   : Library_Graphs.All_Vertex_Iterator;
         Vertex : Library_Graph_Vertex_Id;

      begin
         Elaborable_Vertices := LGV_Sets.Create (Num_Of_Vertices);
         Waiting_Vertices    := LGV_Sets.Create (Num_Of_Vertices);

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, Vertex);

            --  Add the vertex to the proper set depending on whether it can be
            --  elaborated.

            if Is_Elaborable_Vertex (G, Vertex) then
               Insert_Vertex
                 (G      => G,
                  Vertex => Vertex,
                  Set    => Elaborable_Vertices,
                  Msg    => "add elaborable vertex",
                  Step   => Step,
                  Indent => No_Indentation);

            else
               Insert_Vertex
                 (G      => G,
                  Vertex => Vertex,
                  Set    => Waiting_Vertices,
                  Msg    => "add waiting vertex",
                  Step   => Step,
                  Indent => No_Indentation);
            end if;
         end loop;
      end Create_Vertex_Sets;

      -------------------------
      -- Elaborate_Component --
      -------------------------

      procedure Elaborate_Component
        (G                       : Library_Graph;
         Comp                    : Component_Id;
         All_Elaborable_Vertices : LGV_Sets.Membership_Set;
         All_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Order                   : in out Unit_Id_Table;
         Step                    : Elaboration_Order_Step)
      is
         Comp_Elaborable_Vertices : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Vertex                   : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));
         pragma Assert (LGV_Sets.Present (All_Elaborable_Vertices));
         pragma Assert (LGV_Sets.Present (All_Waiting_Vertices));

         Trace_Component
           (G    => G,
            Comp => Comp,
            Msg  => "elaborating component",
            Step => Step);

         --  Divide all vertices of the component into an elaborable and
         --  waiting vertex set.

         Create_Component_Vertex_Sets
           (G                   => G,
            Comp                => Comp,
            Elaborable_Vertices => Comp_Elaborable_Vertices,
            Waiting_Vertices    => Comp_Waiting_Vertices,
            Step                => Step);

         loop
            Trace_Vertices
              (G          => G,
               Set        => Comp_Elaborable_Vertices,
               Set_Msg    => "elaborable component vertices",
               Vertex_Msg => "elaborable component vertex",
               Step       => Step,
               Indent     => Nested_Indentation);

            Trace_Vertices
              (G          => G,
               Set        => Comp_Waiting_Vertices,
               Set_Msg    => "waiting component vertices",
               Vertex_Msg => "waiting component vertex",
               Step       => Step,
               Indent     => Nested_Indentation);

            Vertex :=
              Find_Best_Elaborable_Vertex
                (G      => G,
                 Set    => Comp_Elaborable_Vertices,
                 Step   => Step,
                 Indent => Nested_Indentation);

            --  The component lacks an elaborable vertex. This indicates that
            --  either all vertices of the component have been elaborated or
            --  the graph has a circularity. Locate the best weak vertex that
            --  was compiled with the dynamic model to elaborate from the set
            --  waiting vertices. This action assumes that certain invocations
            --  will not take place at elaboration time. An order produced in
            --  this fashion may fail an ABE check at run time.

            if not Present (Vertex) then
               Vertex :=
                 Find_Best_Weakly_Elaborable_Vertex
                   (G      => G,
                    Set    => Comp_Waiting_Vertices,
                    Step   => Step,
                    Indent => Nested_Indentation);
            end if;

            --  Stop the elaboration when either all vertices of the component
            --  have been elaborated, or the graph contains a circularity.

            exit when not Present (Vertex);

            --  Try to elaborate as many vertices within the component as
            --  possible. Each successful elaboration signals the appropriate
            --  successors and components that they have one less predecessor
            --  to wait on.

            Elaborate_Vertex
              (G                        => G,
               Vertex                   => Vertex,
               All_Elaborable_Vertices  => All_Elaborable_Vertices,
               All_Waiting_Vertices     => All_Waiting_Vertices,
               Comp_Elaborable_Vertices => Comp_Elaborable_Vertices,
               Comp_Waiting_Vertices    => Comp_Waiting_Vertices,
               Order                    => Order,
               Step                     => Step,
               Indent                   => Nested_Indentation);
         end loop;

         LGV_Sets.Destroy (Comp_Elaborable_Vertices);
         LGV_Sets.Destroy (Comp_Waiting_Vertices);
      end Elaborate_Component;

      -----------------------------
      -- Elaborate_Library_Graph --
      -----------------------------

      procedure Elaborate_Library_Graph
        (G      : Library_Graph;
         Order  : out Unit_Id_Table;
         Status : out Elaboration_Order_Status)
      is
         Elaborable_Vertices : LGV_Sets.Membership_Set;
         Step                : Elaboration_Order_Step;
         Vertex              : Library_Graph_Vertex_Id;
         Waiting_Vertices    : LGV_Sets.Membership_Set;

      begin
         pragma Assert (Present (G));

         Step := Initial_Step;

         --  Divide all vertices of the library graph into an elaborable and
         --  waiting vertex set.

         Create_Vertex_Sets
           (G                   => G,
            Elaborable_Vertices => Elaborable_Vertices,
            Waiting_Vertices    => Waiting_Vertices,
            Step                => Step);

         loop
            Step := Step + 1;

            Trace_Vertices
              (G          => G,
               Set        => Elaborable_Vertices,
               Set_Msg    => "elaborable vertices",
               Vertex_Msg => "elaborable vertex",
               Step       => Step,
               Indent     => No_Indentation);

            Trace_Vertices
              (G          => G,
               Set        => Waiting_Vertices,
               Set_Msg    => "waiting vertices",
               Vertex_Msg => "waiting vertex",
               Step       => Step,
               Indent     => No_Indentation);

            Vertex :=
              Find_Best_Elaborable_Vertex
                (G      => G,
                 Set    => Elaborable_Vertices,
                 Step   => Step,
                 Indent => No_Indentation);

            --  The graph lacks an elaborable vertex. This indicates that
            --  either all vertices have been elaborated or the graph has a
            --  circularity. Find the best weak vertex that was compiled with
            --  the dynamic model to elaborate from set of waiting vertices.
            --  This action assumes that certain invocations will not take
            --  place at elaboration time. An order produced in this fashion
            --  may fail an ABE check at run time.

            if not Present (Vertex) then
               Vertex :=
                 Find_Best_Weakly_Elaborable_Vertex
                   (G      => G,
                    Set    => Waiting_Vertices,
                    Step   => Step,
                    Indent => No_Indentation);
            end if;

            --  Stop the elaboration when either all vertices of the graph have
            --  been elaborated, or the graph contains a circularity.

            exit when not Present (Vertex);

            --  Elaborate the component of the vertex by trying to elaborate as
            --  many vertices within the component as possible. Each successful
            --  elaboration signals the appropriate successors and components
            --  that they have one less predecessor to wait on.

            Elaborate_Component
              (G                       => G,
               Comp                    => Component (G, Vertex),
               All_Elaborable_Vertices => Elaborable_Vertices,
               All_Waiting_Vertices    => Waiting_Vertices,
               Order                   => Order,
               Step                    => Step);
         end loop;

         --  The graph contains an Elaborate_All circularity when at least one
         --  edge subject to the related pragma appears in a component.

         if Has_Elaborate_All_Cycle (G) then
            Status := Order_Has_Elaborate_All_Circularity;

         --  The graph contains a circularity when at least one vertex failed
         --  to elaborate.

         elsif LGV_Sets.Size (Waiting_Vertices) /= 0 then
            Status := Order_Has_Circularity;

         --  Otherwise the elaboration order is satisfactory

         else
            Status := Order_OK;
         end if;

         LGV_Sets.Destroy (Elaborable_Vertices);
         LGV_Sets.Destroy (Waiting_Vertices);
      end Elaborate_Library_Graph;

      ---------------------
      -- Elaborate_Units --
      ---------------------

      procedure Elaborate_Units
        (Order         : out Unit_Id_Table;
         Main_Lib_File : File_Name_Type)
      is
         pragma Unreferenced (Main_Lib_File);

         Inv_Graph : Invocation_Graph;
         Lib_Graph : Library_Graph;
         Status    : Elaboration_Order_Status;

      begin
         Start_Phase (Unit_Elaboration);

         --  Initialize all unit-related data structures and gather all units
         --  that need elaboration.

         Initialize_Units;
         Collect_Elaborable_Units;

         --  Create the library graph that captures the dependencies between
         --  library items.

         Lib_Graph := Build_Library_Graph;

         --  Create the invocation graph that represents the flow of execution

         Inv_Graph := Build_Invocation_Graph (Lib_Graph);

         --  Traverse the invocation graph starting from elaboration code in
         --  order to discover transitions of the execution flow from a unit
         --  to a unit that result in extra edges within the library graph.

         Augment_Library_Graph (Inv_Graph, Lib_Graph);

         --  Create the component graph by collapsing all library items into
         --  library units and traversing the library graph.

         Find_Components (Lib_Graph);

         --  Output the contents of the ALI tables and both graphs to standard
         --  output now that they have been fully decorated.

         Write_ALI_Tables;
         Write_Invocation_Graph (Inv_Graph);
         Write_Library_Graph    (Lib_Graph);

         --  Traverse the library graph to determine the elaboration order of
         --  units.

         Elaborate_Library_Graph (Lib_Graph, Order, Status);

         --  The elaboration order is satisfactory

         if Status = Order_OK then
            Validate_Elaboration_Order (Order);

            --  Set attribute Elab_Position of table ALI.Units for all units in
            --  the elaboration order.

            Set_Unit_Elaboration_Positions (Order);

            --  Output the dependencies among units when switch -e (output
            --  complete list of elaboration order dependencies) is active.

            Write_Dependencies (Lib_Graph);

            --  Output the elaboration order when switch -l (output chosen
            --  elaboration order) is in effect.

            Write_Elaboration_Order (Order);

            --  Output the sources referenced in the closure of the order when
            --  switch -R (list sources referenced in closure) is in effect.

            Write_Unit_Closure (Order);

         --  Otherwise the library graph contains at least one circularity

         else
            Diagnose_Circularities (Inv_Graph, Lib_Graph);
         end if;

         Destroy (Inv_Graph);
         Destroy (Lib_Graph);

         --  Destroy all unit-related data structures

         Finalize_Units;
         End_Phase (Unit_Elaboration);

         --  Halt the bind when there is no satisfactory elaboration order

         if Status /= Order_OK then
            raise Unrecoverable_Error;
         end if;
      end Elaborate_Units;

      ----------------------
      -- Elaborate_Vertex --
      ----------------------

      procedure Elaborate_Vertex
        (G                        : Library_Graph;
         Vertex                   : Library_Graph_Vertex_Id;
         All_Elaborable_Vertices  : LGV_Sets.Membership_Set;
         All_Waiting_Vertices     : LGV_Sets.Membership_Set;
         Comp_Elaborable_Vertices : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Order                    : in out Unit_Id_Table;
         Step                     : Elaboration_Order_Step;
         Indent                   : Indentation_Level)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (Needs_Elaboration (G, Vertex));
         pragma Assert (LGV_Sets.Present (All_Elaborable_Vertices));
         pragma Assert (LGV_Sets.Present (All_Waiting_Vertices));
         pragma Assert (LGV_Sets.Present (Comp_Elaborable_Vertices));
         pragma Assert (LGV_Sets.Present (Comp_Waiting_Vertices));

         Trace_Vertex
           (G      => G,
            Vertex => Vertex,
            Msg    => "elaborating vertex",
            Step   => Step,
            Indent => Indent);

         --  Remove the vertex from both elaborable sets. This is needed when
         --  the vertex is both an overall best candidate among all vertices,
         --  and the best candidate within the component.

         LGV_Sets.Delete (All_Elaborable_Vertices,  Vertex);
         LGV_Sets.Delete (Comp_Elaborable_Vertices, Vertex);

         --  Remove the vertex from both waiting sets. This is needed when a
         --  weakly elaborable vertex is both an overall best candidate among
         --  all waiting vertices and the best waiting candidate within the
         --  component.

         LGV_Sets.Delete (All_Waiting_Vertices,  Vertex);
         LGV_Sets.Delete (Comp_Waiting_Vertices, Vertex);

         --  Mark the vertex as elaborated in order to prevent further attempts
         --  to re-elaborate it.

         Set_In_Elaboration_Order (G, Vertex);

         --  Add the unit represented by the vertex to the elaboration order

         Unit_Id_Tables.Append (Order, Unit (G, Vertex));

         --  Notify all successors and their components that they have one
         --  fewer predecessor to wait on. This may cause some successors to
         --  be included in one of the sets.

         Update_Successors
           (G                        => G,
            Vertex                   => Vertex,
            All_Elaborable_Vertices  => All_Elaborable_Vertices,
            All_Waiting_Vertices     => All_Waiting_Vertices,
            Comp_Elaborable_Vertices => Comp_Elaborable_Vertices,
            Comp_Waiting_Vertices    => Comp_Waiting_Vertices,
            Step                     => Step,
            Indent                   => Indent + Nested_Indentation);

         --  Elaborate an eligible completing body immediately after its spec.
         --  This action satisfies the semantics of pragma Elaborate_Body. In
         --  addition, it ensures that a body will not "drift" too far from its
         --  spec in case invocation edges are removed from the library graph.

         if Has_Elaborable_Body (G, Vertex) then
            Elaborate_Vertex
              (G                        => G,
               Vertex                   => Proper_Body (G, Vertex),
               All_Elaborable_Vertices  => All_Elaborable_Vertices,
               All_Waiting_Vertices     => All_Waiting_Vertices,
               Comp_Elaborable_Vertices => Comp_Elaborable_Vertices,
               Comp_Waiting_Vertices    => Comp_Waiting_Vertices,
               Order                    => Order,
               Step                     => Step,
               Indent                   => Indent);
         end if;
      end Elaborate_Vertex;

      ---------------------------------
      -- Find_Best_Elaborable_Vertex --
      ---------------------------------

      function Find_Best_Elaborable_Vertex
        (G      : Library_Graph;
         Set    : LGV_Sets.Membership_Set;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (LGV_Sets.Present (Set));

         return
           Find_Best_Vertex
             (G                   => G,
              Set                 => Set,
              Is_Suitable_Vertex  =>
                Is_Suitable_Elaborable_Vertex'Access,
              Compare_Vertices    =>
                Is_Better_Elaborable_Vertex'Access,
              Initial_Best_Msg    => "initial best elaborable vertex",
              Subsequent_Best_Msg => "better elaborable vertex",
              Step                => Step,
              Indent              => Indent);
      end Find_Best_Elaborable_Vertex;

      ----------------------
      -- Find_Best_Vertex --
      ----------------------

      function Find_Best_Vertex
        (G                   : Library_Graph;
         Set                 : LGV_Sets.Membership_Set;
         Is_Suitable_Vertex  : LGV_Predicate_Ptr;
         Compare_Vertices    : LGV_Comparator_Ptr;
         Initial_Best_Msg    : String;
         Subsequent_Best_Msg : String;
         Step                : Elaboration_Order_Step;
         Indent              : Indentation_Level)
         return Library_Graph_Vertex_Id
      is
         Best_Vertex    : Library_Graph_Vertex_Id;
         Current_Vertex : Library_Graph_Vertex_Id;
         Iter           : LGV_Sets.Iterator;

      begin
         pragma Assert (Present (G));
         pragma Assert (LGV_Sets.Present (Set));
         pragma Assert (Is_Suitable_Vertex /= null);
         pragma Assert (Compare_Vertices /= null);

         --  Assume that there is no candidate

         Best_Vertex := No_Library_Graph_Vertex;

         --  Inspect all vertices in the set, looking for the best candidate
         --  according to the comparator.

         Iter := LGV_Sets.Iterate (Set);
         while LGV_Sets.Has_Next (Iter) loop
            LGV_Sets.Next (Iter, Current_Vertex);
            pragma Assert (Needs_Elaboration (G, Current_Vertex));

            if Is_Suitable_Vertex.all (G, Current_Vertex) then

               --  A previous iteration already picked the best candidate.
               --  Update the best candidate when the current vertex is a
               --  better choice.

               if Present (Best_Vertex) then
                  if Compare_Vertices.all
                       (G           => G,
                        Vertex      => Current_Vertex,
                        Compared_To => Best_Vertex) = Higher_Precedence
                  then
                     Best_Vertex := Current_Vertex;

                     Trace_Vertex
                       (G      => G,
                        Vertex => Best_Vertex,
                        Msg    => Subsequent_Best_Msg,
                        Step   => Step,
                        Indent => Indent);
                  end if;

               --  Otherwise this is the first candidate

               else
                  Best_Vertex := Current_Vertex;

                  Trace_Vertex
                    (G      => G,
                     Vertex => Best_Vertex,
                     Msg    => Initial_Best_Msg,
                     Step   => Step,
                     Indent => Indent);
               end if;
            end if;
         end loop;

         return Best_Vertex;
      end Find_Best_Vertex;

      ----------------------------------------
      -- Find_Best_Weakly_Elaborable_Vertex --
      ----------------------------------------

      function Find_Best_Weakly_Elaborable_Vertex
        (G      : Library_Graph;
         Set    : LGV_Sets.Membership_Set;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (LGV_Sets.Present (Set));

         return
           Find_Best_Vertex
             (G                   => G,
              Set                 => Set,
              Is_Suitable_Vertex  =>
                Is_Suitable_Weakly_Elaborable_Vertex'Access,
              Compare_Vertices    =>
                Is_Better_Weakly_Elaborable_Vertex'Access,
              Initial_Best_Msg    => "initial best weakly elaborable vertex",
              Subsequent_Best_Msg => "better weakly elaborable vertex",
              Step                => Step,
              Indent              => Indent);
      end Find_Best_Weakly_Elaborable_Vertex;

      -------------------------
      -- Has_Elaborable_Body --
      -------------------------

      function Has_Elaborable_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         --  The body of an already-elaborated spec subject to Elaborate_Body
         --  is always elaborable.

         if Is_Spec_With_Elaborate_Body (G, Vertex) then
            return True;

         elsif Is_Spec_With_Body (G, Vertex) then
            return Is_Elaborable_Vertex (G, Proper_Body (G, Vertex));
         end if;

         return False;
      end Has_Elaborable_Body;

      ---------------------------------
      -- Insert_Elaborable_Successor --
      ---------------------------------

      procedure Insert_Elaborable_Successor
        (G                     : Library_Graph;
         Vertex                : Library_Graph_Vertex_Id;
         Elaborable_Vertices   : LGV_Sets.Membership_Set;
         All_Waiting_Vertices  : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices : LGV_Sets.Membership_Set;
         Msg                   : String;
         Step                  : Elaboration_Order_Step;
         Indent                : Indentation_Level)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (LGV_Sets.Present (Elaborable_Vertices));
         pragma Assert (LGV_Sets.Present (All_Waiting_Vertices));
         pragma Assert (LGV_Sets.Present (Comp_Waiting_Vertices));

         Complement : constant Library_Graph_Vertex_Id :=
                        Complementary_Vertex
                          (G                => G,
                           Vertex           => Vertex,
                           Force_Complement => False);

      begin
         --  Remove the successor from both waiting vertex sets because it may
         --  be the best vertex to elaborate across the whole graph and within
         --  its component.

         LGV_Sets.Delete (All_Waiting_Vertices,  Vertex);
         LGV_Sets.Delete (Comp_Waiting_Vertices, Vertex);

         Insert_Vertex
           (G      => G,
            Vertex => Vertex,
            Set    => Elaborable_Vertices,
            Msg    => Msg,
            Step   => Step,
            Indent => Indent);

         if Present (Complement) then

            --  Remove the complement of the successor from both waiting vertex
            --  sets because it may be the best vertex to elaborate across the
            --  whole graph and within its component.

            LGV_Sets.Delete (All_Waiting_Vertices,  Complement);
            LGV_Sets.Delete (Comp_Waiting_Vertices, Complement);

            Insert_Vertex
              (G      => G,
               Vertex => Complement,
               Set    => Elaborable_Vertices,
               Msg    => Msg,
               Step   => Step,
               Indent => Indent);
         end if;
      end Insert_Elaborable_Successor;

      -------------------
      -- Insert_Vertex --
      -------------------

      procedure Insert_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Set    : LGV_Sets.Membership_Set;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (Needs_Elaboration (G, Vertex));
         pragma Assert (LGV_Sets.Present (Set));

         --  Nothing to do when the vertex is already present in the set

         if LGV_Sets.Contains (Set, Vertex) then
            return;
         end if;

         Trace_Vertex
           (G      => G,
            Vertex => Vertex,
            Msg    => Msg,
            Step   => Step,
            Indent => Indent);

         --  Add the vertex to the set

         LGV_Sets.Insert (Set, Vertex);
      end Insert_Vertex;

      ---------------------------------
      -- Is_Better_Elaborable_Vertex --
      ---------------------------------

      function Is_Better_Elaborable_Vertex
        (G           : Library_Graph;
         Vertex      : Library_Graph_Vertex_Id;
         Compared_To : Library_Graph_Vertex_Id) return Precedence_Kind
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (Present (Compared_To));

         --  Prefer a spec with Elaborate_Body over its corresponding body

         if Is_Elaborate_Body_Pair
              (G           => G,
               Spec_Vertex => Vertex,
               Body_Vertex => Compared_To)
         then
            return Higher_Precedence;

         elsif Is_Elaborate_Body_Pair
                 (G           => G,
                  Spec_Vertex => Compared_To,
                  Body_Vertex => Vertex)
         then
            return Lower_Precedence;

         --  Prefer a predefined unit over a non-predefined unit

         elsif Is_Predefined_Unit (G, Vertex)
           and then not Is_Predefined_Unit (G, Compared_To)
         then
            return Higher_Precedence;

         elsif not Is_Predefined_Unit (G, Vertex)
           and then Is_Predefined_Unit (G, Compared_To)
         then
            return Lower_Precedence;

         --  Prefer an internal unit over a non-internal unit

         elsif Is_Internal_Unit (G, Vertex)
           and then not Is_Internal_Unit (G, Compared_To)
         then
            return Higher_Precedence;

         elsif not Is_Internal_Unit (G, Vertex)
           and then Is_Internal_Unit (G, Compared_To)
         then
            return Lower_Precedence;

         --  Prefer a preelaborated unit over a non-preelaborated unit

         elsif Is_Preelaborated_Unit (G, Vertex)
           and then not Is_Preelaborated_Unit (G, Compared_To)
         then
            return Higher_Precedence;

         elsif not Is_Preelaborated_Unit (G, Vertex)
           and then Is_Preelaborated_Unit (G, Compared_To)
         then
            return Lower_Precedence;

         --  Otherwise default to lexicographical order to ensure deterministic
         --  behavior.

         elsif Uname_Less (Name (G, Vertex), Name (G, Compared_To)) then
            return Higher_Precedence;

         else
            return Lower_Precedence;
         end if;
      end Is_Better_Elaborable_Vertex;

      ----------------------------------------
      -- Is_Better_Weakly_Elaborable_Vertex --
      ----------------------------------------

      function Is_Better_Weakly_Elaborable_Vertex
        (G           : Library_Graph;
         Vertex      : Library_Graph_Vertex_Id;
         Compared_To : Library_Graph_Vertex_Id) return Precedence_Kind
      is
         Comp_Strong_Preds   : Natural;
         Comp_Weak_Preds     : Natural;
         Vertex_Strong_Preds : Natural;
         Vertex_Weak_Preds   : Natural;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (Present (Compared_To));

         --  Obtain the number of pending predecessors for both candidates,
         --  taking into account Elaborate_Body pairs.

         Pending_Predecessors_For_Elaboration
           (G            => G,
            Vertex       => Vertex,
            Strong_Preds => Vertex_Strong_Preds,
            Weak_Preds   => Vertex_Weak_Preds);

         Pending_Predecessors_For_Elaboration
           (G            => G,
            Vertex       => Compared_To,
            Strong_Preds => Comp_Strong_Preds,
            Weak_Preds   => Comp_Weak_Preds);

         --  Neither candidate should be waiting on strong predecessors,
         --  otherwise the candidate cannot be weakly elaborated.

         pragma Assert (Vertex_Strong_Preds = 0);
         pragma Assert (Comp_Strong_Preds   = 0);

         --  Prefer a unit with fewer weak predecessors over a unit with more
         --  weak predecessors.

         if Vertex_Weak_Preds < Comp_Weak_Preds then
            return Higher_Precedence;

         elsif Vertex_Weak_Preds > Comp_Weak_Preds then
            return Lower_Precedence;

         --  Otherwise default to lexicographical order to ensure deterministic
         --  behavior.

         elsif Uname_Less (Name (G, Vertex), Name (G, Compared_To)) then
            return Higher_Precedence;

         else
            return Lower_Precedence;
         end if;
      end Is_Better_Weakly_Elaborable_Vertex;

      -----------------------------------
      -- Is_Suitable_Elaborable_Vertex --
      -----------------------------------

      function Is_Suitable_Elaborable_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         --  A vertex is suitable for elaboration as long it is not waiting on
         --  any predecessors, ignoring the static or dynamic model.

         return Is_Elaborable_Vertex (G, Vertex);
      end Is_Suitable_Elaborable_Vertex;

      ------------------------------------------
      -- Is_Suitable_Weakly_Elaborable_Vertex --
      ------------------------------------------

      function Is_Suitable_Weakly_Elaborable_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         --  A vertex is suitable for weak elaboration when it is waiting on
         --  weak predecessors only, and the unit it represents was compiled
         --  using the dynamic model.

         return
           Is_Dynamically_Elaborated (G, Vertex)
             and then Is_Weakly_Elaborable_Vertex (G, Vertex);
      end Is_Suitable_Weakly_Elaborable_Vertex;

      ------------------------------------
      -- Set_Unit_Elaboration_Positions --
      ------------------------------------

      procedure Set_Unit_Elaboration_Positions (Order : Unit_Id_Table) is
         U_Id : Unit_Id;

      begin
         for Position in Unit_Id_Tables.First ..
                         Unit_Id_Tables.Last (Order)
         loop
            U_Id := Order.Table (Position);

            ALI.Units.Table (U_Id).Elab_Position := Position;
         end loop;
      end Set_Unit_Elaboration_Positions;

      ---------------------
      -- Trace_Component --
      ---------------------

      procedure Trace_Component
        (G    : Library_Graph;
         Comp : Component_Id;
         Msg  : String;
         Step : Elaboration_Order_Step)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         --  Nothing to do when switch -d_T (output elaboration order and cycle
         --  detection trace information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Trace_Step (Step);
         Write_Str  (Msg);
         Write_Str  (" (Comp_Id_");
         Write_Int  (Int (Comp));
         Write_Str  (")");
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Nested_Indentation);
         Write_Str  ("pending strong predecessors: ");
         Write_Num  (Int (Pending_Strong_Predecessors (G, Comp)));
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Nested_Indentation);
         Write_Str  ("pending weak predecessors  : ");
         Write_Num  (Int (Pending_Weak_Predecessors (G, Comp)));
         Write_Eol;
      end Trace_Component;

      ----------------
      -- Trace_Step --
      ----------------

      procedure Trace_Step (Step : Elaboration_Order_Step) is
      begin
         --  Nothing to do when switch -d_T (output elaboration order and cycle
         --  detection trace information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Write_Num
           (Val        => Int (Step),
            Val_Indent => Step_Column);
         Write_Str (": ");
      end Trace_Step;

      ------------------
      -- Trace_Vertex --
      ------------------

      procedure Trace_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Attr_Indent : constant Indentation_Level :=
                         Indent + Nested_Indentation;
         Comp        : constant Component_Id := Component (G, Vertex);

      begin
         --  Nothing to do when switch -d_T (output elaboration order and cycle
         --  detection trace information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Trace_Step (Step);
         Indent_By  (Indent);
         Write_Str  (Msg);
         Write_Str  (" (LGV_Id_");
         Write_Int  (Int (Vertex));
         Write_Str  (")");
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Attr_Indent);
         Write_Str  ("name = ");
         Write_Name (Name (G, Vertex));
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Attr_Indent);
         Write_Str  ("Component (Comp_Id_");
         Write_Int  (Int (Comp));
         Write_Str  (")");
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Attr_Indent);
         Write_Str  ("pending strong predecessors: ");
         Write_Num  (Int (Pending_Strong_Predecessors (G, Vertex)));
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Attr_Indent);
         Write_Str  ("pending weak predecessors  : ");
         Write_Num  (Int (Pending_Weak_Predecessors (G, Vertex)));
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Attr_Indent);
         Write_Str  ("pending strong components  : ");
         Write_Num  (Int (Pending_Strong_Predecessors (G, Comp)));
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Attr_Indent);
         Write_Str  ("pending weak components    : ");
         Write_Num  (Int (Pending_Weak_Predecessors (G, Comp)));
         Write_Eol;
      end Trace_Vertex;

      --------------------
      -- Trace_Vertices --
      --------------------

      procedure Trace_Vertices
        (G          : Library_Graph;
         Set        : LGV_Sets.Membership_Set;
         Set_Msg    : String;
         Vertex_Msg : String;
         Step       : Elaboration_Order_Step;
         Indent     : Indentation_Level)
      is
         Vertex_Indent : constant Indentation_Level :=
                           Indent + Nested_Indentation;

         Iter   : LGV_Sets.Iterator;
         Vertex : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (LGV_Sets.Present (Set));

         --  Nothing to do when switch -d_T (output elaboration order and cycle
         --  detection trace information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Trace_Step (Step);
         Indent_By  (Indent);
         Write_Str  (Set_Msg);
         Write_Str  (": ");
         Write_Int  (Int (LGV_Sets.Size (Set)));
         Write_Eol;

         Iter := LGV_Sets.Iterate (Set);
         while LGV_Sets.Has_Next (Iter) loop
            LGV_Sets.Next (Iter, Vertex);

            Trace_Vertex
              (G      => G,
               Vertex => Vertex,
               Msg    => Vertex_Msg,
               Step   => Step,
               Indent => Vertex_Indent);
         end loop;
      end Trace_Vertices;

      ----------------------
      -- Update_Successor --
      ----------------------

      procedure Update_Successor
        (G                        : Library_Graph;
         Edge                     : Library_Graph_Edge_Id;
         All_Elaborable_Vertices  : LGV_Sets.Membership_Set;
         All_Waiting_Vertices     : LGV_Sets.Membership_Set;
         Comp_Elaborable_Vertices : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Step                     : Elaboration_Order_Step;
         Indent                   : Indentation_Level)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));
         pragma Assert (LGV_Sets.Present (All_Elaborable_Vertices));
         pragma Assert (LGV_Sets.Present (All_Waiting_Vertices));
         pragma Assert (LGV_Sets.Present (Comp_Elaborable_Vertices));
         pragma Assert (LGV_Sets.Present (Comp_Waiting_Vertices));

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, Edge);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, Edge);

         pragma Assert (Needs_Elaboration (G, Pred));
         pragma Assert (Needs_Elaboration (G, Succ));

         In_Different_Components : constant Boolean :=
                                     not In_Same_Component
                                           (G     => G,
                                            Left  => Pred,
                                            Right => Succ);

         Succ_Comp     : constant Component_Id      := Component (G, Succ);
         Vertex_Indent : constant Indentation_Level :=
                           Indent + Nested_Indentation;

         Iter   : Component_Vertex_Iterator;
         Vertex : Library_Graph_Vertex_Id;

      begin
         Trace_Vertex
           (G      => G,
            Vertex => Succ,
            Msg    => "updating successor",
            Step   => Step,
            Indent => Indent);

         --  Notify the successor that it has one less predecessor to wait on.
         --  This effectively eliminates the edge that links the two.

         Decrement_Pending_Predecessors
           (G      => G,
            Vertex => Succ,
            Edge   => Edge);

         --  The predecessor and successor reside in different components.
         --  Notify the successor component it has one fewer components to
         --  wait on.

         if In_Different_Components then
            Decrement_Pending_Predecessors
              (G    => G,
               Comp => Succ_Comp,
               Edge => Edge);
         end if;

         --  At this point the successor may become elaborable when its final
         --  predecessor or final predecessor component has been elaborated.

         if Is_Elaborable_Vertex (G, Succ) then

            --  The predecessor and successor reside in different components.
            --  The successor must not be added to the candidates of Pred's
            --  component because this will mix units from the two components.
            --  Instead, the successor is added to the set of all elaborable
            --  vertices.

            if In_Different_Components then
               Insert_Elaborable_Successor
                 (G                     => G,
                  Vertex                => Succ,
                  Elaborable_Vertices   => All_Elaborable_Vertices,
                  All_Waiting_Vertices  => All_Waiting_Vertices,
                  Comp_Waiting_Vertices => Comp_Waiting_Vertices,
                  Msg                   => "add elaborable successor",
                  Step                  => Step,
                  Indent                => Vertex_Indent);

            --  Otherwise the predecessor and successor reside within the same
            --  component. Pred's component gains another elaborable vertex.

            else
               Insert_Elaborable_Successor
                 (G                     => G,
                  Vertex                => Succ,
                  Elaborable_Vertices   => Comp_Elaborable_Vertices,
                  All_Waiting_Vertices  => All_Waiting_Vertices,
                  Comp_Waiting_Vertices => Comp_Waiting_Vertices,
                  Msg                   =>
                    "add elaborable component successor",
                  Step                  => Step,
                  Indent                => Vertex_Indent);
            end if;
         end if;

         --  At this point the successor component may become elaborable when
         --  its final predecessor component is elaborated. This in turn may
         --  allow vertices of the successor component to be elaborated.

         if In_Different_Components
           and then Is_Elaborable_Component (G, Succ_Comp)
         then
            Iter := Iterate_Component_Vertices (G, Succ_Comp);
            while Has_Next (Iter) loop
               Next (Iter, Vertex);

               if Is_Elaborable_Vertex (G, Vertex) then
                  Insert_Elaborable_Successor
                    (G                     => G,
                     Vertex                => Vertex,
                     Elaborable_Vertices   => All_Elaborable_Vertices,
                     All_Waiting_Vertices  => All_Waiting_Vertices,
                     Comp_Waiting_Vertices => Comp_Waiting_Vertices,
                     Msg                   => "add elaborable vertex",
                     Step                  => Step,
                     Indent                => Vertex_Indent);
               end if;
            end loop;
         end if;
      end Update_Successor;

      -----------------------
      -- Update_Successors --
      -----------------------

      procedure Update_Successors
        (G                        : Library_Graph;
         Vertex                   : Library_Graph_Vertex_Id;
         All_Elaborable_Vertices  : LGV_Sets.Membership_Set;
         All_Waiting_Vertices     : LGV_Sets.Membership_Set;
         Comp_Elaborable_Vertices : LGV_Sets.Membership_Set;
         Comp_Waiting_Vertices    : LGV_Sets.Membership_Set;
         Step                     : Elaboration_Order_Step;
         Indent                   : Indentation_Level)
      is
         Edge : Library_Graph_Edge_Id;
         Iter : Edges_To_Successors_Iterator;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (Needs_Elaboration (G, Vertex));
         pragma Assert (LGV_Sets.Present (All_Elaborable_Vertices));
         pragma Assert (LGV_Sets.Present (All_Waiting_Vertices));
         pragma Assert (LGV_Sets.Present (Comp_Elaborable_Vertices));
         pragma Assert (LGV_Sets.Present (Comp_Waiting_Vertices));

         Iter := Iterate_Edges_To_Successors (G, Vertex);
         while Has_Next (Iter) loop
            Next (Iter, Edge);
            pragma Assert (Predecessor (G, Edge) = Vertex);

            Update_Successor
              (G                        => G,
               Edge                     => Edge,
               All_Elaborable_Vertices  => All_Elaborable_Vertices,
               All_Waiting_Vertices     => All_Waiting_Vertices,
               Comp_Elaborable_Vertices => Comp_Elaborable_Vertices,
               Comp_Waiting_Vertices    => Comp_Waiting_Vertices,
               Step                     => Step,
               Indent                   => Indent);
         end loop;
      end Update_Successors;
   end Invocation_And_Library_Graph_Elaborators;

end Bindo.Elaborators;
