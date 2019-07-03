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

with Binderr; use Binderr;
with Butil;   use Butil;
with Debug;   use Debug;
with Output;  use Output;
with Types;   use Types;

with Bindo.Augmentors;
use  Bindo.Augmentors;
use  Bindo.Augmentors.Library_Graph_Augmentors;

with Bindo.Builders;
use  Bindo.Builders;
use  Bindo.Builders.Invocation_Graph_Builders;
use  Bindo.Builders.Library_Graph_Builders;

with Bindo.Diagnostics;
use  Bindo.Diagnostics;
use  Bindo.Diagnostics.Cycle_Diagnostics;

with Bindo.Units;
use  Bindo.Units;

with Bindo.Validators;
use  Bindo.Validators;
use  Bindo.Validators.Elaboration_Order_Validators;
use  Bindo.Validators.Invocation_Graph_Validators;
use  Bindo.Validators.Library_Graph_Validators;

with Bindo.Writers;
use  Bindo.Writers;
use  Bindo.Writers.ALI_Writers;
use  Bindo.Writers.Elaboration_Order_Writers;
use  Bindo.Writers.Invocation_Graph_Writers;
use  Bindo.Writers.Library_Graph_Writers;
use  Bindo.Writers.Unit_Closure_Writers;

with GNAT;        use GNAT;
with GNAT.Graphs; use GNAT.Graphs;
with GNAT.Sets;   use GNAT.Sets;

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
      Add_To_All_Candidates_Msg  : aliased String :=
                                     "add vertex to all candidates";
      Add_To_Comp_Candidates_Msg : aliased String :=
                                     "add vertex to component candidates";

      -----------
      -- Types --
      -----------

      type String_Ptr is access all String;

      -----------------
      -- Visited set --
      -----------------

      package VS is new Membership_Sets
        (Element_Type => Library_Graph_Vertex_Id,
         "="          => "=",
         Hash         => Hash_Library_Graph_Vertex);
      use VS;

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Add_Vertex
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Set    : Membership_Set;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level);
      pragma Inline (Add_Vertex);
      --  Add vertex LGV_Id of library graph G to membership set Set. Msg is
      --  a message emitted for traching purposes. Step is the current step
      --  in the elaboration order. Indent is the desired indentation level
      --  for tracing.

      procedure Add_Vertex_If_Elaborable
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Set    : Membership_Set;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level);
      pragma Inline (Add_Vertex_If_Elaborable);
      --  Add vertex LGV_Id of library graph G to membership set Set if it can
      --  be elaborated. Msg is a message emitted for traching purposes. Step
      --  is the current step in the elaboration order. Indent is the desired
      --  indentation level for tracing.

      function Create_All_Candidates_Set
        (G    : Library_Graph;
         Step : Elaboration_Order_Step) return Membership_Set;
      pragma Inline (Create_All_Candidates_Set);
      --  Collect all elaborable candidate vertices of library graph G in a
      --  set. Step is the current step in the elaboration order.

      function Create_Component_Candidates_Set
        (G    : Library_Graph;
         Comp : Component_Id;
         Step : Elaboration_Order_Step) return Membership_Set;
      pragma Inline (Create_Component_Candidates_Set);
      --  Collect all elaborable candidate vertices that appear in component
      --  Comp of library graph G in a set. Step is the current step in the
      --  elaboration order.

      procedure Elaborate_Component
        (G                  : Library_Graph;
         Comp               : Component_Id;
         All_Candidates     : Membership_Set;
         Remaining_Vertices : in out Natural;
         Order              : in out Unit_Id_Table;
         Step               : Elaboration_Order_Step);
      pragma Inline (Elaborate_Component);
      --  Elaborate as many vertices as possible which appear in component
      --  Comp of library graph G. All_Candidates is the set of all elaborable
      --  vertices across the whole library graph. Remaining_Vertices is the
      --  number of vertices that remain to be elaborated. Order denotes the
      --  elaboration order. Step is the current step in the elaboration order.

      procedure Elaborate_Library_Graph
        (G      : Library_Graph;
         Order  : out Unit_Id_Table;
         Status : out Elaboration_Order_Status);
      pragma Inline (Elaborate_Library_Graph);
      --  Elaborate as many vertices as possible of library graph G. Order is
      --  the elaboration order. Status is the condition of the elaboration
      --  order.

      procedure Elaborate_Units_Common
        (Use_Inv_Graph : Boolean;
         Inv_Graph     : out Invocation_Graph;
         Lib_Graph     : out Library_Graph;
         Order         : out Unit_Id_Table;
         Status        : out Elaboration_Order_Status);
      pragma Inline (Elaborate_Units_Common);
      --  Find the elaboration order of all units in the bind. Use_Inv_Graph
      --  should be set when library graph Lib_Graph is to be augmented with
      --  information from invocation graph Inv_Graph. Order is the elaboration
      --  order. Status is the condition of the elaboration order.

      procedure Elaborate_Units_Dynamic (Order : out Unit_Id_Table);
      pragma Inline (Elaborate_Units_Dynamic);
      --  Find the elaboration order of all units in the bind using the dynamic
      --  model. Order is the elaboration order. In the event where no ordering
      --  is possible, this routine diagnoses the issue(s) and raises exception
      --  Unrecoverable_Error.

      procedure Elaborate_Units_Static (Order : out Unit_Id_Table);
      pragma Inline (Elaborate_Units_Static);
      --  Find the elaboration order of all units in the bind using the static
      --  model. Order is the elaboration order. In the event where no ordering
      --  is possible, this routine diagnoses the issue(s) and raises exception
      --  Unrecoverable_Error.

      procedure Elaborate_Vertex
        (G                  : Library_Graph;
         LGV_Id             : Library_Graph_Vertex_Id;
         All_Candidates     : Membership_Set;
         Comp_Candidates    : Membership_Set;
         Remaining_Vertices : in out Natural;
         Order              : in out Unit_Id_Table;
         Step               : Elaboration_Order_Step;
         Indent             : Indentation_Level);
      pragma Inline (Elaborate_Vertex);
      --  Elaborate vertex LGV_Id of library graph G by adding its unit to
      --  elaboration order Order. The routine updates awaiting successors
      --  where applicable. All_Candidates denotes the set of all elaborable
      --  vertices across the whole library graph. Comp_Candidates is the set
      --  of all elaborable vertices in the component of LGV_Id. Parameter
      --  Remaining_Vertices denotes the number of vertices that remain to
      --  be elaborated. Step is the current step in the elaboration order.
      --  Indent is the desired indentation level for tracing.

      function Find_Best_Candidate
        (G      : Library_Graph;
         Set    : Membership_Set;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level) return Library_Graph_Vertex_Id;
      pragma Inline (Find_Best_Candidate);
      --  Find the most suitable vertex of library graph G for elaboration from
      --  membership set Set. Step denotes the current step in the elaboration
      --  order. Indent is the desired indentation level for tracing.

      function Is_Better_Candidate
        (G           : Library_Graph;
         Best_Candid : Library_Graph_Vertex_Id;
         New_Candid  : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Better_Candidate);
      --  Determine whether new candidate vertex New_Candid of library graph
      --  G is a more suitable choice for elaboration compared to the current
      --  best candidate Best_Candid.

      procedure Trace_Candidate_Vertices
        (G    : Library_Graph;
         Set  : Membership_Set;
         Step : Elaboration_Order_Step);
      pragma Inline (Trace_Candidate_Vertices);
      --  Write the candidate vertices of library graph G present in membership
      --  set Set to standard output. Formal Step denotes the current step in
      --  the elaboration order.

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

      procedure Trace_Unelaborated_Vertices
        (G     : Library_Graph;
         Count : Natural;
         Step  : Elaboration_Order_Step);
      pragma Inline (Trace_Unelaborated_Vertices);
      --  Write the remaining unelaborated vertices of library graph G to
      --  standard output. Count is the number of vertices that remain to
      --  be elaborated. Step is the current step in the elaboration order.

      procedure Trace_Vertex
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level);
      pragma Inline (Trace_Vertex);
      --  Write elaboration-related information for vertex LGV_Id of library
      --  graph G to standard output, starting with message Msg. Step is the
      --  current step in the elaboration order. Indent denotes the desired
      --  indentation level for tracing.

      procedure Update_Successor
        (G               : Library_Graph;
         Pred            : Library_Graph_Vertex_Id;
         Succ            : Library_Graph_Vertex_Id;
         All_Candidates  : Membership_Set;
         Comp_Candidates : Membership_Set;
         Step            : Elaboration_Order_Step;
         Indent          : Indentation_Level);
      pragma Inline (Update_Successor);
      --  Notify successor vertex Succ of library graph G along with its
      --  component that their predecessor Pred has just been elaborated.
      --  This may cause new vertices to become elaborable, and thus be added
      --  to one of the two sets. All_Candidates is the set of all elaborable
      --  vertices across the whole library graph. Comp_Candidates is the set
      --  of all elaborable vertices in the component of Pred. Step is the
      --  current step in the elaboration order. Indent denotes the desired
      --  indentation level for tracing.

      procedure Update_Successors
        (G               : Library_Graph;
         Pred            : Library_Graph_Vertex_Id;
         All_Candidates  : Membership_Set;
         Comp_Candidates : Membership_Set;
         Step            : Elaboration_Order_Step;
         Indent          : Indentation_Level);
      pragma Inline (Update_Successors);
      --  Notify all successors along with their components that their
      --  predecessor vertex Pred of ligrary graph G has just been elaborated.
      --  This may cause new vertices to become elaborable, and thus be added
      --  to one of the two sets. All_Candidates is the set of all elaborable
      --  vertices across the whole library graph. Comp_Candidates is the set
      --  of all elaborable vertices in the component of Pred. Step is the
      --  current step in the elaboration order. Indent denotes the desired
      --  indentation level for tracing.

      ----------------
      -- Add_Vertex --
      ----------------

      procedure Add_Vertex
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Set    : Membership_Set;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level)
      is
      begin
         pragma Assert (Present (LGV_Id));
         pragma Assert (Needs_Elaboration (G, LGV_Id));
         pragma Assert (Present (Set));

         --  Add vertex only when it is not present in the set. This is not
         --  strictly necessary because the set implementation handles this
         --  case, however the check eliminates spurious traces.

         if not Contains (Set, LGV_Id) then
            Trace_Vertex
              (G      => G,
               LGV_Id => LGV_Id,
               Msg    => Msg,
               Step   => Step,
               Indent => Indent);

            Insert (Set, LGV_Id);
         end if;
      end Add_Vertex;

      ------------------------------
      -- Add_Vertex_If_Elaborable --
      ------------------------------

      procedure Add_Vertex_If_Elaborable
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Set    : Membership_Set;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level)
      is
         Aux_LGV_Id : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));
         pragma Assert (Needs_Elaboration (G, LGV_Id));
         pragma Assert (Present (Set));

         if Is_Elaborable_Vertex (G, LGV_Id) then
            Add_Vertex
              (G      => G,
               LGV_Id => LGV_Id,
               Set    => Set,
               Msg    => Msg,
               Step   => Step,
               Indent => Indent);

            --  Assume that there is no extra vertex that needs to be added

            Aux_LGV_Id := No_Library_Graph_Vertex;

            --  A spec-body pair where the spec carries pragma Elaborate_Body
            --  must be treated as one vertex for elaboration purposes. If one
            --  of them is elaborable, then the other is also elaborable. This
            --  property is guaranteed by predicate Is_Elaborable_Vertex.

            if Is_Body_Of_Spec_With_Elaborate_Body (G, LGV_Id) then
               Aux_LGV_Id := Proper_Spec (G, LGV_Id);
               pragma Assert (Present (Aux_LGV_Id));

            elsif Is_Spec_With_Elaborate_Body (G, LGV_Id) then
               Aux_LGV_Id := Proper_Body (G, LGV_Id);
               pragma Assert (Present (Aux_LGV_Id));
            end if;

            if Present (Aux_LGV_Id) then
               pragma Assert (Needs_Elaboration (G, Aux_LGV_Id));

               Add_Vertex
                 (G      => G,
                  LGV_Id => Aux_LGV_Id,
                  Set    => Set,
                  Msg    => Msg,
                  Step   => Step,
                  Indent => Indent);
            end if;
         end if;
      end Add_Vertex_If_Elaborable;

      -------------------------------
      -- Create_All_Candidates_Set --
      -------------------------------

      function Create_All_Candidates_Set
        (G    : Library_Graph;
         Step : Elaboration_Order_Step) return Membership_Set
      is
         Iter   : Library_Graphs.All_Vertex_Iterator;
         LGV_Id : Library_Graph_Vertex_Id;
         Set    : Membership_Set;

      begin
         pragma Assert (Present (G));

         Set  := Create (Number_Of_Vertices (G));
         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, LGV_Id);
            pragma Assert (Present (LGV_Id));

            Add_Vertex_If_Elaborable
              (G      => G,
               LGV_Id => LGV_Id,
               Set    => Set,
               Msg    => Add_To_All_Candidates_Msg,
               Step   => Step,
               Indent => No_Indentation);
         end loop;

         return Set;
      end Create_All_Candidates_Set;

      -------------------------------------
      -- Create_Component_Candidates_Set --
      -------------------------------------

      function Create_Component_Candidates_Set
        (G    : Library_Graph;
         Comp : Component_Id;
         Step : Elaboration_Order_Step) return Membership_Set
      is
         Iter   : Component_Vertex_Iterator;
         LGV_Id : Library_Graph_Vertex_Id;
         Set    : Membership_Set;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Set  := Create (Number_Of_Component_Vertices (G, Comp));
         Iter := Iterate_Component_Vertices (G, Comp);
         while Has_Next (Iter) loop
            Next (Iter, LGV_Id);
            pragma Assert (Present (LGV_Id));

            Add_Vertex_If_Elaborable
              (G      => G,
               LGV_Id => LGV_Id,
               Set    => Set,
               Msg    => Add_To_Comp_Candidates_Msg,
               Step   => Step,
               Indent => No_Indentation);
         end loop;

         return Set;
      end Create_Component_Candidates_Set;

      -------------------------
      -- Elaborate_Component --
      -------------------------

      procedure Elaborate_Component
        (G                  : Library_Graph;
         Comp               : Component_Id;
         All_Candidates     : Membership_Set;
         Remaining_Vertices : in out Natural;
         Order              : in out Unit_Id_Table;
         Step               : Elaboration_Order_Step)
      is
         Candidate       : Library_Graph_Vertex_Id;
         Comp_Candidates : Membership_Set;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));
         pragma Assert (Present (All_Candidates));

         Trace_Component
           (G    => G,
            Comp => Comp,
            Msg  => "elaborating component",
            Step => Step);

         Comp_Candidates := Create_Component_Candidates_Set (G, Comp, Step);

         loop
            Candidate :=
              Find_Best_Candidate
                (G      => G,
                 Set    => Comp_Candidates,
                 Step   => Step,
                 Indent => Nested_Indentation);

            --  Stop the elaboration of the component when there is no suitable
            --  candidate. This indicates that either all vertices within the
            --  component have been elaborated, or the library graph contains a
            --  circularity.

            exit when not Present (Candidate);

            Elaborate_Vertex
              (G                  => G,
               LGV_Id             => Candidate,
               All_Candidates     => All_Candidates,
               Comp_Candidates    => Comp_Candidates,
               Remaining_Vertices => Remaining_Vertices,
               Order              => Order,
               Step               => Step,
               Indent             => Nested_Indentation);
         end loop;

         Destroy (Comp_Candidates);
      end Elaborate_Component;

      -----------------------------
      -- Elaborate_Library_Graph --
      -----------------------------

      procedure Elaborate_Library_Graph
        (G      : Library_Graph;
         Order  : out Unit_Id_Table;
         Status : out Elaboration_Order_Status)
      is
         All_Candidates     : Membership_Set;
         Candidate          : Library_Graph_Vertex_Id;
         Comp               : Component_Id;
         Remaining_Vertices : Natural;
         Step               : Elaboration_Order_Step;

      begin
         pragma Assert (Present (G));

         Step := Initial_Step;

         All_Candidates     := Create_All_Candidates_Set (G, Step);
         Remaining_Vertices := Number_Of_Vertices (G);

         loop
            Step := Step + 1;

            Trace_Candidate_Vertices
              (G    => G,
               Set  => All_Candidates,
               Step => Step);

            Trace_Unelaborated_Vertices
              (G     => G,
               Count => Remaining_Vertices,
               Step  => Step);

            Candidate :=
              Find_Best_Candidate
                (G      => G,
                 Set    => All_Candidates,
                 Step   => Step,
                 Indent => No_Indentation);

            --  Stop the elaboration when there is no suitable candidate. This
            --  indicates that either all units were elaborated or the library
            --  graph contains a circularity.

            exit when not Present (Candidate);

            --  Elaborate the component of the candidate vertex by trying to
            --  elaborate as many vertices within the component as possible.
            --  Each successful elaboration signals the appropriate successors
            --  and their components that they have one less predecessor to
            --  wait on. This may add new candidates to set All_Candidates.

            Comp := Component (G, Candidate);
            pragma Assert (Present (Comp));

            Elaborate_Component
              (G                  => G,
               Comp               => Comp,
               All_Candidates     => All_Candidates,
               Remaining_Vertices => Remaining_Vertices,
               Order              => Order,
               Step               => Step);
         end loop;

         Destroy (All_Candidates);

         --  The library graph contains an Elaborate_All circularity when
         --  at least one edge subject to the related pragma appears in a
         --  component.

         if Has_Elaborate_All_Cycle (G) then
            Status := Order_Has_Elaborate_All_Circularity;

         --  The library contains a circularity when at least one vertex failed
         --  to elaborate.

         elsif Remaining_Vertices /= 0 then
            Status := Order_Has_Circularity;

         --  Otherwise the elaboration order is satisfactory

         else
            Status := Order_OK;
         end if;
      end Elaborate_Library_Graph;

      ---------------------
      -- Elaborate_Units --
      ---------------------

      procedure Elaborate_Units
        (Order         : out Unit_Id_Table;
         Main_Lib_File : File_Name_Type)
      is
         Main_Lib_Unit : constant Unit_Id :=
                           Corresponding_Unit (Unit_Name_Type (Main_Lib_File));

      begin
         pragma Assert (Present (Main_Lib_Unit));

         --  Initialize all unit-related data structures and gather all units
         --  that need elaboration.

         Initialize_Units;
         Collect_Elaborable_Units;

         Write_ALI_Tables;

         --  Choose the proper elaboration strategy based on whether the main
         --  library unit was compiled with dynamic elaboration checks.

         if Is_Dynamically_Elaborated (Main_Lib_Unit) then
            Elaborate_Units_Dynamic (Order);
         else
            Elaborate_Units_Static (Order);
         end if;

         Validate_Elaboration_Order (Order);
         Write_Elaboration_Order    (Order);

         --  Enumerate the sources referenced in the closure of the order

         Write_Unit_Closure (Order);

         --  Destroy all unit-delated data structures

         Finalize_Units;

      exception
         when others =>
            Finalize_Units;
            raise;
      end Elaborate_Units;

      ----------------------------
      -- Elaborate_Units_Common --
      ----------------------------

      procedure Elaborate_Units_Common
        (Use_Inv_Graph : Boolean;
         Inv_Graph     : out Invocation_Graph;
         Lib_Graph     : out Library_Graph;
         Order         : out Unit_Id_Table;
         Status        : out Elaboration_Order_Status)
      is
      begin
         --  Create, validate, and output the library graph which captures the
         --  dependencies between library items.

         Lib_Graph := Build_Library_Graph;
         Validate_Library_Graph (Lib_Graph);
         Write_Library_Graph    (Lib_Graph);

         --  Create, validate, output, and use the invocation graph which
         --  represents the flow of execusion only when requested by the
         --  caller.

         if Use_Inv_Graph then
            Inv_Graph := Build_Invocation_Graph (Lib_Graph);
            Validate_Invocation_Graph (Inv_Graph);
            Write_Invocation_Graph    (Inv_Graph);

         --  Otherwise the invocation graph is not used. Create a dummy graph
         --  as this allows for a uniform behavior on the caller side.

         else
            Inv_Graph :=
              Invocation_Graphs.Create
                (Initial_Vertices => 1,
                 Initial_Edges    => 1);
         end if;

         --  Traverse the invocation graph starting from elaboration code in
         --  order to discover transitions of the execution flow from a unit
         --  to a unit which result in extra edges within the library graph.

         Augment_Library_Graph (Inv_Graph, Lib_Graph);

         --  Create and output the component graph by collapsing all library
         --  items into library units and traversing the library graph.

         Find_Components     (Lib_Graph);
         Write_Library_Graph (Lib_Graph);

         --  Traverse the library graph to determine the elaboration order of
         --  units.

         Elaborate_Library_Graph
           (G      => Lib_Graph,
            Order  => Order,
            Status => Status);
      end Elaborate_Units_Common;

      -----------------------------
      -- Elaborate_Units_Dynamic --
      -----------------------------

      procedure Elaborate_Units_Dynamic (Order : out Unit_Id_Table) is
         Dyn_Inv_Graph : Invocation_Graph;
         Dyn_Lib_Graph : Library_Graph;
         Dyn_Order     : Unit_Id_Table;
         Mix_Inv_Graph : Invocation_Graph;
         Mix_Lib_Graph : Library_Graph;
         Mix_Order     : Unit_Id_Table;
         Status        : Elaboration_Order_Status;

      begin
         --  Attempt to elaborate the units in the library graph by mixing in
         --  the information from the invocation graph. This assumes that all
         --  invocations will take place at elaboration time.

         Elaborate_Units_Common
           (Use_Inv_Graph => True,
            Inv_Graph     => Mix_Inv_Graph,
            Lib_Graph     => Mix_Lib_Graph,
            Order         => Mix_Order,
            Status        => Status);

         --  The elaboration order is satisfactory

         if Status = Order_OK then
            Order := Mix_Order;

         --  The library graph contains an Elaborate_All circularity. There is
         --  no point in re-elaborating the units without the information from
         --  the invocation graph because the circularity will persist.

         elsif Status = Order_Has_Elaborate_All_Circularity then
            Error_Msg ("elaboration circularity detected");

            --  Report error here

         --  Otherwise the library graph contains a circularity, or the extra
         --  information provided by the invocation graph caused a circularity.
         --  Re-elaborate the units without using the invocation graph. This
         --  assumes that all invocations will not take place at elaboration
         --  time.

         else
            pragma Assert (Status = Order_Has_Circularity);

            Elaborate_Units_Common
              (Use_Inv_Graph => False,
               Inv_Graph     => Dyn_Inv_Graph,
               Lib_Graph     => Dyn_Lib_Graph,
               Order         => Dyn_Order,
               Status        => Status);

            --  The elaboration order is satisfactory. The elaboration of the
            --  program may still fail at runtime with an ABE.

            if Status = Order_OK then
               Order := Dyn_Order;

            --  Otherwise the library graph contains a circularity without the
            --  extra information provided by the invocation graph. Diagnose
            --  the circularity.

            else
               Error_Msg ("elaboration circularity detected");

               --  Report error here
            end if;

            Destroy (Dyn_Inv_Graph);
            Destroy (Dyn_Lib_Graph);
         end if;

         Destroy (Mix_Inv_Graph);
         Destroy (Mix_Lib_Graph);

         --  Halt the bind as there is no satisfactory elaboration order

         if Status /= Order_OK then
            raise Unrecoverable_Error;
         end if;
      end Elaborate_Units_Dynamic;

      ----------------------------
      -- Elaborate_Units_Static --
      ----------------------------

      procedure Elaborate_Units_Static (Order : out Unit_Id_Table) is
         Inv_Graph : Invocation_Graph;
         Lib_Graph : Library_Graph;
         Status    : Elaboration_Order_Status;

      begin
         --  Attempt to elaborate the units in the library graph by mixing in
         --  the information from the invocation graph. This assumes that all
         --  invocations will take place at elaboration time.

         Elaborate_Units_Common
           (Use_Inv_Graph => True,
            Inv_Graph     => Inv_Graph,
            Lib_Graph     => Lib_Graph,
            Order         => Order,
            Status        => Status);

         --  The augmented library graph contains a circularity

         if Status /= Order_OK then
            Error_Msg ("elaboration circularity detected");

            --  Report error here
         end if;

         Destroy (Inv_Graph);
         Destroy (Lib_Graph);

         --  Halt the bind as there is no satisfactory elaboration order

         if Status /= Order_OK then
            raise Unrecoverable_Error;
         end if;
      end Elaborate_Units_Static;

      ----------------------
      -- Elaborate_Vertex --
      ----------------------

      procedure Elaborate_Vertex
        (G                  : Library_Graph;
         LGV_Id             : Library_Graph_Vertex_Id;
         All_Candidates     : Membership_Set;
         Comp_Candidates    : Membership_Set;
         Remaining_Vertices : in out Natural;
         Order              : in out Unit_Id_Table;
         Step               : Elaboration_Order_Step;
         Indent             : Indentation_Level)
      is
         Body_LGV_Id : Library_Graph_Vertex_Id;
         U_Id        : Unit_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));
         pragma Assert (Needs_Elaboration (G, LGV_Id));
         pragma Assert (Present (All_Candidates));
         pragma Assert (Present (Comp_Candidates));

         Trace_Vertex
           (G      => G,
            LGV_Id => LGV_Id,
            Msg    => "elaborating vertex",
            Step   => Step,
            Indent => Indent);

         --  Remove the vertex from both candidate sets. This is needed when
         --  the vertex is both an overall best candidate among all vertices,
         --  and the best candidate within the component. There is no need to
         --  check that the vertex is present in either set because the set
         --  implementation handles this case.

         Delete (All_Candidates,  LGV_Id);
         Delete (Comp_Candidates, LGV_Id);

         --  Mark the vertex as elaborated in order to prevent further attempts
         --  to re-elaborate it.

         Set_In_Elaboration_Order (G, LGV_Id);

         --  Add the unit represented by the vertex to the elaboration order

         U_Id := Unit (G, LGV_Id);
         pragma Assert (Present (U_Id));

         Unit_Id_Tables.Append (Order, U_Id);

         --  There is now one fewer vertex to elaborate

         Remaining_Vertices := Remaining_Vertices - 1;

         --  Notify all successors and their components that they have one
         --  fewer predecessor to wait on. This may cause some successors to
         --  be included in one of the sets.

         Update_Successors
           (G               => G,
            Pred            => LGV_Id,
            All_Candidates  => All_Candidates,
            Comp_Candidates => Comp_Candidates,
            Step            => Step,
            Indent          => Indent + Nested_Indentation);

         --  The vertex denotes a spec with a completing body, and is subject
         --  to pragma Elaborate_Body. Elaborate the body in order to satisfy
         --  the semantics of the pragma.

         if Is_Spec_With_Elaborate_Body (G, LGV_Id) then
            Body_LGV_Id := Proper_Body (G, LGV_Id);
            pragma Assert (Present (Body_LGV_Id));

            Elaborate_Vertex
              (G                  => G,
               LGV_Id             => Body_LGV_Id,
               All_Candidates     => All_Candidates,
               Comp_Candidates    => Comp_Candidates,
               Remaining_Vertices => Remaining_Vertices,
               Order              => Order,
               Step               => Step,
               Indent             => Indent);
         end if;
      end Elaborate_Vertex;

      -------------------------
      -- Find_Best_Candidate --
      -------------------------

      function Find_Best_Candidate
        (G      : Library_Graph;
         Set    : Membership_Set;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level) return Library_Graph_Vertex_Id
      is
         Best : Library_Graph_Vertex_Id;
         Curr : Library_Graph_Vertex_Id;
         Iter : Iterator;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Set));

         --  Assume that there is no candidate

         Best := No_Library_Graph_Vertex;

         --  Inspect all vertices in the set, looking for the best candidate to
         --  elaborate.

         Iter := Iterate (Set);
         while Has_Next (Iter) loop
            Next (Iter, Curr);

            pragma Assert (Present (Curr));
            pragma Assert (Needs_Elaboration (G, Curr));

            --  Update the best candidate when there is no such candidate

            if not Present (Best) then
               Best := Curr;

               Trace_Vertex
                 (G      => G,
                  LGV_Id => Best,
                  Msg    => "initial best candidate vertex",
                  Step   => Step,
                  Indent => Indent);

            --  Update the best candidate when the current vertex is a better
            --  choice.

            elsif Is_Better_Candidate
                    (G           => G,
                     Best_Candid => Best,
                     New_Candid  => Curr)
            then
               Best := Curr;

               Trace_Vertex
                 (G      => G,
                  LGV_Id => Best,
                  Msg    => "best candidate vertex",
                  Step   => Step,
                  Indent => Indent);
            end if;
         end loop;

         return Best;
      end Find_Best_Candidate;

      -------------------------
      -- Is_Better_Candidate --
      -------------------------

      function Is_Better_Candidate
        (G           : Library_Graph;
         Best_Candid : Library_Graph_Vertex_Id;
         New_Candid  : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Best_Candid));
         pragma Assert (Present (New_Candid));

         --  Prefer a predefined unit over a non-predefined unit

         if Is_Predefined_Unit (G, Best_Candid)
           and then not Is_Predefined_Unit (G, New_Candid)
         then
            return False;

         elsif not Is_Predefined_Unit (G, Best_Candid)
           and then Is_Predefined_Unit (G, New_Candid)
         then
            return True;

         --  Prefer an internal unit over a non-iternal unit

         elsif Is_Internal_Unit (G, Best_Candid)
           and then not Is_Internal_Unit (G, New_Candid)
         then
            return False;

         elsif not Is_Internal_Unit (G, Best_Candid)
           and then Is_Internal_Unit (G, New_Candid)
         then
            return True;

         --  Prefer a preelaborated unit over a non-preelaborated unit

         elsif Is_Preelaborated_Unit (G, Best_Candid)
           and then not Is_Preelaborated_Unit (G, New_Candid)
         then
            return False;

         elsif not Is_Preelaborated_Unit (G, Best_Candid)
           and then Is_Preelaborated_Unit (G, New_Candid)
         then
            return True;

         --  Otherwise default to lexicographical order to ensure deterministic
         --  behavior.

         else
            return Uname_Less (Name (G, Best_Candid), Name (G, New_Candid));
         end if;
      end Is_Better_Candidate;

      ------------------------------
      -- Trace_Candidate_Vertices --
      ------------------------------

      procedure Trace_Candidate_Vertices
        (G    : Library_Graph;
         Set  : Membership_Set;
         Step : Elaboration_Order_Step)
      is
         Iter   : Iterator;
         LGV_Id : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Set));

         --  Nothing to do when switch -d_T (output elaboration order trace
         --  information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Trace_Step (Step);
         Write_Str  ("candidate vertices: ");
         Write_Int  (Int (Size (Set)));
         Write_Eol;

         Iter := Iterate (Set);
         while Has_Next (Iter) loop
            Next (Iter, LGV_Id);
            pragma Assert (Present (LGV_Id));

            Trace_Vertex
              (G      => G,
               LGV_Id => LGV_Id,
               Msg    => "candidate vertex",
               Step   => Step,
               Indent => Nested_Indentation);
         end loop;
      end Trace_Candidate_Vertices;

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

         --  Nothing to do when switch -d_T (output elaboration order trace
         --  information) is not in effect.

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
         Write_Str  ("pending predecessors: ");
         Write_Num  (Int (Pending_Predecessors (G, Comp)));
         Write_Eol;
      end Trace_Component;

      ----------------
      -- Trace_Step --
      ----------------

      procedure Trace_Step (Step : Elaboration_Order_Step) is
      begin
         --  Nothing to do when switch -d_T (output elaboration order trace
         --  information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Write_Num
           (Val        => Int (Step),
            Val_Indent => Step_Column);
         Write_Str (": ");
      end Trace_Step;

      ---------------------------------
      -- Trace_Unelaborated_Vertices --
      ---------------------------------

      procedure Trace_Unelaborated_Vertices
        (G     : Library_Graph;
         Count : Natural;
         Step  : Elaboration_Order_Step)
      is
         Iter   : Library_Graphs.All_Vertex_Iterator;
         LGV_Id : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -d_T (output elaboration order trace
         --  information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Trace_Step (Step);
         Write_Str  ("remaining unelaborated vertices: ");
         Write_Int  (Int (Count));
         Write_Eol;

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, LGV_Id);
            pragma Assert (Present (LGV_Id));

            if Needs_Elaboration (G, LGV_Id)
              and then not In_Elaboration_Order (G, LGV_Id)
            then
               Trace_Vertex
                 (G      => G,
                  LGV_Id => LGV_Id,
                  Msg    => "remaining vertex",
                  Step   => Step,
                  Indent => Nested_Indentation);
            end if;
         end loop;
      end Trace_Unelaborated_Vertices;

      ------------------
      -- Trace_Vertex --
      ------------------

      procedure Trace_Vertex
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Msg    : String;
         Step   : Elaboration_Order_Step;
         Indent : Indentation_Level)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         Comp : constant Component_Id := Component (G, LGV_Id);

         pragma Assert (Present (Comp));

      begin
         --  Nothing to do when switch -d_T (output elaboration order trace
         --  information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Trace_Step (Step);
         Indent_By  (Indent);
         Write_Str  (Msg);
         Write_Str  (" (LGV_Id_");
         Write_Int  (Int (LGV_Id));
         Write_Str  (")");
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Indent + Nested_Indentation);
         Write_Str  ("name = ");
         Write_Name (Name (G, LGV_Id));
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Indent + Nested_Indentation);
         Write_Str  ("Component (Comp_Id_");
         Write_Int  (Int (Comp));
         Write_Str  (")");
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Indent + Nested_Indentation);
         Write_Str  ("pending predecessors: ");
         Write_Num  (Int (Pending_Predecessors (G, LGV_Id)));
         Write_Eol;

         Trace_Step (Step);
         Indent_By  (Indent + Nested_Indentation);
         Write_Str  ("pending components  : ");
         Write_Num  (Int (Pending_Predecessors (G, Comp)));
         Write_Eol;
      end Trace_Vertex;

      ----------------------
      -- Update_Successor --
      ----------------------

      procedure Update_Successor
        (G               : Library_Graph;
         Pred            : Library_Graph_Vertex_Id;
         Succ            : Library_Graph_Vertex_Id;
         All_Candidates  : Membership_Set;
         Comp_Candidates : Membership_Set;
         Step            : Elaboration_Order_Step;
         Indent          : Indentation_Level)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Pred));
         pragma Assert (Needs_Elaboration (G, Pred));
         pragma Assert (Present (Succ));
         pragma Assert (Needs_Elaboration (G, Succ));
         pragma Assert (Present (All_Candidates));
         pragma Assert (Present (Comp_Candidates));

         Pred_Comp : constant Component_Id := Component (G, Pred);
         Succ_Comp : constant Component_Id := Component (G, Succ);

         pragma Assert (Present (Pred_Comp));
         pragma Assert (Present (Succ_Comp));

         In_Different_Components : constant Boolean := Pred_Comp /= Succ_Comp;

         Candidate : Library_Graph_Vertex_Id;
         Iter      : Component_Vertex_Iterator;
         Msg       : String_Ptr;
         Set       : Membership_Set;

      begin
         Trace_Vertex
           (G      => G,
            LGV_Id => Succ,
            Msg    => "updating successor",
            Step   => Step,
            Indent => Indent);

         --  Notify the successor that it has one less predecessor to wait on.
         --  This effectively eliminates the edge that links the two.

         Decrement_Pending_Predecessors (G, Succ);

         --  The predecessor and successor reside in different components.
         --  Notify the successor component it has one fewer components to
         --  wait on.

         if In_Different_Components then
            Decrement_Pending_Predecessors (G, Succ_Comp);
         end if;

         --  At this point the successor may become elaborable when its final
         --  predecessor or final predecessor component is elaborated.

         --  The predecessor and successor reside in different components.
         --  The successor must not be added to the candidates of Pred's
         --  component because this will mix units from the two components.
         --  Instead, the successor is added to the set of all candidates
         --  that must be elaborated.

         if In_Different_Components then
            Msg := Add_To_All_Candidates_Msg'Access;
            Set := All_Candidates;

         --  Otherwise the predecessor and successor reside within the same
         --  component. Pred's component gains another elaborable node.

         else
            Msg := Add_To_Comp_Candidates_Msg'Access;
            Set := Comp_Candidates;
         end if;

         Add_Vertex_If_Elaborable
           (G      => G,
            LGV_Id => Succ,
            Set    => Set,
            Msg    => Msg.all,
            Step   => Step,
            Indent => Indent + Nested_Indentation);

         --  At this point the successor component may become elaborable when
         --  its final predecessor component is elaborated. This in turn may
         --  allow vertices of the successor component to be elaborated.

         if In_Different_Components
           and then Is_Elaborable_Component (G, Succ_Comp)
         then
            Iter := Iterate_Component_Vertices (G, Succ_Comp);
            while Has_Next (Iter) loop
               Next (Iter, Candidate);
               pragma Assert (Present (Candidate));

               Add_Vertex_If_Elaborable
                 (G      => G,
                  LGV_Id => Candidate,
                  Set    => All_Candidates,
                  Msg    => Add_To_All_Candidates_Msg,
                  Step   => Step,
                  Indent => Indent + Nested_Indentation);
            end loop;
         end if;
      end Update_Successor;

      -----------------------
      -- Update_Successors --
      -----------------------

      procedure Update_Successors
        (G               : Library_Graph;
         Pred            : Library_Graph_Vertex_Id;
         All_Candidates  : Membership_Set;
         Comp_Candidates : Membership_Set;
         Step            : Elaboration_Order_Step;
         Indent          : Indentation_Level)
      is
         Iter   : Edges_To_Successors_Iterator;
         LGE_Id : Library_Graph_Edge_Id;
         Succ   : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Pred));
         pragma Assert (Needs_Elaboration (G, Pred));
         pragma Assert (Present (All_Candidates));
         pragma Assert (Present (Comp_Candidates));

         Iter := Iterate_Edges_To_Successors (G, Pred);
         while Has_Next (Iter) loop
            Next (Iter, LGE_Id);

            pragma Assert (Present (LGE_Id));
            pragma Assert (Predecessor (G, LGE_Id) = Pred);

            Succ := Successor (G, LGE_Id);
            pragma Assert (Present (Succ));

            Update_Successor
              (G               => G,
               Pred            => Pred,
               Succ            => Succ,
               All_Candidates  => All_Candidates,
               Comp_Candidates => Comp_Candidates,
               Step            => Step,
               Indent          => Indent);
         end loop;
      end Update_Successors;
   end Invocation_And_Library_Graph_Elaborators;

end Bindo.Elaborators;
