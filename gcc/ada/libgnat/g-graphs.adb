------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           G N A T . G R A P H S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2018-2020, Free Software Foundation, Inc.      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body GNAT.Graphs is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Sequence_Next_Component return Component_Id;
   --  Produce the next handle for a component. The handle is guaranteed to be
   --  unique across all graphs.

   --------------------
   -- Directed_Graph --
   --------------------

   package body Directed_Graphs is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Add_Component
        (G        : Directed_Graph;
         Comp     : Component_Id;
         Vertices : Vertex_List.Doubly_Linked_List);
      pragma Inline (Add_Component);
      --  Add component Comp which houses vertices Vertices to graph G

      procedure Ensure_Created (G : Directed_Graph);
      pragma Inline (Ensure_Created);
      --  Verify that graph G is created. Raise Not_Created if this is not the
      --  case.

      procedure Ensure_Not_Present
        (G : Directed_Graph;
         E : Edge_Id);
      pragma Inline (Ensure_Not_Present);
      --  Verify that graph G lacks edge E. Raise Duplicate_Edge if this is not
      --  the case.

      procedure Ensure_Not_Present
        (G : Directed_Graph;
         V : Vertex_Id);
      pragma Inline (Ensure_Not_Present);
      --  Verify that graph G lacks vertex V. Raise Duplicate_Vertex if this is
      --  not the case.

      procedure Ensure_Present
        (G    : Directed_Graph;
         Comp : Component_Id);
      pragma Inline (Ensure_Present);
      --  Verify that component Comp exists in graph G. Raise Missing_Component
      --  if this is not the case.

      procedure Ensure_Present
        (G : Directed_Graph;
         E : Edge_Id);
      pragma Inline (Ensure_Present);
      --  Verify that edge E is present in graph G. Raise Missing_Edge if this
      --  is not the case.

      procedure Ensure_Present
        (G : Directed_Graph;
         V : Vertex_Id);
      pragma Inline (Ensure_Present);
      --  Verify that vertex V is present in graph G. Raise Missing_Vertex if
      --  this is not the case.

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Directed_Graph_Attributes, Directed_Graph);

      function Get_Component_Attributes
        (G    : Directed_Graph;
         Comp : Component_Id) return Component_Attributes;
      pragma Inline (Get_Component_Attributes);
      --  Obtain the attributes of component Comp of graph G

      function Get_Edge_Attributes
        (G : Directed_Graph;
         E : Edge_Id) return Edge_Attributes;
      pragma Inline (Get_Edge_Attributes);
      --  Obtain the attributes of edge E of graph G

      function Get_Vertex_Attributes
        (G : Directed_Graph;
         V : Vertex_Id) return Vertex_Attributes;
      pragma Inline (Get_Vertex_Attributes);
      --  Obtain the attributes of vertex V of graph G

      function Get_Outgoing_Edges
        (G : Directed_Graph;
         V : Vertex_Id) return Edge_Set.Membership_Set;
      pragma Inline (Get_Outgoing_Edges);
      --  Obtain the Outgoing_Edges attribute of vertex V of graph G

      function Get_Vertices
        (G    : Directed_Graph;
         Comp : Component_Id) return Vertex_List.Doubly_Linked_List;
      pragma Inline (Get_Vertices);
      --  Obtain the Vertices attribute of component Comp of graph G

      procedure Set_Component
        (G   : Directed_Graph;
         V   : Vertex_Id;
         Val : Component_Id);
      pragma Inline (Set_Component);
      --  Set attribute Component of vertex V of graph G to value Val

      procedure Set_Outgoing_Edges
        (G   : Directed_Graph;
         V   : Vertex_Id;
         Val : Edge_Set.Membership_Set);
      pragma Inline (Set_Outgoing_Edges);
      --  Set attribute Outgoing_Edges of vertex V of graph G to value Val

      procedure Set_Vertex_Attributes
        (G   : Directed_Graph;
         V   : Vertex_Id;
         Val : Vertex_Attributes);
      pragma Inline (Set_Vertex_Attributes);
      --  Set the attributes of vertex V of graph G to value Val

      -------------------
      -- Add_Component --
      -------------------

      procedure Add_Component
        (G        : Directed_Graph;
         Comp     : Component_Id;
         Vertices : Vertex_List.Doubly_Linked_List)
      is
      begin
         pragma Assert (Present (G));

         --  Add the component to the set of all components in the graph

         Component_Map.Put
           (T     => G.Components,
            Key   => Comp,
            Value => (Vertices => Vertices));
      end Add_Component;

      --------------
      -- Add_Edge --
      --------------

      procedure Add_Edge
        (G           : Directed_Graph;
         E           : Edge_Id;
         Source      : Vertex_Id;
         Destination : Vertex_Id)
      is
      begin
         Ensure_Created (G);
         Ensure_Not_Present (G, E);
         Ensure_Present (G, Source);
         Ensure_Present (G, Destination);

         --  Add the edge to the set of all edges in the graph

         Edge_Map.Put
           (T     => G.All_Edges,
            Key   => E,
            Value =>
              (Destination => Destination,
               Source      => Source));

         --  Associate the edge with its source vertex which effectively "owns"
         --  the edge.

         Edge_Set.Insert
           (S    => Get_Outgoing_Edges (G, Source),
            Elem => E);
      end Add_Edge;

      ----------------
      -- Add_Vertex --
      ----------------

      procedure Add_Vertex
        (G : Directed_Graph;
         V : Vertex_Id)
      is
      begin
         Ensure_Created (G);
         Ensure_Not_Present (G, V);

         --  Add the vertex to the set of all vertices in the graph

         Vertex_Map.Put
           (T     => G.All_Vertices,
            Key   => V,
            Value =>
              (Component      => No_Component,
               Outgoing_Edges => Edge_Set.Nil));

         --  It is assumed that the vertex will have at least one outgoing
         --  edge. It is important not to create the set of edges above as
         --  the call to Put may fail in case the vertices are iterated.
         --  This would lead to a memory leak because the set would not be
         --  reclaimed.

         Set_Outgoing_Edges (G, V, Edge_Set.Create (1));
      end Add_Vertex;

      ---------------
      -- Component --
      ---------------

      function Component
        (G : Directed_Graph;
         V : Vertex_Id) return Component_Id
      is
      begin
         Ensure_Created (G);
         Ensure_Present (G, V);

         return Get_Vertex_Attributes (G, V).Component;
      end Component;

      ------------------------
      -- Contains_Component --
      ------------------------

      function Contains_Component
        (G    : Directed_Graph;
         Comp : Component_Id) return Boolean
      is
      begin
         Ensure_Created (G);

         return Component_Map.Contains (G.Components, Comp);
      end Contains_Component;

      -------------------
      -- Contains_Edge --
      -------------------

      function Contains_Edge
        (G : Directed_Graph;
         E : Edge_Id) return Boolean
      is
      begin
         Ensure_Created (G);

         return Edge_Map.Contains (G.All_Edges, E);
      end Contains_Edge;

      ---------------------
      -- Contains_Vertex --
      ---------------------

      function Contains_Vertex
        (G : Directed_Graph;
         V : Vertex_Id) return Boolean
      is
      begin
         Ensure_Created (G);

         return Vertex_Map.Contains (G.All_Vertices, V);
      end Contains_Vertex;

      ------------
      -- Create --
      ------------

      function Create
        (Initial_Vertices : Positive;
         Initial_Edges    : Positive) return Directed_Graph
      is
         G : constant Directed_Graph := new Directed_Graph_Attributes;

      begin
         G.All_Edges    := Edge_Map.Create      (Initial_Edges);
         G.All_Vertices := Vertex_Map.Create    (Initial_Vertices);
         G.Components   := Component_Map.Create (Initial_Vertices);

         return G;
      end Create;

      -----------------
      -- Delete_Edge --
      -----------------

      procedure Delete_Edge
        (G : Directed_Graph;
         E : Edge_Id)
      is
         Source : Vertex_Id;

      begin
         Ensure_Created (G);
         Ensure_Present (G, E);

         Source := Source_Vertex (G, E);
         Ensure_Present (G, Source);

         --  Delete the edge from its source vertex which effectively "owns"
         --  the edge.

         Edge_Set.Delete (Get_Outgoing_Edges (G, Source), E);

         --  Delete the edge from the set of all edges

         Edge_Map.Delete (G.All_Edges, E);
      end Delete_Edge;

      ------------------------
      -- Destination_Vertex --
      ------------------------

      function Destination_Vertex
        (G : Directed_Graph;
         E : Edge_Id) return Vertex_Id
      is
      begin
         Ensure_Created (G);
         Ensure_Present (G, E);

         return Get_Edge_Attributes (G, E).Destination;
      end Destination_Vertex;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (G : in out Directed_Graph) is
      begin
         Ensure_Created (G);

         Edge_Map.Destroy      (G.All_Edges);
         Vertex_Map.Destroy    (G.All_Vertices);
         Component_Map.Destroy (G.Components);

         Free (G);
      end Destroy;

      ----------------------------------
      -- Destroy_Component_Attributes --
      ----------------------------------

      procedure Destroy_Component_Attributes
        (Attrs : in out Component_Attributes)
      is
      begin
         Vertex_List.Destroy (Attrs.Vertices);
      end Destroy_Component_Attributes;

      -----------------------------
      -- Destroy_Edge_Attributes --
      -----------------------------

      procedure Destroy_Edge_Attributes (Attrs : in out Edge_Attributes) is
         pragma Unreferenced (Attrs);
      begin
         null;
      end Destroy_Edge_Attributes;

      --------------------
      -- Destroy_Vertex --
      --------------------

      procedure Destroy_Vertex (V : in out Vertex_Id) is
         pragma Unreferenced (V);
      begin
         null;
      end Destroy_Vertex;

      -------------------------------
      -- Destroy_Vertex_Attributes --
      -------------------------------

      procedure Destroy_Vertex_Attributes (Attrs : in out Vertex_Attributes) is
      begin
         Edge_Set.Destroy (Attrs.Outgoing_Edges);
      end Destroy_Vertex_Attributes;

      --------------------
      -- Ensure_Created --
      --------------------

      procedure Ensure_Created (G : Directed_Graph) is
      begin
         if not Present (G) then
            raise Not_Created;
         end if;
      end Ensure_Created;

      ------------------------
      -- Ensure_Not_Present --
      ------------------------

      procedure Ensure_Not_Present
        (G : Directed_Graph;
         E : Edge_Id)
      is
      begin
         if Contains_Edge (G, E) then
            raise Duplicate_Edge;
         end if;
      end Ensure_Not_Present;

      ------------------------
      -- Ensure_Not_Present --
      ------------------------

      procedure Ensure_Not_Present
        (G : Directed_Graph;
         V : Vertex_Id)
      is
      begin
         if Contains_Vertex (G, V) then
            raise Duplicate_Vertex;
         end if;
      end Ensure_Not_Present;

      --------------------
      -- Ensure_Present --
      --------------------

      procedure Ensure_Present
        (G    : Directed_Graph;
         Comp : Component_Id)
      is
      begin
         if not Contains_Component (G, Comp) then
            raise Missing_Component;
         end if;
      end Ensure_Present;

      --------------------
      -- Ensure_Present --
      --------------------

      procedure Ensure_Present
        (G : Directed_Graph;
         E : Edge_Id)
      is
      begin
         if not Contains_Edge (G, E) then
            raise Missing_Edge;
         end if;
      end Ensure_Present;

      --------------------
      -- Ensure_Present --
      --------------------

      procedure Ensure_Present
        (G : Directed_Graph;
         V : Vertex_Id)
      is
      begin
         if not Contains_Vertex (G, V) then
            raise Missing_Vertex;
         end if;
      end Ensure_Present;

      ---------------------
      -- Find_Components --
      ---------------------

      procedure Find_Components (G : Directed_Graph) is

         --  The components of graph G are discovered using Tarjan's strongly
         --  connected component algorithm. Do not modify this code unless you
         --  intimately understand the algorithm.

         ----------------
         -- Tarjan_Map --
         ----------------

         type Visitation_Number is new Natural;
         No_Visitation_Number    : constant Visitation_Number :=
                                      Visitation_Number'First;
         First_Visitation_Number : constant Visitation_Number :=
                                     No_Visitation_Number + 1;

         type Tarjan_Attributes is record
            Index : Visitation_Number := No_Visitation_Number;
            --  Visitation number

            Low_Link : Visitation_Number := No_Visitation_Number;
            --  Lowest visitation number

            On_Stack : Boolean := False;
            --  Set when the corresponding vertex appears on the Stack
         end record;

         No_Tarjan_Attributes : constant Tarjan_Attributes :=
           (Index    => No_Visitation_Number,
            Low_Link => No_Visitation_Number,
            On_Stack => False);

         procedure Destroy_Tarjan_Attributes
           (Attrs : in out Tarjan_Attributes);
         --  Destroy the contents of attributes Attrs

         package Tarjan_Map is new Dynamic_Hash_Tables
           (Key_Type              => Vertex_Id,
            Value_Type            => Tarjan_Attributes,
            No_Value              => No_Tarjan_Attributes,
            Expansion_Threshold   => 1.5,
            Expansion_Factor      => 2,
            Compression_Threshold => 0.3,
            Compression_Factor    => 2,
            "="                   => Same_Vertex,
            Destroy_Value         => Destroy_Tarjan_Attributes,
            Hash                  => Hash_Vertex);

         ------------------
         -- Tarjan_Stack --
         ------------------

         package Tarjan_Stack is new Doubly_Linked_Lists
           (Element_Type    => Vertex_Id,
            "="             => Same_Vertex,
            Destroy_Element => Destroy_Vertex);

         -----------------
         -- Global data --
         -----------------

         Attrs : Tarjan_Map.Dynamic_Hash_Table   := Tarjan_Map.Nil;
         Stack : Tarjan_Stack.Doubly_Linked_List := Tarjan_Stack.Nil;

         -----------------------
         -- Local subprograms --
         -----------------------

         procedure Associate_All_Vertices;
         pragma Inline (Associate_All_Vertices);
         --  Associate all vertices in the graph with the corresponding
         --  components that house them.

         procedure Associate_Vertices (Comp : Component_Id);
         pragma Inline (Associate_Vertices);
         --  Associate all vertices of component Comp with the component

         procedure Create_Component (V : Vertex_Id);
         pragma Inline (Create_Component);
         --  Create a new component with root vertex V

         function Get_Tarjan_Attributes
           (V : Vertex_Id) return Tarjan_Attributes;
         pragma Inline (Get_Tarjan_Attributes);
         --  Obtain the Tarjan attributes of vertex V

         function Index (V : Vertex_Id) return Visitation_Number;
         pragma Inline (Index);
         --  Obtain the Index attribute of vertex V

         procedure Initialize_Components;
         pragma Inline (Initialize_Components);
         --  Initialize or reinitialize the components of the graph

         function Is_Visited (V : Vertex_Id) return Boolean;
         pragma Inline (Is_Visited);
         --  Determine whether vertex V has been visited

         function Low_Link (V : Vertex_Id) return Visitation_Number;
         pragma Inline (Low_Link);
         --  Obtain the Low_Link attribute of vertex V

         function On_Stack (V : Vertex_Id) return Boolean;
         pragma Inline (On_Stack);
         --  Obtain the On_Stack attribute of vertex V

         function Pop return Vertex_Id;
         pragma Inline (Pop);
         --  Pop a vertex off Stack

         procedure Push (V : Vertex_Id);
         pragma Inline (Push);
         --  Push vertex V on Stack

         procedure Record_Visit (V : Vertex_Id);
         pragma Inline (Record_Visit);
         --  Save the visitation of vertex V by setting relevant attributes

         function Sequence_Next_Index return Visitation_Number;
         pragma Inline (Sequence_Next_Index);
         --  Procedure the next visitation number of the DFS traversal

         procedure Set_Index
           (V   : Vertex_Id;
            Val : Visitation_Number);
         pragma Inline (Set_Index);
         --  Set attribute Index of vertex V to value Val

         procedure Set_Low_Link
           (V   : Vertex_Id;
            Val : Visitation_Number);
         pragma Inline (Set_Low_Link);
         --  Set attribute Low_Link of vertex V to value Val

         procedure Set_On_Stack
           (V   : Vertex_Id;
            Val : Boolean);
         pragma Inline (Set_On_Stack);
         --  Set attribute On_Stack of vertex V to value Val

         procedure Set_Tarjan_Attributes
           (V   : Vertex_Id;
            Val : Tarjan_Attributes);
         pragma Inline (Set_Tarjan_Attributes);
         --  Set the attributes of vertex V to value Val

         procedure Visit_Successors (V : Vertex_Id);
         pragma Inline (Visit_Successors);
         --  Visit the successors of vertex V

         procedure Visit_Vertex (V : Vertex_Id);
         pragma Inline (Visit_Vertex);
         --  Visit single vertex V

         procedure Visit_Vertices;
         pragma Inline (Visit_Vertices);
         --  Visit all vertices in the graph

         ----------------------------
         -- Associate_All_Vertices --
         ----------------------------

         procedure Associate_All_Vertices is
            Comp : Component_Id;
            Iter : Component_Iterator;

         begin
            Iter := Iterate_Components (G);
            while Has_Next (Iter) loop
               Next (Iter, Comp);

               Associate_Vertices (Comp);
            end loop;
         end Associate_All_Vertices;

         ------------------------
         -- Associate_Vertices --
         ------------------------

         procedure Associate_Vertices (Comp : Component_Id) is
            Iter : Component_Vertex_Iterator;
            V    : Vertex_Id;

         begin
            Iter := Iterate_Component_Vertices (G, Comp);
            while Has_Next (Iter) loop
               Next (Iter, V);

               Set_Component (G, V, Comp);
            end loop;
         end Associate_Vertices;

         ----------------------
         -- Create_Component --
         ----------------------

         procedure Create_Component (V : Vertex_Id) is
            Curr_V   : Vertex_Id;
            Vertices : Vertex_List.Doubly_Linked_List;

         begin
            Vertices := Vertex_List.Create;

            --  Collect all vertices that comprise the current component by
            --  popping the stack until reaching the root vertex V.

            loop
               Curr_V := Pop;
               Vertex_List.Append (Vertices, Curr_V);

               exit when Same_Vertex (Curr_V, V);
            end loop;

            Add_Component
              (G        => G,
               Comp     => Sequence_Next_Component,
               Vertices => Vertices);
         end Create_Component;

         -------------------------------
         -- Destroy_Tarjan_Attributes --
         -------------------------------

         procedure Destroy_Tarjan_Attributes
           (Attrs : in out Tarjan_Attributes)
         is
            pragma Unreferenced (Attrs);
         begin
            null;
         end Destroy_Tarjan_Attributes;

         ---------------------------
         -- Get_Tarjan_Attributes --
         ---------------------------

         function Get_Tarjan_Attributes
           (V : Vertex_Id) return Tarjan_Attributes
         is
         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            return Tarjan_Map.Get (Attrs, V);
         end Get_Tarjan_Attributes;

         -----------
         -- Index --
         -----------

         function Index (V : Vertex_Id) return Visitation_Number is
         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            return Get_Tarjan_Attributes (V).Index;
         end Index;

         ---------------------------
         -- Initialize_Components --
         ---------------------------

         procedure Initialize_Components is
         begin
            pragma Assert (Present (G));

            --  The graph already contains a set of components. Reinitialize
            --  them in order to accommodate the new set of components about to
            --  be computed.

            if Number_Of_Components (G) > 0 then
               Component_Map.Destroy (G.Components);
               G.Components := Component_Map.Create (Number_Of_Vertices (G));
            end if;
         end Initialize_Components;

         ----------------
         -- Is_Visited --
         ----------------

         function Is_Visited (V : Vertex_Id) return Boolean is
         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            return Index (V) /= No_Visitation_Number;
         end Is_Visited;

         --------------
         -- Low_Link --
         --------------

         function Low_Link (V : Vertex_Id) return Visitation_Number is
         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            return Get_Tarjan_Attributes (V).Low_Link;
         end Low_Link;

         --------------
         -- On_Stack --
         --------------

         function On_Stack (V : Vertex_Id) return Boolean is
         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            return Get_Tarjan_Attributes (V).On_Stack;
         end On_Stack;

         ---------
         -- Pop --
         ---------

         function Pop return Vertex_Id is
            V : Vertex_Id;

         begin
            V := Tarjan_Stack.Last (Stack);
            Tarjan_Stack.Delete_Last (Stack);
            Set_On_Stack (V, False);

            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            return V;
         end Pop;

         ----------
         -- Push --
         ----------

         procedure Push (V : Vertex_Id) is
         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            Tarjan_Stack.Append (Stack, V);
            Set_On_Stack (V, True);
         end Push;

         ------------------
         -- Record_Visit --
         ------------------

         procedure Record_Visit (V : Vertex_Id) is
            Index : constant Visitation_Number := Sequence_Next_Index;

         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            Set_Index    (V, Index);
            Set_Low_Link (V, Index);
         end Record_Visit;

         -------------------------
         -- Sequence_Next_Index --
         -------------------------

         Index_Sequencer : Visitation_Number := First_Visitation_Number;
         --  The counter for visitation numbers. Do not directly manipulate its
         --  value because this will destroy the Index and Low_Link invariants
         --  of the algorithm.

         function Sequence_Next_Index return Visitation_Number is
            Index : constant Visitation_Number := Index_Sequencer;

         begin
            Index_Sequencer := Index_Sequencer + 1;
            return Index;
         end Sequence_Next_Index;

         ---------------
         -- Set_Index --
         ---------------

         procedure Set_Index
           (V   : Vertex_Id;
            Val : Visitation_Number)
         is
            TA : Tarjan_Attributes;

         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            TA := Get_Tarjan_Attributes (V);
            TA.Index := Val;
            Set_Tarjan_Attributes (V, TA);
         end Set_Index;

         ------------------
         -- Set_Low_Link --
         ------------------

         procedure Set_Low_Link
           (V   : Vertex_Id;
            Val : Visitation_Number)
         is
            TA : Tarjan_Attributes;

         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            TA := Get_Tarjan_Attributes (V);
            TA.Low_Link := Val;
            Set_Tarjan_Attributes (V, TA);
         end Set_Low_Link;

         ------------------
         -- Set_On_Stack --
         ------------------

         procedure Set_On_Stack
           (V   : Vertex_Id;
            Val : Boolean)
         is
            TA : Tarjan_Attributes;

         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            TA := Get_Tarjan_Attributes (V);
            TA.On_Stack := Val;
            Set_Tarjan_Attributes (V, TA);
         end Set_On_Stack;

         ---------------------------
         -- Set_Tarjan_Attributes --
         ---------------------------

         procedure Set_Tarjan_Attributes
           (V   : Vertex_Id;
            Val : Tarjan_Attributes)
         is
         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            Tarjan_Map.Put (Attrs, V, Val);
         end Set_Tarjan_Attributes;

         ----------------------
         -- Visit_Successors --
         ----------------------

         procedure Visit_Successors (V : Vertex_Id) is
            E    : Edge_Id;
            Iter : Outgoing_Edge_Iterator;
            Succ : Vertex_Id;

         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            Iter := Iterate_Outgoing_Edges (G, V);
            while Has_Next (Iter) loop
               Next (Iter, E);

               Succ := Destination_Vertex (G, E);
               pragma Assert (Contains_Vertex (G, Succ));

               --  The current successor has not been visited yet. Extend the
               --  DFS traversal into it.

               if not Is_Visited (Succ) then
                  Visit_Vertex (Succ);

                  Set_Low_Link (V,
                    Visitation_Number'Min (Low_Link (V), Low_Link (Succ)));

               --  The current successor has been visited, and still remains on
               --  the stack which indicates that it does not participate in a
               --  component yet.

               elsif On_Stack (Succ) then
                  Set_Low_Link (V,
                    Visitation_Number'Min (Low_Link (V), Index (Succ)));
               end if;
            end loop;
         end Visit_Successors;

         ------------------
         -- Visit_Vertex --
         ------------------

         procedure Visit_Vertex (V : Vertex_Id) is
         begin
            pragma Assert (Present (G));
            pragma Assert (Contains_Vertex (G, V));

            if not Is_Visited (V) then
               Record_Visit     (V);
               Push             (V);
               Visit_Successors (V);

               --  The current vertex is the root of a component

               if Low_Link (V) = Index (V) then
                  Create_Component (V);
               end if;
            end if;
         end Visit_Vertex;

         --------------------
         -- Visit_Vertices --
         --------------------

         procedure Visit_Vertices is
            Iter : All_Vertex_Iterator;
            V    : Vertex_Id;

         begin
            Iter := Iterate_All_Vertices (G);
            while Has_Next (Iter) loop
               Next (Iter, V);

               Visit_Vertex (V);
            end loop;
         end Visit_Vertices;

      --  Start of processing for Find_Components

      begin
         --  Initialize or reinitialize the components of the graph

         Initialize_Components;

         --  Prepare the extra attributes needed for each vertex, global
         --  visitation number, and the stack where examined vertices are
         --  placed.

         Attrs := Tarjan_Map.Create (Number_Of_Vertices (G));
         Stack := Tarjan_Stack.Create;

         --  Start the DFS traversal of Tarjan's SCC algorithm

         Visit_Vertices;

         Tarjan_Map.Destroy   (Attrs);
         Tarjan_Stack.Destroy (Stack);

         --  Associate each vertex with the component it belongs to

         Associate_All_Vertices;
      end Find_Components;

      ------------------------------
      -- Get_Component_Attributes --
      ------------------------------

      function Get_Component_Attributes
        (G    : Directed_Graph;
         Comp : Component_Id) return Component_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Contains_Component (G, Comp));

         return Component_Map.Get (G.Components, Comp);
      end Get_Component_Attributes;

      -------------------------
      -- Get_Edge_Attributes --
      -------------------------

      function Get_Edge_Attributes
        (G : Directed_Graph;
         E : Edge_Id) return Edge_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Contains_Edge (G, E));

         return Edge_Map.Get (G.All_Edges, E);
      end Get_Edge_Attributes;

      ---------------------------
      -- Get_Vertex_Attributes --
      ---------------------------

      function Get_Vertex_Attributes
        (G : Directed_Graph;
         V : Vertex_Id) return Vertex_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Contains_Vertex (G, V));

         return Vertex_Map.Get (G.All_Vertices, V);
      end Get_Vertex_Attributes;

      ------------------------
      -- Get_Outgoing_Edges --
      ------------------------

      function Get_Outgoing_Edges
        (G : Directed_Graph;
         V : Vertex_Id) return Edge_Set.Membership_Set
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Contains_Vertex (G, V));

         return Get_Vertex_Attributes (G, V).Outgoing_Edges;
      end Get_Outgoing_Edges;

      ------------------
      -- Get_Vertices --
      ------------------

      function Get_Vertices
        (G    : Directed_Graph;
         Comp : Component_Id) return Vertex_List.Doubly_Linked_List
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Contains_Component (G, Comp));

         return Get_Component_Attributes (G, Comp).Vertices;
      end Get_Vertices;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : All_Edge_Iterator) return Boolean is
      begin
         return Edge_Map.Has_Next (Edge_Map.Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : All_Vertex_Iterator) return Boolean is
      begin
         return Vertex_Map.Has_Next (Vertex_Map.Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Component_Iterator) return Boolean is
      begin
         return Component_Map.Has_Next (Component_Map.Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Component_Vertex_Iterator) return Boolean is
      begin
         return Vertex_List.Has_Next (Vertex_List.Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Outgoing_Edge_Iterator) return Boolean is
      begin
         return Edge_Set.Has_Next (Edge_Set.Iterator (Iter));
      end Has_Next;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (G : Directed_Graph) return Boolean is
      begin
         Ensure_Created (G);

         return
           Edge_Map.Is_Empty (G.All_Edges)
             and then Vertex_Map.Is_Empty (G.All_Vertices);
      end Is_Empty;

      -----------------------
      -- Iterate_All_Edges --
      -----------------------

      function Iterate_All_Edges
        (G : Directed_Graph) return All_Edge_Iterator
      is
      begin
         Ensure_Created (G);

         return All_Edge_Iterator (Edge_Map.Iterate (G.All_Edges));
      end Iterate_All_Edges;

      --------------------------
      -- Iterate_All_Vertices --
      --------------------------

      function Iterate_All_Vertices
        (G : Directed_Graph) return All_Vertex_Iterator
      is
      begin
         Ensure_Created (G);

         return All_Vertex_Iterator (Vertex_Map.Iterate (G.All_Vertices));
      end Iterate_All_Vertices;

      ------------------------
      -- Iterate_Components --
      ------------------------

      function Iterate_Components
        (G : Directed_Graph) return Component_Iterator
      is
      begin
         Ensure_Created (G);

         return Component_Iterator (Component_Map.Iterate (G.Components));
      end Iterate_Components;

      --------------------------------
      -- Iterate_Component_Vertices --
      --------------------------------

      function Iterate_Component_Vertices
        (G    : Directed_Graph;
         Comp : Component_Id) return Component_Vertex_Iterator
      is
      begin
         Ensure_Created (G);
         Ensure_Present (G, Comp);

         return
           Component_Vertex_Iterator
             (Vertex_List.Iterate (Get_Vertices (G, Comp)));
      end Iterate_Component_Vertices;

      ----------------------------
      -- Iterate_Outgoing_Edges --
      ----------------------------

      function Iterate_Outgoing_Edges
        (G : Directed_Graph;
         V : Vertex_Id) return Outgoing_Edge_Iterator
      is
      begin
         Ensure_Created (G);
         Ensure_Present (G, V);

         return
           Outgoing_Edge_Iterator
             (Edge_Set.Iterate (Get_Outgoing_Edges (G, V)));
      end Iterate_Outgoing_Edges;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out All_Edge_Iterator;
         E    : out Edge_Id)
      is
      begin
         Edge_Map.Next (Edge_Map.Iterator (Iter), E);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out All_Vertex_Iterator;
         V    : out Vertex_Id)
      is
      begin
         Vertex_Map.Next (Vertex_Map.Iterator (Iter), V);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Component_Iterator;
         Comp : out Component_Id)
      is
      begin
         Component_Map.Next (Component_Map.Iterator (Iter), Comp);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Component_Vertex_Iterator;
         V    : out Vertex_Id)
      is
      begin
         Vertex_List.Next (Vertex_List.Iterator (Iter), V);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Outgoing_Edge_Iterator;
         E    : out Edge_Id)
      is
      begin
         Edge_Set.Next (Edge_Set.Iterator (Iter), E);
      end Next;

      ----------------------------------
      -- Number_Of_Component_Vertices --
      ----------------------------------

      function Number_Of_Component_Vertices
        (G    : Directed_Graph;
         Comp : Component_Id) return Natural
      is
      begin
         Ensure_Created (G);
         Ensure_Present (G, Comp);

         return Vertex_List.Size (Get_Vertices (G, Comp));
      end Number_Of_Component_Vertices;

      --------------------------
      -- Number_Of_Components --
      --------------------------

      function Number_Of_Components (G : Directed_Graph) return Natural is
      begin
         Ensure_Created (G);

         return Component_Map.Size (G.Components);
      end Number_Of_Components;

      ---------------------
      -- Number_Of_Edges --
      ---------------------

      function Number_Of_Edges (G : Directed_Graph) return Natural is
      begin
         Ensure_Created (G);

         return Edge_Map.Size (G.All_Edges);
      end Number_Of_Edges;

      ------------------------------
      -- Number_Of_Outgoing_Edges --
      ------------------------------

      function Number_Of_Outgoing_Edges
        (G : Directed_Graph;
         V : Vertex_Id) return Natural
      is
      begin
         Ensure_Created (G);
         Ensure_Present (G, V);

         return Edge_Set.Size (Get_Outgoing_Edges (G, V));
      end Number_Of_Outgoing_Edges;

      ------------------------
      -- Number_Of_Vertices --
      ------------------------

      function Number_Of_Vertices (G : Directed_Graph) return Natural is
      begin
         Ensure_Created (G);

         return Vertex_Map.Size (G.All_Vertices);
      end Number_Of_Vertices;

      -------------
      -- Present --
      -------------

      function Present (G : Directed_Graph) return Boolean is
      begin
         return G /= Nil;
      end Present;

      -------------------
      -- Set_Component --
      -------------------

      procedure Set_Component
        (G   : Directed_Graph;
         V   : Vertex_Id;
         Val : Component_Id)
      is
         VA : Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Contains_Vertex (G, V));

         VA := Get_Vertex_Attributes (G, V);
         VA.Component := Val;
         Set_Vertex_Attributes (G, V, VA);
      end Set_Component;

      ------------------------
      -- Set_Outgoing_Edges --
      ------------------------

      procedure Set_Outgoing_Edges
        (G   : Directed_Graph;
         V   : Vertex_Id;
         Val : Edge_Set.Membership_Set)
      is
         VA : Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Contains_Vertex (G, V));

         VA := Get_Vertex_Attributes (G, V);
         VA.Outgoing_Edges := Val;
         Set_Vertex_Attributes (G, V, VA);
      end Set_Outgoing_Edges;

      ---------------------------
      -- Set_Vertex_Attributes --
      ---------------------------

      procedure Set_Vertex_Attributes
        (G   : Directed_Graph;
         V   : Vertex_Id;
         Val : Vertex_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Contains_Vertex (G, V));

         Vertex_Map.Put (G.All_Vertices, V, Val);
      end Set_Vertex_Attributes;

      -------------------
      -- Source_Vertex --
      -------------------

      function Source_Vertex
        (G : Directed_Graph;
         E : Edge_Id) return Vertex_Id
      is
      begin
         Ensure_Created (G);
         Ensure_Present (G, E);

         return Get_Edge_Attributes (G, E).Source;
      end Source_Vertex;
   end Directed_Graphs;

   --------------------
   -- Hash_Component --
   --------------------

   function Hash_Component (Comp : Component_Id) return Bucket_Range_Type is
   begin
      return Bucket_Range_Type (Comp);
   end Hash_Component;

   -------------
   -- Present --
   -------------

   function Present (Comp : Component_Id) return Boolean is
   begin
      return Comp /= No_Component;
   end Present;

   -----------------------------
   -- Sequence_Next_Component --
   -----------------------------

   Component_Sequencer : Component_Id := First_Component;
   --  The counter for component handles. Do not directly manipulate its value
   --  because this will destroy the invariant of the handles.

   function Sequence_Next_Component return Component_Id is
      Component : constant Component_Id := Component_Sequencer;

   begin
      Component_Sequencer := Component_Sequencer + 1;
      return Component;
   end Sequence_Next_Component;

end GNAT.Graphs;
