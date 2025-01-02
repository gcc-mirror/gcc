------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           G N A T . G R A P H S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2018-2025, Free Software Foundation, Inc.      --
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

--  Note: this unit is used during bootstrap, see ADA_GENERATED_FILES in
--  gcc-interface/Make-lang.in for details on the constraints.

with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;
with GNAT.Lists;           use GNAT.Lists;
with GNAT.Sets;            use GNAT.Sets;

package GNAT.Graphs is

   ---------------
   -- Component --
   ---------------

   --  The following type denotes a strongly connected component handle
   --  (referred to as simply "component") in a graph.

   type Component_Id is new Natural;
   No_Component : constant Component_Id := Component_Id'First;

   function Hash_Component (Comp : Component_Id) return Bucket_Range_Type;
   --  Map component Comp into the range of buckets

   function Present (Comp : Component_Id) return Boolean;
   --  Determine whether component Comp exists

   ---------------------
   -- Directed_Graphs --
   ---------------------

   --  The following package offers a directed graph abstraction with the
   --  following characteristics:
   --
   --    * Dynamic resizing based on number of vertices and edges
   --    * Creation of multiple instances, of different sizes
   --    * Discovery of strongly connected components
   --    * Iterable attributes
   --
   --  The following use pattern must be employed when operating this graph:
   --
   --    Graph : Directed_Graph := Create (<some size>, <some size>);
   --
   --    <various operations>
   --
   --    Destroy (Graph);
   --
   --  The destruction of the graph reclaims all storage occupied by it.

   generic

      --------------
      -- Vertices --
      --------------

      type Vertex_Id is private;
      --  The handle of a vertex

      No_Vertex : Vertex_Id;
      --  An indicator for a nonexistent vertex

      with function Hash_Vertex (V : Vertex_Id) return Bucket_Range_Type;
      --  Map vertex V into the range of buckets

      with function Same_Vertex
             (Left  : Vertex_Id;
              Right : Vertex_Id) return Boolean;
      --  Compare vertex Left to vertex Right for identity

      -----------
      -- Edges --
      -----------

      type Edge_Id is private;
      --  The handle of an edge

      No_Edge : Edge_Id;
      --  An indicator for a nonexistent edge

      with function Hash_Edge (E : Edge_Id) return Bucket_Range_Type;
      --  Map edge E into the range of buckets

      with function Same_Edge
             (Left  : Edge_Id;
              Right : Edge_Id) return Boolean;
      --  Compare edge Left to edge Right for identity

   package Directed_Graphs is

      --  The following exceptions are raised when an attempt is made to add
      --  the same edge or vertex in a graph.

      Duplicate_Edge   : exception;
      Duplicate_Vertex : exception;

      --  The following exceptions are raised when an attempt is made to delete
      --  or reference a nonexistent component, edge, or vertex in a graph.

      Missing_Component : exception;
      Missing_Edge      : exception;
      Missing_Vertex    : exception;

      ----------------------
      -- Graph operations --
      ----------------------

      --  The following type denotes a graph handle. Each instance must be
      --  created using routine Create.

      type Directed_Graph is private;
      Nil : constant Directed_Graph;

      procedure Add_Edge
        (G           : Directed_Graph;
         E           : Edge_Id;
         Source      : Vertex_Id;
         Destination : Vertex_Id);
      --  Add edge E to graph G which links vertex source Source and desination
      --  vertex Destination. The edge is "owned" by vertex Source. This action
      --  raises the following exceptions:
      --
      --    * Duplicate_Edge, when the edge is already present in the graph
      --
      --    * Iterated, when the graph has an outstanding edge iterator
      --
      --    * Missing_Vertex, when either the source or desination are not
      --      present in the graph.

      procedure Add_Vertex
        (G : Directed_Graph;
         V : Vertex_Id);
      --  Add vertex V to graph G. This action raises the following exceptions:
      --
      --    * Duplicate_Vertex, when the vertex is already present in the graph
      --
      --    * Iterated, when the graph has an outstanding vertex iterator

      function Component
        (G : Directed_Graph;
         V : Vertex_Id) return Component_Id;
      --  Obtain the component where vertex V of graph G resides. This action
      --  raises the following exceptions:
      --
      --    * Missing_Vertex, when the vertex is not present in the graph

      function Contains_Component
        (G    : Directed_Graph;
         Comp : Component_Id) return Boolean;
      --  Determine whether graph G contains component Comp

      function Contains_Edge
        (G : Directed_Graph;
         E : Edge_Id) return Boolean;
      --  Determine whether graph G contains edge E

      function Contains_Vertex
        (G : Directed_Graph;
         V : Vertex_Id) return Boolean;
      --  Determine whether graph G contains vertex V

      function Create
        (Initial_Vertices : Positive;
         Initial_Edges    : Positive) return Directed_Graph;
      --  Create a new graph with vertex capacity Initial_Vertices and edge
      --  capacity Initial_Edges. This routine must be called at the start of
      --  a graph's lifetime.

      procedure Delete_Edge
        (G : Directed_Graph;
         E : Edge_Id);
      --  Delete edge E from graph G. This action raises these exceptions:
      --
      --    * Iterated, when the graph has an outstanding edge iterator
      --
      --    * Missing_Edge, when the edge is not present in the graph
      --
      --    * Missing_Vertex, when the source vertex that "owns" the edge is
      --      not present in the graph.

      function Destination_Vertex
        (G : Directed_Graph;
         E : Edge_Id) return Vertex_Id;
      --  Obtain the destination vertex of edge E of graph G. This action
      --  raises the following exceptions:
      --
      --    * Missing_Edge, when the edge is not present in the graph

      procedure Destroy (G : in out Directed_Graph);
      --  Destroy the contents of graph G, rendering it unusable. This routine
      --  must be called at the end of a graph's lifetime. This action raises
      --  the following exceptions:
      --
      --    * Iterated, if the graph has any outstanding iterator

      procedure Find_Components (G : Directed_Graph);
      --  Find all components of graph G. This action raises the following
      --  exceptions:
      --
      --    * Iterated, when the components or vertices of the graph have an
      --      outstanding iterator.

      function Is_Empty (G : Directed_Graph) return Boolean;
      --  Determine whether graph G is empty

      function Number_Of_Component_Vertices
        (G    : Directed_Graph;
         Comp : Component_Id) return Natural;
      --  Obtain the total number of vertices of component Comp of graph G

      function Number_Of_Components (G : Directed_Graph) return Natural;
      --  Obtain the total number of components of graph G

      function Number_Of_Edges (G : Directed_Graph) return Natural;
      --  Obtain the total number of edges of graph G

      function Number_Of_Outgoing_Edges
        (G : Directed_Graph;
         V : Vertex_Id) return Natural;
      --  Obtain the total number of outgoing edges of vertex V of graph G

      function Number_Of_Vertices (G : Directed_Graph) return Natural;
      --  Obtain the total number of vertices of graph G

      function Present (G : Directed_Graph) return Boolean;
      --  Determine whether graph G exists

      function Source_Vertex
        (G : Directed_Graph;
         E : Edge_Id) return Vertex_Id;
      --  Obtain the source vertex that "owns" edge E of graph G. This action
      --  raises the following exceptions:
      --
      --    * Missing_Edge, when the edge is not present in the graph

      -------------------------
      -- Iterator operations --
      -------------------------

      --  The following types represent iterators over various attributes of a
      --  graph. Each iterator locks all mutation operations of its associated
      --  attribute, and unlocks them once it is exhausted. The iterators must
      --  be used with the following pattern:
      --
      --    Iter : Iterate_XXX (Graph);
      --    while Has_Next (Iter) loop
      --       Next (Iter, Element);
      --    end loop;
      --
      --  It is possible to advance the iterators by using Next only, however
      --  this risks raising Iterator_Exhausted.

      --  The following type represents an iterator over all edges of a graph

      type All_Edge_Iterator is private;

      function Has_Next (Iter : All_Edge_Iterator) return Boolean;
      --  Determine whether iterator Iter has more edges to examine

      function Iterate_All_Edges (G : Directed_Graph) return All_Edge_Iterator;
      --  Obtain an iterator over all edges of graph G

      procedure Next
        (Iter : in out All_Edge_Iterator;
         E    : out Edge_Id);
      --  Return the current edge referenced by iterator Iter and advance to
      --  the next available edge. This action raises the following exceptions:
      --
      --    * Iterator_Exhausted, when the iterator has been exhausted and
      --      further attempts are made to advance it.

      --  The following type represents an iterator over all vertices of a
      --  graph.

      type All_Vertex_Iterator is private;

      function Has_Next (Iter : All_Vertex_Iterator) return Boolean;
      --  Determine whether iterator Iter has more vertices to examine

      function Iterate_All_Vertices
        (G : Directed_Graph) return All_Vertex_Iterator;
      --  Obtain an iterator over all vertices of graph G

      procedure Next
        (Iter : in out All_Vertex_Iterator;
         V    : out Vertex_Id);
      --  Return the current vertex referenced by iterator Iter and advance
      --  to the next available vertex. This action raises the following
      --  exceptions:
      --
      --    * Iterator_Exhausted, when the iterator has been exhausted and
      --      further attempts are made to advance it.

      --  The following type represents an iterator over all components of a
      --  graph.

      type Component_Iterator is private;

      function Has_Next (Iter : Component_Iterator) return Boolean;
      --  Determine whether iterator Iter has more components to examine

      function Iterate_Components
        (G : Directed_Graph) return Component_Iterator;
      --  Obtain an iterator over all components of graph G

      procedure Next
        (Iter : in out Component_Iterator;
         Comp : out Component_Id);
      --  Return the current component referenced by iterator Iter and advance
      --  to the next component. This action raises the following exceptions:
      --
      --    * Iterator_Exhausted, when the iterator has been exhausted and
      --      further attempts are made to advance it.

      --  The following type prepresents an iterator over all vertices of a
      --  component.

      type Component_Vertex_Iterator is private;

      function Has_Next (Iter : Component_Vertex_Iterator) return Boolean;
      --  Determine whether iterator Iter has more vertices to examine

      function Iterate_Component_Vertices
        (G    : Directed_Graph;
         Comp : Component_Id) return Component_Vertex_Iterator;
      --  Obtain an iterator over all vertices that comprise component Comp of
      --  graph G.

      procedure Next
        (Iter : in out Component_Vertex_Iterator;
         V    : out Vertex_Id);
      --  Return the current vertex referenced by iterator Iter and advance to
      --  the next vertex. This action raises the following exceptions:
      --
      --    * Iterator_Exhausted, when the iterator has been exhausted and
      --      further attempts are made to advance it.

      --  The following type represents an iterator over all outgoing edges of
      --  a vertex.

      type Outgoing_Edge_Iterator is private;

      function Has_Next (Iter : Outgoing_Edge_Iterator) return Boolean;
      --  Determine whether iterator Iter has more outgoing edges to examine

      function Iterate_Outgoing_Edges
        (G : Directed_Graph;
         V : Vertex_Id) return Outgoing_Edge_Iterator;
      --  Obtain an iterator over all the outgoing edges "owned" by vertex V of
      --  graph G.

      procedure Next
        (Iter : in out Outgoing_Edge_Iterator;
         E    : out Edge_Id);
      --  Return the current outgoing edge referenced by iterator Iter and
      --  advance to the next available outgoing edge. This action raises the
      --  following exceptions:
      --
      --    * Iterator_Exhausted, when the iterator has been exhausted and
      --      further attempts are made to advance it.

   private
      pragma Unreferenced (No_Edge);

      --------------
      -- Edge_Map --
      --------------

      type Edge_Attributes is record
         Destination : Vertex_Id := No_Vertex;
         --  The target of a directed edge

         Source : Vertex_Id := No_Vertex;
         --  The origin of a directed edge. The source vertex "owns" the edge.
      end record;

      No_Edge_Attributes : constant Edge_Attributes :=
        (Destination => No_Vertex,
         Source      => No_Vertex);

      procedure Destroy_Edge_Attributes (Attrs : in out Edge_Attributes);
      --  Destroy the contents of attributes Attrs

      package Edge_Map is new Dynamic_Hash_Tables
        (Key_Type              => Edge_Id,
         Value_Type            => Edge_Attributes,
         No_Value              => No_Edge_Attributes,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => Same_Edge,
         Destroy_Value         => Destroy_Edge_Attributes,
         Hash                  => Hash_Edge);

      --------------
      -- Edge_Set --
      --------------

      package Edge_Set is new Membership_Sets
        (Element_Type => Edge_Id,
         "="          => "=",
         Hash         => Hash_Edge);

      -----------------
      -- Vertex_List --
      -----------------

      procedure Destroy_Vertex (V : in out Vertex_Id);
      --  Destroy the contents of a vertex

      package Vertex_List is new Doubly_Linked_Lists
        (Element_Type    => Vertex_Id,
         "="             => Same_Vertex,
         Destroy_Element => Destroy_Vertex);

      ----------------
      -- Vertex_Map --
      ----------------

      type Vertex_Attributes is record
         Component : Component_Id := No_Component;
         --  The component where a vertex lives

         Outgoing_Edges : Edge_Set.Membership_Set := Edge_Set.Nil;
         --  The set of edges that extend out from a vertex
      end record;

      No_Vertex_Attributes : constant Vertex_Attributes :=
        (Component      => No_Component,
         Outgoing_Edges => Edge_Set.Nil);

      procedure Destroy_Vertex_Attributes (Attrs : in out Vertex_Attributes);
      --  Destroy the contents of attributes Attrs

      package Vertex_Map is new Dynamic_Hash_Tables
        (Key_Type              => Vertex_Id,
         Value_Type            => Vertex_Attributes,
         No_Value              => No_Vertex_Attributes,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => Same_Vertex,
         Destroy_Value         => Destroy_Vertex_Attributes,
         Hash                  => Hash_Vertex);

      -------------------
      -- Component_Map --
      -------------------

      type Component_Attributes is record
         Vertices : Vertex_List.Doubly_Linked_List := Vertex_List.Nil;
      end record;

      No_Component_Attributes : constant Component_Attributes :=
        (Vertices => Vertex_List.Nil);

      procedure Destroy_Component_Attributes
        (Attrs : in out Component_Attributes);
      --  Destroy the contents of attributes Attrs

      package Component_Map is new Dynamic_Hash_Tables
        (Key_Type              => Component_Id,
         Value_Type            => Component_Attributes,
         No_Value              => No_Component_Attributes,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Destroy_Value         => Destroy_Component_Attributes,
         Hash                  => Hash_Component);

      -----------
      -- Graph --
      -----------

      type Directed_Graph_Attributes is record
         All_Edges : Edge_Map.Dynamic_Hash_Table := Edge_Map.Nil;
         --  The map of edge -> edge attributes for all edges in the graph

         All_Vertices : Vertex_Map.Dynamic_Hash_Table := Vertex_Map.Nil;
         --  The map of vertex -> vertex attributes for all vertices in the
         --  graph.

         Components : Component_Map.Dynamic_Hash_Table := Component_Map.Nil;
         --  The map of component -> component attributes for all components
         --  in the graph.
      end record;

      type Directed_Graph is access Directed_Graph_Attributes;
      Nil : constant Directed_Graph := null;

      ---------------
      -- Iterators --
      ---------------

      type All_Edge_Iterator         is new Edge_Map.Iterator;
      type All_Vertex_Iterator       is new Vertex_Map.Iterator;
      type Component_Iterator        is new Component_Map.Iterator;
      type Component_Vertex_Iterator is new Vertex_List.Iterator;
      type Outgoing_Edge_Iterator    is new Edge_Set.Iterator;
   end Directed_Graphs;

private
   First_Component : constant Component_Id := No_Component + 1;

end GNAT.Graphs;
