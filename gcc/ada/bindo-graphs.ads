------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         B I N D O . G R A P H S                          --
--                                                                          --
--                                 S p e c                                  --
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

--  For full architecture, see unit Bindo.

--  The following unit defines the various graphs used in determining the
--  elaboration order of units.

with Bindo.Units; use Bindo.Units;

with GNAT;                 use GNAT;
with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;
with GNAT.Graphs;          use GNAT.Graphs;
with GNAT.Sets;            use GNAT.Sets;

package Bindo.Graphs is

   ---------------------------
   -- Invocation graph edge --
   ---------------------------

   --  The following type denotes an invocation graph edge handle

   type Invocation_Graph_Edge_Id is new Natural;
   No_Invocation_Graph_Edge    : constant Invocation_Graph_Edge_Id :=
                                   Invocation_Graph_Edge_Id'First;
   First_Invocation_Graph_Edge : constant Invocation_Graph_Edge_Id :=
                                   No_Invocation_Graph_Edge + 1;

   function Hash_Invocation_Graph_Edge
     (IGE_Id : Invocation_Graph_Edge_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Invocation_Graph_Edge);
   --  Obtain the hash value of key IGE_Id

   function Present (IGE_Id : Invocation_Graph_Edge_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether invocation graph edge IGE_Id exists

   ------------------------------
   --  Invocation graph vertex --
   ------------------------------

   --  The following type denotes an invocation graph vertex handle

   type Invocation_Graph_Vertex_Id is new Natural;
   No_Invocation_Graph_Vertex    : constant Invocation_Graph_Vertex_Id :=
                                     Invocation_Graph_Vertex_Id'First;
   First_Invocation_Graph_Vertex : constant Invocation_Graph_Vertex_Id :=
                                     No_Invocation_Graph_Vertex + 1;

   function Hash_Invocation_Graph_Vertex
     (IGV_Id : Invocation_Graph_Vertex_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Invocation_Graph_Vertex);
   --  Obtain the hash value of key IGV_Id

   function Present (IGV_Id : Invocation_Graph_Vertex_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether invocation graph vertex IGV_Id exists

   ------------------------
   -- Library graph edge --
   ------------------------

   --  The following type denotes a library graph edge handle

   type Library_Graph_Edge_Id is new Natural;
   No_Library_Graph_Edge    : constant Library_Graph_Edge_Id :=
                                Library_Graph_Edge_Id'First;
   First_Library_Graph_Edge : constant Library_Graph_Edge_Id :=
                                No_Library_Graph_Edge + 1;

   function Hash_Library_Graph_Edge
     (LGE_Id : Library_Graph_Edge_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Library_Graph_Edge);
   --  Obtain the hash value of key LGE_Id

   function Present (LGE_Id : Library_Graph_Edge_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether library graph edge LGE_Id exists

   --------------------------
   -- Library graph vertex --
   --------------------------

   --  The following type denotes a library graph vertex handle

   type Library_Graph_Vertex_Id is new Natural;
   No_Library_Graph_Vertex    : constant Library_Graph_Vertex_Id :=
                                  Library_Graph_Vertex_Id'First;
   First_Library_Graph_Vertex : constant Library_Graph_Vertex_Id :=
                                  No_Library_Graph_Vertex + 1;

   function Hash_Library_Graph_Vertex
     (LGV_Id : Library_Graph_Vertex_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Library_Graph_Vertex);
   --  Obtain the hash value of key LGV_Id

   function Present (LGV_Id : Library_Graph_Vertex_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether library graph vertex LGV_Id exists

   -----------------------
   -- Invocation_Graphs --
   -----------------------

   package Invocation_Graphs is

      -----------
      -- Graph --
      -----------

      --  The following type denotes an invocation graph handle. Each instance
      --  must be created using routine Create.

      type Invocation_Graph is private;
      Nil : constant Invocation_Graph;

      ----------------------
      -- Graph operations --
      ----------------------

      procedure Add_Edge
        (G      : Invocation_Graph;
         Source : Invocation_Graph_Vertex_Id;
         Target : Invocation_Graph_Vertex_Id;
         IR_Id  : Invocation_Relation_Id);
      pragma Inline (Add_Edge);
      --  Create a new edge in invocation graph G with source vertex Source and
      --  destination vertex Target. IR_Id is the invocation relation the edge
      --  describes.

      procedure Add_Vertex
        (G      : Invocation_Graph;
         IC_Id  : Invocation_Construct_Id;
         LGV_Id : Library_Graph_Vertex_Id);
      pragma Inline (Add_Vertex);
      --  Create a new vertex in invocation graph G. IC_Id is the invocation
      --  construct the vertex describes. LGV_Id is the library graph vertex
      --  where the invocation construct appears.

      function Create
        (Initial_Vertices : Positive;
         Initial_Edges    : Positive) return Invocation_Graph;
      pragma Inline (Create);
      --  Create a new empty graph with vertex capacity Initial_Vertices and
      --  edge capacity Initial_Edges.

      procedure Destroy (G : in out Invocation_Graph);
      pragma Inline (Destroy);
      --  Destroy the contents of invocation graph G, rendering it unusable

      function Present (G : Invocation_Graph) return Boolean;
      pragma Inline (Present);
      --  Determine whether invocation graph G exists

      -----------------------
      -- Vertex attributes --
      -----------------------

      function Construct
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Invocation_Construct_Id;
      pragma Inline (Construct);
      --  Obtain the invocation construct vertex IGV_Id of invocation graph G
      --  describes.

      function Corresponding_Vertex
        (G     : Invocation_Graph;
         IS_Id : Invocation_Signature_Id) return Invocation_Graph_Vertex_Id;
      pragma Inline (Corresponding_Vertex);
      --  Obtain the vertex of invocation graph G that corresponds to signature
      --  IS_Id.

      function Lib_Vertex
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Lib_Vertex);
      --  Obtain the library graph vertex where vertex IGV_Id of invocation
      --  graph appears.

      function Name
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Name_Id;
      pragma Inline (Name);
      --  Obtain the name of the construct vertex IGV_Id of invocation graph G
      --  describes.

      ---------------------
      -- Edge attributes --
      ---------------------

      function Kind
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Kind;
      pragma Inline (Kind);
      --  Obtain the nature of edge IGE_Id of invocation graph G

      function Relation
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Relation_Id;
      pragma Inline (Relation);
      --  Obtain the relation edge IGE_Id of invocation graph G describes

      function Target
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Graph_Vertex_Id;
      pragma Inline (Target);
      --  Obtain the target vertex edge IGE_Id of invocation graph G designates

      ----------------
      -- Statistics --
      ----------------

      function Invocation_Graph_Edge_Count
        (G    : Invocation_Graph;
         Kind : Invocation_Kind) return Natural;
      pragma Inline (Invocation_Graph_Edge_Count);
      --  Obtain the total number of edges of kind Kind in invocation graph G

      function Number_Of_Edges (G : Invocation_Graph) return Natural;
      pragma Inline (Number_Of_Edges);
      --  Obtain the total number of edges in invocation graph G

      function Number_Of_Edges_To_Targets
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Natural;
      pragma Inline (Number_Of_Edges_To_Targets);
      --  Obtain the total number of edges to targets vertex IGV_Id of
      --  invocation graph G has.

      function Number_Of_Elaboration_Roots
        (G : Invocation_Graph) return Natural;
      pragma Inline (Number_Of_Elaboration_Roots);
      --  Obtain the total number of elaboration roots in invocation graph G

      function Number_Of_Vertices (G : Invocation_Graph) return Natural;
      pragma Inline (Number_Of_Vertices);
      --  Obtain the total number of vertices in invocation graph G

      ---------------
      -- Iterators --
      ---------------

      --  The following type represents an iterator over all edges of an
      --  invocation graph.

      type All_Edge_Iterator is private;

      function Has_Next (Iter : All_Edge_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more edges to examine

      function Iterate_All_Edges
        (G : Invocation_Graph) return All_Edge_Iterator;
      pragma Inline (Iterate_All_Edges);
      --  Obtain an iterator over all edges of invocation graph G

      procedure Next
        (Iter   : in out All_Edge_Iterator;
         IGE_Id : out Invocation_Graph_Edge_Id);
      pragma Inline (Next);
      --  Return the current edge referenced by iterator Iter and advance to
      --  the next available edge.

      --  The following type represents an iterator over all vertices of an
      --  invocation graph.

      type All_Vertex_Iterator is private;

      function Has_Next (Iter : All_Vertex_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more vertices to examine

      function Iterate_All_Vertices
        (G : Invocation_Graph) return All_Vertex_Iterator;
      pragma Inline (Iterate_All_Vertices);
      --  Obtain an iterator over all vertices of invocation graph G

      procedure Next
        (Iter   : in out All_Vertex_Iterator;
         IGV_Id : out Invocation_Graph_Vertex_Id);
      pragma Inline (Next);
      --  Return the current vertex referenced by iterator Iter and advance
      --  to the next available vertex.

      --  The following type represents an iterator over all edges that reach
      --  targets starting from a particular source vertex.

      type Edges_To_Targets_Iterator is private;

      function Has_Next (Iter : Edges_To_Targets_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more edges to examine

      function Iterate_Edges_To_Targets
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Edges_To_Targets_Iterator;
      pragma Inline (Iterate_Edges_To_Targets);
      --  Obtain an iterator over all edges to targets with source vertex
      --  IGV_Id of invocation graph G.

      procedure Next
        (Iter   : in out Edges_To_Targets_Iterator;
         IGE_Id : out Invocation_Graph_Edge_Id);
      pragma Inline (Next);
      --  Return the current edge referenced by iterator Iter and advance to
      --  the next available edge.

      --  The following type represents an iterator over all vertices of an
      --  invocation graph that denote the elaboration procedure or a spec or
      --  a body, referred to as elaboration root.

      type Elaboration_Root_Iterator is private;

      function Has_Next (Iter : Elaboration_Root_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more elaboration roots to examine

      function Iterate_Elaboration_Roots
        (G : Invocation_Graph) return Elaboration_Root_Iterator;
      pragma Inline (Iterate_Elaboration_Roots);
      --  Obtain an iterator over all elaboration roots of invocation graph G

      procedure Next
        (Iter : in out Elaboration_Root_Iterator;
         Root : out Invocation_Graph_Vertex_Id);
      pragma Inline (Next);
      --  Return the current elaboration root referenced by iterator Iter and
      --  advance to the next available elaboration root.

   private

      --------------
      -- Vertices --
      --------------

      procedure Destroy_Invocation_Graph_Vertex
        (IGV_Id : in out Invocation_Graph_Vertex_Id);
      pragma Inline (Destroy_Invocation_Graph_Vertex);
      --  Destroy invocation graph vertex IGV_Id

      --  The following type represents the attributes of an invocation graph
      --  vertex.

      type Invocation_Graph_Vertex_Attributes is record
         Construct : Invocation_Construct_Id := No_Invocation_Construct;
         --  Reference to the invocation construct this vertex represents

         Lib_Vertex : Library_Graph_Vertex_Id := No_Library_Graph_Vertex;
         --  Reference to the library graph vertex where this vertex resides
      end record;

      No_Invocation_Graph_Vertex_Attributes :
        constant Invocation_Graph_Vertex_Attributes :=
          (Construct  => No_Invocation_Construct,
           Lib_Vertex => No_Library_Graph_Vertex);

      procedure Destroy_Invocation_Graph_Vertex_Attributes
        (Attrs : in out Invocation_Graph_Vertex_Attributes);
      pragma Inline (Destroy_Invocation_Graph_Vertex_Attributes);
      --  Destroy the contents of attributes Attrs

      package VA is new Dynamic_Hash_Tables
        (Key_Type              => Invocation_Graph_Vertex_Id,
         Value_Type            => Invocation_Graph_Vertex_Attributes,
         No_Value              => No_Invocation_Graph_Vertex_Attributes,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Destroy_Value         => Destroy_Invocation_Graph_Vertex_Attributes,
         Hash                  => Hash_Invocation_Graph_Vertex);

      -----------
      -- Edges --
      -----------

      procedure Destroy_Invocation_Graph_Edge
        (IGE_Id : in out Invocation_Graph_Edge_Id);
      pragma Inline (Destroy_Invocation_Graph_Edge);
      --  Destroy invocation graph edge IGE_Id

      --  The following type represents the attributes of an invocation graph
      --  edge.

      type Invocation_Graph_Edge_Attributes is record
         Relation : Invocation_Relation_Id := No_Invocation_Relation;
         --  Reference to the invocation relation this edge represents
      end record;

      No_Invocation_Graph_Edge_Attributes :
        constant Invocation_Graph_Edge_Attributes :=
          (Relation => No_Invocation_Relation);

      procedure Destroy_Invocation_Graph_Edge_Attributes
        (Attrs : in out Invocation_Graph_Edge_Attributes);
      pragma Inline (Destroy_Invocation_Graph_Edge_Attributes);
      --  Destroy the contents of attributes Attrs

      package EA is new Dynamic_Hash_Tables
        (Key_Type              => Invocation_Graph_Edge_Id,
         Value_Type            => Invocation_Graph_Edge_Attributes,
         No_Value              => No_Invocation_Graph_Edge_Attributes,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Destroy_Value         => Destroy_Invocation_Graph_Edge_Attributes,
         Hash                  => Hash_Invocation_Graph_Edge);

      ---------------
      -- Relations --
      ---------------

      --  The following type represents a relation between a source and target
      --  vertices.

      type Source_Target_Relation is record
         Source : Invocation_Graph_Vertex_Id := No_Invocation_Graph_Vertex;
         --  The source vertex

         Target : Invocation_Graph_Vertex_Id := No_Invocation_Graph_Vertex;
         --  The destination vertex
      end record;

      No_Source_Target_Relation :
        constant Source_Target_Relation :=
          (Source => No_Invocation_Graph_Vertex,
           Target => No_Invocation_Graph_Vertex);

      function Hash_Source_Target_Relation
        (Rel : Source_Target_Relation) return Bucket_Range_Type;
      pragma Inline (Hash_Source_Target_Relation);
      --  Obtain the hash value of key Rel

      package ST is new Membership_Sets
        (Element_Type => Source_Target_Relation,
         "="          => "=",
         Hash         => Hash_Source_Target_Relation);

      ----------------
      -- Statistics --
      ----------------

      type Invocation_Graph_Edge_Counts is array (Invocation_Kind) of Natural;

      ----------------
      -- Signatures --
      ----------------

      function Hash_Invocation_Signature
        (IS_Id : Invocation_Signature_Id) return Bucket_Range_Type;
      pragma Inline (Hash_Invocation_Signature);
      --  Obtain the hash value of key IS_Id

      package SV is new Dynamic_Hash_Tables
        (Key_Type              => Invocation_Signature_Id,
         Value_Type            => Invocation_Graph_Vertex_Id,
         No_Value              => No_Invocation_Graph_Vertex,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Destroy_Value         => Destroy_Invocation_Graph_Vertex,
         Hash                  => Hash_Invocation_Signature);

      -----------------------
      -- Elaboration roots --
      -----------------------

      package ER is new Membership_Sets
        (Element_Type => Invocation_Graph_Vertex_Id,
         "="          => "=",
         Hash         => Hash_Invocation_Graph_Vertex);

      -----------
      -- Graph --
      -----------

      package DG is new Directed_Graphs
        (Vertex_Id   => Invocation_Graph_Vertex_Id,
         No_Vertex   => No_Invocation_Graph_Vertex,
         Hash_Vertex => Hash_Invocation_Graph_Vertex,
         Same_Vertex => "=",
         Edge_id     => Invocation_Graph_Edge_Id,
         No_Edge     => No_Invocation_Graph_Edge,
         Hash_Edge   => Hash_Invocation_Graph_Edge,
         Same_Edge   => "=");

      --  The following type represents the attributes of an invocation graph

      type Invocation_Graph_Attributes is record
         Counts : Invocation_Graph_Edge_Counts := (others => 0);
         --  Edge statistics

         Edge_Attributes : EA.Dynamic_Hash_Table := EA.Nil;
         --  The map of edge -> edge attributes for all edges in the graph

         Graph : DG.Directed_Graph := DG.Nil;
         --  The underlying graph describing the relations between edges and
         --  vertices.

         Relations : ST.Membership_Set := ST.Nil;
         --  The set of relations between source and targets, used to prevent
         --  duplicate edges in the graph.

         Roots : ER.Membership_Set := ER.Nil;
         --  The set of elaboration root vertices

         Signature_To_Vertex : SV.Dynamic_Hash_Table := SV.Nil;
         --  The map of signature -> vertex

         Vertex_Attributes : VA.Dynamic_Hash_Table := VA.Nil;
         --  The map of vertex -> vertex attributes for all vertices in the
         --  graph.
      end record;

      type Invocation_Graph is access Invocation_Graph_Attributes;
      Nil : constant Invocation_Graph := null;

      ---------------
      -- Iterators --
      ---------------

      type All_Edge_Iterator         is new DG.All_Edge_Iterator;
      type All_Vertex_Iterator       is new DG.All_Vertex_Iterator;
      type Edges_To_Targets_Iterator is new DG.Outgoing_Edge_Iterator;
      type Elaboration_Root_Iterator is new ER.Iterator;
   end Invocation_Graphs;

   --------------------
   -- Library_Graphs --
   --------------------

   package Library_Graphs is

      --  The following type represents the various kinds of library edges

      type Library_Graph_Edge_Kind is
        (Body_Before_Spec_Edge,
         --  Successor denotes spec, Predecessor denotes a body. This is a
         --  special edge kind used only during the discovery of components.
         --  Note that a body can never be elaborated before its spec.

         Elaborate_Edge,
         --  Successor withs Predecessor, and has pragma Elaborate for it

         Elaborate_All_Edge,
         --  Successor withs Predecessor, and has pragma Elaborate_All for it

         Forced_Edge,
         --  Successor is forced to with Predecessor by virtue of an existing
         --  elaboration order provided in a file.

         Invocation_Edge,
         --  An invocation construct in unit Successor invokes a target in unit
         --  Predecessor.

         Spec_Before_Body_Edge,
         --  Successor denotes a body, Predecessor denotes a spec

         With_Edge,
         --  Successor withs Predecessor

         No_Edge);

      -----------
      -- Graph --
      -----------

      --  The following type denotes a library graph handle. Each instance must
      --  be created using routine Create.

      type Library_Graph is private;
      Nil : constant Library_Graph;

      ----------------------
      -- Graph operations --
      ----------------------

      procedure Add_Edge
        (G    : Library_Graph;
         Pred : Library_Graph_Vertex_Id;
         Succ : Library_Graph_Vertex_Id;
         Kind : Library_Graph_Edge_Kind);
      pragma Inline (Add_Edge);
      --  Create a new edge in library graph G with source vertex Pred and
      --  destination vertex Succ. Kind denotes the nature of the edge.

      procedure Add_Vertex
        (G    : Library_Graph;
         U_Id : Unit_Id);
      pragma Inline (Add_Vertex);
      --  Create a new vertex in library graph G. U_Id is the unit the vertex
      --  describes.

      function Create
        (Initial_Vertices : Positive;
         Initial_Edges    : Positive) return Library_Graph;
      pragma Inline (Create);
      --  Create a new empty graph with vertex capacity Initial_Vertices and
      --  edge capacity Initial_Edges.

      procedure Destroy (G : in out Library_Graph);
      pragma Inline (Destroy);
      --  Destroy the contents of library graph G, rendering it unusable

      procedure Find_Components (G : Library_Graph);
      pragma Inline (Find_Components);
      --  Find all components in library graph G

      function Present (G : Library_Graph) return Boolean;
      pragma Inline (Present);
      --  Determine whether library graph G exists

      -----------------------
      -- Vertex attributes --
      -----------------------

      function Component
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Component_Id;
      pragma Inline (Component);
      --  Obtain the component where vertex LGV_Id of library graph G resides

      function Corresponding_Item
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Corresponding_Item);
      --  Obtain the complementary vertex which represents the corresponding
      --  spec or body of vertex LGV_Id of library graph G.

      function Corresponding_Vertex
        (G    : Library_Graph;
         U_Id : Unit_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Corresponding_Vertex);
      --  Obtain the corresponding vertex of library graph G which represents
      --  unit U_Id.

      procedure Decrement_Pending_Predecessors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id);
      pragma Inline (Decrement_Pending_Predecessors);
      --  Decrease the number of pending predecessors vertex LGV_Id of library
      --  graph G must wait on until it can be elaborated.

      function In_Elaboration_Order
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (In_Elaboration_Order);
      --  Determine whether vertex LGV_Id of library graph G is already in some
      --  elaboration order.

      function Name
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Unit_Name_Type;
      pragma Inline (Name);
      --  Obtain the name of the unit which vertex LGV_Id of library graph G
      --  represents.

      function Pending_Predecessors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Natural;
      pragma Inline (Pending_Predecessors);
      --  Obtain the number of pending predecessors vertex LGV_Id of library
      --  graph G must wait on until it can be elaborated.

      procedure Set_Corresponding_Item
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Val    : Library_Graph_Vertex_Id);
      pragma Inline (Set_Corresponding_Item);
      --  Set the complementary vertex which represents the corresponding
      --  spec or body of vertex LGV_Id of library graph G to value Val.

      procedure Set_In_Elaboration_Order
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Val    : Boolean := True);
      pragma Inline (Set_In_Elaboration_Order);
      --  Mark vertex LGV_Id of library graph G as included in some elaboration
      --  order depending on value Val.

      function Unit
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Unit_Id;
      pragma Inline (Unit);
      --  Obtain the unit vertex LGV_Id of library graph G represents

      ---------------------
      -- Edge attributes --
      ---------------------

      function Kind
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Edge_Kind;
      pragma Inline (Kind);
      --  Obtain the nature of edge LGE_Id of library graph G

      function Predecessor
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Predecessor);
      --  Obtain the predecessor vertex of edge LGE_Id of library graph G

      function Successor
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Successor);
      --  Obtain the successor vertex of edge LGE_Id of library graph G

      --------------------------
      -- Component attributes --
      --------------------------

      procedure Decrement_Pending_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id);
      pragma Inline (Decrement_Pending_Predecessors);
      --  Decrease the number of pending predecessors component Comp of library
      --  graph G must wait on until it can be elaborated.

      function Pending_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id) return Natural;
      pragma Inline (Pending_Predecessors);
      --  Obtain the number of pending predecessors component Comp of library
      --  graph G must wait on until it can be elaborated.

      ---------------
      -- Semantics --
      ---------------

      function Is_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Body);
      --  Determine whether vertex LGV_Id of library graph G denotes a body

      function Is_Body_Of_Spec_With_Elaborate_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Body_Of_Spec_With_Elaborate_Body);
      --  Determine whether vertex LGV_Id of library graph G denotes a body
      --  with a corresponding spec, and the spec has pragma Elaborate_Body.

      function Is_Body_With_Spec
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Body_With_Spec);
      --  Determine whether vertex LGV_Id of library graph G denotes a body
      --  with a corresponding spec.

      function Is_Elaborable_Component
        (G    : Library_Graph;
         Comp : Component_Id) return Boolean;
      pragma Inline (Is_Elaborable_Component);
      --  Determine whether component Comp of library graph G can be elaborated

      function Is_Elaborable_Vertex
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Elaborable_Vertex);
      --  Determine whether vertex LGV_Id of library graph G can be elaborated

      function Is_Internal_Unit
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Internal_Unit);
      --  Determine whether vertex LGV_Id of library graph G denotes an
      --  internal unit.

      function Is_Predefined_Unit
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Predefined_Unit);
      --  Determine whether vertex LGV_Id of library graph G denotes a
      --  predefined unit.

      function Is_Preelaborated_Unit
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Preelaborated_Unit);
      --  Determine whether vertex LGV_Id of library graph G denotes a unit
      --  subjec to pragma Pure or Preelaborable.

      function Is_Spec
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Spec);
      --  Determine whether vertex LGV_Id of library graph G denotes a spec

      function Is_Spec_With_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Spec_With_Body);
      --  Determine whether vertex LGV_Id of library graph G denotes a spec
      --  with a corresponding body.

      function Is_Spec_With_Elaborate_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Spec_With_Elaborate_Body);
      --  Determine whether vertex LGV_Id of library graph G denotes a spec
      --  with a corresponding body, and is subject to pragma Elaborate_Body.

      function Links_Vertices_In_Same_Component
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Links_Vertices_In_Same_Component);
      --  Determine whether edge LGE_Id of library graph G links a predecessor
      --  and a successor that reside within the same component.

      function Needs_Elaboration
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Needs_Elaboration);
      --  Determine whether vertex LGV_Id of library graph G represents a unit
      --  that needs to be elaborated.

      function Proper_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Proper_Body);
      --  Obtain the body of vertex LGV_Id of library graph G

      function Proper_Spec
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Proper_Spec);
      --  Obtain the spec of vertex LGV_Id of library graph G

      ----------------
      -- Statistics --
      ----------------

      function Library_Graph_Edge_Count
        (G    : Library_Graph;
         Kind : Library_Graph_Edge_Kind) return Natural;
      pragma Inline (Library_Graph_Edge_Count);
      --  Obtain the total number of edges of kind Kind in library graph G

      function Number_Of_Component_Vertices
        (G    : Library_Graph;
         Comp : Component_Id) return Natural;
      pragma Inline (Number_Of_Component_Vertices);
      --  Obtain the total number of vertices component Comp of library graph
      --  contains.

      function Number_Of_Components (G : Library_Graph) return Natural;
      pragma Inline (Number_Of_Components);
      --  Obtain the total number of components in library graph G

      function Number_Of_Edges (G : Library_Graph) return Natural;
      pragma Inline (Number_Of_Edges);
      --  Obtain the total number of edges in library graph G

      function Number_Of_Edges_To_Successors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Natural;
      pragma Inline (Number_Of_Edges_To_Successors);
      --  Obtain the total number of edges to successors vertex LGV_Id of
      --  library graph G has.

      function Number_Of_Vertices (G : Library_Graph) return Natural;
      pragma Inline (Number_Of_Vertices);
      --  Obtain the total number of vertices in library graph G

      ---------------
      -- Iterators --
      ---------------

      --  The following type represents an iterator over all edges of a library
      --  graph.

      type All_Edge_Iterator is private;

      function Has_Next (Iter : All_Edge_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more edges to examine

      function Iterate_All_Edges (G : Library_Graph) return All_Edge_Iterator;
      pragma Inline (Iterate_All_Edges);
      --  Obtain an iterator over all edges of library graph G

      procedure Next
        (Iter   : in out All_Edge_Iterator;
         LGE_Id : out Library_Graph_Edge_Id);
      pragma Inline (Next);
      --  Return the current edge referenced by iterator Iter and advance to
      --  the next available edge.

      --  The following type represents an iterator over all vertices of a
      --  library graph.

      type All_Vertex_Iterator is private;

      function Has_Next (Iter : All_Vertex_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more vertices to examine

      function Iterate_All_Vertices
        (G : Library_Graph) return All_Vertex_Iterator;
      pragma Inline (Iterate_All_Vertices);
      --  Obtain an iterator over all vertices of library graph G

      procedure Next
        (Iter   : in out All_Vertex_Iterator;
         LGV_Id : out Library_Graph_Vertex_Id);
      pragma Inline (Next);
      --  Return the current vertex referenced by iterator Iter and advance
      --  to the next available vertex.

      --  The following type represents an iterator over all components of a
      --  library graph.

      type Component_Iterator is private;

      function Has_Next (Iter : Component_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more components to examine

      function Iterate_Components
        (G : Library_Graph) return Component_Iterator;
      pragma Inline (Iterate_Components);
      --  Obtain an iterator over all components of library graph G

      procedure Next
        (Iter : in out Component_Iterator;
         Comp : out Component_Id);
      pragma Inline (Next);
      --  Return the current component referenced by iterator Iter and advance
      --  to the next available component.

      --  The following type represents an iterator over all vertices of a
      --  component.

      type Component_Vertex_Iterator is private;

      function Has_Next (Iter : Component_Vertex_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more vertices to examine

      function Iterate_Component_Vertices
        (G    : Library_Graph;
         Comp : Component_Id) return Component_Vertex_Iterator;
      pragma Inline (Iterate_Component_Vertices);
      --  Obtain an iterator over all vertices of component Comp of library
      --  graph G.

      procedure Next
        (Iter   : in out Component_Vertex_Iterator;
         LGV_Id : out Library_Graph_Vertex_Id);
      pragma Inline (Next);
      --  Return the current vertex referenced by iterator Iter and advance
      --  to the next available vertex.

      --  The following type represents an iterator over all edges that reach
      --  successors starting from a particular predecessor vertex.

      type Edges_To_Successors_Iterator is private;

      function Has_Next (Iter : Edges_To_Successors_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more edges to examine

      function Iterate_Edges_To_Successors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Edges_To_Successors_Iterator;
      pragma Inline (Iterate_Components);
      --  Obtain an iterator over all edges to successors with predecessor
      --  vertex LGV_Id of library graph G.

      procedure Next
        (Iter   : in out Edges_To_Successors_Iterator;
         LGE_Id : out Library_Graph_Edge_Id);
      pragma Inline (Next);
      --  Return the current edge referenced by iterator Iter and advance to
      --  the next available edge.

   private

      --------------
      -- Vertices --
      --------------

      procedure Destroy_Library_Graph_Vertex
        (LGV_Id : in out Library_Graph_Vertex_Id);
      pragma Inline (Destroy_Library_Graph_Vertex);
      --  Destroy library graph vertex LGV_Id

      --  The following type represents the attributes of a library graph
      --  vertex.

      type Library_Graph_Vertex_Attributes is record
         Corresponding_Item : Library_Graph_Vertex_Id :=
                                No_Library_Graph_Vertex;
         --  The reference to the corresponding spec or body. This attribute is
         --  set as follows:
         --
         --    * If predicate Is_Body_With_Spec is True, the reference denotes
         --      the corresponding spec.
         --
         --    * If predicate Is_Spec_With_Body is True, the reference denotes
         --      the corresponding body.
         --
         --    * Otherwise the attribute remains empty.

         In_Elaboration_Order : Boolean := False;
         --  Set when this vertex is elaborated

         Pending_Predecessors : Natural := 0;
         --  The number of pending predecessor vertices this vertex must wait
         --  on before it can be elaborated.

         Unit : Unit_Id := No_Unit_Id;
         --  The reference to unit this vertex represents
      end record;

      No_Library_Graph_Vertex_Attributes :
        constant Library_Graph_Vertex_Attributes :=
          (Corresponding_Item   => No_Library_Graph_Vertex,
           In_Elaboration_Order => False,
           Pending_Predecessors => 0,
           Unit                 => No_Unit_Id);

      procedure Destroy_Library_Graph_Vertex_Attributes
        (Attrs : in out Library_Graph_Vertex_Attributes);
      pragma Inline (Destroy_Library_Graph_Vertex_Attributes);
      --  Destroy the contents of attributes Attrs

      package VA is new Dynamic_Hash_Tables
        (Key_Type              => Library_Graph_Vertex_Id,
         Value_Type            => Library_Graph_Vertex_Attributes,
         No_Value              => No_Library_Graph_Vertex_Attributes,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Destroy_Value         => Destroy_Library_Graph_Vertex_Attributes,
         Hash                  => Hash_Library_Graph_Vertex);

      -----------
      -- Edges --
      -----------

      procedure Destroy_Library_Graph_Edge
        (LGE_Id : in out Library_Graph_Edge_Id);
      pragma Inline (Destroy_Library_Graph_Edge);
      --  Destroy library graph edge LGE_Id

      --  The following type represents the attributes of a library graph edge

      type Library_Graph_Edge_Attributes is record
         Kind : Library_Graph_Edge_Kind := No_Edge;
         --  The nature of the library graph edge
      end record;

      No_Library_Graph_Edge_Attributes :
        constant Library_Graph_Edge_Attributes :=
          (Kind => No_Edge);

      procedure Destroy_Library_Graph_Edge_Attributes
        (Attrs : in out Library_Graph_Edge_Attributes);
      pragma Inline (Destroy_Library_Graph_Edge_Attributes);
      --  Destroy the contents of attributes Attrs

      package EA is new Dynamic_Hash_Tables
        (Key_Type              => Library_Graph_Edge_Id,
         Value_Type            => Library_Graph_Edge_Attributes,
         No_Value              => No_Library_Graph_Edge_Attributes,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Destroy_Value         => Destroy_Library_Graph_Edge_Attributes,
         Hash                  => Hash_Library_Graph_Edge);

      ----------------
      -- Components --
      ----------------

      --  The following type represents the attributes of a component

      type Component_Attributes is record
         Pending_Predecessors : Natural := 0;
         --  The number of pending predecessor components this component must
         --  wait on before it can be elaborated.
      end record;

      No_Component_Attributes : constant Component_Attributes :=
        (Pending_Predecessors => 0);

      procedure Destroy_Component_Attributes
        (Attrs : in out Component_Attributes);
      pragma Inline (Destroy_Component_Attributes);
      --  Destroy the contents of attributes Attrs

      package CA is new Dynamic_Hash_Tables
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

      ---------------
      -- Relations --
      ---------------

      --  The following type represents a relation between a predecessor and
      --  successor vertices.

      type Predecessor_Successor_Relation is record
         Predecessor : Library_Graph_Vertex_Id := No_Library_Graph_Vertex;
         --  The source vertex

         Successor : Library_Graph_Vertex_Id := No_Library_Graph_Vertex;
         --  The destination vertex
      end record;

      No_Predecessor_Successor_Relation :
        constant Predecessor_Successor_Relation :=
          (Predecessor => No_Library_Graph_Vertex,
           Successor   => No_Library_Graph_Vertex);

      function Hash_Predecessor_Successor_Relation
        (Rel : Predecessor_Successor_Relation) return Bucket_Range_Type;
      pragma Inline (Hash_Predecessor_Successor_Relation);
      --  Obtain the hash value of key Rel

      package PS is new Membership_Sets
        (Element_Type => Predecessor_Successor_Relation,
         "="          => "=",
         Hash         => Hash_Predecessor_Successor_Relation);

      ----------------
      -- Statistics --
      ----------------

      type Library_Graph_Edge_Counts is
        array (Library_Graph_Edge_Kind) of Natural;

      -----------
      -- Units --
      -----------

      package UV is new Dynamic_Hash_Tables
        (Key_Type              => Unit_Id,
         Value_Type            => Library_Graph_Vertex_Id,
         No_Value              => No_Library_Graph_Vertex,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Destroy_Value         => Destroy_Library_Graph_Vertex,
         Hash                  => Hash_Unit);

      -----------
      -- Graph --
      -----------

      package DG is new Directed_Graphs
        (Vertex_Id   => Library_Graph_Vertex_Id,
         No_Vertex   => No_Library_Graph_Vertex,
         Hash_Vertex => Hash_Library_Graph_Vertex,
         Same_Vertex => "=",
         Edge_Id     => Library_Graph_Edge_Id,
         No_Edge     => No_Library_Graph_Edge,
         Hash_Edge   => Hash_Library_Graph_Edge,
         Same_Edge   => "=");

      --  The following type represents the attributes of a library graph

      type Library_Graph_Attributes is record
         Component_Attributes : CA.Dynamic_Hash_Table := CA.Nil;
         --  The map of component -> component attributes for all components in
         --  the graph.

         Counts : Library_Graph_Edge_Counts := (others => 0);
         --  Edge statistics

         Edge_Attributes : EA.Dynamic_Hash_Table := EA.Nil;
         --  The map of edge -> edge attributes for all edges in the graph

         Graph : DG.Directed_Graph := DG.Nil;
         --  The underlying graph describing the relations between edges and
         --  vertices.

         Relations : PS.Membership_Set := PS.Nil;
         --  The set of relations between successors and predecessors, used to
         --  prevent duplicate edges in the graph.

         Unit_To_Vertex : UV.Dynamic_Hash_Table := UV.Nil;
         --  The map of unit -> vertex

         Vertex_Attributes : VA.Dynamic_Hash_Table := VA.Nil;
         --  The map of vertex -> vertex attributes for all vertices in the
         --  graph.
      end record;

      type Library_Graph is access Library_Graph_Attributes;
      Nil : constant Library_Graph := null;

      ---------------
      -- Iterators --
      ---------------

      type All_Edge_Iterator            is new DG.All_Edge_Iterator;
      type All_Vertex_Iterator          is new DG.All_Vertex_Iterator;
      type Component_Iterator           is new DG.Component_Iterator;
      type Component_Vertex_Iterator    is new DG.Component_Vertex_Iterator;
      type Edges_To_Successors_Iterator is new DG.Outgoing_Edge_Iterator;
   end Library_Graphs;

end Bindo.Graphs;
