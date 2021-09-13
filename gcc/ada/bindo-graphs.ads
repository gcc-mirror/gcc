------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         B I N D O . G R A P H S                          --
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

--  The following unit defines the various graphs used in determining the
--  elaboration order of units.

with Types; use Types;

with Bindo.Units; use Bindo.Units;

with GNAT;                 use GNAT;
with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;
with GNAT.Graphs;          use GNAT.Graphs;
with GNAT.Lists;           use GNAT.Lists;
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

   procedure Destroy_Invocation_Graph_Edge
     (Edge : in out Invocation_Graph_Edge_Id);
   pragma Inline (Destroy_Invocation_Graph_Edge);
   --  Destroy invocation graph edge Edge

   function Hash_Invocation_Graph_Edge
     (Edge : Invocation_Graph_Edge_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Invocation_Graph_Edge);
   --  Obtain the hash value of key Edge

   function Present (Edge : Invocation_Graph_Edge_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether invocation graph edge Edge exists

   package IGE_Lists is new Doubly_Linked_Lists
     (Element_Type    => Invocation_Graph_Edge_Id,
      "="             => "=",
      Destroy_Element => Destroy_Invocation_Graph_Edge);

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
     (Vertex : Invocation_Graph_Vertex_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Invocation_Graph_Vertex);
   --  Obtain the hash value of key Vertex

   function Present (Vertex : Invocation_Graph_Vertex_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether invocation graph vertex Vertex exists

   package IGV_Sets is new Membership_Sets
     (Element_Type => Invocation_Graph_Vertex_Id,
      "="          => "=",
      Hash         => Hash_Invocation_Graph_Vertex);

   -------------------------
   -- Library graph cycle --
   -------------------------

   type Library_Graph_Cycle_Id is new Natural;
   No_Library_Graph_Cycle    : constant Library_Graph_Cycle_Id :=
                                 Library_Graph_Cycle_Id'First;
   First_Library_Graph_Cycle : constant Library_Graph_Cycle_Id :=
                                 No_Library_Graph_Cycle + 1;

   procedure Destroy_Library_Graph_Cycle
     (Cycle : in out Library_Graph_Cycle_Id);
   pragma Inline (Destroy_Library_Graph_Cycle);
   --  Destroy library graph cycle Cycle

   function Hash_Library_Graph_Cycle
     (Cycle : Library_Graph_Cycle_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Library_Graph_Cycle);
   --  Obtain the hash value of key Cycle

   function Present (Cycle : Library_Graph_Cycle_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether library graph cycle Cycle exists

   package LGC_Lists is new Doubly_Linked_Lists
     (Element_Type    => Library_Graph_Cycle_Id,
      "="             => "=",
      Destroy_Element => Destroy_Library_Graph_Cycle);

   ------------------------
   -- Library graph edge --
   ------------------------

   --  The following type denotes a library graph edge handle

   type Library_Graph_Edge_Id is new Natural;
   No_Library_Graph_Edge    : constant Library_Graph_Edge_Id :=
                                Library_Graph_Edge_Id'First;
   First_Library_Graph_Edge : constant Library_Graph_Edge_Id :=
                                No_Library_Graph_Edge + 1;

   procedure Destroy_Library_Graph_Edge
     (Edge : in out Library_Graph_Edge_Id);
   pragma Inline (Destroy_Library_Graph_Edge);
   --  Destroy library graph edge Edge

   function Hash_Library_Graph_Edge
     (Edge : Library_Graph_Edge_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Library_Graph_Edge);
   --  Obtain the hash value of key Edge

   function Present (Edge : Library_Graph_Edge_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether library graph edge Edge exists

   package LGE_Lists is new Doubly_Linked_Lists
     (Element_Type    => Library_Graph_Edge_Id,
      "="             => "=",
      Destroy_Element => Destroy_Library_Graph_Edge);

   package LGE_Sets is new Membership_Sets
     (Element_Type => Library_Graph_Edge_Id,
      "="          => "=",
      Hash         => Hash_Library_Graph_Edge);

   --------------------------
   -- Library graph vertex --
   --------------------------

   --  The following type denotes a library graph vertex handle

   type Library_Graph_Vertex_Id is new Natural;
   No_Library_Graph_Vertex    : constant Library_Graph_Vertex_Id :=
                                  Library_Graph_Vertex_Id'First;
   First_Library_Graph_Vertex : constant Library_Graph_Vertex_Id :=
                                  No_Library_Graph_Vertex + 1;

   procedure Destroy_Library_Graph_Vertex
     (Vertex : in out Library_Graph_Vertex_Id);
   pragma Inline (Destroy_Library_Graph_Vertex);
   --  Destroy library graph vertex Vertex

   function Hash_Library_Graph_Vertex
     (Vertex : Library_Graph_Vertex_Id) return Bucket_Range_Type;
   pragma Inline (Hash_Library_Graph_Vertex);
   --  Obtain the hash value of key Vertex

   function Present (Vertex : Library_Graph_Vertex_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether library graph vertex Vertex exists

   package LGV_Lists is new Doubly_Linked_Lists
     (Element_Type    => Library_Graph_Vertex_Id,
      "="             => "=",
      Destroy_Element => Destroy_Library_Graph_Vertex);

   package LGV_Sets is new Membership_Sets
     (Element_Type => Library_Graph_Vertex_Id,
      "="          => "=",
      Hash         => Hash_Library_Graph_Vertex);

   --------------------
   -- Library_Graphs --
   --------------------

   package Library_Graphs is

      --  The following type represents the various kinds of library graph
      --  cycles. The ordering of kinds is significant, where a literal with
      --  lower ordinal has a higher precedence than one with higher ordinal.

      type Library_Graph_Cycle_Kind is
        (Elaborate_Body_Cycle,
         --  A cycle that involves at least one spec-body pair, where the
         --  spec is subject to pragma Elaborate_Body. This is the highest
         --  precedence cycle.

         Elaborate_Cycle,
         --  A cycle that involves at least one Elaborate edge

         Elaborate_All_Cycle,
         --  A cycle that involves at least one Elaborate_All edge

         Forced_Cycle,
         --  A cycle that involves at least one edge which is a byproduct of
         --  the forced-elaboration-order file.

         Invocation_Cycle,
         --  A cycle that involves at least one invocation edge. This is the
         --  lowest precedence cycle.

         No_Cycle_Kind);

      --  The following type represents the various kinds of library edges. The
      --  order is important here, and corresponds to the order in which edges
      --  are added to the graph. See Add_Edge_Kind_Check for details. If
      --  changes are made such that new edge kinds are added or similar, we
      --  need to make sure this type matches the code in Add_Edge_Kind_Check,
      --  and Add_Edge_Kind_Check matches the order of edge adding. Likewise,
      --  if the edge-adding order changes, we need consistency between this
      --  enumeration type, the edge-adding order, and Add_Edge_Kind_Check.

      type Library_Graph_Edge_Kind is
        (Spec_Before_Body_Edge,
         --  Successor denotes a body, Predecessor denotes a spec

         Elaborate_Edge,
         --  Successor withs Predecessor, and has pragma Elaborate for it

         Elaborate_All_Edge,
         --  Successor withs Predecessor, and has pragma Elaborate_All for it

         With_Edge,
         --  Successor withs Predecessor

         Forced_Edge,
         --  Successor is forced to with Predecessor by virtue of an existing
         --  elaboration order provided in a file.

         Invocation_Edge,
         --  An invocation construct in unit Successor invokes a target in unit
         --  Predecessor.

         Body_Before_Spec_Edge,
         --  Successor denotes spec, Predecessor denotes a body. This is a
         --  special edge kind used only during the discovery of components.
         --  Note that a body can never be elaborated before its spec.

         No_Edge);

      -----------
      -- Graph --
      -----------

      --  The following type denotes a library graph handle. Each instance must
      --  be created using routine Create.

      type Library_Graph is private;
      Nil : constant Library_Graph;

      type LGE_Predicate_Ptr is access function
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;

      type LGV_Comparator_Ptr is access function
        (G           : Library_Graph;
         Vertex      : Library_Graph_Vertex_Id;
         Compared_To : Library_Graph_Vertex_Id) return Precedence_Kind;

      type LGV_Predicate_Ptr is access function
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;

      ----------------------
      -- Graph operations --
      ----------------------

      procedure Add_Edge
        (G              : Library_Graph;
         Pred           : Library_Graph_Vertex_Id;
         Succ           : Library_Graph_Vertex_Id;
         Kind           : Library_Graph_Edge_Kind;
         Activates_Task : Boolean);
      pragma Inline (Add_Edge);
      --  Create a new edge in library graph G with source vertex Pred and
      --  destination vertex Succ. Kind denotes the nature of the edge. Flag
      --  Activates_Task should be set when the edge involves task activation.

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

      procedure Find_Cycles (G : Library_Graph);
      pragma Inline (Find_Cycles);
      --  Find all cycles in library graph G

      function Highest_Precedence_Cycle
        (G : Library_Graph) return Library_Graph_Cycle_Id;
      pragma Inline (Highest_Precedence_Cycle);
      --  Obtain the cycle with highest precedence among all other cycles of
      --  library graph G.

      function Present (G : Library_Graph) return Boolean;
      pragma Inline (Present);
      --  Determine whether library graph G exists

      -----------------------
      -- Vertex attributes --
      -----------------------

      function Component
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Component_Id;
      pragma Inline (Component);
      --  Obtain the component where vertex Vertex of library graph G resides

      function Corresponding_Item
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Corresponding_Item);
      --  Obtain the complementary vertex which represents the corresponding
      --  spec or body of vertex Vertex of library graph G.

      function Corresponding_Vertex
        (G    : Library_Graph;
         U_Id : Unit_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Corresponding_Vertex);
      --  Obtain the corresponding vertex of library graph G which represents
      --  unit U_Id.

      procedure Decrement_Pending_Predecessors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Edge   : Library_Graph_Edge_Id);
      pragma Inline (Decrement_Pending_Predecessors);
      --  Decrease the number of pending predecessors vertex Vertex which was
      --  reached via edge Edge of library graph G must wait until it can be
      --  elaborated.

      function File_Name
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return File_Name_Type;
      pragma Inline (File_Name);
      --  Obtain the name of the file where vertex Vertex of library graph G
      --  resides.

      function In_Elaboration_Order
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (In_Elaboration_Order);
      --  Determine whether vertex Vertex of library graph G is already in some
      --  elaboration order.

      function Invocation_Graph_Encoding
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id)
         return Invocation_Graph_Encoding_Kind;
      pragma Inline (Invocation_Graph_Encoding);
      --  Obtain the encoding format used to capture information related to
      --  invocation vertices and edges that reside within vertex Vertex of
      --  library graph G.

      function Name
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Unit_Name_Type;
      pragma Inline (Name);
      --  Obtain the name of the unit which vertex Vertex of library graph G
      --  represents.

      procedure Pending_Predecessors_For_Elaboration
        (G            : Library_Graph;
         Vertex       : Library_Graph_Vertex_Id;
         Strong_Preds : out Natural;
         Weak_Preds   : out Natural);
      pragma Inline (Pending_Predecessors_For_Elaboration);
      --  Obtain the number of pending strong and weak predecessors of vertex
      --  Vertex of library graph G, taking into account Elaborate_Body pairs.
      --  Strong predecessors are returned in Strong_Preds. Weak predecessors
      --  are returned in Weak_Preds.

      function Pending_Strong_Predecessors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Natural;
      pragma Inline (Pending_Strong_Predecessors);
      --  Obtain the number of pending strong predecessors vertex Vertex of
      --  library graph G must wait on until it can be elaborated.

      function Pending_Weak_Predecessors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Natural;
      pragma Inline (Pending_Weak_Predecessors);
      --  Obtain the number of pending weak predecessors vertex Vertex of
      --  library graph G must wait on until it can be elaborated.

      procedure Set_Corresponding_Item
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Val    : Library_Graph_Vertex_Id);
      pragma Inline (Set_Corresponding_Item);
      --  Set the complementary vertex which represents the corresponding
      --  spec or body of vertex Vertex of library graph G to value Val.

      procedure Set_In_Elaboration_Order
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Val    : Boolean := True);
      pragma Inline (Set_In_Elaboration_Order);
      --  Mark vertex Vertex of library graph G as included in some elaboration
      --  order depending on value Val.

      function Unit
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Unit_Id;
      pragma Inline (Unit);
      --  Obtain the unit vertex Vertex of library graph G represents

      ---------------------
      -- Edge attributes --
      ---------------------

      function Activates_Task
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Activates_Task);
      --  Determine whether edge Edge of library graph G activates a task

      function Kind
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Edge_Kind;
      pragma Inline (Kind);
      --  Obtain the nature of edge Edge of library graph G

      function Predecessor
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Predecessor);
      --  Obtain the predecessor vertex of edge Edge of library graph G

      function Successor
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Successor);
      --  Obtain the successor vertex of edge Edge of library graph G

      --------------------------
      -- Component attributes --
      --------------------------

      procedure Decrement_Pending_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id;
         Edge : Library_Graph_Edge_Id);
      pragma Inline (Decrement_Pending_Predecessors);
      --  Decrease the number of pending predecessors component Comp which was
      --  reached via edge Edge of library graph G must wait on until it can be
      --  elaborated.

      function Pending_Strong_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id) return Natural;
      pragma Inline (Pending_Strong_Predecessors);
      --  Obtain the number of pending strong predecessors component Comp of
      --  library graph G must wait on until it can be elaborated.

      function Pending_Weak_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id) return Natural;
      pragma Inline (Pending_Weak_Predecessors);
      --  Obtain the number of pending weak predecessors component Comp of
      --  library graph G must wait on until it can be elaborated.

      ----------------------
      -- Cycle attributes --
      ----------------------

      function Invocation_Edge_Count
        (G      : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Natural;
      pragma Inline (Invocation_Edge_Count);
      --  Obtain the number of invocation edges in cycle Cycle of library
      --  graph G.

      function Kind
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Library_Graph_Cycle_Kind;
      pragma Inline (Kind);
      --  Obtain the nature of cycle Cycle of library graph G

      function Length
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Natural;
      pragma Inline (Length);
      --  Obtain the length of cycle Cycle of library graph G

      ---------------
      -- Semantics --
      ---------------

      function Complementary_Vertex
        (G                : Library_Graph;
         Vertex           : Library_Graph_Vertex_Id;
         Force_Complement : Boolean) return Library_Graph_Vertex_Id;
      pragma Inline (Complementary_Vertex);
      --  Obtain the complementary vertex of vertex Vertex of library graph G
      --  as follows:
      --
      --    * If Vertex is the spec of an Elaborate_Body pair, return the body
      --    * If Vertex is the body of an Elaborate_Body pair, return the spec
      --
      --  This behavior can be forced by setting flag Force_Complement to True.

      function Contains_Elaborate_All_Edge
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Boolean;
      pragma Inline (Contains_Elaborate_All_Edge);
      --  Determine whether cycle Cycle of library graph G contains an
      --  Elaborate_All edge.

      function Contains_Static_Successor_Edge
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Boolean;
      pragma Inline (Contains_Static_Successor_Edge);
      --  Determine whether cycle Cycle of library graph G contains an edge
      --  where the successor was compiled using the static model.

      function Contains_Task_Activation
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Boolean;
      pragma Inline (Contains_Task_Activation);
      --  Determine whether cycle Cycle of library graph G contains an
      --  invocation edge where the path it represents involves a task
      --  activation.

      function Has_Elaborate_All_Cycle (G : Library_Graph) return Boolean;
      pragma Inline (Has_Elaborate_All_Cycle);
      --  Determine whether library graph G contains a cycle involving pragma
      --  Elaborate_All.

      function Has_No_Elaboration_Code
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Has_No_Elaboration_Code);
      --  Determine whether vertex Vertex of library graph G represents a unit
      --  that lacks elaboration code.

      function In_Same_Component
        (G     : Library_Graph;
         Left  : Library_Graph_Vertex_Id;
         Right : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (In_Same_Component);
      --  Determine whether vertices Left and Right of library graph G reside
      --  in the same component.

      function Is_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Body);
      --  Determine whether vertex Vertex of library graph G denotes a body

      function Is_Body_Of_Spec_With_Elaborate_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Body_Of_Spec_With_Elaborate_Body);
      --  Determine whether vertex Vertex of library graph G denotes a body
      --  with a corresponding spec, and the spec has pragma Elaborate_Body.

      function Is_Body_With_Spec
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Body_With_Spec);
      --  Determine whether vertex Vertex of library graph G denotes a body
      --  with a corresponding spec.

      function Is_Dynamically_Elaborated
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Dynamically_Elaborated);
      --  Determine whether vertex Vertex of library graph G was compiled
      --  using the dynamic model.

      function Is_Elaborable_Component
        (G    : Library_Graph;
         Comp : Component_Id) return Boolean;
      pragma Inline (Is_Elaborable_Component);
      --  Determine whether component Comp of library graph G is not waiting on
      --  any predecessors, and can thus be elaborated.

      function Is_Elaborable_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Elaborable_Vertex);
      --  Determine whether vertex Vertex of library graph G is not waiting on
      --  any predecessors, and can thus be elaborated.

      function Is_Elaborate_All_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Elaborate_All_Edge);
      --  Determine whether edge Edge of library graph G is an edge whose
      --  predecessor is subject to pragma Elaborate_All.

      function Is_Elaborate_Body_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Elaborate_Body_Edge);
      --  Determine whether edge Edge of library graph G has a successor
      --  that is either a spec subject to pragma Elaborate_Body, or a body
      --  that completes such a spec.

      function Is_Elaborate_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Elaborate_Edge);
      --  Determine whether edge Edge of library graph G is an edge whose
      --  predecessor is subject to pragma Elaborate.

      function Is_Elaborate_Body_Pair
        (G           : Library_Graph;
         Spec_Vertex : Library_Graph_Vertex_Id;
         Body_Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Elaborate_Body_Pair);
      --  Determine whether vertices Spec_Vertex and Body_Vertex of library
      --  graph G denote a spec subject to Elaborate_Body and its completing
      --  body.

      function Is_Forced_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Forced_Edge);
      --  Determine whether edge Edge of library graph G is a byproduct of the
      --  forced-elaboration-order file.

      function Is_Internal_Unit
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Internal_Unit);
      --  Determine whether vertex Vertex of library graph G denotes an
      --  internal unit.

      function Is_Invocation_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Invocation_Edge);
      --  Determine whether edge Edge of library graph G came from the
      --  traversal of the invocation graph.

      function Is_Predefined_Unit
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Predefined_Unit);
      --  Determine whether vertex Vertex of library graph G denotes a
      --  predefined unit.

      function Is_Preelaborated_Unit
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Preelaborated_Unit);
      --  Determine whether vertex Vertex of library graph G denotes a unit
      --  subject to pragma Pure or Preelaborable.

      function Is_Spec
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Spec);
      --  Determine whether vertex Vertex of library graph G denotes a spec

      function Is_Spec_Before_Body_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Spec_Before_Body_Edge);
      --  Determine whether edge Edge of library graph G links a predecessor
      --  spec and a successor body belonging to the same unit.

      function Is_Spec_With_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Spec_With_Body);
      --  Determine whether vertex Vertex of library graph G denotes a spec
      --  with a corresponding body.

      function Is_Spec_With_Elaborate_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Spec_With_Elaborate_Body);
      --  Determine whether vertex Vertex of library graph G denotes a spec
      --  with a corresponding body, and is subject to pragma Elaborate_Body.

      function Is_Weakly_Elaborable_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Weakly_Elaborable_Vertex);
      --  Determine whether vertex Vertex of library graph G is waiting on
      --  weak predecessors only, in which case it can be elaborated assuming
      --  that the weak edges will not be exercised at elaboration time.

      function Is_With_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_With_Edge);
      --  Determine whether edge Edge of library graph G is the result of a
      --  with dependency between its successor and predecessor.

      function Needs_Elaboration
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Needs_Elaboration);
      --  Determine whether vertex Vertex of library graph G represents a unit
      --  that needs to be elaborated.

      function Proper_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Proper_Body);
      --  Obtain the body of vertex Vertex of library graph G

      function Proper_Spec
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Proper_Spec);
      --  Obtain the spec of vertex Vertex of library graph G

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

      function Number_Of_Cycles (G : Library_Graph) return Natural;
      pragma Inline (Number_Of_Cycles);
      --  Obtain the total number of cycles in library graph G

      function Number_Of_Edges (G : Library_Graph) return Natural;
      pragma Inline (Number_Of_Edges);
      --  Obtain the total number of edges in library graph G

      function Number_Of_Edges_To_Successors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Natural;
      pragma Inline (Number_Of_Edges_To_Successors);
      --  Obtain the total number of edges to successors vertex Vertex of
      --  library graph G has.

      function Number_Of_Vertices (G : Library_Graph) return Natural;
      pragma Inline (Number_Of_Vertices);
      --  Obtain the total number of vertices in library graph G

      ---------------
      -- Iterators --
      ---------------

      --  The following type represents an iterator over all cycles of a
      --  library graph.

      type All_Cycle_Iterator is private;

      function Has_Next (Iter : All_Cycle_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more cycles to examine

      function Iterate_All_Cycles
        (G : Library_Graph) return All_Cycle_Iterator;
      pragma Inline (Iterate_All_Cycles);
      --  Obtain an iterator over all cycles of library graph G

      procedure Next
        (Iter  : in out All_Cycle_Iterator;
         Cycle : out Library_Graph_Cycle_Id);
      pragma Inline (Next);
      --  Return the current cycle referenced by iterator Iter and advance to
      --  the next available cycle.

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
        (Iter : in out All_Edge_Iterator;
         Edge : out Library_Graph_Edge_Id);
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
         Vertex : out Library_Graph_Vertex_Id);
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
         Vertex : out Library_Graph_Vertex_Id);
      pragma Inline (Next);
      --  Return the current vertex referenced by iterator Iter and advance
      --  to the next available vertex.

      --  The following type represents an iterator over all edges that form a
      --  cycle.

      type Edges_Of_Cycle_Iterator is private;

      function Has_Next (Iter : Edges_Of_Cycle_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more edges to examine

      function Iterate_Edges_Of_Cycle
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Edges_Of_Cycle_Iterator;
      pragma Inline (Iterate_Edges_Of_Cycle);
      --  Obtain an iterator over all edges that form cycle Cycle of library
      --  graph G.

      procedure Next
        (Iter : in out Edges_Of_Cycle_Iterator;
         Edge : out Library_Graph_Edge_Id);
      pragma Inline (Next);
      --  Return the current edge referenced by iterator Iter and advance to
      --  the next available edge.

      --  The following type represents an iterator over all edges that reach
      --  successors starting from a particular predecessor vertex.

      type Edges_To_Successors_Iterator is private;

      function Has_Next (Iter : Edges_To_Successors_Iterator) return Boolean;
      pragma Inline (Has_Next);
      --  Determine whether iterator Iter has more edges to examine

      function Iterate_Edges_To_Successors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Edges_To_Successors_Iterator;
      pragma Inline (Iterate_Edges_To_Successors);
      --  Obtain an iterator over all edges to successors with predecessor
      --  vertex Vertex of library graph G.

      procedure Next
        (Iter : in out Edges_To_Successors_Iterator;
         Edge : out Library_Graph_Edge_Id);
      pragma Inline (Next);
      --  Return the current edge referenced by iterator Iter and advance to
      --  the next available edge.

   private

      --------------
      -- Vertices --
      --------------

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

         Pending_Strong_Predecessors : Natural := 0;
         --  The number of pending strong predecessor vertices this vertex must
         --  wait on before it can be elaborated.

         Pending_Weak_Predecessors : Natural := 0;
         --  The number of weak predecessor vertices this vertex must wait on
         --  before it can be elaborated.

         Unit : Unit_Id := No_Unit_Id;
         --  The reference to unit this vertex represents
      end record;

      No_Library_Graph_Vertex_Attributes :
        constant Library_Graph_Vertex_Attributes :=
          (Corresponding_Item          => No_Library_Graph_Vertex,
           In_Elaboration_Order        => False,
           Pending_Strong_Predecessors => 0,
           Pending_Weak_Predecessors   => 0,
           Unit                        => No_Unit_Id);

      procedure Destroy_Library_Graph_Vertex_Attributes
        (Attrs : in out Library_Graph_Vertex_Attributes);
      pragma Inline (Destroy_Library_Graph_Vertex_Attributes);
      --  Destroy the contents of attributes Attrs

      package LGV_Tables is new Dynamic_Hash_Tables
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

      --  The following type represents the attributes of a library graph edge

      type Library_Graph_Edge_Attributes is record
         Activates_Task : Boolean := False;
         --  Set for an invocation edge, where at least one of the paths the
         --  edge represents activates a task.

         Kind : Library_Graph_Edge_Kind := No_Edge;
         --  The nature of the library graph edge
      end record;

      No_Library_Graph_Edge_Attributes :
        constant Library_Graph_Edge_Attributes :=
          (Activates_Task => False,
           Kind           => No_Edge);

      procedure Destroy_Library_Graph_Edge_Attributes
        (Attrs : in out Library_Graph_Edge_Attributes);
      pragma Inline (Destroy_Library_Graph_Edge_Attributes);
      --  Destroy the contents of attributes Attrs

      package LGE_Tables is new Dynamic_Hash_Tables
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
         Pending_Strong_Predecessors : Natural := 0;
         --  The number of pending strong predecessor components this component
         --  must wait on before it can be elaborated.

         Pending_Weak_Predecessors : Natural := 0;
         --  The number of pending weak predecessor components this component
         --  must wait on before it can be elaborated.
      end record;

      No_Component_Attributes : constant Component_Attributes :=
        (Pending_Strong_Predecessors => 0,
         Pending_Weak_Predecessors   => 0);

      procedure Destroy_Component_Attributes
        (Attrs : in out Component_Attributes);
      pragma Inline (Destroy_Component_Attributes);
      --  Destroy the contents of attributes Attrs

      package Component_Tables is new Dynamic_Hash_Tables
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

      ------------
      -- Cycles --
      ------------

      --  The following type represents the attributes of a cycle

      type Library_Graph_Cycle_Attributes is record
         Invocation_Edge_Count : Natural := 0;
         --  The number of invocation edges within the cycle

         Kind : Library_Graph_Cycle_Kind := No_Cycle_Kind;
         --  The nature of the cycle

         Path : LGE_Lists.Doubly_Linked_List := LGE_Lists.Nil;
         --  The path of edges that form the cycle
      end record;

      No_Library_Graph_Cycle_Attributes :
        constant Library_Graph_Cycle_Attributes :=
          (Invocation_Edge_Count => 0,
           Kind                  => No_Cycle_Kind,
           Path                  => LGE_Lists.Nil);

      procedure Destroy_Library_Graph_Cycle_Attributes
        (Attrs : in out Library_Graph_Cycle_Attributes);
      pragma Inline (Destroy_Library_Graph_Cycle_Attributes);
      --  Destroy the contents of attributes Attrs

      function Hash_Library_Graph_Cycle_Attributes
        (Attrs : Library_Graph_Cycle_Attributes) return Bucket_Range_Type;
      pragma Inline (Hash_Library_Graph_Cycle_Attributes);
      --  Obtain the hash of key Attrs

      function Same_Library_Graph_Cycle_Attributes
        (Left  : Library_Graph_Cycle_Attributes;
         Right : Library_Graph_Cycle_Attributes) return Boolean;
      pragma Inline (Same_Library_Graph_Cycle_Attributes);
      --  Determine whether cycle attributes Left and Right are the same

      package LGC_Tables is new Dynamic_Hash_Tables
        (Key_Type              => Library_Graph_Cycle_Id,
         Value_Type            => Library_Graph_Cycle_Attributes,
         No_Value              => No_Library_Graph_Cycle_Attributes,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Destroy_Value         => Destroy_Library_Graph_Cycle_Attributes,
         Hash                  => Hash_Library_Graph_Cycle);

      --------------------
      -- Recorded edges --
      --------------------

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

      package RE_Sets is new Membership_Sets
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

      package Unit_Tables is new Dynamic_Hash_Tables
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
         Component_Attributes : Component_Tables.Dynamic_Hash_Table :=
                                  Component_Tables.Nil;
         --  The map of component -> component attributes for all components in
         --  the graph.

         Counts : Library_Graph_Edge_Counts := (others => 0);
         --  Edge statistics

         Cycle_Attributes : LGC_Tables.Dynamic_Hash_Table := LGC_Tables.Nil;
         --  The map of cycle -> cycle attributes for all cycles in the graph

         Cycles : LGC_Lists.Doubly_Linked_List := LGC_Lists.Nil;
         --  The list of all cycles in the graph, sorted based on precedence

         Edge_Attributes : LGE_Tables.Dynamic_Hash_Table := LGE_Tables.Nil;
         --  The map of edge -> edge attributes for all edges in the graph

         Graph : DG.Directed_Graph := DG.Nil;
         --  The underlying graph describing the relations between edges and
         --  vertices.

         Recorded_Edges : RE_Sets.Membership_Set := RE_Sets.Nil;
         --  The set of recorded edges, used to prevent duplicate edges in the
         --  graph.

         Unit_To_Vertex : Unit_Tables.Dynamic_Hash_Table := Unit_Tables.Nil;
         --  The map of unit -> vertex

         Vertex_Attributes : LGV_Tables.Dynamic_Hash_Table := LGV_Tables.Nil;
         --  The map of vertex -> vertex attributes for all vertices in the
         --  graph.
      end record;

      type Library_Graph is access Library_Graph_Attributes;
      Nil : constant Library_Graph := null;

      ---------------
      -- Iterators --
      ---------------

      type All_Cycle_Iterator           is new LGC_Lists.Iterator;
      type All_Edge_Iterator            is new DG.All_Edge_Iterator;
      type All_Vertex_Iterator          is new DG.All_Vertex_Iterator;
      type Component_Iterator           is new DG.Component_Iterator;
      type Component_Vertex_Iterator    is new DG.Component_Vertex_Iterator;
      type Edges_Of_Cycle_Iterator      is new LGE_Lists.Iterator;
      type Edges_To_Successors_Iterator is new DG.Outgoing_Edge_Iterator;
   end Library_Graphs;

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
        (G           : Invocation_Graph;
         IC_Id       : Invocation_Construct_Id;
         Body_Vertex : Library_Graph_Vertex_Id;
         Spec_Vertex : Library_Graph_Vertex_Id);
      pragma Inline (Add_Vertex);
      --  Create a new vertex in invocation graph G. IC_Id is the invocation
      --  construct the vertex describes. Body_Vertex denotes the library graph
      --  vertex where the invocation construct's body is declared. Spec_Vertex
      --  is the library graph vertex where the invocation construct's spec is
      --  declared.

      function Create
        (Initial_Vertices : Positive;
         Initial_Edges    : Positive;
         Lib_Graph        : Library_Graphs.Library_Graph)
        return Invocation_Graph;
      pragma Inline (Create);
      --  Create a new empty graph with vertex capacity Initial_Vertices
      --  and edge capacity Initial_Edges. Lib_Graph is the library graph
      --  corresponding to this invocation graph.

      function Get_Lib_Graph
        (G : Invocation_Graph) return Library_Graphs.Library_Graph;
      pragma Inline (Get_Lib_Graph);
      --  Return the library graph corresponding to this invocation graph

      procedure Destroy (G : in out Invocation_Graph);
      pragma Inline (Destroy);
      --  Destroy the contents of invocation graph G, rendering it unusable

      function Present (G : Invocation_Graph) return Boolean;
      pragma Inline (Present);
      --  Determine whether invocation graph G exists

      -----------------------
      -- Vertex attributes --
      -----------------------

      function Body_Vertex
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Body_Vertex);
      --  Obtain the library graph vertex where the body of the invocation
      --  construct represented by vertex Vertex of invocation graph G is
      --  declared.

      function Column
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Nat;
      pragma Inline (Column);
      --  Obtain the column number where the invocation construct vertex Vertex
      --  of invocation graph G describes.

      function Construct
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Invocation_Construct_Id;
      pragma Inline (Construct);
      --  Obtain the invocation construct vertex Vertex of invocation graph G
      --  describes.

      function Corresponding_Vertex
        (G     : Invocation_Graph;
         IS_Id : Invocation_Signature_Id) return Invocation_Graph_Vertex_Id;
      pragma Inline (Corresponding_Vertex);
      --  Obtain the vertex of invocation graph G that corresponds to signature
      --  IS_Id.

      function Line
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Nat;
      pragma Inline (Line);
      --  Obtain the line number where the invocation construct vertex Vertex
      --  of invocation graph G describes.

      function Name
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Name_Id;
      pragma Inline (Name);
      --  Obtain the name of the construct vertex Vertex of invocation graph G
      --  describes.

      function Spec_Vertex
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Library_Graph_Vertex_Id;
      pragma Inline (Spec_Vertex);
      --  Obtain the library graph vertex where the spec of the invocation
      --  construct represented by vertex Vertex of invocation graph G is
      --  declared.

      ---------------------
      -- Edge attributes --
      ---------------------

      function Extra
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) return Name_Id;
      pragma Inline (Extra);
      --  Obtain the extra name used in error diagnostics of edge Edge of
      --  invocation graph G.

      function Kind
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) return Invocation_Kind;
      pragma Inline (Kind);
      --  Obtain the nature of edge Edge of invocation graph G

      function Relation
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) return Invocation_Relation_Id;
      pragma Inline (Relation);
      --  Obtain the relation edge Edge of invocation graph G describes

      function Target
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) return Invocation_Graph_Vertex_Id;
      pragma Inline (Target);
      --  Obtain the target vertex edge Edge of invocation graph G designates

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
         Vertex : Invocation_Graph_Vertex_Id) return Natural;
      pragma Inline (Number_Of_Edges_To_Targets);
      --  Obtain the total number of edges to targets vertex Vertex of
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
        (Iter : in out All_Edge_Iterator;
         Edge : out Invocation_Graph_Edge_Id);
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
         Vertex : out Invocation_Graph_Vertex_Id);
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
         Vertex : Invocation_Graph_Vertex_Id) return Edges_To_Targets_Iterator;
      pragma Inline (Iterate_Edges_To_Targets);
      --  Obtain an iterator over all edges to targets with source vertex
      --  Vertex of invocation graph G.

      procedure Next
        (Iter : in out Edges_To_Targets_Iterator;
         Edge : out Invocation_Graph_Edge_Id);
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
        (Vertex : in out Invocation_Graph_Vertex_Id);
      pragma Inline (Destroy_Invocation_Graph_Vertex);
      --  Destroy invocation graph vertex Vertex

      --  The following type represents the attributes of an invocation graph
      --  vertex.

      type Invocation_Graph_Vertex_Attributes is record
         Body_Vertex : Library_Graph_Vertex_Id := No_Library_Graph_Vertex;
         --  Reference to the library graph vertex where the body of this
         --  vertex resides.

         Construct : Invocation_Construct_Id := No_Invocation_Construct;
         --  Reference to the invocation construct this vertex represents

         Spec_Vertex : Library_Graph_Vertex_Id := No_Library_Graph_Vertex;
         --  Reference to the library graph vertex where the spec of this
         --  vertex resides.
      end record;

      No_Invocation_Graph_Vertex_Attributes :
        constant Invocation_Graph_Vertex_Attributes :=
          (Body_Vertex => No_Library_Graph_Vertex,
           Construct   => No_Invocation_Construct,
           Spec_Vertex => No_Library_Graph_Vertex);

      procedure Destroy_Invocation_Graph_Vertex_Attributes
        (Attrs : in out Invocation_Graph_Vertex_Attributes);
      pragma Inline (Destroy_Invocation_Graph_Vertex_Attributes);
      --  Destroy the contents of attributes Attrs

      package IGV_Tables is new Dynamic_Hash_Tables
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
        (Edge : in out Invocation_Graph_Edge_Id);
      pragma Inline (Destroy_Invocation_Graph_Edge);
      --  Destroy invocation graph edge Edge

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

      package IGE_Tables is new Dynamic_Hash_Tables
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

      package Relation_Sets is new Membership_Sets
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

      package Signature_Tables is new Dynamic_Hash_Tables
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

      package IGV_Sets is new Membership_Sets
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

         Edge_Attributes : IGE_Tables.Dynamic_Hash_Table := IGE_Tables.Nil;
         --  The map of edge -> edge attributes for all edges in the graph

         Graph : DG.Directed_Graph := DG.Nil;
         --  The underlying graph describing the relations between edges and
         --  vertices.

         Relations : Relation_Sets.Membership_Set := Relation_Sets.Nil;
         --  The set of relations between source and targets, used to prevent
         --  duplicate edges in the graph.

         Roots : IGV_Sets.Membership_Set := IGV_Sets.Nil;
         --  The set of elaboration root vertices

         Signature_To_Vertex : Signature_Tables.Dynamic_Hash_Table :=
                                 Signature_Tables.Nil;
         --  The map of signature -> vertex

         Vertex_Attributes : IGV_Tables.Dynamic_Hash_Table := IGV_Tables.Nil;
         --  The map of vertex -> vertex attributes for all vertices in the
         --  graph.

         Lib_Graph : Library_Graphs.Library_Graph;
      end record;

      type Invocation_Graph is access Invocation_Graph_Attributes;
      Nil : constant Invocation_Graph := null;

      ---------------
      -- Iterators --
      ---------------

      type All_Edge_Iterator         is new DG.All_Edge_Iterator;
      type All_Vertex_Iterator       is new DG.All_Vertex_Iterator;
      type Edges_To_Targets_Iterator is new DG.Outgoing_Edge_Iterator;
      type Elaboration_Root_Iterator is new IGV_Sets.Iterator;
   end Invocation_Graphs;

end Bindo.Graphs;
