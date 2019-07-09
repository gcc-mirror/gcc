------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         B I N D O . G R A P H S                          --
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

with Ada.Unchecked_Deallocation;

with Butil;  use Butil;
with Debug;  use Debug;
with Output; use Output;

with Bindo.Writers;
use  Bindo.Writers;

package body Bindo.Graphs is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Sequence_Next_Cycle return Library_Graph_Cycle_Id;
   pragma Inline (Sequence_Next_Cycle);
   --  Generate a new unique library graph cycle handle

   function Sequence_Next_Edge return Invocation_Graph_Edge_Id;
   pragma Inline (Sequence_Next_Edge);
   --  Generate a new unique invocation graph edge handle

   function Sequence_Next_Edge return Library_Graph_Edge_Id;
   pragma Inline (Sequence_Next_Edge);
   --  Generate a new unique library graph edge handle

   function Sequence_Next_Vertex return Invocation_Graph_Vertex_Id;
   pragma Inline (Sequence_Next_Vertex);
   --  Generate a new unique invocation graph vertex handle

   function Sequence_Next_Vertex return Library_Graph_Vertex_Id;
   pragma Inline (Sequence_Next_Vertex);
   --  Generate a new unique library graph vertex handle

   -----------------------------------
   -- Destroy_Invocation_Graph_Edge --
   -----------------------------------

   procedure Destroy_Invocation_Graph_Edge
     (Edge : in out Invocation_Graph_Edge_Id)
   is
      pragma Unreferenced (Edge);
   begin
      null;
   end Destroy_Invocation_Graph_Edge;

   ---------------------------------
   -- Destroy_Library_Graph_Cycle --
   ---------------------------------

   procedure Destroy_Library_Graph_Cycle
     (Cycle : in out Library_Graph_Cycle_Id)
   is
      pragma Unreferenced (Cycle);
   begin
      null;
   end Destroy_Library_Graph_Cycle;

   --------------------------------
   -- Destroy_Library_Graph_Edge --
   --------------------------------

   procedure Destroy_Library_Graph_Edge
     (Edge : in out Library_Graph_Edge_Id)
   is
      pragma Unreferenced (Edge);
   begin
      null;
   end Destroy_Library_Graph_Edge;

   --------------------------------
   -- Hash_Invocation_Graph_Edge --
   --------------------------------

   function Hash_Invocation_Graph_Edge
     (Edge : Invocation_Graph_Edge_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (Edge));

      return Bucket_Range_Type (Edge);
   end Hash_Invocation_Graph_Edge;

   ----------------------------------
   -- Hash_Invocation_Graph_Vertex --
   ----------------------------------

   function Hash_Invocation_Graph_Vertex
     (Vertex : Invocation_Graph_Vertex_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (Vertex));

      return Bucket_Range_Type (Vertex);
   end Hash_Invocation_Graph_Vertex;

   ------------------------------
   -- Hash_Library_Graph_Cycle --
   ------------------------------

   function Hash_Library_Graph_Cycle
     (Cycle : Library_Graph_Cycle_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (Cycle));

      return Bucket_Range_Type (Cycle);
   end Hash_Library_Graph_Cycle;

   -----------------------------
   -- Hash_Library_Graph_Edge --
   -----------------------------

   function Hash_Library_Graph_Edge
     (Edge : Library_Graph_Edge_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (Edge));

      return Bucket_Range_Type (Edge);
   end Hash_Library_Graph_Edge;

   -------------------------------
   -- Hash_Library_Graph_Vertex --
   -------------------------------

   function Hash_Library_Graph_Vertex
     (Vertex : Library_Graph_Vertex_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (Vertex));

      return Bucket_Range_Type (Vertex);
   end Hash_Library_Graph_Vertex;

   -----------------------
   -- Invocation_Graphs --
   -----------------------

   package body Invocation_Graphs is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Invocation_Graph_Attributes, Invocation_Graph);

      function Get_IGE_Attributes
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id)
         return Invocation_Graph_Edge_Attributes;
      pragma Inline (Get_IGE_Attributes);
      --  Obtain the attributes of edge Edge of invocation graph G

      function Get_IGV_Attributes
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id)
         return Invocation_Graph_Vertex_Attributes;
      pragma Inline (Get_IGV_Attributes);
      --  Obtain the attributes of vertex Vertex of invocation graph G

      procedure Increment_Invocation_Graph_Edge_Count
        (G    : Invocation_Graph;
         Kind : Invocation_Kind);
      pragma Inline (Increment_Invocation_Graph_Edge_Count);
      --  Increment the number of edges of king Kind in invocation graph G by
      --  one.

      function Is_Elaboration_Root
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Elaboration_Root);
      --  Determine whether vertex Vertex of invocation graph denotes the
      --  elaboration procedure of a spec or a body.

      function Is_Existing_Source_Target_Relation
        (G   : Invocation_Graph;
         Rel : Source_Target_Relation) return Boolean;
      pragma Inline (Is_Existing_Source_Target_Relation);
      --  Determine whether a source vertex and a target vertex described by
      --  relation Rel are already related in invocation graph G.

      procedure Save_Elaboration_Root
        (G    : Invocation_Graph;
         Root : Invocation_Graph_Vertex_Id);
      pragma Inline (Save_Elaboration_Root);
      --  Save elaboration root Root of invocation graph G

      procedure Set_Corresponding_Vertex
        (G      : Invocation_Graph;
         IS_Id  : Invocation_Signature_Id;
         Vertex : Invocation_Graph_Vertex_Id);
      pragma Inline (Set_Corresponding_Vertex);
      --  Associate vertex Vertex of invocation graph G with signature IS_Id

      procedure Set_Is_Existing_Source_Target_Relation
        (G   : Invocation_Graph;
         Rel : Source_Target_Relation;
         Val : Boolean := True);
      pragma Inline (Set_Is_Existing_Source_Target_Relation);
      --  Mark a source vertex and a target vertex described by relation Rel as
      --  already related in invocation graph G depending on value Val.

      procedure Set_IGE_Attributes
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id;
         Val  : Invocation_Graph_Edge_Attributes);
      pragma Inline (Set_IGE_Attributes);
      --  Set the attributes of edge Edge of invocation graph G to value Val

      procedure Set_IGV_Attributes
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id;
         Val    : Invocation_Graph_Vertex_Attributes);
      pragma Inline (Set_IGV_Attributes);
      --  Set the attributes of vertex Vertex of invocation graph G to value
      --  Val.

      --------------
      -- Add_Edge --
      --------------

      procedure Add_Edge
        (G      : Invocation_Graph;
         Source : Invocation_Graph_Vertex_Id;
         Target : Invocation_Graph_Vertex_Id;
         IR_Id  : Invocation_Relation_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Source));
         pragma Assert (Present (Target));
         pragma Assert (Present (IR_Id));

         Rel : constant Source_Target_Relation :=
                 (Source => Source,
                  Target => Target);

         Edge : Invocation_Graph_Edge_Id;

      begin
         --  Nothing to do when the source and target are already related by an
         --  edge.

         if Is_Existing_Source_Target_Relation (G, Rel) then
            return;
         end if;

         Edge := Sequence_Next_Edge;

         --  Add the edge to the underlying graph

         DG.Add_Edge
           (G           => G.Graph,
            E           => Edge,
            Source      => Source,
            Destination => Target);

         --  Build and save the attributes of the edge

         Set_IGE_Attributes
           (G    => G,
            Edge => Edge,
            Val  => (Relation => IR_Id));

         --  Mark the source and target as related by the new edge. This
         --  prevents all further attempts to link the same source and target.

         Set_Is_Existing_Source_Target_Relation (G, Rel);

         --  Update the edge statistics

         Increment_Invocation_Graph_Edge_Count (G, Kind (IR_Id));
      end Add_Edge;

      ----------------
      -- Add_Vertex --
      ----------------

      procedure Add_Vertex
        (G           : Invocation_Graph;
         IC_Id       : Invocation_Construct_Id;
         Body_Vertex : Library_Graph_Vertex_Id;
         Spec_Vertex : Library_Graph_Vertex_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (IC_Id));
         pragma Assert (Present (Body_Vertex));
         pragma Assert (Present (Spec_Vertex));

         Construct_Signature : constant Invocation_Signature_Id :=
                                 Signature (IC_Id);
         Vertex : Invocation_Graph_Vertex_Id;

      begin
         --  Nothing to do when the construct already has a vertex

         if Present (Corresponding_Vertex (G, Construct_Signature)) then
            return;
         end if;

         Vertex := Sequence_Next_Vertex;

         --  Add the vertex to the underlying graph

         DG.Add_Vertex (G.Graph, Vertex);

         --  Build and save the attributes of the vertex

         Set_IGV_Attributes
           (G      => G,
            Vertex => Vertex,
            Val    => (Body_Vertex => Body_Vertex,
                       Construct   => IC_Id,
                       Spec_Vertex => Spec_Vertex));

         --  Associate the construct with its corresponding vertex

         Set_Corresponding_Vertex (G, Construct_Signature, Vertex);

         --  Save the vertex for later processing when it denotes a spec or
         --  body elaboration procedure.

         if Is_Elaboration_Root (G, Vertex) then
            Save_Elaboration_Root (G, Vertex);
         end if;
      end Add_Vertex;

      -----------------
      -- Body_Vertex --
      -----------------

      function Body_Vertex
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Get_IGV_Attributes (G, Vertex).Body_Vertex;
      end Body_Vertex;

      ------------
      -- Column --
      ------------

      function Column
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Nat
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Column (Signature (Construct (G, Vertex)));
      end Column;

      ---------------
      -- Construct --
      ---------------

      function Construct
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Invocation_Construct_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Get_IGV_Attributes (G, Vertex).Construct;
      end Construct;

      --------------------------
      -- Corresponding_Vertex --
      --------------------------

      function Corresponding_Vertex
        (G     : Invocation_Graph;
         IS_Id : Invocation_Signature_Id) return Invocation_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IS_Id));

         return Signature_Tables.Get (G.Signature_To_Vertex, IS_Id);
      end Corresponding_Vertex;

      ------------
      -- Create --
      ------------

      function Create
        (Initial_Vertices : Positive;
         Initial_Edges    : Positive) return Invocation_Graph
      is
         G : constant Invocation_Graph := new Invocation_Graph_Attributes;

      begin
         G.Edge_Attributes     := IGE_Tables.Create       (Initial_Edges);
         G.Graph               :=
           DG.Create
             (Initial_Vertices => Initial_Vertices,
              Initial_Edges    => Initial_Edges);
         G.Relations           := Relation_Sets.Create    (Initial_Edges);
         G.Roots               := IGV_Sets.Create         (Initial_Vertices);
         G.Signature_To_Vertex := Signature_Tables.Create (Initial_Vertices);
         G.Vertex_Attributes   := IGV_Tables.Create       (Initial_Vertices);

         return G;
      end Create;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (G : in out Invocation_Graph) is
      begin
         pragma Assert (Present (G));

         IGE_Tables.Destroy       (G.Edge_Attributes);
         DG.Destroy               (G.Graph);
         Relation_Sets.Destroy    (G.Relations);
         IGV_Sets.Destroy         (G.Roots);
         Signature_Tables.Destroy (G.Signature_To_Vertex);
         IGV_Tables.Destroy       (G.Vertex_Attributes);

         Free (G);
      end Destroy;

      -----------------------------------
      -- Destroy_Invocation_Graph_Edge --
      -----------------------------------

      procedure Destroy_Invocation_Graph_Edge
        (Edge : in out Invocation_Graph_Edge_Id)
      is
         pragma Unreferenced (Edge);
      begin
         null;
      end Destroy_Invocation_Graph_Edge;

      ----------------------------------------------
      -- Destroy_Invocation_Graph_Edge_Attributes --
      ----------------------------------------------

      procedure Destroy_Invocation_Graph_Edge_Attributes
        (Attrs : in out Invocation_Graph_Edge_Attributes)
      is
         pragma Unreferenced (Attrs);
      begin
         null;
      end Destroy_Invocation_Graph_Edge_Attributes;

      -------------------------------------
      -- Destroy_Invocation_Graph_Vertex --
      -------------------------------------

      procedure Destroy_Invocation_Graph_Vertex
        (Vertex : in out Invocation_Graph_Vertex_Id)
      is
         pragma Unreferenced (Vertex);
      begin
         null;
      end Destroy_Invocation_Graph_Vertex;

      ------------------------------------------------
      -- Destroy_Invocation_Graph_Vertex_Attributes --
      ------------------------------------------------

      procedure Destroy_Invocation_Graph_Vertex_Attributes
        (Attrs : in out Invocation_Graph_Vertex_Attributes)
      is
         pragma Unreferenced (Attrs);
      begin
         null;
      end Destroy_Invocation_Graph_Vertex_Attributes;

      -----------
      -- Extra --
      -----------

      function Extra
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) return Name_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Extra (Relation (G, Edge));
      end Extra;

      ------------------------
      -- Get_IGE_Attributes --
      ------------------------

      function Get_IGE_Attributes
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id)
         return Invocation_Graph_Edge_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return IGE_Tables.Get (G.Edge_Attributes, Edge);
      end Get_IGE_Attributes;

      ------------------------
      -- Get_IGV_Attributes --
      ------------------------

      function Get_IGV_Attributes
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id)
         return Invocation_Graph_Vertex_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return IGV_Tables.Get (G.Vertex_Attributes, Vertex);
      end Get_IGV_Attributes;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : All_Edge_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.All_Edge_Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : All_Vertex_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.All_Vertex_Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Edges_To_Targets_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.Outgoing_Edge_Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Elaboration_Root_Iterator) return Boolean is
      begin
         return IGV_Sets.Has_Next (IGV_Sets.Iterator (Iter));
      end Has_Next;

      -------------------------------
      -- Hash_Invocation_Signature --
      -------------------------------

      function Hash_Invocation_Signature
        (IS_Id : Invocation_Signature_Id) return Bucket_Range_Type
      is
      begin
         pragma Assert (Present (IS_Id));

         return Bucket_Range_Type (IS_Id);
      end Hash_Invocation_Signature;

      ---------------------------------
      -- Hash_Source_Target_Relation --
      ---------------------------------

      function Hash_Source_Target_Relation
        (Rel : Source_Target_Relation) return Bucket_Range_Type
      is
      begin
         pragma Assert (Present (Rel.Source));
         pragma Assert (Present (Rel.Target));

         return
           Hash_Two_Keys
             (Bucket_Range_Type (Rel.Source),
              Bucket_Range_Type (Rel.Target));
      end Hash_Source_Target_Relation;

      -------------------------------------------
      -- Increment_Invocation_Graph_Edge_Count --
      -------------------------------------------

      procedure Increment_Invocation_Graph_Edge_Count
        (G    : Invocation_Graph;
         Kind : Invocation_Kind)
      is
         pragma Assert (Present (G));

         Count : Natural renames G.Counts (Kind);

      begin
         Count := Count + 1;
      end Increment_Invocation_Graph_Edge_Count;

      ---------------------------------
      -- Invocation_Graph_Edge_Count --
      ---------------------------------

      function Invocation_Graph_Edge_Count
        (G    : Invocation_Graph;
         Kind : Invocation_Kind) return Natural
      is
      begin
         pragma Assert (Present (G));

         return G.Counts (Kind);
      end Invocation_Graph_Edge_Count;

      -------------------------
      -- Is_Elaboration_Root --
      -------------------------

      function Is_Elaboration_Root
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Vertex_Kind : constant Invocation_Construct_Kind :=
                         Kind (Construct (G, Vertex));

      begin
         return
           Vertex_Kind = Elaborate_Body_Procedure
             or else
           Vertex_Kind = Elaborate_Spec_Procedure;
      end Is_Elaboration_Root;

      ----------------------------------------
      -- Is_Existing_Source_Target_Relation --
      ----------------------------------------

      function Is_Existing_Source_Target_Relation
        (G   : Invocation_Graph;
         Rel : Source_Target_Relation) return Boolean
      is
      begin
         pragma Assert (Present (G));

         return Relation_Sets.Contains (G.Relations, Rel);
      end Is_Existing_Source_Target_Relation;

      -----------------------
      -- Iterate_All_Edges --
      -----------------------

      function Iterate_All_Edges
        (G : Invocation_Graph) return All_Edge_Iterator
      is
      begin
         pragma Assert (Present (G));

         return All_Edge_Iterator (DG.Iterate_All_Edges (G.Graph));
      end Iterate_All_Edges;

      --------------------------
      -- Iterate_All_Vertices --
      --------------------------

      function Iterate_All_Vertices
        (G : Invocation_Graph) return All_Vertex_Iterator
      is
      begin
         pragma Assert (Present (G));

         return All_Vertex_Iterator (DG.Iterate_All_Vertices (G.Graph));
      end Iterate_All_Vertices;

      ------------------------------
      -- Iterate_Edges_To_Targets --
      ------------------------------

      function Iterate_Edges_To_Targets
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Edges_To_Targets_Iterator
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return
           Edges_To_Targets_Iterator
             (DG.Iterate_Outgoing_Edges (G.Graph, Vertex));
      end Iterate_Edges_To_Targets;

      -------------------------------
      -- Iterate_Elaboration_Roots --
      -------------------------------

      function Iterate_Elaboration_Roots
        (G : Invocation_Graph) return Elaboration_Root_Iterator
      is
      begin
         pragma Assert (Present (G));

         return Elaboration_Root_Iterator (IGV_Sets.Iterate (G.Roots));
      end Iterate_Elaboration_Roots;

      ----------
      -- Kind --
      ----------

      function Kind
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) return Invocation_Kind
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Kind (Relation (G, Edge));
      end Kind;

      ----------
      -- Line --
      ----------

      function Line
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Nat
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Line (Signature (Construct (G, Vertex)));
      end Line;

      ----------
      -- Name --
      ----------

      function Name
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Name_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Name (Signature (Construct (G, Vertex)));
      end Name;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out All_Edge_Iterator;
         Edge : out Invocation_Graph_Edge_Id)
      is
      begin
         DG.Next (DG.All_Edge_Iterator (Iter), Edge);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out All_Vertex_Iterator;
         Vertex : out Invocation_Graph_Vertex_Id)
      is
      begin
         DG.Next (DG.All_Vertex_Iterator (Iter), Vertex);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Edges_To_Targets_Iterator;
         Edge : out Invocation_Graph_Edge_Id)
      is
      begin
         DG.Next (DG.Outgoing_Edge_Iterator (Iter), Edge);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Elaboration_Root_Iterator;
         Root : out Invocation_Graph_Vertex_Id)
      is
      begin
         IGV_Sets.Next (IGV_Sets.Iterator (Iter), Root);
      end Next;

      ---------------------
      -- Number_Of_Edges --
      ---------------------

      function Number_Of_Edges (G : Invocation_Graph) return Natural is
      begin
         pragma Assert (Present (G));

         return DG.Number_Of_Edges (G.Graph);
      end Number_Of_Edges;

      --------------------------------
      -- Number_Of_Edges_To_Targets --
      --------------------------------

      function Number_Of_Edges_To_Targets
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return DG.Number_Of_Outgoing_Edges (G.Graph, Vertex);
      end Number_Of_Edges_To_Targets;

      ---------------------------------
      -- Number_Of_Elaboration_Roots --
      ---------------------------------

      function Number_Of_Elaboration_Roots
        (G : Invocation_Graph) return Natural
      is
      begin
         pragma Assert (Present (G));

         return IGV_Sets.Size (G.Roots);
      end Number_Of_Elaboration_Roots;

      ------------------------
      -- Number_Of_Vertices --
      ------------------------

      function Number_Of_Vertices (G : Invocation_Graph) return Natural is
      begin
         pragma Assert (Present (G));

         return DG.Number_Of_Vertices (G.Graph);
      end Number_Of_Vertices;

      -------------
      -- Present --
      -------------

      function Present (G : Invocation_Graph) return Boolean is
      begin
         return G /= Nil;
      end Present;

      --------------
      -- Relation --
      --------------

      function Relation
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) return Invocation_Relation_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Get_IGE_Attributes (G, Edge).Relation;
      end Relation;

      ---------------------------
      -- Save_Elaboration_Root --
      ---------------------------

      procedure Save_Elaboration_Root
        (G    : Invocation_Graph;
         Root : Invocation_Graph_Vertex_Id)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Root));

         IGV_Sets.Insert (G.Roots, Root);
      end Save_Elaboration_Root;

      ------------------------------
      -- Set_Corresponding_Vertex --
      ------------------------------

      procedure Set_Corresponding_Vertex
        (G      : Invocation_Graph;
         IS_Id  : Invocation_Signature_Id;
         Vertex : Invocation_Graph_Vertex_Id)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IS_Id));
         pragma Assert (Present (Vertex));

         Signature_Tables.Put (G.Signature_To_Vertex, IS_Id, Vertex);
      end Set_Corresponding_Vertex;

      --------------------------------------------
      -- Set_Is_Existing_Source_Target_Relation --
      --------------------------------------------

      procedure Set_Is_Existing_Source_Target_Relation
        (G   : Invocation_Graph;
         Rel : Source_Target_Relation;
         Val : Boolean := True)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Rel.Source));
         pragma Assert (Present (Rel.Target));

         if Val then
            Relation_Sets.Insert (G.Relations, Rel);
         else
            Relation_Sets.Delete (G.Relations, Rel);
         end if;
      end Set_Is_Existing_Source_Target_Relation;

      ------------------------
      -- Set_IGE_Attributes --
      ------------------------

      procedure Set_IGE_Attributes
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id;
         Val  : Invocation_Graph_Edge_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         IGE_Tables.Put (G.Edge_Attributes, Edge, Val);
      end Set_IGE_Attributes;

      ------------------------
      -- Set_IGV_Attributes --
      ------------------------

      procedure Set_IGV_Attributes
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id;
         Val    : Invocation_Graph_Vertex_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         IGV_Tables.Put (G.Vertex_Attributes, Vertex, Val);
      end Set_IGV_Attributes;

      -----------------
      -- Spec_Vertex --
      -----------------

      function Spec_Vertex
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Get_IGV_Attributes (G, Vertex).Spec_Vertex;
      end Spec_Vertex;

      ------------
      -- Target --
      ------------

      function Target
        (G      : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) return Invocation_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return DG.Destination_Vertex (G.Graph, Edge);
      end Target;
   end Invocation_Graphs;

   --------------------
   -- Library_Graphs --
   --------------------

   package body Library_Graphs is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Add_Body_Before_Spec_Edge
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Edges  : LGE_Lists.Doubly_Linked_List);
      pragma Inline (Add_Body_Before_Spec_Edge);
      --  Create a new edge in library graph G between vertex Vertex and its
      --  corresponding spec or body, where the body is a predecessor and the
      --  spec a successor. Add the edge to list Edges.

      procedure Add_Body_Before_Spec_Edges
        (G     : Library_Graph;
         Edges : LGE_Lists.Doubly_Linked_List);
      pragma Inline (Add_Body_Before_Spec_Edges);
      --  Create new edges in library graph G for all vertices and their
      --  corresponding specs or bodies, where the body is a predecessor
      --  and the spec is a successor. Add all edges to list Edges.

      procedure Add_Cycle
        (G      : Library_Graph;
         Attrs  : Library_Graph_Cycle_Attributes;
         Indent : Indentation_Level);
      pragma Inline (Add_Cycle);
      --  Store a cycle described by attributes Attrs in library graph G,
      --  unless a prior rotation of it already exists. The edges of the cycle
      --  must be in normalized form. Indent is the desired indentation level
      --  for tracing.

      function Add_Edge_With_Return
        (G              : Library_Graph;
         Pred           : Library_Graph_Vertex_Id;
         Succ           : Library_Graph_Vertex_Id;
         Kind           : Library_Graph_Edge_Kind;
         Activates_Task : Boolean) return Library_Graph_Edge_Id;
      pragma Inline (Add_Edge_With_Return);
      --  Create a new edge in library graph G with source vertex Pred and
      --  destination vertex Succ, and return its handle. Kind denotes the
      --  nature of the edge. Activates_Task should be set when the edge
      --  involves a task activation. If Pred and Succ are already related,
      --  no edge is created and No_Library_Graph_Edge is returned.

      procedure Add_Vertex_And_Complement
        (G             : Library_Graph;
         Vertex        : Library_Graph_Vertex_Id;
         Set           : LGV_Sets.Membership_Set;
         Do_Complement : Boolean);
      pragma Inline (Add_Vertex_And_Complement);
      --  Add vertex Vertex of library graph G to set Set. If the vertex is
      --  part of an Elaborate_Body pair, or flag Do_Complement is set, add
      --  the complementary vertex to the set.

      function At_Least_One_Edge_Satisfies
        (G         : Library_Graph;
         Cycle     : Library_Graph_Cycle_Id;
         Predicate : LGE_Predicate_Ptr) return Boolean;
      pragma Inline (At_Least_One_Edge_Satisfies);
      --  Determine whether at least one edge of cycle Cycle of library graph G
      --  satisfies predicate Predicate.

      function Copy_Cycle_Path
        (Cycle_Path : LGE_Lists.Doubly_Linked_List)
         return LGE_Lists.Doubly_Linked_List;
      pragma Inline (Copy_Cycle_Path);
      --  Create a deep copy of list Cycle_Path

      function Cycle_Kind_Of
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Cycle_Kind;
      pragma Inline (Cycle_Kind_Of);
      --  Determine the cycle kind of edge Edge of library graph G if the edge
      --  participated in a circuit.

      procedure Decrement_Library_Graph_Edge_Count
        (G    : Library_Graph;
         Kind : Library_Graph_Edge_Kind);
      pragma Inline (Decrement_Library_Graph_Edge_Count);
      --  Decrement the number of edges of kind King in library graph G by one

      procedure Delete_Body_Before_Spec_Edges
        (G     : Library_Graph;
         Edges : LGE_Lists.Doubly_Linked_List);
      pragma Inline (Delete_Body_Before_Spec_Edges);
      --  Delete all edges in list Edges from library graph G, that link spec
      --  and bodies, where the body acts as the predecessor and the spec as a
      --  successor.

      procedure Delete_Edge
        (G      : Library_Graph;
         Edge : Library_Graph_Edge_Id);
      pragma Inline (Delete_Edge);
      --  Delete edge Edge from library graph G

      procedure Find_All_Cycles_Through_Vertex
        (G                      : Library_Graph;
         Vertex                 : Library_Graph_Vertex_Id;
         End_Vertices           : LGV_Sets.Membership_Set;
         Most_Significant_Edge  : Library_Graph_Edge_Id;
         Invocation_Edge_Count  : Natural;
         Spec_And_Body_Together : Boolean;
         Cycle_Path             : LGE_Lists.Doubly_Linked_List;
         Visited_Vertices       : LGV_Sets.Membership_Set;
         Indent                 : Indentation_Level);
      pragma Inline (Find_All_Cycles_Through_Vertex);
      --  Explore all edges to successors of vertex Vertex of library graph G
      --  in an attempt to find a cycle. A cycle is considered closed when the
      --  Vertex appears in set End_Vertices. Most_Significant_Edge denotes the
      --  edge with the highest significance along the candidate cycle path.
      --  Invocation_Edge_Count denotes the number of invocation edges along
      --  the candidate cycle path. Spec_And_Body_Together should be set when
      --  spec and body vertices must be treated as one vertex. Cycle_Path is
      --  the candidate cycle path. Visited_Vertices denotes the set of visited
      --  vertices so far. Indent is the desired indentation level for tracing.

      procedure Find_All_Cycles_With_Edge
        (G                      : Library_Graph;
         Initial_Edge           : Library_Graph_Edge_Id;
         Spec_And_Body_Together : Boolean;
         Cycle_Path             : LGE_Lists.Doubly_Linked_List;
         Visited_Vertices       : LGV_Sets.Membership_Set;
         Indent                 : Indentation_Level);
      pragma Inline (Find_All_Cycles_With_Edge);
      --  Find all cycles which contain edge Initial_Edge of library graph G.
      --  Spec_And_Body_Together should be set when spec and body vertices must
      --  be treated as one vertex. Cycle_Path is the candidate cycle path.
      --  Visited_Vertices is the set of visited vertices so far. Indent is
      --  the desired indentation level for tracing.

      function Find_First_Lower_Precedence_Cycle
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Library_Graph_Cycle_Id;
      pragma Inline (Find_First_Lower_Precedence_Cycle);
      --  Inspect the list of cycles of library graph G and return the first
      --  cycle whose precedence is lower than that of cycle Cycle. If there
      --  is no such cycle, return No_Library_Graph_Cycle.

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Library_Graph_Attributes, Library_Graph);

      function Get_Component_Attributes
        (G    : Library_Graph;
         Comp : Component_Id) return Component_Attributes;
      pragma Inline (Get_Component_Attributes);
      --  Obtain the attributes of component Comp of library graph G

      function Get_LGC_Attributes
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Library_Graph_Cycle_Attributes;
      pragma Inline (Get_LGC_Attributes);
      --  Obtain the attributes of cycle Cycle of library graph G

      function Get_LGE_Attributes
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id)
         return Library_Graph_Edge_Attributes;
      pragma Inline (Get_LGE_Attributes);
      --  Obtain the attributes of edge Edge of library graph G

      function Get_LGV_Attributes
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id)
         return Library_Graph_Vertex_Attributes;
      pragma Inline (Get_LGV_Attributes);
      --  Obtain the attributes of vertex Edge of library graph G

      function Has_Elaborate_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Has_Elaborate_Body);
      --  Determine whether vertex Vertex of library graph G is subject to
      --  pragma Elaborate_Body.

      function Highest_Precedence_Edge
        (G     : Library_Graph;
         Left  : Library_Graph_Edge_Id;
         Right : Library_Graph_Edge_Id) return Library_Graph_Edge_Id;
      pragma Inline (Highest_Precedence_Edge);
      --  Return the edge with highest precedence among edges Left and Right of
      --  library graph G.

      procedure Increment_Library_Graph_Edge_Count
        (G    : Library_Graph;
         Kind : Library_Graph_Edge_Kind);
      pragma Inline (Increment_Library_Graph_Edge_Count);
      --  Increment the number of edges of king Kind in library graph G by one

      procedure Increment_Pending_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id;
         Edge : Library_Graph_Edge_Id);
      pragma Inline (Increment_Pending_Predecessors);
      --  Increment the number of pending predecessors component Comp which was
      --  reached via edge Edge of library graph G must wait on before it can
      --  be elaborated by one.

      procedure Increment_Pending_Predecessors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Edge   : Library_Graph_Edge_Id);
      pragma Inline (Increment_Pending_Predecessors);
      --  Increment the number of pending predecessors vertex Vertex which was
      --  reached via edge Edge of library graph G must wait on before it can
      --  be elaborated by one.

      procedure Initialize_Components (G : Library_Graph);
      pragma Inline (Initialize_Components);
      --  Initialize on the initial call or re-initialize on subsequent calls
      --  all components of library graph G.

      procedure Insert_And_Sort
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id);
      pragma Inline (Insert_And_Sort);
      --  Insert cycle Cycle in library graph G and sort it based on its
      --  precedence relative to all recorded cycles.

      function Is_Cycle_Initiating_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Cycle_Initiating_Edge);
      --  Determine whether edge Edge of library graph G starts a cycle

      function Is_Cyclic_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Cyclic_Edge);
      --  Determine whether edge Edge of library graph G participates in a
      --  cycle.

      function Is_Cyclic_Elaborate_All_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Cyclic_Elaborate_All_Edge);
      --  Determine whether edge Edge of library graph G participates in a
      --  cycle and has a predecessor that is subject to pragma Elaborate_All.

      function Is_Cyclic_Elaborate_Body_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Cyclic_Elaborate_Body_Edge);
      --  Determine whether edge Edge of library graph G participates in a
      --  cycle and has a successor that is either a spec subject to pragma
      --  Elaborate_Body, or a body that completes such a spec.

      function Is_Cyclic_Elaborate_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Cyclic_Elaborate_Edge);
      --  Determine whether edge Edge of library graph G participates in a
      --  cycle and has a predecessor that is subject to pragma Elaborate.

      function Is_Cyclic_Forced_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Cyclic_Forced_Edge);
      --  Determine whether edge Edge of library graph G participates in a
      --  cycle and came from the forced-elaboration-order file.

      function Is_Cyclic_Invocation_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Cyclic_Invocation_Edge);
      --  Determine whether edge Edge of library graph G participates in a
      --  cycle and came from the traversal of the invocation graph.

      function Is_Cyclic_With_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Cyclic_With_Edge);
      --  Determine whether edge Edge of library graph G participates in a
      --  cycle and is the result of a with dependency between its successor
      --  and predecessor.

      function Is_Recorded_Cycle
        (G     : Library_Graph;
         Attrs : Library_Graph_Cycle_Attributes) return Boolean;
      pragma Inline (Is_Recorded_Cycle);
      --  Determine whether a cycle described by its attributes Attrs has
      --  has already been recorded in library graph G.

      function Is_Recorded_Edge
        (G   : Library_Graph;
         Rel : Predecessor_Successor_Relation) return Boolean;
      pragma Inline (Is_Recorded_Edge);
      --  Determine whether a predecessor vertex and a successor vertex
      --  described by relation Rel are already linked in library graph G.

      function Is_Static_Successor_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Is_Static_Successor_Edge);
      --  Determine whether the successor of invocation edge Edge represents a
      --  unit that was compiled with the static model.

      function Links_Vertices_In_Same_Component
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean;
      pragma Inline (Links_Vertices_In_Same_Component);
      --  Determine whether edge Edge of library graph G links a predecessor
      --  and successor that reside in the same component.

      function Maximum_Invocation_Edge_Count
        (G     : Library_Graph;
         Edge  : Library_Graph_Edge_Id;
         Count : Natural) return Natural;
      pragma Inline (Maximum_Invocation_Edge_Count);
      --  Determine whether edge Edge of library graph G is an invocation edge,
      --  and if it is return Count + 1, otherwise return Count.

      procedure Normalize_And_Add_Cycle
        (G                     : Library_Graph;
         Most_Significant_Edge : Library_Graph_Edge_Id;
         Invocation_Edge_Count : Natural;
         Cycle_Path            : LGE_Lists.Doubly_Linked_List;
         Indent                : Indentation_Level);
      pragma Inline (Normalize_And_Add_Cycle);
      --  Normalize a cycle described by its path Cycle_Path and add it to
      --  library graph G. Most_Significant_Edge denotes the edge with the
      --  highest significance along the cycle path. Invocation_Edge_Count
      --  denotes the number of invocation edges along the cycle path. Indent
      --  is the desired indentation level for tracing.

      procedure Normalize_Cycle_Path
        (Cycle_Path            : LGE_Lists.Doubly_Linked_List;
         Most_Significant_Edge : Library_Graph_Edge_Id);
      pragma Inline (Normalize_Cycle_Path);
      --  Normalize cycle path Path by rotating it until its starting edge is
      --  Sig_Edge.

      function Path
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return LGE_Lists.Doubly_Linked_List;
      pragma Inline (Path);
      --  Obtain the path of edges which comprises cycle Cycle of library
      --  graph G.

      function Precedence
        (G           : Library_Graph;
         Cycle       : Library_Graph_Cycle_Id;
         Compared_To : Library_Graph_Cycle_Id) return Precedence_Kind;
      pragma Inline (Precedence);
      --  Determine the precedence of cycle Cycle of library graph G compared
      --  to cycle Compared_To.

      function Precedence
        (Kind        : Library_Graph_Cycle_Kind;
         Compared_To : Library_Graph_Cycle_Kind) return Precedence_Kind;
      pragma Inline (Precedence);
      --  Determine the precedence of cycle kind Kind compared to cycle kind
      --  Compared_To.

      function Precedence
        (G           : Library_Graph;
         Edge        : Library_Graph_Edge_Id;
         Compared_To : Library_Graph_Edge_Id) return Precedence_Kind;
      pragma Inline (Precedence);
      --  Determine the precedence of edge Edge of library graph G compared to
      --  edge Compared_To.

      function Precedence
        (G           : Library_Graph;
         Vertex      : Library_Graph_Vertex_Id;
         Compared_To : Library_Graph_Vertex_Id) return Precedence_Kind;
      pragma Inline (Precedence);
      --  Determine the precedence of vertex Vertex of library graph G compared
      --  to vertex Compared_To.

      procedure Remove_Vertex_And_Complement
        (G             : Library_Graph;
         Vertex        : Library_Graph_Vertex_Id;
         Set           : LGV_Sets.Membership_Set;
         Do_Complement : Boolean);
      pragma Inline (Remove_Vertex_And_Complement);
      --  Remove vertex Vertex of library graph G from set Set. If the vertex
      --  is part of an Elaborate_Body pair, or Do_Complement is set, remove
      --  the complementary vertex from the set.

      procedure Set_Component_Attributes
        (G    : Library_Graph;
         Comp : Component_Id;
         Val  : Component_Attributes);
      pragma Inline (Set_Component_Attributes);
      --  Set the attributes of component Comp of library graph G to value Val

      procedure Set_Corresponding_Vertex
        (G    : Library_Graph;
         U_Id : Unit_Id;
         Val  : Library_Graph_Vertex_Id);
      pragma Inline (Set_Corresponding_Vertex);
      --  Associate vertex Val of library graph G with unit U_Id

      procedure Set_Is_Recorded_Cycle
        (G     : Library_Graph;
         Attrs : Library_Graph_Cycle_Attributes;
         Val   : Boolean := True);
      pragma Inline (Set_Is_Recorded_Cycle);
      --  Mark a cycle described by its attributes Attrs as recorded in library
      --  graph G depending on value Val.

      procedure Set_Is_Recorded_Edge
        (G   : Library_Graph;
         Rel : Predecessor_Successor_Relation;
         Val : Boolean := True);
      pragma Inline (Set_Is_Recorded_Edge);
      --  Mark a predecessor vertex and a successor vertex described by
      --  relation Rel as already linked depending on value Val.

      procedure Set_LGC_Attributes
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id;
         Val   : Library_Graph_Cycle_Attributes);
      pragma Inline (Set_LGC_Attributes);
      --  Set the attributes of cycle Cycle of library graph G to value Val

      procedure Set_LGE_Attributes
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id;
         Val  : Library_Graph_Edge_Attributes);
      pragma Inline (Set_LGE_Attributes);
      --  Set the attributes of edge Edge of library graph G to value Val

      procedure Set_LGV_Attributes
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Val    : Library_Graph_Vertex_Attributes);
      pragma Inline (Set_LGV_Attributes);
      --  Set the attributes of vertex Vertex of library graph G to value Val

      procedure Trace_Cycle
        (G      : Library_Graph;
         Cycle  : Library_Graph_Cycle_Id;
         Indent : Indentation_Level);
      pragma Inline (Trace_Cycle);
      --  Write the contents of cycle Cycle of library graph G to standard
      --  output. Indent is the desired indentation level for tracing.

      procedure Trace_Edge
        (G      : Library_Graph;
         Edge   : Library_Graph_Edge_Id;
         Indent : Indentation_Level);
      pragma Inline (Trace_Edge);
      --  Write the contents of edge Edge of library graph G to standard
      --  output. Indent is the desired indentation level for tracing.

      procedure Trace_Eol;
      pragma Inline (Trace_Eol);
      --  Write an end-of-line to standard output

      procedure Trace_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Indent : Indentation_Level);
      pragma Inline (Trace_Vertex);
      --  Write the contents of vertex Vertex of library graph G to standard
      --  output. Indent is the desired indentation level for tracing.

      procedure Update_Pending_Predecessors
        (Strong_Predecessors : in out Natural;
         Weak_Predecessors   : in out Natural;
         Update_Weak         : Boolean;
         Value               : Integer);
      pragma Inline (Update_Pending_Predecessors);
      --  Update the number of pending strong or weak predecessors denoted by
      --  Strong_Predecessors and Weak_Predecessors respectively depending on
      --  flag Update_Weak by adding value Value.

      procedure Update_Pending_Predecessors_Of_Components (G : Library_Graph);
      pragma Inline (Update_Pending_Predecessors_Of_Components);
      --  Update the number of pending predecessors all components of library
      --  graph G must wait on before they can be elaborated.

      procedure Update_Pending_Predecessors_Of_Components
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id);
      pragma Inline (Update_Pending_Predecessors_Of_Components);
      --  Update the number of pending predecessors the component of edge
      --  LGE_Is's successor vertex of library graph G must wait on before
      --  it can be elaborated.

      --------------------
      -- Activates_Task --
      --------------------

      function Activates_Task
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Kind (G, Edge) = Invocation_Edge
            and then Get_LGE_Attributes (G, Edge).Activates_Task;
      end Activates_Task;

      -------------------------------
      -- Add_Body_Before_Spec_Edge --
      -------------------------------

      procedure Add_Body_Before_Spec_Edge
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Edges  : LGE_Lists.Doubly_Linked_List)
      is
         Edge : Library_Graph_Edge_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (LGE_Lists.Present (Edges));

         --  A vertex requires a special Body_Before_Spec edge to its
         --  Corresponding_Item when it either denotes a
         --
         --    * Body that completes a previous spec
         --
         --    * Spec with a completing body
         --
         --  The edge creates an intentional circularity between the spec and
         --  body in order to emulate a library unit, and guarantees that both
         --  will appear in the same component.
         --
         --  Due to the structure of the library graph, either the spec or
         --  the body may be visited first, yet Corresponding_Item will still
         --  attempt to create the Body_Before_Spec edge. This is OK because
         --  successor and predecessor are kept consistent in both cases, and
         --  Add_Edge_With_Return will prevent the creation of the second edge.

         --  Assume that that no Body_Before_Spec is necessary

         Edge := No_Library_Graph_Edge;

         --  A body that completes a previous spec

         if Is_Body_With_Spec (G, Vertex) then
            Edge :=
              Add_Edge_With_Return
                (G              => G,
                 Pred           => Vertex,
                 Succ           => Corresponding_Item (G, Vertex),
                 Kind           => Body_Before_Spec_Edge,
                 Activates_Task => False);

         --  A spec with a completing body

         elsif Is_Spec_With_Body (G, Vertex) then
            Edge :=
              Add_Edge_With_Return
                (G              => G,
                 Pred           => Corresponding_Item (G, Vertex),
                 Succ           => Vertex,
                 Kind           => Body_Before_Spec_Edge,
                 Activates_Task => False);
         end if;

         if Present (Edge) then
            LGE_Lists.Append (Edges, Edge);
         end if;
      end Add_Body_Before_Spec_Edge;

      --------------------------------
      -- Add_Body_Before_Spec_Edges --
      --------------------------------

      procedure Add_Body_Before_Spec_Edges
        (G     : Library_Graph;
         Edges : LGE_Lists.Doubly_Linked_List)
      is
         Iter : Elaborable_Units_Iterator;
         U_Id : Unit_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (LGE_Lists.Present (Edges));

         Iter := Iterate_Elaborable_Units;
         while Has_Next (Iter) loop
            Next (Iter, U_Id);

            Add_Body_Before_Spec_Edge
              (G      => G,
               Vertex => Corresponding_Vertex (G, U_Id),
               Edges  => Edges);
         end loop;
      end Add_Body_Before_Spec_Edges;

      ---------------
      -- Add_Cycle --
      ---------------

      procedure Add_Cycle
        (G      : Library_Graph;
         Attrs  : Library_Graph_Cycle_Attributes;
         Indent : Indentation_Level)
      is
         Cycle : Library_Graph_Cycle_Id;

      begin
         pragma Assert (Present (G));

         --  Nothing to do when the cycle has already been recorded, possibly
         --  in a rotated form.

         if Is_Recorded_Cycle (G, Attrs) then
            return;
         end if;

         --  Mark the cycle as recorded. This prevents further attempts to add
         --  rotations of the same cycle.

         Set_Is_Recorded_Cycle (G, Attrs);

         --  Save the attributes of the cycle

         Cycle := Sequence_Next_Cycle;
         Set_LGC_Attributes (G, Cycle, Attrs);

         Trace_Cycle (G, Cycle, Indent);

         --  Insert the cycle in the list of all cycle based on its precedence

         Insert_And_Sort (G, Cycle);
      end Add_Cycle;

      --------------
      -- Add_Edge --
      --------------

      procedure Add_Edge
        (G              : Library_Graph;
         Pred           : Library_Graph_Vertex_Id;
         Succ           : Library_Graph_Vertex_Id;
         Kind           : Library_Graph_Edge_Kind;
         Activates_Task : Boolean)
      is
         Edge : Library_Graph_Edge_Id;
         pragma Unreferenced (Edge);

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));
         pragma Assert (Kind /= No_Edge);
         pragma Assert (not Activates_Task or else Kind = Invocation_Edge);

         Edge :=
           Add_Edge_With_Return
             (G              => G,
              Pred           => Pred,
              Succ           => Succ,
              Kind           => Kind,
              Activates_Task => Activates_Task);
      end Add_Edge;

      --------------------------
      -- Add_Edge_With_Return --
      --------------------------

      function Add_Edge_With_Return
        (G              : Library_Graph;
         Pred           : Library_Graph_Vertex_Id;
         Succ           : Library_Graph_Vertex_Id;
         Kind           : Library_Graph_Edge_Kind;
         Activates_Task : Boolean) return Library_Graph_Edge_Id
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));
         pragma Assert (Kind /= No_Edge);

         Rel : constant Predecessor_Successor_Relation :=
                 (Predecessor => Pred,
                  Successor   => Succ);

         Edge : Library_Graph_Edge_Id;

      begin
         --  Nothing to do when the predecessor and successor are already
         --  related by an edge.

         if Is_Recorded_Edge (G, Rel) then
            return No_Library_Graph_Edge;
         end if;

         Edge := Sequence_Next_Edge;

         --  Add the edge to the underlying graph. Note that the predecessor
         --  is the source of the edge because it will later need to notify
         --  all its successors that it has been elaborated.

         DG.Add_Edge
           (G           => G.Graph,
            E           => Edge,
            Source      => Pred,
            Destination => Succ);

         --  Construct and save the attributes of the edge

         Set_LGE_Attributes
           (G    => G,
            Edge => Edge,
            Val  =>
              (Activates_Task => Activates_Task,
               Kind           => Kind));

         --  Mark the predecessor and successor as related by the new edge.
         --  This prevents all further attempts to link the same predecessor
         --  and successor.

         Set_Is_Recorded_Edge (G, Rel);

         --  Update the number of pending predecessors the successor must wait
         --  on before it is elaborated.

         Increment_Pending_Predecessors
           (G      => G,
            Vertex => Succ,
            Edge   => Edge);

         --  Update the edge statistics

         Increment_Library_Graph_Edge_Count (G, Kind);

         return Edge;
      end Add_Edge_With_Return;

      ----------------
      -- Add_Vertex --
      ----------------

      procedure Add_Vertex
        (G    : Library_Graph;
         U_Id : Unit_Id)
      is
         Vertex : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (U_Id));

         --  Nothing to do when the unit already has a vertex

         if Present (Corresponding_Vertex (G, U_Id)) then
            return;
         end if;

         Vertex := Sequence_Next_Vertex;

         --  Add the vertex to the underlying graph

         DG.Add_Vertex (G.Graph, Vertex);

         --  Construct and save the attributes of the vertex

         Set_LGV_Attributes
           (G      => G,
            Vertex => Vertex,
            Val    =>
              (Corresponding_Item          => No_Library_Graph_Vertex,
               In_Elaboration_Order        => False,
               Pending_Strong_Predecessors => 0,
               Pending_Weak_Predecessors   => 0,
               Unit                        => U_Id));

         --  Associate the unit with its corresponding vertex

         Set_Corresponding_Vertex (G, U_Id, Vertex);
      end Add_Vertex;

      -------------------------------
      -- Add_Vertex_And_Complement --
      -------------------------------

      procedure Add_Vertex_And_Complement
        (G             : Library_Graph;
         Vertex        : Library_Graph_Vertex_Id;
         Set           : LGV_Sets.Membership_Set;
         Do_Complement : Boolean)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (LGV_Sets.Present (Set));

         Complement : constant Library_Graph_Vertex_Id :=
                        Complementary_Vertex
                          (G                => G,
                           Vertex           => Vertex,
                           Force_Complement => Do_Complement);

      begin
         LGV_Sets.Insert (Set, Vertex);

         if Present (Complement) then
            LGV_Sets.Insert (Set, Complement);
         end if;
      end Add_Vertex_And_Complement;

      ---------------------------------
      -- At_Least_One_Edge_Satisfies --
      ---------------------------------

      function At_Least_One_Edge_Satisfies
        (G         : Library_Graph;
         Cycle     : Library_Graph_Cycle_Id;
         Predicate : LGE_Predicate_Ptr) return Boolean
      is
         Edge      : Library_Graph_Edge_Id;
         Iter      : Edges_Of_Cycle_Iterator;
         Satisfied : Boolean;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));
         pragma Assert (Predicate /= null);

         --  Assume that the predicate cannot be satisfied

         Satisfied := False;

         --  IMPORTANT:
         --
         --    * The iteration must run to completion in order to unlock the
         --      edges of the cycle.

         Iter := Iterate_Edges_Of_Cycle (G, Cycle);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            Satisfied := Satisfied or else Predicate.all (G, Edge);
         end loop;

         return Satisfied;
      end At_Least_One_Edge_Satisfies;

      --------------------------
      -- Complementary_Vertex --
      --------------------------

      function Complementary_Vertex
        (G                : Library_Graph;
         Vertex           : Library_Graph_Vertex_Id;
         Force_Complement : Boolean) return Library_Graph_Vertex_Id
      is
         Complement : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         --  Assume that there is no complementary vertex

         Complement := No_Library_Graph_Vertex;

         --  The caller requests the complement explicitly

         if Force_Complement then
            Complement := Corresponding_Item (G, Vertex);

         --  The vertex is a completing body of a spec subject to pragma
         --  Elaborate_Body. The complementary vertex is the spec.

         elsif Is_Body_Of_Spec_With_Elaborate_Body (G, Vertex) then
            Complement := Proper_Spec (G, Vertex);

         --  The vertex is a spec subject to pragma Elaborate_Body. The
         --  complementary vertex is the body.

         elsif Is_Spec_With_Elaborate_Body (G, Vertex) then
            Complement := Proper_Body (G, Vertex);
         end if;

         return Complement;
      end Complementary_Vertex;

      ---------------
      -- Component --
      ---------------

      function Component
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Component_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return DG.Component (G.Graph, Vertex);
      end Component;

      ---------------------------------
      -- Contains_Elaborate_All_Edge --
      ---------------------------------

      function Contains_Elaborate_All_Edge
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return
           At_Least_One_Edge_Satisfies
             (G         => G,
              Cycle     => Cycle,
              Predicate => Is_Elaborate_All_Edge'Access);
      end Contains_Elaborate_All_Edge;

      ------------------------------------
      -- Contains_Static_Successor_Edge --
      ------------------------------------

      function Contains_Static_Successor_Edge
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return
           At_Least_One_Edge_Satisfies
             (G         => G,
              Cycle     => Cycle,
              Predicate => Is_Static_Successor_Edge'Access);
      end Contains_Static_Successor_Edge;

      ------------------------------
      -- Contains_Task_Activation --
      ------------------------------

      function Contains_Task_Activation
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return
           At_Least_One_Edge_Satisfies
             (G         => G,
              Cycle     => Cycle,
              Predicate => Activates_Task'Access);
      end Contains_Task_Activation;

      ---------------------
      -- Copy_Cycle_Path --
      ---------------------

      function Copy_Cycle_Path
        (Cycle_Path : LGE_Lists.Doubly_Linked_List)
         return LGE_Lists.Doubly_Linked_List
      is
         Edge : Library_Graph_Edge_Id;
         Iter : LGE_Lists.Iterator;
         Path : LGE_Lists.Doubly_Linked_List;

      begin
         pragma Assert (LGE_Lists.Present (Cycle_Path));

         Path := LGE_Lists.Create;
         Iter := LGE_Lists.Iterate (Cycle_Path);
         while LGE_Lists.Has_Next (Iter) loop
            LGE_Lists.Next (Iter, Edge);

            LGE_Lists.Append (Path, Edge);
         end loop;

         return Path;
      end Copy_Cycle_Path;

      ------------------------
      -- Corresponding_Item --
      ------------------------

      function Corresponding_Item
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Get_LGV_Attributes (G, Vertex).Corresponding_Item;
      end Corresponding_Item;

      --------------------------
      -- Corresponding_Vertex --
      --------------------------

      function Corresponding_Vertex
        (G    : Library_Graph;
         U_Id : Unit_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (U_Id));

         return Unit_Tables.Get (G.Unit_To_Vertex, U_Id);
      end Corresponding_Vertex;

      ------------
      -- Create --
      ------------

      function Create
        (Initial_Vertices : Positive;
         Initial_Edges    : Positive) return Library_Graph
      is
         G : constant Library_Graph := new Library_Graph_Attributes;

      begin
         G.Component_Attributes := Component_Tables.Create (Initial_Vertices);
         G.Cycle_Attributes     := LGC_Tables.Create       (Initial_Vertices);
         G.Cycles               := LGC_Lists.Create;
         G.Edge_Attributes      := LGE_Tables.Create       (Initial_Edges);
         G.Graph                :=
           DG.Create
             (Initial_Vertices => Initial_Vertices,
              Initial_Edges    => Initial_Edges);
         G.Recorded_Cycles      := RC_Sets.Create          (Initial_Vertices);
         G.Recorded_Edges       := RE_Sets.Create          (Initial_Edges);
         G.Unit_To_Vertex       := Unit_Tables.Create      (Initial_Vertices);
         G.Vertex_Attributes    := LGV_Tables.Create       (Initial_Vertices);

         return G;
      end Create;

      -------------------
      -- Cycle_Kind_Of --
      -------------------

      function Cycle_Kind_Of
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Cycle_Kind
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

      begin
         if Is_Cyclic_Elaborate_All_Edge (G, Edge) then
            return Elaborate_All_Cycle;

         elsif Is_Cyclic_Elaborate_Body_Edge (G, Edge) then
            return Elaborate_Body_Cycle;

         elsif Is_Cyclic_Elaborate_Edge (G, Edge) then
            return Elaborate_Cycle;

         elsif Is_Cyclic_Forced_Edge (G, Edge) then
            return Forced_Cycle;

         elsif Is_Cyclic_Invocation_Edge (G, Edge) then
            return Invocation_Cycle;

         else
            return No_Cycle_Kind;
         end if;
      end Cycle_Kind_Of;

      ----------------------------------------
      -- Decrement_Library_Graph_Edge_Count --
      ----------------------------------------

      procedure Decrement_Library_Graph_Edge_Count
        (G    : Library_Graph;
         Kind : Library_Graph_Edge_Kind)
      is
         pragma Assert (Present (G));

         Count : Natural renames G.Counts (Kind);

      begin
         Count := Count - 1;
      end Decrement_Library_Graph_Edge_Count;

      ------------------------------------
      -- Decrement_Pending_Predecessors --
      ------------------------------------

      procedure Decrement_Pending_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id;
         Edge : Library_Graph_Edge_Id)
      is
         Attrs : Component_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Attrs := Get_Component_Attributes (G, Comp);

         Update_Pending_Predecessors
           (Strong_Predecessors => Attrs.Pending_Strong_Predecessors,
            Weak_Predecessors   => Attrs.Pending_Weak_Predecessors,
            Update_Weak         => Is_Invocation_Edge (G, Edge),
            Value               => -1);

         Set_Component_Attributes (G, Comp, Attrs);
      end Decrement_Pending_Predecessors;

      ------------------------------------
      -- Decrement_Pending_Predecessors --
      ------------------------------------

      procedure Decrement_Pending_Predecessors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Edge   : Library_Graph_Edge_Id)
      is
         Attrs : Library_Graph_Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Attrs := Get_LGV_Attributes (G, Vertex);

         Update_Pending_Predecessors
           (Strong_Predecessors => Attrs.Pending_Strong_Predecessors,
            Weak_Predecessors   => Attrs.Pending_Weak_Predecessors,
            Update_Weak         => Is_Invocation_Edge (G, Edge),
            Value               => -1);

         Set_LGV_Attributes (G, Vertex, Attrs);
      end Decrement_Pending_Predecessors;

      -----------------------------------
      -- Delete_Body_Before_Spec_Edges --
      -----------------------------------

      procedure Delete_Body_Before_Spec_Edges
        (G     : Library_Graph;
         Edges : LGE_Lists.Doubly_Linked_List)
      is
         Edge : Library_Graph_Edge_Id;
         Iter : LGE_Lists.Iterator;

      begin
         pragma Assert (Present (G));
         pragma Assert (LGE_Lists.Present (Edges));

         Iter := LGE_Lists.Iterate (Edges);
         while LGE_Lists.Has_Next (Iter) loop
            LGE_Lists.Next (Iter, Edge);
            pragma Assert (Kind (G, Edge) = Body_Before_Spec_Edge);

            Delete_Edge (G, Edge);
         end loop;
      end Delete_Body_Before_Spec_Edges;

      -----------------
      -- Delete_Edge --
      -----------------

      procedure Delete_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, Edge);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, Edge);
         Rel  : constant Predecessor_Successor_Relation :=
                  (Predecessor => Pred,
                   Successor   => Succ);

      begin
         --  Update the edge statistics

         Decrement_Library_Graph_Edge_Count (G, Kind (G, Edge));

         --  Update the number of pending predecessors the successor must wait
         --  on before it is elaborated.

         Decrement_Pending_Predecessors
           (G      => G,
            Vertex => Succ,
            Edge   => Edge);

         --  Delete the link between the predecessor and successor. This allows
         --  for further attempts to link the same predecessor and successor.

         RE_Sets.Delete (G.Recorded_Edges, Rel);

         --  Delete the attributes of the edge

         LGE_Tables.Delete (G.Edge_Attributes, Edge);

         --  Delete the edge from the underlying graph

         DG.Delete_Edge (G.Graph, Edge);
      end Delete_Edge;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (G : in out Library_Graph) is
      begin
         pragma Assert (Present (G));

         Component_Tables.Destroy (G.Component_Attributes);
         LGC_Tables.Destroy       (G.Cycle_Attributes);
         LGC_Lists.Destroy        (G.Cycles);
         LGE_Tables.Destroy       (G.Edge_Attributes);
         DG.Destroy               (G.Graph);
         RC_Sets.Destroy          (G.Recorded_Cycles);
         RE_Sets.Destroy          (G.Recorded_Edges);
         Unit_Tables.Destroy      (G.Unit_To_Vertex);
         LGV_Tables.Destroy       (G.Vertex_Attributes);

         Free (G);
      end Destroy;

      ----------------------------------
      -- Destroy_Component_Attributes --
      ----------------------------------

      procedure Destroy_Component_Attributes
        (Attrs : in out Component_Attributes)
      is
         pragma Unreferenced (Attrs);
      begin
         null;
      end Destroy_Component_Attributes;

      --------------------------------------------
      -- Destroy_Library_Graph_Cycle_Attributes --
      --------------------------------------------

      procedure Destroy_Library_Graph_Cycle_Attributes
        (Attrs : in out Library_Graph_Cycle_Attributes)
      is
      begin
         LGE_Lists.Destroy (Attrs.Path);
      end Destroy_Library_Graph_Cycle_Attributes;

      -------------------------------------------
      -- Destroy_Library_Graph_Edge_Attributes --
      -------------------------------------------

      procedure Destroy_Library_Graph_Edge_Attributes
        (Attrs : in out Library_Graph_Edge_Attributes)
      is
         pragma Unreferenced (Attrs);
      begin
         null;
      end Destroy_Library_Graph_Edge_Attributes;

      ----------------------------------
      -- Destroy_Library_Graph_Vertex --
      ----------------------------------

      procedure Destroy_Library_Graph_Vertex
        (Vertex : in out Library_Graph_Vertex_Id)
      is
         pragma Unreferenced (Vertex);
      begin
         null;
      end Destroy_Library_Graph_Vertex;

      ---------------------------------------------
      -- Destroy_Library_Graph_Vertex_Attributes --
      ---------------------------------------------

      procedure Destroy_Library_Graph_Vertex_Attributes
        (Attrs : in out Library_Graph_Vertex_Attributes)
      is
         pragma Unreferenced (Attrs);
      begin
         null;
      end Destroy_Library_Graph_Vertex_Attributes;

      ---------------
      -- File_Name --
      ---------------

      function File_Name
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return File_Name_Type
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return File_Name (Unit (G, Vertex));
      end File_Name;

      ------------------------------------
      -- Find_All_Cycles_Through_Vertex --
      ------------------------------------

      procedure Find_All_Cycles_Through_Vertex
        (G                      : Library_Graph;
         Vertex                 : Library_Graph_Vertex_Id;
         End_Vertices           : LGV_Sets.Membership_Set;
         Most_Significant_Edge  : Library_Graph_Edge_Id;
         Invocation_Edge_Count  : Natural;
         Spec_And_Body_Together : Boolean;
         Cycle_Path             : LGE_Lists.Doubly_Linked_List;
         Visited_Vertices       : LGV_Sets.Membership_Set;
         Indent                 : Indentation_Level)
      is
         Edge_Indent : constant Indentation_Level :=
                         Indent + Nested_Indentation;

         Iter      : Edges_To_Successors_Iterator;
         Next_Edge : Library_Graph_Edge_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (LGV_Sets.Present (End_Vertices));
         pragma Assert (Present (Most_Significant_Edge));
         pragma Assert (LGE_Lists.Present (Cycle_Path));
         pragma Assert (LGV_Sets.Present (Visited_Vertices));

         --  Nothing to do when there is no vertex

         if not Present (Vertex) then
            return;
         end if;

         --  The current vertex denotes the end vertex of the cycle and closes
         --  the circuit. Normalize the cycle such that it is rotated with its
         --  most significant edge first, and record it for diagnostics.

         if LGV_Sets.Contains (End_Vertices, Vertex) then
            Trace_Vertex (G, Vertex, Indent);

            Normalize_And_Add_Cycle
              (G                     => G,
               Most_Significant_Edge => Most_Significant_Edge,
               Invocation_Edge_Count => Invocation_Edge_Count,
               Cycle_Path            => Cycle_Path,
               Indent                => Indent + Nested_Indentation);

         --  Otherwise extend the search for a cycle only when the vertex has
         --  not been visited yet.

         elsif not LGV_Sets.Contains (Visited_Vertices, Vertex) then
            Trace_Vertex (G, Vertex, Indent);

            --  Prepare for vertex backtracking

            LGV_Sets.Insert (Visited_Vertices, Vertex);

            --  Extend the search via all edges to successors of the vertex

            Iter := Iterate_Edges_To_Successors (G, Vertex);
            while Has_Next (Iter) loop
               Next (Iter, Next_Edge);

               if Is_Cyclic_Edge (G, Next_Edge) then
                  Trace_Edge (G, Next_Edge, Edge_Indent);

                  --  Prepare for edge backtracking. Prepending ensures that
                  --  final ordering of edges can be traversed from successor
                  --  to predecessor.

                  LGE_Lists.Prepend (Cycle_Path, Next_Edge);

                  --  Extend the search via the successor of the next edge

                  Find_All_Cycles_Through_Vertex
                    (G                      => G,
                     Vertex                 => Successor (G, Next_Edge),
                     End_Vertices           => End_Vertices,

                     --  The next edge may be more important than the current
                     --  most important edge, thus "upgrading" the nature of
                     --  the cycle, and shifting its point of normalization.

                     Most_Significant_Edge  =>
                       Highest_Precedence_Edge
                         (G     => G,
                          Left  => Next_Edge,
                          Right => Most_Significant_Edge),

                     --  The next edge may be an invocation edge, in which case
                     --  the count of invocation edges increases by one.

                     Invocation_Edge_Count  =>
                       Maximum_Invocation_Edge_Count
                         (G     => G,
                          Edge  => Next_Edge,
                          Count => Invocation_Edge_Count),
                     Spec_And_Body_Together => Spec_And_Body_Together,
                     Cycle_Path             => Cycle_Path,
                     Visited_Vertices       => Visited_Vertices,
                     Indent                 => Indent);

                  --  Backtrack the edge

                  LGE_Lists.Delete_First (Cycle_Path);
               end if;
            end loop;

            --  Extend the search via the complementary vertex when the current
            --  vertex is part of an Elaborate_Body pair, or the initial edge
            --  is an Elaborate_All edge.

            Find_All_Cycles_Through_Vertex
              (G                      => G,
               Vertex                 =>
                 Complementary_Vertex
                   (G                => G,
                    Vertex           => Vertex,
                    Force_Complement => Spec_And_Body_Together),
               End_Vertices           => End_Vertices,
               Most_Significant_Edge  => Most_Significant_Edge,
               Invocation_Edge_Count  => Invocation_Edge_Count,
               Spec_And_Body_Together => Spec_And_Body_Together,
               Cycle_Path             => Cycle_Path,
               Visited_Vertices       => Visited_Vertices,
               Indent                 => Indent);

            --  Backtrack the vertex

            LGV_Sets.Delete (Visited_Vertices, Vertex);
         end if;
      end Find_All_Cycles_Through_Vertex;

      -------------------------------
      -- Find_All_Cycles_With_Edge --
      -------------------------------

      procedure Find_All_Cycles_With_Edge
        (G                      : Library_Graph;
         Initial_Edge           : Library_Graph_Edge_Id;
         Spec_And_Body_Together : Boolean;
         Cycle_Path             : LGE_Lists.Doubly_Linked_List;
         Visited_Vertices       : LGV_Sets.Membership_Set;
         Indent                 : Indentation_Level)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Initial_Edge));
         pragma Assert (LGE_Lists.Present (Cycle_Path));
         pragma Assert (LGV_Sets.Present (Visited_Vertices));

         Pred : constant Library_Graph_Vertex_Id :=
                           Predecessor (G, Initial_Edge);
         Succ : constant Library_Graph_Vertex_Id :=
                           Successor   (G, Initial_Edge);

         End_Vertices : LGV_Sets.Membership_Set;

      begin
         Trace_Edge (G, Initial_Edge, Indent);

         --  Use a set to represent the end vertices of the cycle. The set is
         --  needed to accommodate the Elaborate_All and Elaborate_Body cases
         --  where a cycle may terminate on either a spec or a body vertex.

         End_Vertices := LGV_Sets.Create (2);
         Add_Vertex_And_Complement
           (G             => G,
            Vertex        => Pred,
            Set           => End_Vertices,
            Do_Complement => Spec_And_Body_Together);

         --  Prepare for edge backtracking
         --
         --  The initial edge starts the path. During the traversal, edges with
         --  higher precedence may be discovered, in which case they supersede
         --  the initial edge in terms of significance. Prepending to the cycle
         --  path ensures that the vertices can be visited in the proper order
         --  for diagnostics.

         LGE_Lists.Prepend (Cycle_Path, Initial_Edge);

         --  Prepare for vertex backtracking
         --
         --  The predecessor is considered the terminator of the path. Add it
         --  to the set of visited vertices along with its complement vertex
         --  in the Elaborate_All and Elaborate_Body cases to prevent infinite
         --  recursion.

         Add_Vertex_And_Complement
           (G             => G,
            Vertex        => Pred,
            Set           => Visited_Vertices,
            Do_Complement => Spec_And_Body_Together);

         --  Traverse a potential cycle by continuously visiting successors
         --  until either the predecessor of the initial edge is reached, or
         --  no more successors are available.

         Find_All_Cycles_Through_Vertex
           (G                      => G,
            Vertex                 => Succ,
            End_Vertices           => End_Vertices,
            Most_Significant_Edge  => Initial_Edge,
            Invocation_Edge_Count  =>
              Maximum_Invocation_Edge_Count
                (G     => G,
                 Edge  => Initial_Edge,
                 Count => 0),
            Spec_And_Body_Together => Spec_And_Body_Together,
            Cycle_Path             => Cycle_Path,
            Visited_Vertices       => Visited_Vertices,
            Indent                 => Indent + Nested_Indentation);

         --  Backtrack the edge

         LGE_Lists.Delete_First (Cycle_Path);

         --  Backtrack the predecessor, along with the complement vertex in the
         --  Elaborate_All and Elaborate_Body cases.

         Remove_Vertex_And_Complement
           (G             => G,
            Vertex        => Pred,
            Set           => Visited_Vertices,
            Do_Complement => Spec_And_Body_Together);

         LGV_Sets.Destroy (End_Vertices);
      end Find_All_Cycles_With_Edge;

      ---------------------
      -- Find_Components --
      ---------------------

      procedure Find_Components (G : Library_Graph) is
         Edges : LGE_Lists.Doubly_Linked_List;

      begin
         pragma Assert (Present (G));

         --  Initialize or reinitialize the components of the graph

         Initialize_Components (G);

         --  Create a set of special edges that link a predecessor body with a
         --  successor spec. This is an illegal dependency, however using such
         --  edges eliminates the need to create yet another graph, where both
         --  spec and body are collapsed into a single vertex.

         Edges := LGE_Lists.Create;
         Add_Body_Before_Spec_Edges (G, Edges);

         DG.Find_Components (G.Graph);

         --  Remove the special edges that link a predecessor body with a
         --  successor spec because they cause unresolvable circularities.

         Delete_Body_Before_Spec_Edges (G, Edges);
         LGE_Lists.Destroy (Edges);

         --  Update the number of predecessors various components must wait on
         --  before they can be elaborated.

         Update_Pending_Predecessors_Of_Components (G);
      end Find_Components;

      -----------------
      -- Find_Cycles --
      -----------------

      procedure Find_Cycles (G : Library_Graph) is
         Cycle_Path       : LGE_Lists.Doubly_Linked_List;
         Edge             : Library_Graph_Edge_Id;
         Iter             : All_Edge_Iterator;
         Visited_Vertices : LGV_Sets.Membership_Set;

      begin
         pragma Assert (Present (G));

         --  Use a list of edges to describe the path of a cycle

         Cycle_Path := LGE_Lists.Create;

         --  Use a set of visited vertices to prevent infinite traversal of the
         --  graph.

         Visited_Vertices := LGV_Sets.Create (Number_Of_Vertices (G));

         --  Inspect all edges, trying to find an edge that links two vertices
         --  in the same component.

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            --  Find all cycles involving the current edge. Duplicate cycles in
            --  the forms of rotations are not saved for diagnostic purposes.

            if Is_Cycle_Initiating_Edge (G, Edge) then
               Find_All_Cycles_With_Edge
                 (G                      => G,
                  Initial_Edge           => Edge,
                  Spec_And_Body_Together => Is_Elaborate_All_Edge (G, Edge),
                  Cycle_Path             => Cycle_Path,
                  Visited_Vertices       => Visited_Vertices,
                  Indent                 => No_Indentation);

               Trace_Eol;
            end if;
         end loop;

         LGE_Lists.Destroy (Cycle_Path);
         LGV_Sets.Destroy  (Visited_Vertices);
      end Find_Cycles;

      ---------------------------------------
      -- Find_First_Lower_Precedence_Cycle --
      ---------------------------------------

      function Find_First_Lower_Precedence_Cycle
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Library_Graph_Cycle_Id
      is
         Current_Cycle : Library_Graph_Cycle_Id;
         Iter          : All_Cycle_Iterator;
         Lesser_Cycle  : Library_Graph_Cycle_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         --  Assume that there is no lesser cycle

         Lesser_Cycle := No_Library_Graph_Cycle;

         --  Find a cycle with a slightly lower precedence than the input
         --  cycle.
         --
         --  IMPORTANT:
         --
         --    * The iterator must run to completion in order to unlock the
         --      list of all cycles.

         Iter := Iterate_All_Cycles (G);
         while Has_Next (Iter) loop
            Next (Iter, Current_Cycle);

            if not Present (Lesser_Cycle)
              and then Precedence
                         (G           => G,
                          Cycle       => Cycle,
                          Compared_To => Current_Cycle) = Higher_Precedence
            then
               Lesser_Cycle := Current_Cycle;
            end if;
         end loop;

         return Lesser_Cycle;
      end Find_First_Lower_Precedence_Cycle;

      ------------------------------
      -- Get_Component_Attributes --
      ------------------------------

      function Get_Component_Attributes
        (G    : Library_Graph;
         Comp : Component_Id) return Component_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         return Component_Tables.Get (G.Component_Attributes, Comp);
      end Get_Component_Attributes;

      ------------------------
      -- Get_LGC_Attributes --
      ------------------------

      function Get_LGC_Attributes
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Library_Graph_Cycle_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return LGC_Tables.Get (G.Cycle_Attributes, Cycle);
      end Get_LGC_Attributes;

      ------------------------
      -- Get_LGE_Attributes --
      ------------------------

      function Get_LGE_Attributes
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Edge_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return LGE_Tables.Get (G.Edge_Attributes, Edge);
      end Get_LGE_Attributes;

      ------------------------
      -- Get_LGV_Attributes --
      ------------------------

      function Get_LGV_Attributes
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id)
         return Library_Graph_Vertex_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return LGV_Tables.Get (G.Vertex_Attributes, Vertex);
      end Get_LGV_Attributes;

      -----------------------------
      -- Has_Elaborate_All_Cycle --
      -----------------------------

      function Has_Elaborate_All_Cycle (G : Library_Graph) return Boolean is
         Edge : Library_Graph_Edge_Id;
         Iter : All_Edge_Iterator;
         Seen : Boolean;

      begin
         pragma Assert (Present (G));

         --  Assume that no cyclic Elaborate_All edge has been seen

         Seen := False;

         --  IMPORTANT:
         --
         --    * The iteration must run to completion in order to unlock the
         --      graph.

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            if not Seen and then Is_Cyclic_Elaborate_All_Edge (G, Edge) then
               Seen := True;
            end if;
         end loop;

         return Seen;
      end Has_Elaborate_All_Cycle;

      ------------------------
      -- Has_Elaborate_Body --
      ------------------------

      function Has_Elaborate_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         U_Id  : constant Unit_Id := Unit (G, Vertex);
         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         --  Treat the spec and body as decoupled when switch -d_b (ignore the
         --  effects of pragma Elaborate_Body) is in effect.

         return U_Rec.Elaborate_Body and not Debug_Flag_Underscore_B;
      end Has_Elaborate_Body;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : All_Cycle_Iterator) return Boolean is
      begin
         return LGC_Lists.Has_Next (LGC_Lists.Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : All_Edge_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.All_Edge_Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : All_Vertex_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.All_Vertex_Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Component_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.Component_Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Component_Vertex_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.Component_Vertex_Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Edges_Of_Cycle_Iterator) return Boolean is
      begin
         return LGE_Lists.Has_Next (LGE_Lists.Iterator (Iter));
      end Has_Next;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Edges_To_Successors_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.Outgoing_Edge_Iterator (Iter));
      end Has_Next;

      -----------------------------
      -- Has_No_Elaboration_Code --
      -----------------------------

      function Has_No_Elaboration_Code
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Has_No_Elaboration_Code (Unit (G, Vertex));
      end Has_No_Elaboration_Code;

      -----------------------------------------
      -- Hash_Library_Graph_Cycle_Attributes --
      -----------------------------------------

      function Hash_Library_Graph_Cycle_Attributes
        (Attrs : Library_Graph_Cycle_Attributes) return Bucket_Range_Type
      is
         Edge : Library_Graph_Edge_Id;
         Hash : Bucket_Range_Type;
         Iter : LGE_Lists.Iterator;

      begin
         pragma Assert (LGE_Lists.Present (Attrs.Path));

         --  The hash is obtained in the following manner:
         --
         --    (((edge1 * 31) + edge2) * 31) + edgeN

         Hash := 0;
         Iter := LGE_Lists.Iterate (Attrs.Path);
         while LGE_Lists.Has_Next (Iter) loop
            LGE_Lists.Next (Iter, Edge);

            Hash := (Hash * 31) + Bucket_Range_Type (Edge);
         end loop;

         return Hash;
      end Hash_Library_Graph_Cycle_Attributes;

      -----------------------------------------
      -- Hash_Predecessor_Successor_Relation --
      -----------------------------------------

      function Hash_Predecessor_Successor_Relation
        (Rel : Predecessor_Successor_Relation) return Bucket_Range_Type
      is
      begin
         pragma Assert (Present (Rel.Predecessor));
         pragma Assert (Present (Rel.Successor));

         return
           Hash_Two_Keys
             (Bucket_Range_Type (Rel.Predecessor),
              Bucket_Range_Type (Rel.Successor));
      end Hash_Predecessor_Successor_Relation;

      ------------------------------
      -- Highest_Precedence_Cycle --
      ------------------------------

      function Highest_Precedence_Cycle
        (G : Library_Graph) return Library_Graph_Cycle_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (LGC_Lists.Present (G.Cycles));

         if LGC_Lists.Is_Empty (G.Cycles) then
            return No_Library_Graph_Cycle;

         --  The highest precedence cycle is always the first in the list of
         --  all cycles.

         else
            return LGC_Lists.First (G.Cycles);
         end if;
      end Highest_Precedence_Cycle;

      -----------------------------
      -- Highest_Precedence_Edge --
      -----------------------------

      function Highest_Precedence_Edge
        (G     : Library_Graph;
         Left  : Library_Graph_Edge_Id;
         Right : Library_Graph_Edge_Id) return Library_Graph_Edge_Id
      is
         Edge_Prec : Precedence_Kind;

      begin
         pragma Assert (Present (G));

         --  Both edges are available, pick the one with highest precedence

         if Present (Left) and then Present (Right) then
            Edge_Prec :=
              Precedence
                (G           => G,
                 Edge        => Left,
                 Compared_To => Right);

            if Edge_Prec = Higher_Precedence then
               return Left;

            --  The precedence rules for edges are such that no two edges can
            --  ever have the same precedence.

            else
               pragma Assert (Edge_Prec = Lower_Precedence);
               return Right;
            end if;

         --  Otherwise at least one edge must be present

         elsif Present (Left) then
            return Left;

         else
            pragma Assert (Present (Right));

            return Right;
         end if;
      end Highest_Precedence_Edge;

      --------------------------
      -- In_Elaboration_Order --
      --------------------------

      function In_Elaboration_Order
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Get_LGV_Attributes (G, Vertex).In_Elaboration_Order;
      end In_Elaboration_Order;

      -----------------------
      -- In_Same_Component --
      -----------------------

      function In_Same_Component
        (G     : Library_Graph;
         Left  : Library_Graph_Vertex_Id;
         Right : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Left));
         pragma Assert (Present (Right));

         return Component (G, Left) = Component (G, Right);
      end In_Same_Component;

      ----------------------------------------
      -- Increment_Library_Graph_Edge_Count --
      ----------------------------------------

      procedure Increment_Library_Graph_Edge_Count
        (G    : Library_Graph;
         Kind : Library_Graph_Edge_Kind)
      is
         pragma Assert (Present (G));

         Count : Natural renames G.Counts (Kind);

      begin
         Count := Count + 1;
      end Increment_Library_Graph_Edge_Count;

      ------------------------------------
      -- Increment_Pending_Predecessors --
      ------------------------------------

      procedure Increment_Pending_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id;
         Edge : Library_Graph_Edge_Id)
      is
         Attrs : Component_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Attrs := Get_Component_Attributes (G, Comp);

         Update_Pending_Predecessors
           (Strong_Predecessors => Attrs.Pending_Strong_Predecessors,
            Weak_Predecessors   => Attrs.Pending_Weak_Predecessors,
            Update_Weak         => Is_Invocation_Edge (G, Edge),
            Value               => 1);

         Set_Component_Attributes (G, Comp, Attrs);
      end Increment_Pending_Predecessors;

      ------------------------------------
      -- Increment_Pending_Predecessors --
      ------------------------------------

      procedure Increment_Pending_Predecessors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Edge   : Library_Graph_Edge_Id)
      is
         Attrs : Library_Graph_Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Attrs := Get_LGV_Attributes (G, Vertex);

         Update_Pending_Predecessors
           (Strong_Predecessors => Attrs.Pending_Strong_Predecessors,
            Weak_Predecessors   => Attrs.Pending_Weak_Predecessors,
            Update_Weak         => Is_Invocation_Edge (G, Edge),
            Value               => 1);

         Set_LGV_Attributes (G, Vertex, Attrs);
      end Increment_Pending_Predecessors;

      ---------------------------
      -- Initialize_Components --
      ---------------------------

      procedure Initialize_Components (G : Library_Graph) is
      begin
         pragma Assert (Present (G));

         --  The graph already contains a set of components. Reinitialize
         --  them in order to accommodate the new set of components about to
         --  be computed.

         if Number_Of_Components (G) > 0 then
            Component_Tables.Destroy (G.Component_Attributes);

            G.Component_Attributes :=
              Component_Tables.Create (Number_Of_Vertices (G));
         end if;
      end Initialize_Components;

      ---------------------
      -- Insert_And_Sort --
      ---------------------

      procedure Insert_And_Sort
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id)
      is
         Lesser_Cycle : Library_Graph_Cycle_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));
         pragma Assert (LGC_Lists.Present (G.Cycles));

         --  The input cycle is the first to be inserted

         if LGC_Lists.Is_Empty (G.Cycles) then
            LGC_Lists.Prepend (G.Cycles, Cycle);

         --  Otherwise the list of all cycles contains at least one cycle.
         --  Insert the input cycle based on its precedence.

         else
            Lesser_Cycle := Find_First_Lower_Precedence_Cycle (G, Cycle);

            --  The list contains at least one cycle, and the input cycle has a
            --  higher precedence compared to some cycle in the list.

            if Present (Lesser_Cycle) then
               LGC_Lists.Insert_Before
                 (L      => G.Cycles,
                  Before => Lesser_Cycle,
                  Elem   => Cycle);

            --  Otherwise the input cycle has the lowest precedence among all
            --  cycles.

            else
               LGC_Lists.Append (G.Cycles, Cycle);
            end if;
         end if;
      end Insert_And_Sort;

      ---------------------------
      -- Invocation_Edge_Count --
      ---------------------------

      function Invocation_Edge_Count
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return Get_LGC_Attributes (G, Cycle).Invocation_Edge_Count;
      end Invocation_Edge_Count;

      -------------------------------
      -- Invocation_Graph_Encoding --
      -------------------------------

      function Invocation_Graph_Encoding
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id)
         return Invocation_Graph_Encoding_Kind
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Invocation_Graph_Encoding (Unit (G, Vertex));
      end Invocation_Graph_Encoding;

      -------------
      -- Is_Body --
      -------------

      function Is_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         U_Id  : constant Unit_Id := Unit (G, Vertex);
         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Utype = Is_Body or else U_Rec.Utype = Is_Body_Only;
      end Is_Body;

      -----------------------------------------
      -- Is_Body_Of_Spec_With_Elaborate_Body --
      -----------------------------------------

      function Is_Body_Of_Spec_With_Elaborate_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         if Is_Body_With_Spec (G, Vertex) then
            return
              Is_Spec_With_Elaborate_Body
                (G      => G,
                 Vertex => Proper_Spec (G, Vertex));
         end if;

         return False;
      end Is_Body_Of_Spec_With_Elaborate_Body;

      -----------------------
      -- Is_Body_With_Spec --
      -----------------------

      function Is_Body_With_Spec
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         U_Id  : constant Unit_Id := Unit (G, Vertex);
         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Utype = Is_Body;
      end Is_Body_With_Spec;

      ------------------------------
      -- Is_Cycle_Initiating_Edge --
      ------------------------------

      function Is_Cycle_Initiating_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Is_Cyclic_Elaborate_All_Edge (G, Edge)
             or else Is_Cyclic_Elaborate_Body_Edge (G, Edge)
             or else Is_Cyclic_Elaborate_Edge (G, Edge)
             or else Is_Cyclic_Forced_Edge (G, Edge)
             or else Is_Cyclic_Invocation_Edge (G, Edge);
      end Is_Cycle_Initiating_Edge;

      --------------------
      -- Is_Cyclic_Edge --
      --------------------

      function Is_Cyclic_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Is_Cycle_Initiating_Edge (G, Edge)
             or else Is_Cyclic_With_Edge (G, Edge);
      end Is_Cyclic_Edge;

      ----------------------------------
      -- Is_Cyclic_Elaborate_All_Edge --
      ----------------------------------

      function Is_Cyclic_Elaborate_All_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Is_Elaborate_All_Edge (G, Edge)
             and then Links_Vertices_In_Same_Component (G, Edge);
      end Is_Cyclic_Elaborate_All_Edge;

      -----------------------------------
      -- Is_Cyclic_Elaborate_Body_Edge --
      -----------------------------------

      function Is_Cyclic_Elaborate_Body_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Is_Elaborate_Body_Edge (G, Edge)
             and then Links_Vertices_In_Same_Component (G, Edge);
      end Is_Cyclic_Elaborate_Body_Edge;

      ------------------------------
      -- Is_Cyclic_Elaborate_Edge --
      ------------------------------

      function Is_Cyclic_Elaborate_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Is_Elaborate_Edge (G, Edge)
             and then Links_Vertices_In_Same_Component (G, Edge);
      end Is_Cyclic_Elaborate_Edge;

      ---------------------------
      -- Is_Cyclic_Forced_Edge --
      ---------------------------

      function Is_Cyclic_Forced_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Is_Forced_Edge (G, Edge)
             and then Links_Vertices_In_Same_Component (G, Edge);
      end Is_Cyclic_Forced_Edge;

      -------------------------------
      -- Is_Cyclic_Invocation_Edge --
      -------------------------------

      function Is_Cyclic_Invocation_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Is_Invocation_Edge (G, Edge)
             and then Links_Vertices_In_Same_Component (G, Edge);
      end Is_Cyclic_Invocation_Edge;

      -------------------------
      -- Is_Cyclic_With_Edge --
      -------------------------

      function Is_Cyclic_With_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         --  Ignore Elaborate_Body edges because they also appear as with
         --  edges, but have special successors.

         return
           Is_With_Edge (G, Edge)
             and then Links_Vertices_In_Same_Component (G, Edge)
             and then not Is_Elaborate_Body_Edge (G, Edge);
      end Is_Cyclic_With_Edge;

      -------------------------------
      -- Is_Dynamically_Elaborated --
      -------------------------------

      function Is_Dynamically_Elaborated
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Is_Dynamically_Elaborated (Unit (G, Vertex));
      end Is_Dynamically_Elaborated;

      -----------------------------
      -- Is_Elaborable_Component --
      -----------------------------

      function Is_Elaborable_Component
        (G    : Library_Graph;
         Comp : Component_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         --  A component is elaborable when:
         --
         --    * It is not waiting on strong predecessors, and
         --    * It is not waiting on weak predecessors

         return
           Pending_Strong_Predecessors (G, Comp) = 0
             and then Pending_Weak_Predecessors (G, Comp) = 0;
      end Is_Elaborable_Component;

      --------------------------
      -- Is_Elaborable_Vertex --
      --------------------------

      function Is_Elaborable_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Complement : constant Library_Graph_Vertex_Id :=
                        Complementary_Vertex
                          (G                => G,
                           Vertex           => Vertex,
                           Force_Complement => False);

         Strong_Preds : Natural;
         Weak_Preds   : Natural;

      begin
         --  A vertex is elaborable when:
         --
         --    * It has not been elaborated yet, and
         --    * The complement vertex of an Elaborate_Body pair has not been
         --      elaborated yet, and
         --    * It resides within an elaborable component, and
         --    * It is not waiting on strong predecessors, and
         --    * It is not waiting on weak predecessors

         if In_Elaboration_Order (G, Vertex) then
            return False;

         elsif Present (Complement)
           and then In_Elaboration_Order (G, Complement)
         then
            return False;

         elsif not Is_Elaborable_Component (G, Component (G, Vertex)) then
            return False;
         end if;

         Pending_Predecessors_For_Elaboration
           (G            => G,
            Vertex       => Vertex,
            Strong_Preds => Strong_Preds,
            Weak_Preds   => Weak_Preds);

         return Strong_Preds = 0 and then Weak_Preds = 0;
      end Is_Elaborable_Vertex;

      ---------------------------
      -- Is_Elaborate_All_Edge --
      ---------------------------

      function Is_Elaborate_All_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Kind (G, Edge) = Elaborate_All_Edge;
      end Is_Elaborate_All_Edge;

      ----------------------------
      -- Is_Elaborate_Body_Edge --
      ----------------------------

      function Is_Elaborate_Body_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         Succ : constant Library_Graph_Vertex_Id := Successor (G, Edge);

      begin
         return
           Kind (G, Edge) = With_Edge
             and then
               (Is_Spec_With_Elaborate_Body (G, Succ)
                 or else Is_Body_Of_Spec_With_Elaborate_Body (G, Succ));
      end Is_Elaborate_Body_Edge;

      -----------------------
      -- Is_Elaborate_Edge --
      -----------------------

      function Is_Elaborate_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Kind (G, Edge) = Elaborate_Edge;
      end Is_Elaborate_Edge;

      ----------------------------
      -- Is_Elaborate_Body_Pair --
      ----------------------------

      function Is_Elaborate_Body_Pair
        (G           : Library_Graph;
         Spec_Vertex : Library_Graph_Vertex_Id;
         Body_Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Spec_Vertex));
         pragma Assert (Present (Body_Vertex));

         return
           Is_Spec_With_Elaborate_Body (G, Spec_Vertex)
             and then Is_Body_Of_Spec_With_Elaborate_Body (G, Body_Vertex)
             and then Proper_Body (G, Spec_Vertex) = Body_Vertex;
      end Is_Elaborate_Body_Pair;

      --------------------
      -- Is_Forced_Edge --
      --------------------

      function Is_Forced_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Kind (G, Edge) = Forced_Edge;
      end Is_Forced_Edge;

      ----------------------
      -- Is_Internal_Unit --
      ----------------------

      function Is_Internal_Unit
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Is_Internal_Unit (Unit (G, Vertex));
      end Is_Internal_Unit;

      ------------------------
      -- Is_Invocation_Edge --
      ------------------------

      function Is_Invocation_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Kind (G, Edge) = Invocation_Edge;
      end Is_Invocation_Edge;

      ------------------------
      -- Is_Predefined_Unit --
      ------------------------

      function Is_Predefined_Unit
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Is_Predefined_Unit (Unit (G, Vertex));
      end Is_Predefined_Unit;

      ---------------------------
      -- Is_Preelaborated_Unit --
      ---------------------------

      function Is_Preelaborated_Unit
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         U_Id  : constant Unit_Id := Unit (G, Vertex);
         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Preelab or else U_Rec.Pure;
      end Is_Preelaborated_Unit;

      -----------------------
      -- Is_Recorded_Cycle --
      -----------------------

      function Is_Recorded_Cycle
        (G     : Library_Graph;
         Attrs : Library_Graph_Cycle_Attributes) return Boolean
      is
      begin
         pragma Assert (Present (G));

         return RC_Sets.Contains (G.Recorded_Cycles, Attrs);
      end Is_Recorded_Cycle;

      ----------------------
      -- Is_Recorded_Edge --
      ----------------------

      function Is_Recorded_Edge
        (G   : Library_Graph;
         Rel : Predecessor_Successor_Relation) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Rel.Predecessor));
         pragma Assert (Present (Rel.Successor));

         return RE_Sets.Contains (G.Recorded_Edges, Rel);
      end Is_Recorded_Edge;

      -------------
      -- Is_Spec --
      -------------

      function Is_Spec
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         U_Id  : constant Unit_Id := Unit (G, Vertex);
         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Utype = Is_Spec or else U_Rec.Utype = Is_Spec_Only;
      end Is_Spec;

      -----------------------
      -- Is_Spec_With_Body --
      -----------------------

      function Is_Spec_With_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         U_Id  : constant Unit_Id := Unit (G, Vertex);
         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Utype = Is_Spec;
      end Is_Spec_With_Body;

      ---------------------------------
      -- Is_Spec_With_Elaborate_Body --
      ---------------------------------

      function Is_Spec_With_Elaborate_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return
           Is_Spec_With_Body (G, Vertex)
             and then Has_Elaborate_Body (G, Vertex);
      end Is_Spec_With_Elaborate_Body;

      ------------------------------
      -- Is_Static_Successor_Edge --
      ------------------------------

      function Is_Static_Successor_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return
           Is_Invocation_Edge (G, Edge)
             and then not Is_Dynamically_Elaborated (G, Successor (G, Edge));
      end Is_Static_Successor_Edge;

      ---------------------------------
      -- Is_Weakly_Elaborable_Vertex --
      ----------------------------------

      function Is_Weakly_Elaborable_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Complement : constant Library_Graph_Vertex_Id :=
                        Complementary_Vertex
                          (G                => G,
                           Vertex           => Vertex,
                           Force_Complement => False);

         Strong_Preds : Natural;
         Weak_Preds   : Natural;

      begin
         --  A vertex is weakly elaborable when:
         --
         --    * It has not been elaborated yet, and
         --    * The complement vertex of an Elaborate_Body pair has not been
         --      elaborated yet, and
         --    * It resides within an elaborable component, and
         --    * It is not waiting on strong predecessors, and
         --    * It is waiting on at least one weak predecessor

         if In_Elaboration_Order (G, Vertex) then
            return False;

         elsif Present (Complement)
           and then In_Elaboration_Order (G, Complement)
         then
            return False;

         elsif not Is_Elaborable_Component (G, Component (G, Vertex)) then
            return False;
         end if;

         Pending_Predecessors_For_Elaboration
           (G            => G,
            Vertex       => Vertex,
            Strong_Preds => Strong_Preds,
            Weak_Preds   => Weak_Preds);

         return Strong_Preds = 0 and then Weak_Preds >= 1;
      end Is_Weakly_Elaborable_Vertex;

      ------------------
      -- Is_With_Edge --
      ------------------

      function Is_With_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Kind (G, Edge) = With_Edge;
      end Is_With_Edge;

      ------------------------
      -- Iterate_All_Cycles --
      ------------------------

      function Iterate_All_Cycles
        (G : Library_Graph) return All_Cycle_Iterator
      is
      begin
         pragma Assert (Present (G));

         return All_Cycle_Iterator (LGC_Lists.Iterate (G.Cycles));
      end Iterate_All_Cycles;

      -----------------------
      -- Iterate_All_Edges --
      -----------------------

      function Iterate_All_Edges
        (G : Library_Graph) return All_Edge_Iterator
      is
      begin
         pragma Assert (Present (G));

         return All_Edge_Iterator (DG.Iterate_All_Edges (G.Graph));
      end Iterate_All_Edges;

      --------------------------
      -- Iterate_All_Vertices --
      --------------------------

      function Iterate_All_Vertices
        (G : Library_Graph) return All_Vertex_Iterator
      is
      begin
         pragma Assert (Present (G));

         return All_Vertex_Iterator (DG.Iterate_All_Vertices (G.Graph));
      end Iterate_All_Vertices;

      ------------------------
      -- Iterate_Components --
      ------------------------

      function Iterate_Components
        (G : Library_Graph) return Component_Iterator
      is
      begin
         pragma Assert (Present (G));

         return Component_Iterator (DG.Iterate_Components (G.Graph));
      end Iterate_Components;

      --------------------------------
      -- Iterate_Component_Vertices --
      --------------------------------

      function Iterate_Component_Vertices
        (G    : Library_Graph;
         Comp : Component_Id) return Component_Vertex_Iterator
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         return
           Component_Vertex_Iterator
             (DG.Iterate_Component_Vertices (G.Graph, Comp));
      end Iterate_Component_Vertices;

      ----------------------------
      -- Iterate_Edges_Of_Cycle --
      ----------------------------

      function Iterate_Edges_Of_Cycle
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Edges_Of_Cycle_Iterator
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return Edges_Of_Cycle_Iterator (LGE_Lists.Iterate (Path (G, Cycle)));
      end Iterate_Edges_Of_Cycle;

      ---------------------------------
      -- Iterate_Edges_To_Successors --
      ---------------------------------

      function Iterate_Edges_To_Successors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Edges_To_Successors_Iterator
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return
           Edges_To_Successors_Iterator
             (DG.Iterate_Outgoing_Edges (G.Graph, Vertex));
      end Iterate_Edges_To_Successors;

      ----------
      -- Kind --
      ----------

      function Kind
        (G      : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Library_Graph_Cycle_Kind
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return Get_LGC_Attributes (G, Cycle).Kind;
      end Kind;

      ----------
      -- Kind --
      ----------

      function Kind
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Edge_Kind
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return Get_LGE_Attributes (G, Edge).Kind;
      end Kind;

      ------------
      -- Length --
      ------------

      function Length
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return LGE_Lists.Size (Path (G, Cycle));
      end Length;

      ------------------------------
      -- Library_Graph_Edge_Count --
      ------------------------------

      function Library_Graph_Edge_Count
        (G    : Library_Graph;
         Kind : Library_Graph_Edge_Kind) return Natural
      is
      begin
         pragma Assert (Present (G));

         return G.Counts (Kind);
      end Library_Graph_Edge_Count;

      --------------------------------------
      -- Links_Vertices_In_Same_Component --
      --------------------------------------

      function Links_Vertices_In_Same_Component
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         --  An edge is part of a cycle when both the successor and predecessor
         --  reside in the same component.

         return
           In_Same_Component
             (G     => G,
              Left  => Predecessor (G, Edge),
              Right => Successor   (G, Edge));
      end Links_Vertices_In_Same_Component;

      -----------------------------------
      -- Maximum_Invocation_Edge_Count --
      -----------------------------------

      function Maximum_Invocation_Edge_Count
        (G     : Library_Graph;
         Edge  : Library_Graph_Edge_Id;
         Count : Natural) return Natural
      is
         New_Count : Natural;

      begin
         pragma Assert (Present (G));

         New_Count := Count;

         if Present (Edge) and then Is_Invocation_Edge (G, Edge) then
            New_Count := New_Count + 1;
         end if;

         return New_Count;
      end Maximum_Invocation_Edge_Count;

      ----------
      -- Name --
      ----------

      function Name
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Unit_Name_Type
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Name (Unit (G, Vertex));
      end Name;

      -----------------------
      -- Needs_Elaboration --
      -----------------------

      function Needs_Elaboration
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Needs_Elaboration (Unit (G, Vertex));
      end Needs_Elaboration;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter  : in out All_Cycle_Iterator;
         Cycle : out Library_Graph_Cycle_Id)
      is
      begin
         LGC_Lists.Next (LGC_Lists.Iterator (Iter), Cycle);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out All_Edge_Iterator;
         Edge : out Library_Graph_Edge_Id)
      is
      begin
         DG.Next (DG.All_Edge_Iterator (Iter), Edge);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out All_Vertex_Iterator;
         Vertex : out Library_Graph_Vertex_Id)
      is
      begin
         DG.Next (DG.All_Vertex_Iterator (Iter), Vertex);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Edges_Of_Cycle_Iterator;
         Edge : out Library_Graph_Edge_Id)
      is
      begin
         LGE_Lists.Next (LGE_Lists.Iterator (Iter), Edge);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Component_Iterator;
         Comp : out Component_Id)
      is
      begin
         DG.Next (DG.Component_Iterator (Iter), Comp);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Edges_To_Successors_Iterator;
         Edge : out Library_Graph_Edge_Id)
      is
      begin
         DG.Next (DG.Outgoing_Edge_Iterator (Iter), Edge);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out Component_Vertex_Iterator;
         Vertex : out Library_Graph_Vertex_Id)
      is
      begin
         DG.Next (DG.Component_Vertex_Iterator (Iter), Vertex);
      end Next;

      -----------------------------
      -- Normalize_And_Add_Cycle --
      -----------------------------

      procedure Normalize_And_Add_Cycle
        (G                     : Library_Graph;
         Most_Significant_Edge : Library_Graph_Edge_Id;
         Invocation_Edge_Count : Natural;
         Cycle_Path            : LGE_Lists.Doubly_Linked_List;
         Indent                : Indentation_Level)
      is
         Path : LGE_Lists.Doubly_Linked_List;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Most_Significant_Edge));
         pragma Assert (LGE_Lists.Present (Cycle_Path));

         --  Replicate the path of the cycle in order to avoid sharing lists

         Path := Copy_Cycle_Path (Cycle_Path);

         --  Normalize the path of the cycle such that its most significant
         --  edge is the first in the list of edges.

         Normalize_Cycle_Path
           (Cycle_Path            => Path,
            Most_Significant_Edge => Most_Significant_Edge);

         --  Save the cycle for diagnostic purposes. Its kind is determined by
         --  its most significant edge.

         Add_Cycle
           (G      => G,
            Attrs  =>
             (Invocation_Edge_Count => Invocation_Edge_Count,
              Kind                  =>
                Cycle_Kind_Of
                  (G    => G,
                   Edge => Most_Significant_Edge),
              Path                  => Path),
            Indent => Indent);
      end Normalize_And_Add_Cycle;

      --------------------------
      -- Normalize_Cycle_Path --
      --------------------------

      procedure Normalize_Cycle_Path
        (Cycle_Path            : LGE_Lists.Doubly_Linked_List;
         Most_Significant_Edge : Library_Graph_Edge_Id)
      is
         Edge : Library_Graph_Edge_Id;

      begin
         pragma Assert (LGE_Lists.Present (Cycle_Path));
         pragma Assert (Present (Most_Significant_Edge));

         --  Perform at most |Cycle_Path| rotations in case the cycle is
         --  malformed and the significant edge does not appear within.

         for Rotation in 1 .. LGE_Lists.Size (Cycle_Path) loop
            Edge := LGE_Lists.First (Cycle_Path);

            --  The cycle is already rotated such that the most significant
            --  edge is first.

            if Edge = Most_Significant_Edge then
               return;

            --  Otherwise rotate the cycle by relocating the current edge from
            --  the start to the end of the path. This preserves the order of
            --  the path.

            else
               LGE_Lists.Delete_First (Cycle_Path);
               LGE_Lists.Append (Cycle_Path, Edge);
            end if;
         end loop;

         pragma Assert (False);
      end Normalize_Cycle_Path;

      ----------------------------------
      -- Number_Of_Component_Vertices --
      ----------------------------------

      function Number_Of_Component_Vertices
        (G    : Library_Graph;
         Comp : Component_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         return DG.Number_Of_Component_Vertices (G.Graph, Comp);
      end Number_Of_Component_Vertices;

      --------------------------
      -- Number_Of_Components --
      --------------------------

      function Number_Of_Components (G : Library_Graph) return Natural is
      begin
         pragma Assert (Present (G));

         return DG.Number_Of_Components (G.Graph);
      end Number_Of_Components;

      ----------------------
      -- Number_Of_Cycles --
      ----------------------

      function Number_Of_Cycles (G : Library_Graph) return Natural is
      begin
         pragma Assert (Present (G));

         return LGC_Lists.Size (G.Cycles);
      end Number_Of_Cycles;

      ---------------------
      -- Number_Of_Edges --
      ---------------------

      function Number_Of_Edges (G : Library_Graph) return Natural is
      begin
         pragma Assert (Present (G));

         return DG.Number_Of_Edges (G.Graph);
      end Number_Of_Edges;

      -----------------------------------
      -- Number_Of_Edges_To_Successors --
      -----------------------------------

      function Number_Of_Edges_To_Successors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Natural
      is
      begin
         pragma Assert (Present (G));

         return DG.Number_Of_Outgoing_Edges (G.Graph, Vertex);
      end Number_Of_Edges_To_Successors;

      ------------------------
      -- Number_Of_Vertices --
      ------------------------

      function Number_Of_Vertices (G : Library_Graph) return Natural is
      begin
         pragma Assert (Present (G));

         return DG.Number_Of_Vertices (G.Graph);
      end Number_Of_Vertices;

      ----------
      -- Path --
      ----------

      function Path
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) return LGE_Lists.Doubly_Linked_List
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         return Get_LGC_Attributes (G, Cycle).Path;
      end Path;

      ------------------------------------------
      -- Pending_Predecessors_For_Elaboration --
      ------------------------------------------

      procedure Pending_Predecessors_For_Elaboration
        (G            : Library_Graph;
         Vertex       : Library_Graph_Vertex_Id;
         Strong_Preds : out Natural;
         Weak_Preds   : out Natural)
      is
         Complement         : Library_Graph_Vertex_Id;
         Spec_Vertex        : Library_Graph_Vertex_Id;
         Total_Strong_Preds : Natural;
         Total_Weak_Preds   : Natural;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Total_Strong_Preds := Pending_Strong_Predecessors (G, Vertex);
         Total_Weak_Preds   := Pending_Weak_Predecessors   (G, Vertex);

         --  Assume that there is no complementary vertex that needs to be
         --  examined.

         Complement  := No_Library_Graph_Vertex;
         Spec_Vertex := No_Library_Graph_Vertex;

         if Is_Body_Of_Spec_With_Elaborate_Body (G, Vertex) then
            Complement  := Proper_Spec (G, Vertex);
            Spec_Vertex := Complement;

         elsif Is_Spec_With_Elaborate_Body (G, Vertex) then
            Complement  := Proper_Body (G, Vertex);
            Spec_Vertex := Vertex;
         end if;

         --  The vertex is part of an Elaborate_Body pair. Take into account
         --  the strong and weak predecessors of the complementary vertex.

         if Present (Complement) then
            Total_Strong_Preds :=
              Pending_Strong_Predecessors (G, Complement) + Total_Strong_Preds;
            Total_Weak_Preds :=
              Pending_Weak_Predecessors   (G, Complement) + Total_Weak_Preds;

            --  The body of an Elaborate_Body pair is the successor of a strong
            --  edge where the predecessor is the spec. This edge must not be
            --  considered for elaboration purposes because the pair is treated
            --  as one vertex. Account for the edge only when the spec has not
            --  been elaborated yet.

            if not In_Elaboration_Order (G, Spec_Vertex) then
               Total_Strong_Preds := Total_Strong_Preds - 1;
            end if;
         end if;

         Strong_Preds := Total_Strong_Preds;
         Weak_Preds   := Total_Weak_Preds;
      end Pending_Predecessors_For_Elaboration;

      ---------------------------------
      -- Pending_Strong_Predecessors --
      ---------------------------------

      function Pending_Strong_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         return Get_Component_Attributes (G, Comp).Pending_Strong_Predecessors;
      end Pending_Strong_Predecessors;

      ---------------------------------
      -- Pending_Strong_Predecessors --
      ---------------------------------

      function Pending_Strong_Predecessors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Get_LGV_Attributes (G, Vertex).Pending_Strong_Predecessors;
      end Pending_Strong_Predecessors;

      -------------------------------
      -- Pending_Weak_Predecessors --
      -------------------------------

      function Pending_Weak_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         return Get_Component_Attributes (G, Comp).Pending_Weak_Predecessors;
      end Pending_Weak_Predecessors;

      -------------------------------
      -- Pending_Weak_Predecessors --
      -------------------------------

      function Pending_Weak_Predecessors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Get_LGV_Attributes (G, Vertex).Pending_Weak_Predecessors;
      end Pending_Weak_Predecessors;

      ----------------
      -- Precedence --
      ----------------

      function Precedence
        (G           : Library_Graph;
         Cycle       : Library_Graph_Cycle_Id;
         Compared_To : Library_Graph_Cycle_Id) return Precedence_Kind
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));
         pragma Assert (Present (Compared_To));

         Comp_Invs  : constant Natural :=
                        Invocation_Edge_Count (G, Compared_To);
         Comp_Len   : constant Natural := Length (G, Compared_To);
         Cycle_Invs : constant Natural := Invocation_Edge_Count (G, Cycle);
         Cycle_Len  : constant Natural := Length (G, Cycle);
         Kind_Prec  : constant Precedence_Kind :=
                        Precedence
                          (Kind        => Kind (G, Cycle),
                           Compared_To => Kind (G, Compared_To));

      begin
         if Kind_Prec = Higher_Precedence
              or else
            Kind_Prec = Lower_Precedence
         then
            return Kind_Prec;

         --  Otherwise both cycles have the same precedence based on their
         --  kind. Prefer a cycle with fewer invocation edges.

         elsif Cycle_Invs < Comp_Invs then
            return Higher_Precedence;

         elsif Cycle_Invs > Comp_Invs then
            return Lower_Precedence;

         --  Otherwise both cycles have the same number of invocation edges.
         --  Prefer a cycle with a smaller length.

         elsif Cycle_Len < Comp_Len then
            return Higher_Precedence;

         elsif Cycle_Len > Comp_Len then
            return Lower_Precedence;

         else
            return Equal_Precedence;
         end if;
      end Precedence;

      ----------------
      -- Precedence --
      ----------------

      function Precedence
        (Kind        : Library_Graph_Cycle_Kind;
         Compared_To : Library_Graph_Cycle_Kind) return Precedence_Kind
      is
         Comp_Pos : constant Integer :=
                      Library_Graph_Cycle_Kind'Pos (Compared_To);
         Kind_Pos : constant Integer := Library_Graph_Cycle_Kind'Pos (Kind);

      begin
         --  A lower ordinal indicates higher precedence

         if Kind_Pos < Comp_Pos then
            return Higher_Precedence;

         elsif Kind_Pos > Comp_Pos then
            return Lower_Precedence;

         else
            return Equal_Precedence;
         end if;
      end Precedence;

      ----------------
      -- Precedence --
      ----------------

      function Precedence
        (G           : Library_Graph;
         Edge        : Library_Graph_Edge_Id;
         Compared_To : Library_Graph_Edge_Id) return Precedence_Kind
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));
         pragma Assert (Present (Compared_To));

         Kind_Prec : constant Precedence_Kind :=
                       Precedence
                         (Kind        => Cycle_Kind_Of (G, Edge),
                          Compared_To => Cycle_Kind_Of (G, Compared_To));

      begin
         if Kind_Prec = Higher_Precedence
              or else
            Kind_Prec = Lower_Precedence
         then
            return Kind_Prec;

         --  Otherwise both edges have the same precedence based on their cycle
         --  kinds. Prefer an edge whose successor has higher precedence.

         else
            return
              Precedence
                (G           => G,
                 Vertex      => Successor (G, Edge),
                 Compared_To => Successor (G, Compared_To));
         end if;
      end Precedence;

      ----------------
      -- Precedence --
      ----------------

      function Precedence
        (G           : Library_Graph;
         Vertex      : Library_Graph_Vertex_Id;
         Compared_To : Library_Graph_Vertex_Id) return Precedence_Kind
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (Present (Compared_To));

         --  Use lexicographical order to determine precedence and ensure
         --  deterministic behavior.

         if Uname_Less (Name (G, Vertex), Name (G, Compared_To)) then
            return Higher_Precedence;
         else
            return Lower_Precedence;
         end if;
      end Precedence;

      -----------------
      -- Predecessor --
      -----------------

      function Predecessor
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return DG.Source_Vertex (G.Graph, Edge);
      end Predecessor;

      -------------
      -- Present --
      -------------

      function Present (G : Library_Graph) return Boolean is
      begin
         return G /= Nil;
      end Present;

      -----------------
      -- Proper_Body --
      -----------------

      function Proper_Body
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         --  When the vertex denotes a spec with a completing body, return the
         --  body.

         if Is_Spec_With_Body (G, Vertex) then
            return Corresponding_Item (G, Vertex);

         --  Otherwise the vertex must be a body

         else
            pragma Assert (Is_Body (G, Vertex));
            return Vertex;
         end if;
      end Proper_Body;

      -----------------
      -- Proper_Spec --
      -----------------

      function Proper_Spec
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         --  When the vertex denotes a body that completes a spec, return the
         --  spec.

         if Is_Body_With_Spec (G, Vertex) then
            return Corresponding_Item (G, Vertex);

         --  Otherwise the vertex must denote a spec

         else
            pragma Assert (Is_Spec (G, Vertex));
            return Vertex;
         end if;
      end Proper_Spec;

      ----------------------------------
      -- Remove_Vertex_And_Complement --
      ----------------------------------

      procedure Remove_Vertex_And_Complement
        (G             : Library_Graph;
         Vertex        : Library_Graph_Vertex_Id;
         Set           : LGV_Sets.Membership_Set;
         Do_Complement : Boolean)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));
         pragma Assert (LGV_Sets.Present (Set));

         Complement : constant Library_Graph_Vertex_Id :=
                        Complementary_Vertex
                          (G                => G,
                           Vertex           => Vertex,
                           Force_Complement => Do_Complement);

      begin
         LGV_Sets.Delete (Set, Vertex);

         if Present (Complement) then
            LGV_Sets.Delete (Set, Complement);
         end if;
      end Remove_Vertex_And_Complement;

      -----------------------------------------
      -- Same_Library_Graph_Cycle_Attributes --
      -----------------------------------------

      function Same_Library_Graph_Cycle_Attributes
        (Left  : Library_Graph_Cycle_Attributes;
         Right : Library_Graph_Cycle_Attributes) return Boolean
      is
      begin
         --  Two cycles are the same when
         --
         --    * They are of the same kind
         --    * They have the same number of invocation edges in their paths
         --    * Their paths are the same length
         --    * The edges comprising their paths are the same

         return
            Left.Invocation_Edge_Count = Right.Invocation_Edge_Count
              and then Left.Kind = Right.Kind
              and then LGE_Lists.Equal (Left.Path, Right.Path);
      end Same_Library_Graph_Cycle_Attributes;

      ------------------------------
      -- Set_Component_Attributes --
      ------------------------------

      procedure Set_Component_Attributes
        (G    : Library_Graph;
         Comp : Component_Id;
         Val  : Component_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Component_Tables.Put (G.Component_Attributes, Comp, Val);
      end Set_Component_Attributes;

      ----------------------------
      -- Set_Corresponding_Item --
      ----------------------------

      procedure Set_Corresponding_Item
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Val    : Library_Graph_Vertex_Id)
      is
         Attrs : Library_Graph_Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Attrs := Get_LGV_Attributes (G, Vertex);
         Attrs.Corresponding_Item := Val;
         Set_LGV_Attributes (G, Vertex, Attrs);
      end Set_Corresponding_Item;

      ------------------------------
      -- Set_Corresponding_Vertex --
      ------------------------------

      procedure Set_Corresponding_Vertex
        (G    : Library_Graph;
         U_Id : Unit_Id;
         Val  : Library_Graph_Vertex_Id)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (U_Id));

         Unit_Tables.Put (G.Unit_To_Vertex, U_Id, Val);
      end Set_Corresponding_Vertex;

      ------------------------------
      -- Set_In_Elaboration_Order --
      ------------------------------

      procedure Set_In_Elaboration_Order
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Val    : Boolean := True)
      is
         Attrs : Library_Graph_Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Attrs := Get_LGV_Attributes (G, Vertex);
         Attrs.In_Elaboration_Order := Val;
         Set_LGV_Attributes (G, Vertex, Attrs);
      end Set_In_Elaboration_Order;

      ---------------------------
      -- Set_Is_Recorded_Cycle --
      ---------------------------

      procedure Set_Is_Recorded_Cycle
        (G     : Library_Graph;
         Attrs : Library_Graph_Cycle_Attributes;
         Val   : Boolean := True)
      is
      begin
         pragma Assert (Present (G));

         if Val then
            RC_Sets.Insert (G.Recorded_Cycles, Attrs);
         else
            RC_Sets.Delete (G.Recorded_Cycles, Attrs);
         end if;
      end Set_Is_Recorded_Cycle;

      --------------------------
      -- Set_Is_Recorded_Edge --
      --------------------------

      procedure Set_Is_Recorded_Edge
        (G   : Library_Graph;
         Rel : Predecessor_Successor_Relation;
         Val : Boolean := True)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Rel.Predecessor));
         pragma Assert (Present (Rel.Successor));

         if Val then
            RE_Sets.Insert (G.Recorded_Edges, Rel);
         else
            RE_Sets.Delete (G.Recorded_Edges, Rel);
         end if;
      end Set_Is_Recorded_Edge;

      ------------------------
      -- Set_LGC_Attributes --
      ------------------------

      procedure Set_LGC_Attributes
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id;
         Val   : Library_Graph_Cycle_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         LGC_Tables.Put (G.Cycle_Attributes, Cycle, Val);
      end Set_LGC_Attributes;

      ------------------------
      -- Set_LGE_Attributes --
      ------------------------

      procedure Set_LGE_Attributes
        (G      : Library_Graph;
         Edge : Library_Graph_Edge_Id;
         Val    : Library_Graph_Edge_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         LGE_Tables.Put (G.Edge_Attributes, Edge, Val);
      end Set_LGE_Attributes;

      ------------------------
      -- Set_LGV_Attributes --
      ------------------------

      procedure Set_LGV_Attributes
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Val    : Library_Graph_Vertex_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         LGV_Tables.Put (G.Vertex_Attributes, Vertex, Val);
      end Set_LGV_Attributes;

      ---------------
      -- Successor --
      ---------------

      function Successor
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         return DG.Destination_Vertex (G.Graph, Edge);
      end Successor;

      -----------------
      -- Trace_Cycle --
      -----------------

      procedure Trace_Cycle
        (G      : Library_Graph;
         Cycle  : Library_Graph_Cycle_Id;
         Indent : Indentation_Level)
      is
         Attr_Indent : constant Indentation_Level :=
                         Indent + Nested_Indentation;
         Edge_Indent : constant Indentation_Level :=
                         Attr_Indent + Nested_Indentation;

         Edge : Library_Graph_Edge_Id;
         Iter : Edges_Of_Cycle_Iterator;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         --  Nothing to do when switch -d_T (output elaboration order and cycle
         --  detection trace information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Indent_By (Indent);
         Write_Str ("cycle (Cycle_Id_");
         Write_Int (Int (Cycle));
         Write_Str (")");
         Write_Eol;

         Indent_By (Attr_Indent);
         Write_Str ("kind = ");
         Write_Str (Kind (G, Cycle)'Img);
         Write_Eol;

         Indent_By (Attr_Indent);
         Write_Str ("invocation edges = ");
         Write_Int (Int (Invocation_Edge_Count (G, Cycle)));
         Write_Eol;

         Indent_By (Attr_Indent);
         Write_Str ("length: ");
         Write_Int (Int (Length (G, Cycle)));
         Write_Eol;

         Iter := Iterate_Edges_Of_Cycle (G, Cycle);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            Indent_By (Edge_Indent);
            Write_Str ("library graph edge (LGE_Id_");
            Write_Int (Int (Edge));
            Write_Str (")");
            Write_Eol;
         end loop;
      end Trace_Cycle;

      ----------------
      -- Trace_Edge --
      ----------------

      procedure Trace_Edge
        (G      : Library_Graph;
         Edge   : Library_Graph_Edge_Id;
         Indent : Indentation_Level)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         Attr_Indent : constant Indentation_Level :=
                         Indent + Nested_Indentation;

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, Edge);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, Edge);

      begin
         --  Nothing to do when switch -d_T (output elaboration order and cycle
         --  detection trace information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Indent_By (Indent);
         Write_Str ("library graph edge (LGE_Id_");
         Write_Int (Int (Edge));
         Write_Str (")");
         Write_Eol;

         Indent_By (Attr_Indent);
         Write_Str ("kind = ");
         Write_Str (Kind (G, Edge)'Img);
         Write_Eol;

         Indent_By  (Attr_Indent);
         Write_Str  ("Predecessor (LGV_Id_");
         Write_Int  (Int (Pred));
         Write_Str  (") name = ");
         Write_Name (Name (G, Pred));
         Write_Eol;

         Indent_By  (Attr_Indent);
         Write_Str  ("Successor   (LGV_Id_");
         Write_Int  (Int (Succ));
         Write_Str  (") name = ");
         Write_Name (Name (G, Succ));
         Write_Eol;
      end Trace_Edge;

      ---------------
      -- Trace_Eol --
      ---------------

      procedure Trace_Eol is
      begin
         --  Nothing to do when switch -d_T (output elaboration order and cycle
         --  detection trace information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Write_Eol;
      end Trace_Eol;

      ------------------
      -- Trace_Vertex --
      ------------------

      procedure Trace_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id;
         Indent : Indentation_Level)
      is
         Attr_Indent : constant Indentation_Level :=
                         Indent + Nested_Indentation;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         --  Nothing to do when switch -d_T (output elaboration order and cycle
         --  detection trace information) is not in effect.

         if not Debug_Flag_Underscore_TT then
            return;
         end if;

         Indent_By (Indent);
         Write_Str ("library graph vertex (LGV_Id_");
         Write_Int (Int (Vertex));
         Write_Str (")");
         Write_Eol;

         Indent_By (Attr_Indent);
         Write_Str ("Component (Comp_Id_");
         Write_Int (Int (Component (G, Vertex)));
         Write_Str (")");
         Write_Eol;

         Indent_By  (Attr_Indent);
         Write_Str  ("Unit (U_Id_");
         Write_Int  (Int (Unit (G, Vertex)));
         Write_Str  (") name = ");
         Write_Name (Name (G, Vertex));
         Write_Eol;
      end Trace_Vertex;

      ----------
      -- Unit --
      ----------

      function Unit
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) return Unit_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         return Get_LGV_Attributes (G, Vertex).Unit;
      end Unit;

      ---------------------------------
      -- Update_Pending_Predecessors --
      ---------------------------------

      procedure Update_Pending_Predecessors
        (Strong_Predecessors : in out Natural;
         Weak_Predecessors   : in out Natural;
         Update_Weak         : Boolean;
         Value               : Integer)
      is
      begin
         if Update_Weak then
            Weak_Predecessors := Weak_Predecessors + Value;
         else
            Strong_Predecessors := Strong_Predecessors + Value;
         end if;
      end Update_Pending_Predecessors;

      -----------------------------------------------
      -- Update_Pending_Predecessors_Of_Components --
      -----------------------------------------------

      procedure Update_Pending_Predecessors_Of_Components
        (G : Library_Graph)
      is
         Edge : Library_Graph_Edge_Id;
         Iter : All_Edge_Iterator;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            Update_Pending_Predecessors_Of_Components (G, Edge);
         end loop;
      end Update_Pending_Predecessors_Of_Components;

      -----------------------------------------------
      -- Update_Pending_Predecessors_Of_Components --
      -----------------------------------------------

      procedure Update_Pending_Predecessors_Of_Components
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         Pred_Comp : constant Component_Id :=
                       Component (G, Predecessor (G, Edge));
         Succ_Comp : constant Component_Id :=
                       Component (G, Successor   (G, Edge));

         pragma Assert (Present (Pred_Comp));
         pragma Assert (Present (Succ_Comp));

      begin
         --  The edge links a successor and a predecessor coming from two
         --  different SCCs. This indicates that the SCC of the successor
         --  must wait on another predecessor until it can be elaborated.

         if Pred_Comp /= Succ_Comp then
            Increment_Pending_Predecessors
              (G    => G,
               Comp => Succ_Comp,
               Edge => Edge);
         end if;
      end Update_Pending_Predecessors_Of_Components;
   end Library_Graphs;

   -------------
   -- Present --
   -------------

   function Present (Edge : Invocation_Graph_Edge_Id) return Boolean is
   begin
      return Edge /= No_Invocation_Graph_Edge;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (Vertex : Invocation_Graph_Vertex_Id) return Boolean is
   begin
      return Vertex /= No_Invocation_Graph_Vertex;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (Cycle : Library_Graph_Cycle_Id) return Boolean is
   begin
      return Cycle /= No_Library_Graph_Cycle;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (Edge : Library_Graph_Edge_Id) return Boolean is
   begin
      return Edge /= No_Library_Graph_Edge;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (Vertex : Library_Graph_Vertex_Id) return Boolean is
   begin
      return Vertex /= No_Library_Graph_Vertex;
   end Present;

   --------------------------
   -- Sequence_Next_Edge --
   --------------------------

   IGE_Sequencer : Invocation_Graph_Edge_Id := First_Invocation_Graph_Edge;
   --  The counter for invocation graph edges. Do not directly manipulate its
   --  value.

   function Sequence_Next_Edge return Invocation_Graph_Edge_Id is
      Edge : constant Invocation_Graph_Edge_Id := IGE_Sequencer;

   begin
      IGE_Sequencer := IGE_Sequencer + 1;
      return Edge;
   end Sequence_Next_Edge;

   --------------------------
   -- Sequence_Next_Vertex --
   --------------------------

   IGV_Sequencer : Invocation_Graph_Vertex_Id := First_Invocation_Graph_Vertex;
   --  The counter for invocation graph vertices. Do not directly manipulate
   --  its value.

   function Sequence_Next_Vertex return Invocation_Graph_Vertex_Id is
      Vertex : constant Invocation_Graph_Vertex_Id := IGV_Sequencer;

   begin
      IGV_Sequencer := IGV_Sequencer + 1;
      return Vertex;
   end Sequence_Next_Vertex;

   --------------------------
   -- Sequence_Next_Cycle --
   --------------------------

   LGC_Sequencer : Library_Graph_Cycle_Id := First_Library_Graph_Cycle;
   --  The counter for library graph cycles. Do not directly manipulate its
   --  value.

   function Sequence_Next_Cycle return Library_Graph_Cycle_Id is
      Cycle : constant Library_Graph_Cycle_Id := LGC_Sequencer;

   begin
      LGC_Sequencer := LGC_Sequencer + 1;
      return Cycle;
   end Sequence_Next_Cycle;

   --------------------------
   -- Sequence_Next_Edge --
   --------------------------

   LGE_Sequencer : Library_Graph_Edge_Id := First_Library_Graph_Edge;
   --  The counter for library graph edges. Do not directly manipulate its
   --  value.

   function Sequence_Next_Edge return Library_Graph_Edge_Id is
      Edge : constant Library_Graph_Edge_Id := LGE_Sequencer;

   begin
      LGE_Sequencer := LGE_Sequencer + 1;
      return Edge;
   end Sequence_Next_Edge;

   --------------------------
   -- Sequence_Next_Vertex --
   --------------------------

   LGV_Sequencer : Library_Graph_Vertex_Id := First_Library_Graph_Vertex;
   --  The counter for library graph vertices. Do not directly manipulate its
   --  value.

   function Sequence_Next_Vertex return Library_Graph_Vertex_Id is
      Vertex : constant Library_Graph_Vertex_Id := LGV_Sequencer;

   begin
      LGV_Sequencer := LGV_Sequencer + 1;
      return Vertex;
   end Sequence_Next_Vertex;

end Bindo.Graphs;
