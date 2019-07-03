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

with GNAT.Lists; use GNAT.Lists;

package body Bindo.Graphs is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Sequence_Next_IGE_Id return Invocation_Graph_Edge_Id;
   pragma Inline (Sequence_Next_IGE_Id);
   --  Generate a new unique invocation graph edge handle

   function Sequence_Next_IGV_Id return Invocation_Graph_Vertex_Id;
   pragma Inline (Sequence_Next_IGV_Id);
   --  Generate a new unique invocation graph vertex handle

   function Sequence_Next_LGE_Id return Library_Graph_Edge_Id;
   pragma Inline (Sequence_Next_LGE_Id);
   --  Generate a new unique library graph edge handle

   function Sequence_Next_LGV_Id return Library_Graph_Vertex_Id;
   pragma Inline (Sequence_Next_LGV_Id);
   --  Generate a new unique library graph vertex handle

   --------------------------------
   -- Hash_Invocation_Graph_Edge --
   --------------------------------

   function Hash_Invocation_Graph_Edge
     (IGE_Id : Invocation_Graph_Edge_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (IGE_Id));

      return Bucket_Range_Type (IGE_Id);
   end Hash_Invocation_Graph_Edge;

   ----------------------------------
   -- Hash_Invocation_Graph_Vertex --
   ----------------------------------

   function Hash_Invocation_Graph_Vertex
     (IGV_Id : Invocation_Graph_Vertex_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (IGV_Id));

      return Bucket_Range_Type (IGV_Id);
   end Hash_Invocation_Graph_Vertex;

   -----------------------------
   -- Hash_Library_Graph_Edge --
   -----------------------------

   function Hash_Library_Graph_Edge
     (LGE_Id : Library_Graph_Edge_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (LGE_Id));

      return Bucket_Range_Type (LGE_Id);
   end Hash_Library_Graph_Edge;

   -------------------------------
   -- Hash_Library_Graph_Vertex --
   -------------------------------

   function Hash_Library_Graph_Vertex
     (LGV_Id : Library_Graph_Vertex_Id) return Bucket_Range_Type
   is
   begin
      pragma Assert (Present (LGV_Id));

      return Bucket_Range_Type (LGV_Id);
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
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id)
         return Invocation_Graph_Edge_Attributes;
      pragma Inline (Get_IGE_Attributes);
      --  Obtain the attributes of edge IGE_Id of invocation graph G

      function Get_IGV_Attributes
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id)
         return Invocation_Graph_Vertex_Attributes;
      pragma Inline (Get_IGV_Attributes);
      --  Obtain the attributes of vertex IGV_Id of invocation graph G

      procedure Increment_Invocation_Graph_Edge_Count
        (G    : Invocation_Graph;
         Kind : Invocation_Kind);
      pragma Inline (Increment_Invocation_Graph_Edge_Count);
      --  Increment the number of edges of king Kind in invocation graph G by
      --  one.

      function Is_Elaboration_Root
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Boolean;
      pragma Inline (Is_Elaboration_Root);
      --  Determine whether vertex IGV_Id of invocation graph denotes the
      --  elaboration procedure of a spec or a body.

      function Is_Existing_Source_Target_Relation
        (G   : Invocation_Graph;
         Rel : Source_Target_Relation) return Boolean;
      pragma Inline (Is_Existing_Source_Target_Relation);
      --  Determine whether a source vertex and a target vertex desctibed by
      --  relation Rel are already related in invocation graph G.

      procedure Save_Elaboration_Root
        (G    : Invocation_Graph;
         Root : Invocation_Graph_Vertex_Id);
      pragma Inline (Save_Elaboration_Root);
      --  Save elaboration root Root of invocation graph G

      procedure Set_Corresponding_Vertex
        (G      : Invocation_Graph;
         IS_Id  : Invocation_Signature_Id;
         IGV_Id : Invocation_Graph_Vertex_Id);
      pragma Inline (Set_Corresponding_Vertex);
      --  Associate vertex IGV_Id of invocation graph G with signature IS_Id

      procedure Set_Is_Existing_Source_Target_Relation
        (G   : Invocation_Graph;
         Rel : Source_Target_Relation;
         Val : Boolean := True);
      pragma Inline (Set_Is_Existing_Source_Target_Relation);
      --  Mark a source vertex and a target vertex desctibed by relation Rel as
      --  already related in invocation graph G depending on value Val.

      procedure Set_IGE_Attributes
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id;
         Val    : Invocation_Graph_Edge_Attributes);
      pragma Inline (Set_IGE_Attributes);
      --  Set the attributes of edge IGE_Id of invocation graph G to value Val

      procedure Set_IGV_Attributes
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id;
         Val    : Invocation_Graph_Vertex_Attributes);
      pragma Inline (Set_IGV_Attributes);
      --  Set the attributes of vertex IGV_Id of invocation graph G to value
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

         IR_Rec : Invocation_Relation_Record renames
                    Invocation_Relations.Table (IR_Id);

         IGE_Id : Invocation_Graph_Edge_Id;

      begin
         --  Nothing to do when the source and target are already related by an
         --  edge.

         if Is_Existing_Source_Target_Relation (G, Rel) then
            return;
         end if;

         IGE_Id := Sequence_Next_IGE_Id;

         --  Add the edge to the underlying graph

         DG.Add_Edge
           (G           => G.Graph,
            E           => IGE_Id,
            Source      => Source,
            Destination => Target);

         --  Build and save the attributes of the edge

         Set_IGE_Attributes
           (G      => G,
            IGE_Id => IGE_Id,
            Val    => (Relation => IR_Id));

         --  Mark the source and target as related by the new edge. This
         --  prevents all further attempts to link the same source and target.

         Set_Is_Existing_Source_Target_Relation (G, Rel);

         --  Update the edge statistics

         Increment_Invocation_Graph_Edge_Count (G, IR_Rec.Kind);
      end Add_Edge;

      ----------------
      -- Add_Vertex --
      ----------------

      procedure Add_Vertex
        (G      : Invocation_Graph;
         IC_Id  : Invocation_Construct_Id;
         LGV_Id : Library_Graph_Vertex_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (IC_Id));
         pragma Assert (Present (LGV_Id));

         IC_Rec : Invocation_Construct_Record renames
                    Invocation_Constructs.Table (IC_Id);

         pragma Assert (Present (IC_Rec.Signature));

         IGV_Id : Invocation_Graph_Vertex_Id;

      begin
         --  Nothing to do when the construct already has a vertex

         if Present (Corresponding_Vertex (G, IC_Rec.Signature)) then
            return;
         end if;

         IGV_Id := Sequence_Next_IGV_Id;

         --  Add the vertex to the underlying graph

         DG.Add_Vertex (G.Graph, IGV_Id);

         --  Build and save the attributes of the vertex

         Set_IGV_Attributes
           (G      => G,
            IGV_Id => IGV_Id,
            Val    => (Construct  => IC_Id,
                       Lib_Vertex => LGV_Id));

         --  Associate the construct with its corresponding vertex

         Set_Corresponding_Vertex (G, IC_Rec.Signature, IGV_Id);

         --  Save the vertex for later processing when it denotes a spec or
         --  body elaboration procedure.

         if Is_Elaboration_Root (G, IGV_Id) then
            Save_Elaboration_Root (G, IGV_Id);
         end if;
      end Add_Vertex;

      ---------------
      -- Construct --
      ---------------

      function Construct
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Invocation_Construct_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGV_Id));

         return Get_IGV_Attributes (G, IGV_Id).Construct;
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

         return SV.Get (G.Signature_To_Vertex, IS_Id);
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
         G.Edge_Attributes     := EA.Create (Initial_Edges);
         G.Graph               :=
           DG.Create
             (Initial_Vertices => Initial_Vertices,
              Initial_Edges    => Initial_Edges);
         G.Relations           := ST.Create (Initial_Edges);
         G.Roots               := ER.Create (Initial_Vertices);
         G.Signature_To_Vertex := SV.Create (Initial_Vertices);
         G.Vertex_Attributes   := VA.Create (Initial_Vertices);

         return G;
      end Create;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (G : in out Invocation_Graph) is
      begin
         pragma Assert (Present (G));

         EA.Destroy (G.Edge_Attributes);
         DG.Destroy (G.Graph);
         ST.Destroy (G.Relations);
         ER.Destroy (G.Roots);
         SV.Destroy (G.Signature_To_Vertex);
         VA.Destroy (G.Vertex_Attributes);

         Free (G);
      end Destroy;

      -----------------------------------
      -- Destroy_Invocation_Graph_Edge --
      -----------------------------------

      procedure Destroy_Invocation_Graph_Edge
        (IGE_Id : in out Invocation_Graph_Edge_Id)
      is
         pragma Unreferenced (IGE_Id);
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
        (IGV_Id : in out Invocation_Graph_Vertex_Id)
      is
         pragma Unreferenced (IGV_Id);
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

      ------------------------
      -- Get_IGE_Attributes --
      ------------------------

      function Get_IGE_Attributes
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id)
         return Invocation_Graph_Edge_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGE_Id));

         return EA.Get (G.Edge_Attributes, IGE_Id);
      end Get_IGE_Attributes;

      ------------------------
      -- Get_IGV_Attributes --
      ------------------------

      function Get_IGV_Attributes
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id)
         return Invocation_Graph_Vertex_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGV_Id));

         return VA.Get (G.Vertex_Attributes, IGV_Id);
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
         return ER.Has_Next (ER.Iterator (Iter));
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
         IGV_Id : Invocation_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (IGV_Id));

         IC_Id : constant Invocation_Construct_Id := Construct (G, IGV_Id);

         pragma Assert (Present (IC_Id));

         IC_Rec : Invocation_Construct_Record renames
                    Invocation_Constructs.Table (IC_Id);

      begin
         return
           IC_Rec.Kind = Elaborate_Body_Procedure
             or else
           IC_Rec.Kind = Elaborate_Spec_Procedure;
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

         return ST.Contains (G.Relations, Rel);
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
         IGV_Id : Invocation_Graph_Vertex_Id) return Edges_To_Targets_Iterator
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGV_Id));

         return
           Edges_To_Targets_Iterator
             (DG.Iterate_Outgoing_Edges (G.Graph, IGV_Id));
      end Iterate_Edges_To_Targets;

      -------------------------------
      -- Iterate_Elaboration_Roots --
      -------------------------------

      function Iterate_Elaboration_Roots
        (G : Invocation_Graph) return Elaboration_Root_Iterator
      is
      begin
         pragma Assert (Present (G));

         return Elaboration_Root_Iterator (ER.Iterate (G.Roots));
      end Iterate_Elaboration_Roots;

      ----------
      -- Kind --
      ----------

      function Kind
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Kind
      is
         pragma Assert (Present (G));
         pragma Assert (Present (IGE_Id));

         IR_Id : constant Invocation_Relation_Id := Relation (G, IGE_Id);

         pragma Assert (Present (IR_Id));

         IR_Rec : Invocation_Relation_Record renames
                    Invocation_Relations.Table (IR_Id);

      begin
         return IR_Rec.Kind;
      end Kind;

      ----------------
      -- Lib_Vertex --
      ----------------

      function Lib_Vertex
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGV_Id));

         return Get_IGV_Attributes (G, IGV_Id).Lib_Vertex;
      end Lib_Vertex;

      ----------
      -- Name --
      ----------

      function Name
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id) return Name_Id
      is
         pragma Assert (Present (G));
         pragma Assert (Present (IGV_Id));

         IC_Id : constant Invocation_Construct_Id := Construct (G, IGV_Id);

         pragma Assert (Present (IC_Id));

         IC_Rec : Invocation_Construct_Record renames
                    Invocation_Constructs.Table (IC_Id);

         pragma Assert (Present (IC_Rec.Signature));

         IS_Rec : Invocation_Signature_Record renames
                    Invocation_Signatures.Table (IC_Rec.Signature);

      begin
         return IS_Rec.Name;
      end Name;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out All_Edge_Iterator;
         IGE_Id : out Invocation_Graph_Edge_Id)
      is
      begin
         DG.Next (DG.All_Edge_Iterator (Iter), IGE_Id);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out All_Vertex_Iterator;
         IGV_Id : out Invocation_Graph_Vertex_Id)
      is
      begin
         DG.Next (DG.All_Vertex_Iterator (Iter), IGV_Id);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out Edges_To_Targets_Iterator;
         IGE_Id : out Invocation_Graph_Edge_Id)
      is
      begin
         DG.Next (DG.Outgoing_Edge_Iterator (Iter), IGE_Id);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Elaboration_Root_Iterator;
         Root : out Invocation_Graph_Vertex_Id)
      is
      begin
         ER.Next (ER.Iterator (Iter), Root);
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
         IGV_Id : Invocation_Graph_Vertex_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGV_Id));

         return DG.Number_Of_Outgoing_Edges (G.Graph, IGV_Id);
      end Number_Of_Edges_To_Targets;

      ---------------------------------
      -- Number_Of_Elaboration_Roots --
      ---------------------------------

      function Number_Of_Elaboration_Roots
        (G : Invocation_Graph) return Natural
      is
      begin
         pragma Assert (Present (G));

         return ER.Size (G.Roots);
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
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Relation_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGE_Id));

         return Get_IGE_Attributes (G, IGE_Id).Relation;
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

         ER.Insert (G.Roots, Root);
      end Save_Elaboration_Root;

      ------------------------------
      -- Set_Corresponding_Vertex --
      ------------------------------

      procedure Set_Corresponding_Vertex
        (G      : Invocation_Graph;
         IS_Id  : Invocation_Signature_Id;
         IGV_Id : Invocation_Graph_Vertex_Id)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IS_Id));
         pragma Assert (Present (IGV_Id));

         SV.Put (G.Signature_To_Vertex, IS_Id, IGV_Id);
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
            ST.Insert (G.Relations, Rel);
         else
            ST.Delete (G.Relations, Rel);
         end if;
      end Set_Is_Existing_Source_Target_Relation;

      ------------------------
      -- Set_IGE_Attributes --
      ------------------------

      procedure Set_IGE_Attributes
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id;
         Val    : Invocation_Graph_Edge_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGE_Id));

         EA.Put (G.Edge_Attributes, IGE_Id, Val);
      end Set_IGE_Attributes;

      ------------------------
      -- Set_IGV_Attributes --
      ------------------------

      procedure Set_IGV_Attributes
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id;
         Val    : Invocation_Graph_Vertex_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGV_Id));

         VA.Put (G.Vertex_Attributes, IGV_Id, Val);
      end Set_IGV_Attributes;

      ------------
      -- Target --
      ------------

      function Target
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id) return Invocation_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (IGE_Id));

         return DG.Destination_Vertex (G.Graph, IGE_Id);
      end Target;
   end Invocation_Graphs;

   --------------------
   -- Library_Graphs --
   --------------------

   package body Library_Graphs is

      ---------------
      -- Edge list --
      ---------------

      package EL is new Doubly_Linked_Lists
        (Element_Type    => Library_Graph_Edge_Id,
         "="             => "=",
         Destroy_Element => Destroy_Library_Graph_Edge);

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Add_Body_Before_Spec_Edge
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Edges  : EL.Doubly_Linked_List);
      pragma Inline (Add_Body_Before_Spec_Edge);
      --  Create a new edge in library graph G between vertex LGV_Id and its
      --  corresponding spec or body, where the body is a predecessor and the
      --  spec a successor. Add the edge to list Edges.

      procedure Add_Body_Before_Spec_Edges
        (G     : Library_Graph;
         Edges : EL.Doubly_Linked_List);
      pragma Inline (Add_Body_Before_Spec_Edges);
      --  Create new edges in library graph G for all vertices and their
      --  corresponding specs or bodies, where the body is a predecessor
      --  and the spec is a successor. Add all edges to list Edges.

      function Add_Edge_With_Return
        (G    : Library_Graph;
         Pred : Library_Graph_Vertex_Id;
         Succ : Library_Graph_Vertex_Id;
         Kind : Library_Graph_Edge_Kind) return Library_Graph_Edge_Id;
      pragma Inline (Add_Edge_With_Return);
      --  Create a new edge in library graph G with source vertex Pred and
      --  destination vertex Succ, and return its handle. Kind denotes the
      --  nature of the edge. If Pred and Succ are already related, no edge
      --  is created and No_Library_Graph_Edge is returned.

      procedure Decrement_Library_Graph_Edge_Count
        (G    : Library_Graph;
         Kind : Library_Graph_Edge_Kind);
      pragma Inline (Decrement_Library_Graph_Edge_Count);
      --  Decrement the number of edges of kind King in library graph G by one

      procedure Delete_Body_Before_Spec_Edges
        (G     : Library_Graph;
         Edges : EL.Doubly_Linked_List);
      pragma Inline (Delete_Body_Before_Spec_Edges);
      --  Delete all edges in list Edges from library graph G, that link spec
      --  and bodies, where the body acts as the predecessor and the spec as a
      --  successor.

      procedure Delete_Edge
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id);
      pragma Inline (Delete_Edge);
      --  Delete edge LGE_Id from library graph G

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Library_Graph_Attributes, Library_Graph);

      function Get_Component_Attributes
        (G    : Library_Graph;
         Comp : Component_Id) return Component_Attributes;
      pragma Inline (Get_Component_Attributes);
      --  Obtain the attributes of component Comp of library graph G

      function Get_LGE_Attributes
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id)
         return Library_Graph_Edge_Attributes;
      pragma Inline (Get_LGE_Attributes);
      --  Obtain the attributes of edge LGE_Id of library graph G

      function Get_LGV_Attributes
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id)
         return Library_Graph_Vertex_Attributes;
      pragma Inline (Get_LGV_Attributes);
      --  Obtain the attributes of vertex LGE_Id of library graph G

      function Has_Elaborate_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean;
      pragma Inline (Has_Elaborate_Body);
      --  Determine whether vertex LGV_Id of library graph G is subject to
      --  pragma Elaborate_Body.

      procedure Increment_Library_Graph_Edge_Count
        (G    : Library_Graph;
         Kind : Library_Graph_Edge_Kind);
      pragma Inline (Increment_Library_Graph_Edge_Count);
      --  Increment the number of edges of king Kind in library graph G by one

      procedure Increment_Pending_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id);
      pragma Inline (Increment_Pending_Predecessors);
      --  Increment the number of pending precedessors component Comp of
      --  library graph G must wait on before it can be elaborated by one.

      procedure Increment_Pending_Predecessors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id);
      pragma Inline (Increment_Pending_Predecessors);
      --  Increment the number of pending precedessors vertex LGV_Id of library
      --  graph G must wait on before it can be elaborated by one.

      procedure Initialize_Components (G : Library_Graph);
      pragma Inline (Initialize_Components);
      --  Initialize on the initial call or re-initialize on subsequent calls
      --  all components of library graph G.

      function Is_Elaborable_Vertex
        (G            : Library_Graph;
         LGV_Id       : Library_Graph_Vertex_Id;
         Predecessors : Natural) return Boolean;
      pragma Inline (Is_Elaborable_Vertex);
      --  Determine whether vertex LGV_Id of library graph G can be elaborated
      --  given that it meets number of predecessors Predecessors.

      function Is_Existing_Predecessor_Successor_Relation
        (G   : Library_Graph;
         Rel : Predecessor_Successor_Relation) return Boolean;
      pragma Inline (Is_Existing_Predecessor_Successor_Relation);
      --  Determine whether a predecessor vertex and a successor vertex
      --  desctibed by relation Rel are already related in library graph G.

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

      procedure Set_Is_Existing_Predecessor_Successor_Relation
        (G   : Library_Graph;
         Rel : Predecessor_Successor_Relation;
         Val : Boolean := True);
      pragma Inline (Set_Is_Existing_Predecessor_Successor_Relation);
      --  Mark a a predecessor vertex and a successor vertex desctibed by
      --  relation Rel as already related depending on value Val.

      procedure Set_LGE_Attributes
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id;
         Val    : Library_Graph_Edge_Attributes);
      pragma Inline (Set_LGE_Attributes);
      --  Set the attributes of edge LGE_Id of library graph G to value Val

      procedure Set_LGV_Attributes
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Val    : Library_Graph_Vertex_Attributes);
      pragma Inline (Set_LGV_Attributes);
      --  Set the attributes of vertex LGV_Id of library graph G to value Val

      procedure Update_Pending_Predecessors_Of_Components (G : Library_Graph);
      pragma Inline (Update_Pending_Predecessors_Of_Components);
      --  Update the number of pending predecessors all components of library
      --  graph G must wait on before they can be elaborated.

      procedure Update_Pending_Predecessors_Of_Components
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id);
      pragma Inline (Update_Pending_Predecessors_Of_Components);
      --  Update the number of pending predecessors the component of edge
      --  LGE_Is's successor vertex of library graph G must wait on before
      --  it can be elaborated.

      -------------------------------
      -- Add_Body_Before_Spec_Edge --
      -------------------------------

      procedure Add_Body_Before_Spec_Edge
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Edges  : EL.Doubly_Linked_List)
      is
         LGE_Id : Library_Graph_Edge_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));
         pragma Assert (EL.Present (Edges));

         --  A vertex requires a special Body_Before_Spec edge to its
         --  Corresponging_Item when it either denotes a
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

         LGE_Id := No_Library_Graph_Edge;

         --  A body that completes a previous spec

         if Is_Body_With_Spec (G, LGV_Id) then
            LGE_Id :=
              Add_Edge_With_Return
                (G    => G,
                 Pred => LGV_Id,                           --  body
                 Succ => Corresponding_Item (G, LGV_Id),   --  spec
                 Kind => Body_Before_Spec_Edge);

         --  A spec with a completing body

         elsif Is_Spec_With_Body (G, LGV_Id) then
            LGE_Id :=
              Add_Edge_With_Return
                (G    => G,
                 Pred => Corresponding_Item (G, LGV_Id),   --  body
                 Succ => LGV_Id,                           --  spec
                 Kind => Body_Before_Spec_Edge);
         end if;

         if Present (LGE_Id) then
            EL.Append (Edges, LGE_Id);
         end if;
      end Add_Body_Before_Spec_Edge;

      --------------------------------
      -- Add_Body_Before_Spec_Edges --
      --------------------------------

      procedure Add_Body_Before_Spec_Edges
        (G     : Library_Graph;
         Edges : EL.Doubly_Linked_List)
      is
         Iter   : Elaborable_Units_Iterator;
         LGV_Id : Library_Graph_Vertex_Id;
         U_Id   : Unit_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (EL.Present (Edges));

         Iter := Iterate_Elaborable_Units;
         while Has_Next (Iter) loop
            Next (Iter, U_Id);

            LGV_Id := Corresponding_Vertex (G, U_Id);
            pragma Assert (Present (LGV_Id));

            Add_Body_Before_Spec_Edge (G, LGV_Id, Edges);
         end loop;
      end Add_Body_Before_Spec_Edges;

      --------------
      -- Add_Edge --
      --------------

      procedure Add_Edge
        (G    : Library_Graph;
         Pred : Library_Graph_Vertex_Id;
         Succ : Library_Graph_Vertex_Id;
         Kind : Library_Graph_Edge_Kind)
      is
         LGE_Id : Library_Graph_Edge_Id;
         pragma Unreferenced (LGE_Id);

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));
         pragma Assert (Kind /= No_Edge);

         LGE_Id :=
           Add_Edge_With_Return
             (G    => G,
              Pred => Pred,
              Succ => Succ,
              Kind => Kind);
      end Add_Edge;

      --------------------------
      -- Add_Edge_With_Return --
      --------------------------

      function Add_Edge_With_Return
        (G    : Library_Graph;
         Pred : Library_Graph_Vertex_Id;
         Succ : Library_Graph_Vertex_Id;
         Kind : Library_Graph_Edge_Kind) return Library_Graph_Edge_Id
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));
         pragma Assert (Kind /= No_Edge);

         Rel : constant Predecessor_Successor_Relation :=
                 (Predecessor => Pred,
                  Successor   => Succ);

         LGE_Id : Library_Graph_Edge_Id;

      begin
         --  Nothing to do when the predecessor and successor are already
         --  related by an edge.

         if Is_Existing_Predecessor_Successor_Relation (G, Rel) then
            return No_Library_Graph_Edge;
         end if;

         LGE_Id := Sequence_Next_LGE_Id;

         --  Add the edge to the underlying graph. Note that the predecessor
         --  is the source of the edge because it will later need to notify
         --  all its successors that it has been elaborated.

         DG.Add_Edge
           (G           => G.Graph,
            E           => LGE_Id,
            Source      => Pred,
            Destination => Succ);

         --  Construct and save the attributes of the edge

         Set_LGE_Attributes
           (G      => G,
            LGE_Id => LGE_Id,
            Val    => (Kind => Kind));

         --  Mark the predecessor and successor as related by the new edge.
         --  This prevents all further attempts to link the same predecessor
         --  and successor.

         Set_Is_Existing_Predecessor_Successor_Relation (G, Rel);

         --  Update the number of pending predecessors the successor must wait
         --  on before it is elaborated.

         Increment_Pending_Predecessors (G, Succ);

         --  Update the edge statistics

         Increment_Library_Graph_Edge_Count (G, Kind);

         return LGE_Id;
      end Add_Edge_With_Return;

      ----------------
      -- Add_Vertex --
      ----------------

      procedure Add_Vertex
        (G    : Library_Graph;
         U_Id : Unit_Id)
      is
         LGV_Id : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (U_Id));

         --  Nothing to do when the unit already has a vertex

         if Present (Corresponding_Vertex (G, U_Id)) then
            return;
         end if;

         LGV_Id := Sequence_Next_LGV_Id;

         --  Add the vertex to the underlying graph

         DG.Add_Vertex (G.Graph, LGV_Id);

         --  Construct and save the attributes of the vertex

         Set_LGV_Attributes
           (G      => G,
            LGV_Id => LGV_Id,
            Val    => (Corresponding_Item   => No_Library_Graph_Vertex,
                       In_Elaboration_Order => False,
                       Pending_Predecessors => 0,
                       Unit                 => U_Id));

         --  Associate the unit with its corresponding vertex

         Set_Corresponding_Vertex (G, U_Id, LGV_Id);
      end Add_Vertex;

      ---------------
      -- Component --
      ---------------

      function Component
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Component_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         return DG.Component (G.Graph, LGV_Id);
      end Component;

      ------------------------
      -- Corresponding_Item --
      ------------------------

      function Corresponding_Item
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         return Get_LGV_Attributes (G, LGV_Id).Corresponding_Item;
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

         return UV.Get (G.Unit_To_Vertex, U_Id);
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
         G.Component_Attributes := CA.Create (Initial_Vertices);
         G.Edge_Attributes      := EA.Create (Initial_Edges);
         G.Graph                :=
           DG.Create
             (Initial_Vertices => Initial_Vertices,
              Initial_Edges    => Initial_Edges);
         G.Relations            := PS.Create (Initial_Edges);
         G.Unit_To_Vertex       := UV.Create (Initial_Vertices);
         G.Vertex_Attributes    := VA.Create (Initial_Vertices);

         return G;
      end Create;

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
         Comp : Component_Id)
      is
         Attrs : Component_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Attrs := Get_Component_Attributes (G, Comp);
         Attrs.Pending_Predecessors := Attrs.Pending_Predecessors - 1;
         Set_Component_Attributes (G, Comp, Attrs);
      end Decrement_Pending_Predecessors;

      ------------------------------------
      -- Decrement_Pending_Predecessors --
      ------------------------------------

      procedure Decrement_Pending_Predecessors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id)
      is
         Attrs : Library_Graph_Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         Attrs := Get_LGV_Attributes (G, LGV_Id);
         Attrs.Pending_Predecessors := Attrs.Pending_Predecessors - 1;
         Set_LGV_Attributes (G, LGV_Id, Attrs);
      end Decrement_Pending_Predecessors;

      -----------------------------------
      -- Delete_Body_Before_Spec_Edges --
      -----------------------------------

      procedure Delete_Body_Before_Spec_Edges
        (G     : Library_Graph;
         Edges : EL.Doubly_Linked_List)
      is
         Iter   : EL.Iterator;
         LGE_Id : Library_Graph_Edge_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (EL.Present (Edges));

         Iter := EL.Iterate (Edges);
         while EL.Has_Next (Iter) loop
            EL.Next (Iter, LGE_Id);
            pragma Assert (Present (LGE_Id));
            pragma Assert (Kind (G, LGE_Id) = Body_Before_Spec_Edge);

            Delete_Edge (G, LGE_Id);
         end loop;
      end Delete_Body_Before_Spec_Edges;

      -----------------
      -- Delete_Edge --
      -----------------

      procedure Delete_Edge
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGE_Id));

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, LGE_Id);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, LGE_Id);

         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));

         Rel : constant Predecessor_Successor_Relation :=
                 (Predecessor => Pred,
                  Successor   => Succ);

      begin
         --  Update the edge statistics

         Decrement_Library_Graph_Edge_Count (G, Kind (G, LGE_Id));

         --  Update the number of pending predecessors the successor must wait
         --  on before it is elaborated.

         Decrement_Pending_Predecessors (G, Succ);

         --  Delete the link between the predecessor and successor. This allows
         --  for further attempts to link the same predecessor and successor.

         PS.Delete (G.Relations, Rel);

         --  Delete the attributes of the edge

         EA.Delete (G.Edge_Attributes, LGE_Id);

         --  Delete the edge from the underlying graph

         DG.Delete_Edge (G.Graph, LGE_Id);
      end Delete_Edge;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (G : in out Library_Graph) is
      begin
         pragma Assert (Present (G));

         CA.Destroy (G.Component_Attributes);
         EA.Destroy (G.Edge_Attributes);
         DG.Destroy (G.Graph);
         PS.Destroy (G.Relations);
         UV.Destroy (G.Unit_To_Vertex);
         VA.Destroy (G.Vertex_Attributes);

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

      --------------------------------
      -- Destroy_Library_Graph_Edge --
      --------------------------------

      procedure Destroy_Library_Graph_Edge
        (LGE_Id : in out Library_Graph_Edge_Id)
      is
         pragma Unreferenced (LGE_Id);
      begin
         null;
      end Destroy_Library_Graph_Edge;

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
        (LGV_Id : in out Library_Graph_Vertex_Id)
      is
         pragma Unreferenced (LGV_Id);
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

      ---------------------
      -- Find_Components --
      ---------------------

      procedure Find_Components (G : Library_Graph) is
         Edges : EL.Doubly_Linked_List;

      begin
         pragma Assert (Present (G));

         --  Initialize or reinitialize the components of the graph

         Initialize_Components (G);

         --  Create a set of special edges that link a predecessor body with a
         --  successor spec. This is an illegal dependency, however using such
         --  edges eliminates the need to create yet another graph, where both
         --  spec and body are collapsed into a single vertex.

         Edges := EL.Create;
         Add_Body_Before_Spec_Edges (G, Edges);

         DG.Find_Components (G.Graph);

         --  Remove the special edges that link a predecessor body with a
         --  successor spec because they cause unresolvable circularities.

         Delete_Body_Before_Spec_Edges (G, Edges);
         EL.Destroy (Edges);

         --  Update the number of predecessors various components must wait on
         --  before they can be elaborated.

         Update_Pending_Predecessors_Of_Components (G);
      end Find_Components;

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

         return CA.Get (G.Component_Attributes, Comp);
      end Get_Component_Attributes;

      ------------------------
      -- Get_LGE_Attributes --
      ------------------------

      function Get_LGE_Attributes
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id)
         return Library_Graph_Edge_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGE_Id));

         return EA.Get (G.Edge_Attributes, LGE_Id);
      end Get_LGE_Attributes;

      ------------------------
      -- Get_LGV_Attributes --
      ------------------------

      function Get_LGV_Attributes
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id)
         return Library_Graph_Vertex_Attributes
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         return VA.Get (G.Vertex_Attributes, LGV_Id);
      end Get_LGV_Attributes;

      ------------------------
      -- Has_Elaborate_Body --
      ------------------------

      function Has_Elaborate_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Elaborate_Body;
      end Has_Elaborate_Body;

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

      function Has_Next (Iter : Edges_To_Successors_Iterator) return Boolean is
      begin
         return DG.Has_Next (DG.Outgoing_Edge_Iterator (Iter));
      end Has_Next;

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

      --------------------------
      -- In_Elaboration_Order --
      --------------------------

      function In_Elaboration_Order
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         return Get_LGV_Attributes (G, LGV_Id).In_Elaboration_Order;
      end In_Elaboration_Order;

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
         Comp : Component_Id)
      is
         Attrs : Component_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Attrs := Get_Component_Attributes (G, Comp);
         Attrs.Pending_Predecessors := Attrs.Pending_Predecessors + 1;
         Set_Component_Attributes (G, Comp, Attrs);
      end Increment_Pending_Predecessors;

      ------------------------------------
      -- Increment_Pending_Predecessors --
      ------------------------------------

      procedure Increment_Pending_Predecessors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id)
      is
         Attrs : Library_Graph_Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         Attrs := Get_LGV_Attributes (G, LGV_Id);
         Attrs.Pending_Predecessors := Attrs.Pending_Predecessors + 1;
         Set_LGV_Attributes (G, LGV_Id, Attrs);
      end Increment_Pending_Predecessors;

      ---------------------------
      -- Initialize_Components --
      ---------------------------

      procedure Initialize_Components (G : Library_Graph) is
      begin
         pragma Assert (Present (G));

         --  The graph already contains a set of components. Reinitialize
         --  them in order to accomodate the new set of components about to
         --  be computed.

         if Number_Of_Components (G) > 0 then
            CA.Destroy (G.Component_Attributes);
            G.Component_Attributes := CA.Create (Number_Of_Vertices (G));
         end if;
      end Initialize_Components;

      -------------
      -- Is_Body --
      -------------

      function Is_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Utype = Is_Body or else U_Rec.Utype = Is_Body_Only;
      end Is_Body;

      -----------------------------------------
      -- Is_Body_Of_Spec_With_Elaborate_Body --
      -----------------------------------------

      function Is_Body_Of_Spec_With_Elaborate_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         Spec_LGV_Id : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         if Is_Body_With_Spec (G, LGV_Id) then
            Spec_LGV_Id := Proper_Spec (G, LGV_Id);
            pragma Assert (Present (Spec_LGV_Id));

            return Is_Spec_With_Elaborate_Body (G, Spec_LGV_Id);
         end if;

         return False;
      end Is_Body_Of_Spec_With_Elaborate_Body;

      -----------------------
      -- Is_Body_With_Spec --
      -----------------------

      function Is_Body_With_Spec
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Utype = Is_Body;
      end Is_Body_With_Spec;

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

         --  A component can be elaborated when
         --
         --    * The component is no longer wanting on any of its predecessors
         --      to be elaborated.

         return Pending_Predecessors (G, Comp) = 0;
      end Is_Elaborable_Component;

      --------------------------
      -- Is_Elaborable_Vertex --
      --------------------------

      function Is_Elaborable_Vertex
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         Check_LGV_Id : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         Check_LGV_Id := LGV_Id;

         --  A spec-body pair where the spec carries pragma Elaborate_Body must
         --  be treated as one vertex for elaboration purposes. Use the spec as
         --  the point of reference for the composite vertex.

         if Is_Body_Of_Spec_With_Elaborate_Body (G, Check_LGV_Id) then
            Check_LGV_Id := Proper_Spec (G, Check_LGV_Id);
            pragma Assert (Present (Check_LGV_Id));
         end if;

         return
           Is_Elaborable_Vertex
             (G            => G,
              LGV_Id       => Check_LGV_Id,
              Predecessors => 0);
      end Is_Elaborable_Vertex;

      --------------------------
      -- Is_Elaborable_Vertex --
      --------------------------

      function Is_Elaborable_Vertex
        (G            : Library_Graph;
         LGV_Id       : Library_Graph_Vertex_Id;
         Predecessors : Natural) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         Comp : constant Component_Id := Component (G, LGV_Id);

         pragma Assert (Present (Comp));

         Body_LGV_Id : Library_Graph_Vertex_Id;

      begin
         --  The vertex must not be re-elaborated once it has been elaborated

         if In_Elaboration_Order (G, LGV_Id) then
            return False;

         --  The vertex must not be waiting on more precedessors than requested
         --  to be elaborated.

         elsif Pending_Predecessors (G, LGV_Id) /= Predecessors then
            return False;

         --  The component where the vertex resides must not be waiting on any
         --  of its precedessors to be elaborated.

         elsif not Is_Elaborable_Component (G, Comp) then
            return False;

         --  The vertex denotes a spec with a completing body, and is subject
         --  to pragma Elaborate_Body. The body must be elaborable for the
         --  vertex to be elaborated. Account for the sole predecessor of the
         --  body which is the vertex itself.

         elsif Is_Spec_With_Elaborate_Body (G, LGV_Id) then
            Body_LGV_Id := Proper_Body (G, LGV_Id);
            pragma Assert (Present (Body_LGV_Id));

            return
              Is_Elaborable_Vertex
                (G            => G,
                 LGV_Id       => Body_LGV_Id,
                 Predecessors => 1);
         end if;

         --  At this point it is known that the vertex can be elaborated

         return True;
      end Is_Elaborable_Vertex;

      ------------------------------------------------
      -- Is_Existing_Predecessor_Successor_Relation --
      ------------------------------------------------

      function Is_Existing_Predecessor_Successor_Relation
        (G   : Library_Graph;
         Rel : Predecessor_Successor_Relation) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Rel.Predecessor));
         pragma Assert (Present (Rel.Successor));

         return PS.Contains (G.Relations, Rel);
      end Is_Existing_Predecessor_Successor_Relation;

      ----------------------
      -- Is_Internal_Unit --
      ----------------------

      function Is_Internal_Unit
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Internal;
      end Is_Internal_Unit;

      ------------------------
      -- Is_Predefined_Unit --
      ------------------------

      function Is_Predefined_Unit
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Predefined;
      end Is_Predefined_Unit;

      ---------------------------
      -- Is_Preelaborated_Unit --
      ---------------------------

      function Is_Preelaborated_Unit
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Preelab or else U_Rec.Pure;
      end Is_Preelaborated_Unit;

      -------------
      -- Is_Spec --
      -------------

      function Is_Spec
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Utype = Is_Spec or else U_Rec.Utype = Is_Spec_Only;
      end Is_Spec;

      -----------------------
      -- Is_Spec_With_Body --
      -----------------------

      function Is_Spec_With_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         return U_Rec.Utype = Is_Spec;
      end Is_Spec_With_Body;

      ---------------------------------
      -- Is_Spec_With_Elaborate_Body --
      ---------------------------------

      function Is_Spec_With_Elaborate_Body
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         return
           Is_Spec_With_Body (G, LGV_Id)
             and then Has_Elaborate_Body (G, LGV_Id);
      end Is_Spec_With_Elaborate_Body;

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

      ---------------------------------
      -- Iterate_Edges_To_Successors --
      ---------------------------------

      function Iterate_Edges_To_Successors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id)
         return Edges_To_Successors_Iterator
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         return
           Edges_To_Successors_Iterator
             (DG.Iterate_Outgoing_Edges (G.Graph, LGV_Id));
      end Iterate_Edges_To_Successors;

      ----------
      -- Kind --
      ----------

      function Kind
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Edge_Kind
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGE_Id));

         return Get_LGE_Attributes (G, LGE_Id).Kind;
      end Kind;

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
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGE_Id));

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, LGE_Id);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, LGE_Id);

         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));

         Pred_Comp : constant Component_Id := Component (G, Pred);
         Succ_Comp : constant Component_Id := Component (G, Succ);

         pragma Assert (Present (Pred_Comp));
         pragma Assert (Present (Succ_Comp));

      begin
         return Pred_Comp = Succ_Comp;
      end Links_Vertices_In_Same_Component;

      ----------
      -- Name --
      ----------

      function Name
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Unit_Name_Type
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

      begin
         return Name (U_Id);
      end Name;

      -----------------------
      -- Needs_Elaboration --
      -----------------------

      function Needs_Elaboration
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Boolean
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         U_Id : constant Unit_Id := Unit (G, LGV_Id);

         pragma Assert (Present (U_Id));

      begin
         return Needs_Elaboration (U_Id);
      end Needs_Elaboration;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out All_Edge_Iterator;
         LGE_Id : out Library_Graph_Edge_Id)
      is
      begin
         DG.Next (DG.All_Edge_Iterator (Iter), LGE_Id);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out All_Vertex_Iterator;
         LGV_Id : out Library_Graph_Vertex_Id)
      is
      begin
         DG.Next (DG.All_Vertex_Iterator (Iter), LGV_Id);
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
        (Iter   : in out Edges_To_Successors_Iterator;
         LGE_Id : out Library_Graph_Edge_Id)
      is
      begin
         DG.Next (DG.Outgoing_Edge_Iterator (Iter), LGE_Id);
      end Next;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter   : in out Component_Vertex_Iterator;
         LGV_Id : out Library_Graph_Vertex_Id)
      is
      begin
         DG.Next (DG.Component_Vertex_Iterator (Iter), LGV_Id);
      end Next;

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
         LGV_Id : Library_Graph_Vertex_Id) return Natural
      is
      begin
         pragma Assert (Present (G));

         return DG.Number_Of_Outgoing_Edges (G.Graph, LGV_Id);
      end Number_Of_Edges_To_Successors;

      ------------------------
      -- Number_Of_Vertices --
      ------------------------

      function Number_Of_Vertices (G : Library_Graph) return Natural is
      begin
         pragma Assert (Present (G));

         return DG.Number_Of_Vertices (G.Graph);
      end Number_Of_Vertices;

      --------------------------
      -- Pending_Predecessors --
      --------------------------

      function Pending_Predecessors
        (G    : Library_Graph;
         Comp : Component_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         return Get_Component_Attributes (G, Comp).Pending_Predecessors;
      end Pending_Predecessors;

      --------------------------
      -- Pending_Predecessors --
      --------------------------

      function Pending_Predecessors
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Natural
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         return Get_LGV_Attributes (G, LGV_Id).Pending_Predecessors;
      end Pending_Predecessors;

      -----------------
      -- Predecessor --
      -----------------

      function Predecessor
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGE_Id));

         return DG.Source_Vertex (G.Graph, LGE_Id);
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
         LGV_Id : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         --  When the vertex denotes a spec with a completing body, return the
         --  body.

         if Is_Spec_With_Body (G, LGV_Id) then
            return Corresponding_Item (G, LGV_Id);

         --  Otherwise the vertex must be a body

         else
            pragma Assert (Is_Body (G, LGV_Id));
            return LGV_Id;
         end if;
      end Proper_Body;

      -----------------
      -- Proper_Spec --
      -----------------

      function Proper_Spec
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         --  When the vertex denotes a body that completes a spec, return the
         --  spec.

         if Is_Body_With_Spec (G, LGV_Id) then
            return Corresponding_Item (G, LGV_Id);

         --  Otherwise the vertex must denote a spec

         else
            pragma Assert (Is_Spec (G, LGV_Id));
            return LGV_Id;
         end if;
      end Proper_Spec;

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

         CA.Put (G.Component_Attributes, Comp, Val);
      end Set_Component_Attributes;

      ----------------------------
      -- Set_Corresponding_Item --
      ----------------------------

      procedure Set_Corresponding_Item
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Val    : Library_Graph_Vertex_Id)
      is
         Attrs : Library_Graph_Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         Attrs := Get_LGV_Attributes (G, LGV_Id);
         Attrs.Corresponding_Item := Val;
         Set_LGV_Attributes (G, LGV_Id, Attrs);
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

         UV.Put (G.Unit_To_Vertex, U_Id, Val);
      end Set_Corresponding_Vertex;

      ------------------------------
      -- Set_In_Elaboration_Order --
      ------------------------------

      procedure Set_In_Elaboration_Order
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Val    : Boolean := True)
      is
         Attrs : Library_Graph_Vertex_Attributes;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         Attrs := Get_LGV_Attributes (G, LGV_Id);
         Attrs.In_Elaboration_Order := Val;
         Set_LGV_Attributes (G, LGV_Id, Attrs);
      end Set_In_Elaboration_Order;

      ----------------------------------------------------
      -- Set_Is_Existing_Predecessor_Successor_Relation --
      ----------------------------------------------------

      procedure Set_Is_Existing_Predecessor_Successor_Relation
        (G   : Library_Graph;
         Rel : Predecessor_Successor_Relation;
         Val : Boolean := True)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Rel.Predecessor));
         pragma Assert (Present (Rel.Successor));

         if Val then
            PS.Insert (G.Relations, Rel);
         else
            PS.Delete (G.Relations, Rel);
         end if;
      end Set_Is_Existing_Predecessor_Successor_Relation;

      ------------------------
      -- Set_LGE_Attributes --
      ------------------------

      procedure Set_LGE_Attributes
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id;
         Val    : Library_Graph_Edge_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGE_Id));

         EA.Put (G.Edge_Attributes, LGE_Id, Val);
      end Set_LGE_Attributes;

      ------------------------
      -- Set_LGV_Attributes --
      ------------------------

      procedure Set_LGV_Attributes
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id;
         Val    : Library_Graph_Vertex_Attributes)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         VA.Put (G.Vertex_Attributes, LGV_Id, Val);
      end Set_LGV_Attributes;

      ---------------
      -- Successor --
      ---------------

      function Successor
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id) return Library_Graph_Vertex_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGE_Id));

         return DG.Destination_Vertex (G.Graph, LGE_Id);
      end Successor;

      ----------
      -- Unit --
      ----------

      function Unit
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id) return Unit_Id
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (LGV_Id));

         return Get_LGV_Attributes (G, LGV_Id).Unit;
      end Unit;

      -----------------------------------------------
      -- Update_Pending_Predecessors_Of_Components --
      -----------------------------------------------

      procedure Update_Pending_Predecessors_Of_Components
        (G : Library_Graph)
      is
         Iter   : All_Edge_Iterator;
         LGE_Id : Library_Graph_Edge_Id;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, LGE_Id);
            pragma Assert (Present (LGE_Id));

            Update_Pending_Predecessors_Of_Components (G, LGE_Id);
         end loop;
      end Update_Pending_Predecessors_Of_Components;

      -----------------------------------------------
      -- Update_Pending_Predecessors_Of_Components --
      -----------------------------------------------

      procedure Update_Pending_Predecessors_Of_Components
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (LGE_Id));

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, LGE_Id);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, LGE_Id);

         pragma Assert (Present (Pred));
         pragma Assert (Present (Succ));

         Pred_Comp : constant Component_Id := Component (G, Pred);
         Succ_Comp : constant Component_Id := Component (G, Succ);

         pragma Assert (Present (Pred_Comp));
         pragma Assert (Present (Succ_Comp));

      begin
         --  The edge links a successor and a predecessor coming from two
         --  different SCCs. This indicates that the SCC of the successor
         --  must wait on another predecessor until it can be elaborated.

         if Pred_Comp /= Succ_Comp then
            Increment_Pending_Predecessors (G, Succ_Comp);
         end if;
      end Update_Pending_Predecessors_Of_Components;
   end Library_Graphs;

   -------------
   -- Present --
   -------------

   function Present (IGE_Id : Invocation_Graph_Edge_Id) return Boolean is
   begin
      return IGE_Id /= No_Invocation_Graph_Edge;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (IGV_Id : Invocation_Graph_Vertex_Id) return Boolean is
   begin
      return IGV_Id /= No_Invocation_Graph_Vertex;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (LGE_Id : Library_Graph_Edge_Id) return Boolean is
   begin
      return LGE_Id /= No_Library_Graph_Edge;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (LGV_Id : Library_Graph_Vertex_Id) return Boolean is
   begin
      return LGV_Id /= No_Library_Graph_Vertex;
   end Present;

   --------------------------
   -- Sequence_Next_IGE_Id --
   --------------------------

   IGE_Sequencer : Invocation_Graph_Edge_Id := First_Invocation_Graph_Edge;
   --  The counter for invocation graph edges. Do not directly manipulate its
   --  value.

   function Sequence_Next_IGE_Id return Invocation_Graph_Edge_Id is
      IGE_Id : constant Invocation_Graph_Edge_Id := IGE_Sequencer;

   begin
      IGE_Sequencer := IGE_Sequencer + 1;
      return IGE_Id;
   end Sequence_Next_IGE_Id;

   --------------------------
   -- Sequence_Next_IGV_Id --
   --------------------------

   IGV_Sequencer : Invocation_Graph_Vertex_Id := First_Invocation_Graph_Vertex;
   --  The counter for invocation graph vertices. Do not directly manipulate
   --  its value.

   --------------------------
   -- Sequence_Next_IGV_Id --
   --------------------------

   function Sequence_Next_IGV_Id return Invocation_Graph_Vertex_Id is
      IGV_Id : constant Invocation_Graph_Vertex_Id := IGV_Sequencer;

   begin
      IGV_Sequencer := IGV_Sequencer + 1;
      return IGV_Id;
   end Sequence_Next_IGV_Id;

   --------------------------
   -- Sequence_Next_LGE_Id --
   --------------------------

   LGE_Sequencer : Library_Graph_Edge_Id := First_Library_Graph_Edge;
   --  The counter for library graph edges. Do not directly manipulate its
   --  value.

   function Sequence_Next_LGE_Id return Library_Graph_Edge_Id is
      LGE_Id : constant Library_Graph_Edge_Id := LGE_Sequencer;

   begin
      LGE_Sequencer := LGE_Sequencer + 1;
      return LGE_Id;
   end Sequence_Next_LGE_Id;

   --------------------------
   -- Sequence_Next_LGV_Id --
   --------------------------

   LGV_Sequencer : Library_Graph_Vertex_Id := First_Library_Graph_Vertex;
   --  The counter for library graph vertices. Do not directly manipulate its
   --  value.

   function Sequence_Next_LGV_Id return Library_Graph_Vertex_Id is
      LGV_Id : constant Library_Graph_Vertex_Id := LGV_Sequencer;

   begin
      LGV_Sequencer := LGV_Sequencer + 1;
      return LGV_Id;
   end Sequence_Next_LGV_Id;

end Bindo.Graphs;
