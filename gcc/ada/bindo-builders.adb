------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        B I N D O . B U I L D E R S                       --
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

with Bindo.Units; use Bindo.Units;

package body Bindo.Builders is

   -------------------------------
   -- Invocation_Graph_Builders --
   -------------------------------

   package body Invocation_Graph_Builders is

      -----------------
      -- Global data --
      -----------------

      Inv_Graph : Invocation_Graph := Invocation_Graphs.Nil;
      Lib_Graph : Library_Graph    := Library_Graphs.Nil;

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Create_Edge (IR_Id : Invocation_Relation_Id);
      pragma Inline (Create_Edge);
      --  Create a new edge for invocation relation IR_Id in invocation graph
      --  Inv_Graph.

      procedure Create_Edges (U_Id : Unit_Id);
      pragma Inline (Create_Edges);
      --  Create new edges for all invocation relations of unit U_Id

      procedure Create_Vertex
        (IC_Id  : Invocation_Construct_Id;
         LGV_Id : Library_Graph_Vertex_Id);
      pragma Inline (Create_Vertex);
      --  Create a new vertex for invocation construct IC_Id in invocation
      --  graph Inv_Graph. The vertex is linked to vertex LGV_Id of library
      --  graph Lib_Graph.

      procedure Create_Vertices (U_Id : Unit_Id);
      pragma Inline (Create_Vertices);
      --  Create new vertices for all invocation constructs of unit U_Id in
      --  invocation graph Inv_Graph.

      ----------------------------
      -- Build_Invocation_Graph --
      ----------------------------

      function Build_Invocation_Graph
        (Lib_G : Library_Graph) return Invocation_Graph
      is
      begin
         pragma Assert (Present (Lib_G));

         --  Prepare the global data

         Inv_Graph :=
           Create (Initial_Vertices => Number_Of_Elaborable_Units,
                   Initial_Edges    => Number_Of_Elaborable_Units);
         Lib_Graph := Lib_G;

         For_Each_Elaborable_Unit (Create_Vertices'Access);
         For_Each_Elaborable_Unit (Create_Edges'Access);

         return Inv_Graph;
      end Build_Invocation_Graph;

      -----------------
      -- Create_Edge --
      -----------------

      procedure Create_Edge (IR_Id : Invocation_Relation_Id) is
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (IR_Id));

         IR_Rec : Invocation_Relation_Record renames
                    Invocation_Relations.Table (IR_Id);

         pragma Assert (Present (IR_Rec.Invoker));
         pragma Assert (Present (IR_Rec.Target));

         Invoker : Invocation_Graph_Vertex_Id;
         Target  : Invocation_Graph_Vertex_Id;

      begin
         --  Nothing to do when the target denotes an invocation construct that
         --  resides in a unit which will never be elaborated.

         if not Needs_Elaboration (IR_Rec.Target) then
            return;
         end if;

         Invoker := Corresponding_Vertex (Inv_Graph, IR_Rec.Invoker);
         Target  := Corresponding_Vertex (Inv_Graph, IR_Rec.Target);

         pragma Assert (Present (Invoker));
         pragma Assert (Present (Target));

         Add_Edge
           (G      => Inv_Graph,
            Source => Invoker,
            Target => Target,
            IR_Id  => IR_Id);
      end Create_Edge;

      ------------------
      -- Create_Edges --
      ------------------

      procedure Create_Edges (U_Id : Unit_Id) is
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         for IR_Id in U_Rec.First_Invocation_Relation ..
                      U_Rec.Last_Invocation_Relation
         loop
            Create_Edge (IR_Id);
         end loop;
      end Create_Edges;

      -------------------
      -- Create_Vertex --
      -------------------

      procedure Create_Vertex
        (IC_Id  : Invocation_Construct_Id;
         LGV_Id : Library_Graph_Vertex_Id)
      is
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (IC_Id));
         pragma Assert (Present (LGV_Id));

         IC_Rec : Invocation_Construct_Record renames
                    Invocation_Constructs.Table (IC_Id);

         Body_LGV_Id : Library_Graph_Vertex_Id;

      begin
         --  Determine the proper library graph vertex which holds the body of
         --  the invocation construct.

         if IC_Rec.Placement = In_Body then
            Body_LGV_Id := Proper_Body (Lib_Graph, LGV_Id);
         else
            pragma Assert (IC_Rec.Placement = In_Spec);
            Body_LGV_Id := Proper_Spec (Lib_Graph, LGV_Id);
         end if;

         pragma Assert (Present (Body_LGV_Id));

         Add_Vertex
           (G      => Inv_Graph,
            IC_Id  => IC_Id,
            LGV_Id => Body_LGV_Id);
      end Create_Vertex;

      ---------------------
      -- Create_Vertices --
      ---------------------

      procedure Create_Vertices (U_Id : Unit_Id) is
         pragma Assert (Present (Inv_Graph));
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (U_Id));

         U_Rec  : Unit_Record renames ALI.Units.Table (U_Id);
         LGV_Id : constant Library_Graph_Vertex_Id :=
                    Corresponding_Vertex (Lib_Graph, U_Id);

         pragma Assert (Present (LGV_Id));

      begin
         for IC_Id in U_Rec.First_Invocation_Construct ..
                      U_Rec.Last_Invocation_Construct
         loop
            Create_Vertex (IC_Id, LGV_Id);
         end loop;
      end Create_Vertices;
   end Invocation_Graph_Builders;

   ----------------------------
   -- Library_Graph_Builders --
   ----------------------------

   package body Library_Graph_Builders is

      -----------------
      -- Global data --
      -----------------

      Lib_Graph : Library_Graph := Library_Graphs.Nil;

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Create_Spec_And_Body_Edge (U_Id : Unit_Id);
      pragma Inline (Create_Spec_And_Body_Edge);
      --  Establish a link between the spec and body of unit U_Id. In certain
      --  cases this may result in a new edge which is added to library graph
      --  Lib_Graph.

      procedure Create_Vertex (U_Id : Unit_Id);
      pragma Inline (Create_Vertex);
      --  Create a new vertex for unit U_Id in library graph Lib_Graph

      procedure Create_With_Edge
        (W_Id : With_Id;
         Succ : Library_Graph_Vertex_Id);
      pragma Inline (Create_With_Edge);
      --  Create a new edge for with W_Id where the predecessor is the library
      --  graph vertex of the withed unit, and the successor is Succ. The edge
      --  is added to library graph Lib_Graph.

      procedure Create_With_Edges (U_Id : Unit_Id);
      pragma Inline (Create_With_Edges);
      --  Establish links between unit U_Id and its predecessor units. The new
      --  edges are added to library graph Lib_Graph.

      procedure Create_With_Edges
        (U_Id : Unit_Id;
         Succ : Library_Graph_Vertex_Id);
      pragma Inline (Create_With_Edges);
      --  Create new edges for all withs of unit U_Id where the predecessor is
      --  some withed unit, and the successor is Succ. The edges are added to
      --  library graph Lib_Graph.

      function Is_Significant_With (W_Id : With_Id) return Boolean;
      pragma Inline (Is_Significant_With);
      --  Determine whether with W_Id plays a significant role in elaboration

      -------------------------
      -- Build_Library_Graph --
      -------------------------

      function Build_Library_Graph return Library_Graph is
      begin
         --  Prepare the global data

         Lib_Graph :=
           Create (Initial_Vertices => Number_Of_Elaborable_Units,
                   Initial_Edges    => Number_Of_Elaborable_Units);

         For_Each_Elaborable_Unit (Create_Vertex'Access);
         For_Each_Elaborable_Unit (Create_Spec_And_Body_Edge'Access);
         For_Each_Elaborable_Unit (Create_With_Edges'Access);

         return Lib_Graph;
      end Build_Library_Graph;

      -------------------------------
      -- Create_Spec_And_Body_Edge --
      -------------------------------

      procedure Create_Spec_And_Body_Edge (U_Id : Unit_Id) is
         Aux_LGV_Id : Library_Graph_Vertex_Id;
         LGV_Id     : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (U_Id));

         LGV_Id := Corresponding_Vertex (Lib_Graph, U_Id);
         pragma Assert (Present (LGV_Id));

         --  The unit denotes a body that completes a previous spec. Link the
         --  spec and body. Add an edge between the predecessor spec and the
         --  successor body.

         if Is_Body_With_Spec (Lib_Graph, LGV_Id) then
            Aux_LGV_Id :=
              Corresponding_Vertex (Lib_Graph, Corresponding_Spec (U_Id));
            pragma Assert (Present (Aux_LGV_Id));

            Set_Corresponding_Item (Lib_Graph, LGV_Id, Aux_LGV_Id);

            Add_Edge
              (G    => Lib_Graph,
               Pred => Aux_LGV_Id,
               Succ => LGV_Id,
               Kind => Spec_Before_Body_Edge);

         --  The unit denotes a spec with a completing body. Link the spec and
         --  body.

         elsif Is_Spec_With_Body (Lib_Graph, LGV_Id) then
            Aux_LGV_Id :=
              Corresponding_Vertex (Lib_Graph, Corresponding_Body (U_Id));
            pragma Assert (Present (Aux_LGV_Id));

            Set_Corresponding_Item (Lib_Graph, LGV_Id, Aux_LGV_Id);
         end if;
      end Create_Spec_And_Body_Edge;

      -------------------
      -- Create_Vertex --
      -------------------

      procedure Create_Vertex (U_Id : Unit_Id) is
      begin
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (U_Id));

         Add_Vertex
           (G    => Lib_Graph,
            U_Id => U_Id);
      end Create_Vertex;

      ----------------------
      -- Create_With_Edge --
      ----------------------

      procedure Create_With_Edge
        (W_Id : With_Id;
         Succ : Library_Graph_Vertex_Id)
      is
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (W_Id));
         pragma Assert (Present (Succ));

         Withed_Rec  : With_Record renames Withs.Table (W_Id);
         Withed_U_Id : constant Unit_Id :=
                         Corresponding_Unit (Withed_Rec.Uname);

         pragma Assert (Present (Withed_U_Id));

         Aux_LGV_Id    : Library_Graph_Vertex_Id;
         Kind          : Library_Graph_Edge_Kind;
         Withed_LGV_Id : Library_Graph_Vertex_Id;

      begin
         --  Nothing to do when the withed unit does not need to be elaborated.
         --  This prevents spurious dependencies that can never be satisfied.

         if not Needs_Elaboration (Withed_U_Id) then
            return;
         end if;

         Withed_LGV_Id := Corresponding_Vertex (Lib_Graph, Withed_U_Id);
         pragma Assert (Present (Withed_LGV_Id));

         --  The with comes with pragma Elaborate

         if Withed_Rec.Elaborate then
            Kind := Elaborate_Edge;

            --  The withed unit is a spec with a completing body. Add an edge
            --  between the body of the withed predecessor and the withing
            --  successor.

            if Is_Spec_With_Body (Lib_Graph, Withed_LGV_Id) then
               Aux_LGV_Id :=
                 Corresponding_Vertex
                   (Lib_Graph, Corresponding_Body (Withed_U_Id));
               pragma Assert (Present (Aux_LGV_Id));

               Add_Edge
                 (G    => Lib_Graph,
                  Pred => Aux_LGV_Id,
                  Succ => Succ,
                  Kind => Kind);
            end if;

         --  The with comes with pragma Elaborate_All

         elsif Withed_Rec.Elaborate_All then
            Kind := Elaborate_All_Edge;

         --  Otherwise this is a regular with

         else
            Kind := With_Edge;
         end if;

         --  Add an edge between the withed predecessor unit and the withing
         --  successor.

         Add_Edge
           (G    => Lib_Graph,
            Pred => Withed_LGV_Id,
            Succ => Succ,
            Kind => Kind);
      end Create_With_Edge;

      -----------------------
      -- Create_With_Edges --
      -----------------------

      procedure Create_With_Edges (U_Id : Unit_Id) is
         LGV_Id : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (U_Id));

         LGV_Id := Corresponding_Vertex (Lib_Graph, U_Id);
         pragma Assert (Present (LGV_Id));

         Create_With_Edges
           (U_Id => U_Id,
            Succ => LGV_Id);
      end Create_With_Edges;

      -----------------------
      -- Create_With_Edges --
      -----------------------

      procedure Create_With_Edges
        (U_Id : Unit_Id;
         Succ : Library_Graph_Vertex_Id)
      is
         pragma Assert (Present (Lib_Graph));
         pragma Assert (Present (U_Id));
         pragma Assert (Present (Succ));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         for W_Id in U_Rec.First_With .. U_Rec.Last_With loop
            if Is_Significant_With (W_Id) then
               Create_With_Edge (W_Id, Succ);
            end if;
         end loop;
      end Create_With_Edges;

      -------------------------
      -- Is_Significant_With --
      -------------------------

      function Is_Significant_With (W_Id : With_Id) return Boolean is
         pragma Assert (Present (W_Id));

         Withed_Rec  : With_Record renames Withs.Table (W_Id);
         Withed_U_Id : constant Unit_Id :=
                         Corresponding_Unit (Withed_Rec.Uname);

      begin
         --  Nothing to do for a unit which does not exist any more

         if not Present (Withed_U_Id) then
            return False;

         --  Nothing to do for a limited with

         elsif Withed_Rec.Limited_With then
            return False;

         --  Nothing to do when the unit does not need to be elaborated

         elsif not Needs_Elaboration (Withed_U_Id) then
            return False;
         end if;

         return True;
      end Is_Significant_With;
   end Library_Graph_Builders;

end Bindo.Builders;
