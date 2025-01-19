------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      B I N D O . V A L I D A T O R S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019-2025, Free Software Foundation, Inc.      --
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

with Debug;  use Debug;
with Output; use Output;
with Types;  use Types;

with Bindo.Units;
use  Bindo.Units;

with Bindo.Writers;
use  Bindo.Writers;
use  Bindo.Writers.Phase_Writers;

package body Bindo.Validators is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Write_Error
     (Msg  : String;
      Flag : out Boolean);
   pragma Inline (Write_Error);
   --  Write error message Msg to standard output and set flag Flag to True

   ----------------------
   -- Cycle_Validators --
   ----------------------

   package body Cycle_Validators is
      Has_Invalid_Cycle : Boolean := False;
      --  Flag set when the library graph contains an invalid cycle

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Validate_Cycle
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id);
      pragma Inline (Validate_Cycle);
      --  Ensure that a cycle meets the following requirements:
      --
      --    * Is of proper kind
      --    * Has enough edges to form a circuit
      --    * No edge is repeated

      procedure Validate_Cycle_Path
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id);
      pragma Inline (Validate_Cycle_Path);
      --  Ensure that the path of a cycle meets the following requirements:
      --
      --    * No edge is repeated

      --------------------
      -- Validate_Cycle --
      --------------------

      procedure Validate_Cycle
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id)
      is
         Msg : constant String := "Validate_Cycle";

      begin
         pragma Assert (Present (G));

         if not Present (Cycle) then
            Write_Error (Msg, Has_Invalid_Cycle);

            Write_Str ("  empty cycle");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if Kind (G, Cycle) = No_Cycle_Kind then
            Write_Error (Msg, Has_Invalid_Cycle);

            Write_Str ("  cycle (LGC_Id_");
            Write_Int (Int (Cycle));
            Write_Str (") is a No_Cycle");
            Write_Eol;
            Write_Eol;
         end if;

         --  A cycle requires at least one edge (self cycle) to form a circuit

         if Length (G, Cycle) < 1 then
            Write_Error (Msg, Has_Invalid_Cycle);

            Write_Str ("  cycle (LGC_Id_");
            Write_Int (Int (Cycle));
            Write_Str (") does not contain enough edges");
            Write_Eol;
            Write_Eol;
         end if;

         Validate_Cycle_Path (G, Cycle);
      end Validate_Cycle;

      -------------------------
      -- Validate_Cycle_Path --
      -------------------------

      procedure Validate_Cycle_Path
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id)
      is
         Msg : constant String := "Validate_Cycle_Path";

         Edge  : Library_Graph_Edge_Id;
         Edges : LGE_Sets.Membership_Set;
         Iter  : Edges_Of_Cycle_Iterator;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         --  Use a set to detect duplicate edges while traversing the cycle

         Edges := LGE_Sets.Create (Length (G, Cycle));

         --  Inspect the edges of the cycle, trying to catch duplicates

         Iter := Iterate_Edges_Of_Cycle (G, Cycle);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            --  The current edge has already been encountered while traversing
            --  the cycle. This indicates that the cycle is malformed as edges
            --  are not repeated in the circuit.

            if LGE_Sets.Contains (Edges, Edge) then
               Write_Error (Msg, Has_Invalid_Cycle);

               Write_Str ("  library graph edge (LGE_Id_");
               Write_Int (Int (Edge));
               Write_Str (") is repeated in cycle (LGC_Id_");
               Write_Int (Int (Cycle));
               Write_Str (")");
               Write_Eol;

            --  Otherwise add the current edge to the set of encountered edges

            else
               LGE_Sets.Insert (Edges, Edge);
            end if;
         end loop;

         LGE_Sets.Destroy (Edges);
      end Validate_Cycle_Path;

      ---------------------
      -- Validate_Cycles --
      ---------------------

      procedure Validate_Cycles (G : Library_Graph) is
         Cycle : Library_Graph_Cycle_Id;
         Iter  : All_Cycle_Iterator;

      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -d_V (validate bindo cycles, graphs, and
         --  order) is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Start_Phase (Cycle_Validation);

         Iter := Iterate_All_Cycles (G);
         while Has_Next (Iter) loop
            Next (Iter, Cycle);

            Validate_Cycle (G, Cycle);
         end loop;

         End_Phase (Cycle_Validation);

         if Has_Invalid_Cycle then
            raise Invalid_Cycle;
         end if;
      end Validate_Cycles;
   end Cycle_Validators;

   ----------------------------------
   -- Elaboration_Order_Validators --
   ----------------------------------

   package body Elaboration_Order_Validators is
      Has_Invalid_Data : Boolean := False;
      --  Flag set when the elaboration order contains invalid data

      -----------------------
      -- Local subprograms --
      -----------------------

      function Build_Elaborable_Unit_Set return Unit_Sets.Membership_Set;
      pragma Inline (Build_Elaborable_Unit_Set);
      --  Create a set from all units that need to be elaborated

      procedure Report_Missing_Elaboration (U_Id : Unit_Id);
      pragma Inline (Report_Missing_Elaboration);
      --  Emit an error concerning unit U_Id that must be elaborated, but was
      --  not.

      procedure Report_Missing_Elaborations (Set : Unit_Sets.Membership_Set);
      pragma Inline (Report_Missing_Elaborations);
      --  Emit errors on all units in set Set that must be elaborated, but were
      --  not.

      procedure Report_Spurious_Elaboration (U_Id : Unit_Id);
      pragma Inline (Report_Spurious_Elaboration);
      --  Emit an error concerning unit U_Id that is incorrectly elaborated

      procedure Validate_Unit
        (U_Id     : Unit_Id;
         Elab_Set : Unit_Sets.Membership_Set);
      pragma Inline (Validate_Unit);
      --  Validate the elaboration status of unit U_Id. Elab_Set is the set of
      --  all units that need to be elaborated.

      procedure Validate_Units (Order : Unit_Id_Table);
      pragma Inline (Validate_Units);
      --  Validate all units in elaboration order Order

      -------------------------------
      -- Build_Elaborable_Unit_Set --
      -------------------------------

      function Build_Elaborable_Unit_Set return Unit_Sets.Membership_Set is
         Iter : Elaborable_Units_Iterator;
         Set  : Unit_Sets.Membership_Set;
         U_Id : Unit_Id;

      begin
         Set  := Unit_Sets.Create (Number_Of_Elaborable_Units);
         Iter := Iterate_Elaborable_Units;
         while Has_Next (Iter) loop
            Next (Iter, U_Id);

            Unit_Sets.Insert (Set, U_Id);
         end loop;

         return Set;
      end Build_Elaborable_Unit_Set;

      --------------------------------
      -- Report_Missing_Elaboration --
      --------------------------------

      procedure Report_Missing_Elaboration (U_Id : Unit_Id) is
         Msg : constant String := "Report_Missing_Elaboration";

      begin
         pragma Assert (Present (U_Id));
         Write_Error (Msg, Has_Invalid_Data);

         Write_Str  ("unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (Name (U_Id));
         Write_Str  (" must be elaborated");
         Write_Eol;
      end Report_Missing_Elaboration;

      ---------------------------------
      -- Report_Missing_Elaborations --
      ---------------------------------

      procedure Report_Missing_Elaborations (Set : Unit_Sets.Membership_Set) is
         Iter : Unit_Sets.Iterator;
         U_Id : Unit_Id;

      begin
         Iter := Unit_Sets.Iterate (Set);
         while Unit_Sets.Has_Next (Iter) loop
            Unit_Sets.Next (Iter, U_Id);

            Report_Missing_Elaboration (U_Id);
         end loop;
      end Report_Missing_Elaborations;

      ---------------------------------
      -- Report_Spurious_Elaboration --
      ---------------------------------

      procedure Report_Spurious_Elaboration (U_Id : Unit_Id) is
         Msg : constant String := "Report_Spurious_Elaboration";

      begin
         pragma Assert (Present (U_Id));
         Write_Error (Msg, Has_Invalid_Data);

         Write_Str  ("unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (Name (U_Id));
         Write_Str  (" must not be elaborated");
      end Report_Spurious_Elaboration;

      --------------------------------
      -- Validate_Elaboration_Order --
      --------------------------------

      procedure Validate_Elaboration_Order (Order : Unit_Id_Table) is
      begin
         --  Nothing to do when switch -d_V (validate bindo cycles, graphs, and
         --  order) is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Start_Phase (Elaboration_Order_Validation);

         Validate_Units (Order);

         End_Phase (Elaboration_Order_Validation);

         if Has_Invalid_Data then
            raise Invalid_Elaboration_Order;
         end if;
      end Validate_Elaboration_Order;

      -------------------
      -- Validate_Unit --
      -------------------

      procedure Validate_Unit
        (U_Id     : Unit_Id;
         Elab_Set : Unit_Sets.Membership_Set)
      is
      begin
         pragma Assert (Present (U_Id));

         --  The current unit in the elaboration order appears within the set
         --  of units that require elaboration. Remove it from the set.

         if Unit_Sets.Contains (Elab_Set, U_Id) then
            Unit_Sets.Delete (Elab_Set, U_Id);

         --  Otherwise the current unit in the elaboration order must not be
         --  elaborated.

         else
            Report_Spurious_Elaboration (U_Id);
         end if;
      end Validate_Unit;

      --------------------
      -- Validate_Units --
      --------------------

      procedure Validate_Units (Order : Unit_Id_Table) is
         Elab_Set : Unit_Sets.Membership_Set;

      begin
         --  Collect all units in the compilation that need to be elaborated
         --  in a set.

         Elab_Set := Build_Elaborable_Unit_Set;

         --  Validate each unit in the elaboration order against the set of
         --  units that need to be elaborated.

         for Index in Unit_Id_Tables.First .. Unit_Id_Tables.Last (Order) loop
            Validate_Unit
              (U_Id     => Order.Table (Index),
               Elab_Set => Elab_Set);
         end loop;

         --  At this point all units that need to be elaborated should have
         --  been eliminated from the set. Report any units that are missing
         --  their elaboration.

         Report_Missing_Elaborations (Elab_Set);
         Unit_Sets.Destroy (Elab_Set);
      end Validate_Units;
   end Elaboration_Order_Validators;

   ---------------------------------
   -- Invocation_Graph_Validators --
   ---------------------------------

   package body Invocation_Graph_Validators is
      Has_Invalid_Data : Boolean := False;
      --  Flag set when the invocation graph contains invalid data

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Validate_Invocation_Graph_Edge
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id);
      pragma Inline (Validate_Invocation_Graph_Edge);
      --  Verify that the attributes of edge Edge of invocation graph G are
      --  properly set.

      procedure Validate_Invocation_Graph_Edges (G : Invocation_Graph);
      pragma Inline (Validate_Invocation_Graph_Edges);
      --  Verify that the attributes of all edges of invocation graph G are
      --  properly set.

      procedure Validate_Invocation_Graph_Vertex
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id);
      pragma Inline (Validate_Invocation_Graph_Vertex);
      --  Verify that the attributes of vertex Vertex of invocation graph G are
      --  properly set.

      procedure Validate_Invocation_Graph_Vertices (G : Invocation_Graph);
      pragma Inline (Validate_Invocation_Graph_Vertices);
      --  Verify that the attributes of all vertices of invocation graph G are
      --  properly set.

      -------------------------------
      -- Validate_Invocation_Graph --
      -------------------------------

      procedure Validate_Invocation_Graph (G : Invocation_Graph) is
      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -d_V (validate bindo cycles, graphs, and
         --  order) is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Start_Phase (Invocation_Graph_Validation);

         Validate_Invocation_Graph_Vertices (G);
         Validate_Invocation_Graph_Edges    (G);

         End_Phase (Invocation_Graph_Validation);

         if Has_Invalid_Data then
            raise Invalid_Invocation_Graph;
         end if;
      end Validate_Invocation_Graph;

      ------------------------------------
      -- Validate_Invocation_Graph_Edge --
      ------------------------------------

      procedure Validate_Invocation_Graph_Edge
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id)
      is
         Msg : constant String := "Validate_Invocation_Graph_Edge";

      begin
         pragma Assert (Present (G));

         if not Present (Edge) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  empty invocation graph edge");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if not Present (Relation (G, Edge)) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  invocation graph edge (IGE_Id_");
            Write_Int (Int (Edge));
            Write_Str (") lacks Relation");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Target (G, Edge)) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  invocation graph edge (IGE_Id_");
            Write_Int (Int (Edge));
            Write_Str (") lacks Target");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Invocation_Graph_Edge;

      -------------------------------------
      -- Validate_Invocation_Graph_Edges --
      -------------------------------------

      procedure Validate_Invocation_Graph_Edges (G : Invocation_Graph) is
         Edge : Invocation_Graph_Edge_Id;
         Iter : Invocation_Graphs.All_Edge_Iterator;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            Validate_Invocation_Graph_Edge (G, Edge);
         end loop;
      end Validate_Invocation_Graph_Edges;

      --------------------------------------
      -- Validate_Invocation_Graph_Vertex --
      --------------------------------------

      procedure Validate_Invocation_Graph_Vertex
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id)
      is
         Msg : constant String := "Validate_Invocation_Graph_Vertex";

      begin
         pragma Assert (Present (G));

         if not Present (Vertex) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  empty invocation graph vertex");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if not Present (Body_Vertex (G, Vertex)) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  invocation graph vertex (IGV_Id_");
            Write_Int (Int (Vertex));
            Write_Str (") lacks Body_Vertex");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Construct (G, Vertex)) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  invocation graph vertex (IGV_Id_");
            Write_Int (Int (Vertex));
            Write_Str (") lacks Construct");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Spec_Vertex (G, Vertex)) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  invocation graph vertex (IGV_Id_");
            Write_Int (Int (Vertex));
            Write_Str (") lacks Spec_Vertex");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Invocation_Graph_Vertex;

      ----------------------------------------
      -- Validate_Invocation_Graph_Vertices --
      ----------------------------------------

      procedure Validate_Invocation_Graph_Vertices (G : Invocation_Graph) is
         Iter   : Invocation_Graphs.All_Vertex_Iterator;
         Vertex : Invocation_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, Vertex);

            Validate_Invocation_Graph_Vertex (G, Vertex);
         end loop;
      end Validate_Invocation_Graph_Vertices;
   end Invocation_Graph_Validators;

   ------------------------------
   -- Library_Graph_Validators --
   ------------------------------

   package body Library_Graph_Validators is
      Has_Invalid_Data : Boolean := False;
      --  Flag set when the library graph contains invalid data

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Validate_Library_Graph_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id);
      pragma Inline (Validate_Library_Graph_Edge);
      --  Verify that the attributes of edge Edge of library graph G are
      --  properly set.

      procedure Validate_Library_Graph_Edges (G : Library_Graph);
      pragma Inline (Validate_Library_Graph_Edges);
      --  Verify that the attributes of all edges of library graph G are
      --  properly set.

      procedure Validate_Library_Graph_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id);
      pragma Inline (Validate_Library_Graph_Vertex);
      --  Verify that the attributes of vertex Vertex of library graph G are
      --  properly set.

      procedure Validate_Library_Graph_Vertices (G : Library_Graph);
      pragma Inline (Validate_Library_Graph_Vertices);
      --  Verify that the attributes of all vertices of library graph G are
      --  properly set.

      ----------------------------
      -- Validate_Library_Graph --
      ----------------------------

      procedure Validate_Library_Graph (G : Library_Graph) is
      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -d_V (validate bindo cycles, graphs, and
         --  order) is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Start_Phase (Library_Graph_Validation);

         Validate_Library_Graph_Vertices (G);
         Validate_Library_Graph_Edges    (G);

         End_Phase (Library_Graph_Validation);

         if Has_Invalid_Data then
            raise Invalid_Library_Graph;
         end if;
      end Validate_Library_Graph;

      ---------------------------------
      -- Validate_Library_Graph_Edge --
      ---------------------------------

      procedure Validate_Library_Graph_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id)
      is
         Msg : constant String := "Validate_Library_Graph_Edge";

      begin
         pragma Assert (Present (G));

         if not Present (Edge) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  empty library graph edge");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if Kind (G, Edge) = No_Edge then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (Edge));
            Write_Str (") is not a valid edge");
            Write_Eol;
            Write_Eol;

         elsif Kind (G, Edge) = Body_Before_Spec_Edge then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (Edge));
            Write_Str (") is a Body_Before_Spec edge");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Predecessor (G, Edge)) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (Edge));
            Write_Str (") lacks Predecessor");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Successor (G, Edge)) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (Edge));
            Write_Str (") lacks Successor");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Library_Graph_Edge;

      ----------------------------------
      -- Validate_Library_Graph_Edges --
      ----------------------------------

      procedure Validate_Library_Graph_Edges (G : Library_Graph) is
         Edge : Library_Graph_Edge_Id;
         Iter : Library_Graphs.All_Edge_Iterator;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            Validate_Library_Graph_Edge (G, Edge);
         end loop;
      end Validate_Library_Graph_Edges;

      -----------------------------------
      -- Validate_Library_Graph_Vertex --
      -----------------------------------

      procedure Validate_Library_Graph_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id)
      is
         Msg : constant String := "Validate_Library_Graph_Vertex";

      begin
         pragma Assert (Present (G));

         if not Present (Vertex) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  empty library graph vertex");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if (Is_Body_With_Spec (G, Vertex)
               or else
             Is_Spec_With_Body (G, Vertex))
           and then not Present (Corresponding_Item (G, Vertex))
         then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  library graph vertex (LGV_Id_");
            Write_Int (Int (Vertex));
            Write_Str (") lacks Corresponding_Item");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Unit (G, Vertex)) then
            Write_Error (Msg, Has_Invalid_Data);

            Write_Str ("  library graph vertex (LGV_Id_");
            Write_Int (Int (Vertex));
            Write_Str (") lacks Unit");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Library_Graph_Vertex;

      -------------------------------------
      -- Validate_Library_Graph_Vertices --
      -------------------------------------

      procedure Validate_Library_Graph_Vertices (G : Library_Graph) is
         Iter   : Library_Graphs.All_Vertex_Iterator;
         Vertex : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, Vertex);

            Validate_Library_Graph_Vertex (G, Vertex);
         end loop;
      end Validate_Library_Graph_Vertices;
   end Library_Graph_Validators;

   -----------------
   -- Write_Error --
   -----------------

   procedure Write_Error
     (Msg  : String;
      Flag : out Boolean)
   is
   begin
      Write_Str ("ERROR: ");
      Write_Str (Msg);
      Write_Eol;

      Flag := True;
   end Write_Error;

end Bindo.Validators;
