------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      B I N D O . V A L I D A T O R S                     --
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

with Debug;  use Debug;
with Output; use Output;
with Types;  use Types;

with Bindo.Units; use Bindo.Units;

with GNAT;      use GNAT;
with GNAT.Sets; use GNAT.Sets;

package body Bindo.Validators is

   ----------------------------------
   -- Elaboration_Order_Validators --
   ----------------------------------

   package body Elaboration_Order_Validators is
      package US is new Membership_Sets
        (Element_Type => Unit_Id,
         "="          => "=",
         Hash         => Hash_Unit);
      use US;

      Has_Invalid_Data : Boolean := False;
      --  Flag set when the elaboration order contains invalid data

      -----------------------
      -- Local subprograms --
      -----------------------

      function Build_Elaborable_Unit_Set return Membership_Set;
      pragma Inline (Build_Elaborable_Unit_Set);
      --  Create a set from all units that need to be elaborated

      procedure Report_Missing_Elaboration (U_Id : Unit_Id);
      pragma Inline (Report_Missing_Elaboration);
      --  Emit an error concerning unit U_Id that must be elaborated, but was
      --  not.

      procedure Report_Missing_Elaborations (Set : Membership_Set);
      pragma Inline (Report_Missing_Elaborations);
      --  Emit errors on all units in set Set that must be elaborated, but were
      --  not.

      procedure Report_Spurious_Elaboration (U_Id : Unit_Id);
      pragma Inline (Report_Spurious_Elaboration);
      --  Emit an error concerning unit U_Id that is incorrectly elaborated

      procedure Validate_Unit (U_Id : Unit_Id; Elab_Set : Membership_Set);
      pragma Inline (Validate_Unit);
      --  Validate the elaboration status of unit U_Id. Elab_Set is the set of
      --  all units that need to be elaborated.

      procedure Validate_Units (Order : Unit_Id_Table);
      pragma Inline (Validate_Units);
      --  Validate all units in elaboration order Order

      procedure Write_Error (Msg : String);
      pragma Inline (Write_Error);
      --  Write error message Msg to standard output and signal that the
      --  elaboration order is incorrect.

      -------------------------------
      -- Build_Elaborable_Unit_Set --
      -------------------------------

      function Build_Elaborable_Unit_Set return Membership_Set is
         Iter : Elaborable_Units_Iterator;
         Set  : Membership_Set;
         U_Id : Unit_Id;

      begin
         Set  := Create (Number_Of_Elaborable_Units);
         Iter := Iterate_Elaborable_Units;
         while Has_Next (Iter) loop
            Next (Iter, U_Id);
            pragma Assert (Present (U_Id));

            Insert (Set, U_Id);
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
         Write_Error (Msg);

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

      procedure Report_Missing_Elaborations (Set : Membership_Set) is
         Iter : Iterator;
         U_Id : Unit_Id;

      begin
         Iter := Iterate (Set);
         while Has_Next (Iter) loop
            Next (Iter, U_Id);
            pragma Assert (Present (U_Id));

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
         Write_Error (Msg);

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
         --  Nothing to do when switch -d_V (validate bindo graphs and order)
         --  is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Validate_Units (Order);

         if Has_Invalid_Data then
            raise Invalid_Elaboration_Order;
         end if;
      end Validate_Elaboration_Order;

      -------------------
      -- Validate_Unit --
      -------------------

      procedure Validate_Unit (U_Id : Unit_Id; Elab_Set : Membership_Set) is
      begin
         pragma Assert (Present (U_Id));

         --  The current unit in the elaboration order appears within the set
         --  of units that require elaboration. Remove it from the set.

         if Contains (Elab_Set, U_Id) then
            Delete (Elab_Set, U_Id);

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
         Elab_Set : Membership_Set;

      begin
         --  Collect all units in the compilation that need to be elaborated
         --  in a set.

         Elab_Set := Build_Elaborable_Unit_Set;

         --  Validate each unit in the elaboration order against the set of
         --  units that need to be elaborated.

         for Index in Unit_Id_Tables.First ..  Unit_Id_Tables.Last (Order) loop
            Validate_Unit
              (U_Id     => Order.Table (Index),
               Elab_Set => Elab_Set);
         end loop;

         --  At this point all units that need to be elaborated should have
         --  been eliminated from the set. Report any units that are missing
         --  their elaboration.

         Report_Missing_Elaborations (Elab_Set);
         Destroy (Elab_Set);
      end Validate_Units;

      -----------------
      -- Write_Error --
      -----------------

      procedure Write_Error (Msg : String) is
      begin
         Has_Invalid_Data := True;

         Write_Str ("ERROR: ");
         Write_Str (Msg);
         Write_Eol;
      end Write_Error;
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
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id);
      pragma Inline (Validate_Invocation_Graph_Edge);
      --  Verify that the attributes of edge IGE_Id of invocation graph G are
      --  properly set.

      procedure Validate_Invocation_Graph_Edges (G : Invocation_Graph);
      pragma Inline (Validate_Invocation_Graph_Edges);
      --  Verify that the attributes of all edges of invocation graph G are
      --  properly set.

      procedure Validate_Invocation_Graph_Vertex
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id);
      pragma Inline (Validate_Invocation_Graph_Vertex);
      --  Verify that the attributes of vertex IGV_Id of inbocation graph G are
      --  properly set.

      procedure Validate_Invocation_Graph_Vertices (G : Invocation_Graph);
      pragma Inline (Validate_Invocation_Graph_Vertices);
      --  Verify that the attributes of all vertices of invocation graph G are
      --  properly set.

      procedure Write_Error (Msg : String);
      pragma Inline (Write_Error);
      --  Write error message Msg to standard output and signal that the
      --  invocation graph is incorrect.

      -------------------------------
      -- Validate_Invocation_Graph --
      -------------------------------

      procedure Validate_Invocation_Graph (G : Invocation_Graph) is
      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -d_V (validate bindo graphs and order)
         --  is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Validate_Invocation_Graph_Vertices (G);
         Validate_Invocation_Graph_Edges (G);

         if Has_Invalid_Data then
            raise Invalid_Invocation_Graph;
         end if;
      end Validate_Invocation_Graph;

      ------------------------------------
      -- Validate_Invocation_Graph_Edge --
      ------------------------------------

      procedure Validate_Invocation_Graph_Edge
        (G      : Invocation_Graph;
         IGE_Id : Invocation_Graph_Edge_Id)
      is
         Msg : constant String := "Validate_Invocation_Graph_Edge";

      begin
         pragma Assert (Present (G));

         if not Present (IGE_Id) then
            Write_Error (Msg);

            Write_Str ("  emply invocation graph edge");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if not Present (Relation (G, IGE_Id)) then
            Write_Error (Msg);

            Write_Str ("  invocation graph edge (IGE_Id_");
            Write_Int (Int (IGE_Id));
            Write_Str (") lacks Relation");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Target (G, IGE_Id)) then
            Write_Error (Msg);

            Write_Str ("  invocation graph edge (IGE_Id_");
            Write_Int (Int (IGE_Id));
            Write_Str (") lacks Target");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Invocation_Graph_Edge;

      -------------------------------------
      -- Validate_Invocation_Graph_Edges --
      -------------------------------------

      procedure Validate_Invocation_Graph_Edges (G : Invocation_Graph) is
         IGE_Id : Invocation_Graph_Edge_Id;
         Iter   : Invocation_Graphs.All_Edge_Iterator;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, IGE_Id);

            Validate_Invocation_Graph_Edge (G, IGE_Id);
         end loop;
      end Validate_Invocation_Graph_Edges;

      --------------------------------------
      -- Validate_Invocation_Graph_Vertex --
      --------------------------------------

      procedure Validate_Invocation_Graph_Vertex
        (G      : Invocation_Graph;
         IGV_Id : Invocation_Graph_Vertex_Id)
      is
         Msg : constant String := "Validate_Invocation_Graph_Vertex";

      begin
         pragma Assert (Present (G));

         if not Present (IGV_Id) then
            Write_Error (Msg);

            Write_Str ("  emply invocation graph vertex");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if not Present (Construct (G, IGV_Id)) then
            Write_Error (Msg);

            Write_Str ("  invocation graph vertex (IGV_Id_");
            Write_Int (Int (IGV_Id));
            Write_Str (") lacks Construct");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Lib_Vertex (G, IGV_Id)) then
            Write_Error (Msg);

            Write_Str ("  invocation graph vertex (IGV_Id_");
            Write_Int (Int (IGV_Id));
            Write_Str (") lacks Lib_Vertex");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Invocation_Graph_Vertex;

      ----------------------------------------
      -- Validate_Invocation_Graph_Vertices --
      ----------------------------------------

      procedure Validate_Invocation_Graph_Vertices (G : Invocation_Graph) is
         IGV_Id : Invocation_Graph_Vertex_Id;
         Iter   : Invocation_Graphs.All_Vertex_Iterator;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, IGV_Id);

            Validate_Invocation_Graph_Vertex (G, IGV_Id);
         end loop;
      end Validate_Invocation_Graph_Vertices;

      -----------------
      -- Write_Error --
      -----------------

      procedure Write_Error (Msg : String) is
      begin
         Has_Invalid_Data := True;

         Write_Str ("ERROR: ");
         Write_Str (Msg);
         Write_Eol;
      end Write_Error;
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
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id);
      pragma Inline (Validate_Library_Graph_Edge);
      --  Verify that the attributes of edge LGE_Id of library graph G are
      --  properly set.

      procedure Validate_Library_Graph_Edges (G : Library_Graph);
      pragma Inline (Validate_Library_Graph_Edges);
      --  Verify that the attributes of all edges of library graph G are
      --  properly set.

      procedure Validate_Library_Graph_Vertex
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id);
      pragma Inline (Validate_Library_Graph_Vertex);
      --  Verify that the attributes of vertex LGV_Id of library graph G are
      --  properly set.

      procedure Validate_Library_Graph_Vertices (G : Library_Graph);
      pragma Inline (Validate_Library_Graph_Vertices);
      --  Verify that the attributes of all vertices of library graph G are
      --  properly set.

      procedure Write_Error (Msg : String);
      pragma Inline (Write_Error);
      --  Write error message Msg to standard output and signal that the
      --  library graph is incorrect.

      ----------------------------
      -- Validate_Library_Graph --
      ----------------------------

      procedure Validate_Library_Graph (G : Library_Graph) is
      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -d_V (validate bindo graphs and order)
         --  is not in effect.

         if not Debug_Flag_Underscore_VV then
            return;
         end if;

         Validate_Library_Graph_Vertices (G);
         Validate_Library_Graph_Edges (G);

         if Has_Invalid_Data then
            raise Invalid_Library_Graph;
         end if;
      end Validate_Library_Graph;

      ---------------------------------
      -- Validate_Library_Graph_Edge --
      ---------------------------------

      procedure Validate_Library_Graph_Edge
        (G      : Library_Graph;
         LGE_Id : Library_Graph_Edge_Id)
      is
         Msg : constant String := "Validate_Library_Graph_Edge";

      begin
         pragma Assert (Present (G));

         if not Present (LGE_Id) then
            Write_Error (Msg);

            Write_Str ("  emply library graph edge");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if Kind (G, LGE_Id) = No_Edge then
            Write_Error (Msg);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (LGE_Id));
            Write_Str (") is not a valid edge");
            Write_Eol;
            Write_Eol;

         elsif Kind (G, LGE_Id) = Body_Before_Spec_Edge then
            Write_Error (Msg);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (LGE_Id));
            Write_Str (") is a Body_Before_Spec edge");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Predecessor (G, LGE_Id)) then
            Write_Error (Msg);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (LGE_Id));
            Write_Str (") lacks Predecessor");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Successor (G, LGE_Id)) then
            Write_Error (Msg);

            Write_Str ("  library graph edge (LGE_Id_");
            Write_Int (Int (LGE_Id));
            Write_Str (") lacks Successor");
            Write_Eol;
            Write_Eol;
         end if;
      end Validate_Library_Graph_Edge;

      ----------------------------------
      -- Validate_Library_Graph_Edges --
      ----------------------------------

      procedure Validate_Library_Graph_Edges (G : Library_Graph) is
         Iter   : Library_Graphs.All_Edge_Iterator;
         LGE_Id : Library_Graph_Edge_Id;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Edges (G);
         while Has_Next (Iter) loop
            Next (Iter, LGE_Id);
            pragma Assert (Present (LGE_Id));

            Validate_Library_Graph_Edge (G, LGE_Id);
         end loop;
      end Validate_Library_Graph_Edges;

      -----------------------------------
      -- Validate_Library_Graph_Vertex --
      -----------------------------------

      procedure Validate_Library_Graph_Vertex
        (G      : Library_Graph;
         LGV_Id : Library_Graph_Vertex_Id)
      is
         Msg : constant String := "Validate_Library_Graph_Vertex";

      begin
         pragma Assert (Present (G));

         if not Present (LGV_Id) then
            Write_Error (Msg);

            Write_Str ("  empty library graph vertex");
            Write_Eol;
            Write_Eol;
            return;
         end if;

         if (Is_Body_With_Spec (G, LGV_Id)
               or else
             Is_Spec_With_Body (G, LGV_Id))
           and then not Present (Corresponding_Item (G, LGV_Id))
         then
            Write_Error (Msg);

            Write_Str ("  library graph vertex (LGV_Id_");
            Write_Int (Int (LGV_Id));
            Write_Str (") lacks Corresponding_Item");
            Write_Eol;
            Write_Eol;
         end if;

         if not Present (Unit (G, LGV_Id)) then
            Write_Error (Msg);

            Write_Str ("  library graph vertex (LGV_Id_");
            Write_Int (Int (LGV_Id));
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
         LGV_Id : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, LGV_Id);
            pragma Assert (Present (LGV_Id));

            Validate_Library_Graph_Vertex (G, LGV_Id);
         end loop;
      end Validate_Library_Graph_Vertices;

      -----------------
      -- Write_Error --
      -----------------

      procedure Write_Error (Msg : String) is
      begin
         Has_Invalid_Data := True;

         Write_Str ("ERROR: ");
         Write_Str (Msg);
         Write_Eol;
      end Write_Error;
   end Library_Graph_Validators;

end Bindo.Validators;
