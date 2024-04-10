------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        B I N D O . W R I T E R S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019-2024, Free Software Foundation, Inc.      --
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
with Fname;   use Fname;
with Opt;     use Opt;
with Output;  use Output;

with Bindo.Units;
use  Bindo.Units;

with GNAT;        use GNAT;
with GNAT.Graphs; use GNAT.Graphs;
with GNAT.Sets;   use GNAT.Sets;

package body Bindo.Writers is

   -----------------
   -- ALI_Writers --
   -----------------

   package body ALI_Writers is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_All_Units;
      pragma Inline (Write_All_Units);
      --  Write the common form of units to standard output

      procedure Write_Invocation_Construct (IC_Id : Invocation_Construct_Id);
      pragma Inline (Write_Invocation_Construct);
      --  Write invocation construct IC_Id to standard output

      procedure Write_Invocation_Relation (IR_Id : Invocation_Relation_Id);
      pragma Inline (Write_Invocation_Relation);
      --  Write invocation relation IR_Id to standard output

      procedure Write_Invocation_Signature (IS_Id : Invocation_Signature_Id);
      pragma Inline (Write_Invocation_Signature);
      --  Write invocation signature IS_Id to standard output

      procedure Write_Statistics;
      pragma Inline (Write_Statistics);
      --  Write the statistical information of units to standard output

      procedure Write_Unit (U_Id : Unit_Id);
      pragma Inline (Write_Unit);
      --  Write the invocation constructs and relations of unit U_Id to
      --  standard output.

      procedure Write_Unit_Common (U_Id : Unit_Id);
      pragma Inline (Write_Unit_Common);
      --  Write the common form of unit U_Id to standard output

      -----------
      -- Debug --
      -----------

      procedure pau renames Write_All_Units;
      pragma Unreferenced (pau);

      procedure pu (U_Id : Unit_Id) renames Write_Unit_Common;
      pragma Unreferenced (pu);

      ----------------------
      -- Write_ALI_Tables --
      ----------------------

      procedure Write_ALI_Tables is
      begin
         --  Nothing to do when switch -d_A (output invocation tables) is not
         --  in effect.

         if not Debug_Flag_Underscore_AA then
            return;
         end if;

         Write_Str ("ALI Tables");
         Write_Eol;
         Write_Eol;

         Write_Statistics;
         For_Each_Unit (Write_Unit'Access);

         Write_Str ("ALI Tables end");
         Write_Eol;
         Write_Eol;
      end Write_ALI_Tables;

      ---------------------
      -- Write_All_Units --
      ---------------------

      procedure Write_All_Units is
      begin
         For_Each_Unit (Write_Unit_Common'Access);
      end Write_All_Units;

      --------------------------------
      -- Write_Invocation_Construct --
      --------------------------------

      procedure Write_Invocation_Construct (IC_Id : Invocation_Construct_Id) is
      begin
         pragma Assert (Present (IC_Id));

         Write_Str ("  invocation construct (IC_Id_");
         Write_Int (Int (IC_Id));
         Write_Str (")");
         Write_Eol;

         Write_Str ("    Body_Placement = ");
         Write_Str (Body_Placement (IC_Id)'Img);
         Write_Eol;

         Write_Str ("    Kind = ");
         Write_Str (Kind (IC_Id)'Img);
         Write_Eol;

         Write_Str ("    Spec_Placement = ");
         Write_Str (Spec_Placement (IC_Id)'Img);
         Write_Eol;

         Write_Invocation_Signature (Signature (IC_Id));
         Write_Eol;
      end Write_Invocation_Construct;

      -------------------------------
      -- Write_Invocation_Relation --
      -------------------------------

      procedure Write_Invocation_Relation (IR_Id : Invocation_Relation_Id) is
      begin
         pragma Assert (Present (IR_Id));

         Write_Str ("  invocation relation (IR_Id_");
         Write_Int (Int (IR_Id));
         Write_Str (")");
         Write_Eol;

         if Present (Extra (IR_Id)) then
            Write_Str  ("    Extra = ");
            Write_Name (Extra (IR_Id));
         else
            Write_Str ("    Extra = none");
         end if;

         Write_Eol;
         Write_Str ("    Invoker");
         Write_Eol;

         Write_Invocation_Signature (Invoker (IR_Id));

         Write_Str ("    Kind = ");
         Write_Str (Kind (IR_Id)'Img);
         Write_Eol;

         Write_Str ("    Target");
         Write_Eol;

         Write_Invocation_Signature (Target (IR_Id));
         Write_Eol;
      end Write_Invocation_Relation;

      --------------------------------
      -- Write_Invocation_Signature --
      --------------------------------

      procedure Write_Invocation_Signature (IS_Id : Invocation_Signature_Id) is
      begin
         pragma Assert (Present (IS_Id));

         Write_Str ("    Signature (IS_Id_");
         Write_Int (Int (IS_Id));
         Write_Str (")");
         Write_Eol;

         Write_Str ("      Column = ");
         Write_Int (Int (Column (IS_Id)));
         Write_Eol;

         Write_Str ("      Line = ");
         Write_Int (Int (Line (IS_Id)));
         Write_Eol;

         if Present (Locations (IS_Id)) then
            Write_Str  ("      Locations = ");
            Write_Name (Locations (IS_Id));
         else
            Write_Str ("      Locations = none");
         end if;

         Write_Eol;
         Write_Str  ("      Name = ");
         Write_Name (Name (IS_Id));
         Write_Eol;

         Write_Str  ("      Scope = ");
         Write_Name (IS_Scope (IS_Id));
         Write_Eol;
      end Write_Invocation_Signature;

      ----------------------
      -- Write_Statistics --
      ----------------------

      procedure Write_Statistics is
      begin
         Write_Str ("Units             : ");
         Write_Num (Int (Number_Of_Units));
         Write_Eol;

         Write_Str ("Units to elaborate: ");
         Write_Num (Int (Number_Of_Elaborable_Units));
         Write_Eol;
         Write_Eol;
      end Write_Statistics;

      ----------------
      -- Write_Unit --
      ----------------

      procedure Write_Unit (U_Id : Unit_Id) is
         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         Write_Unit_Common (U_Id);

         Write_Str ("  First_Invocation_Construct (IC_Id_");
         Write_Int (Int (U_Rec.First_Invocation_Construct));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  Last_Invocation_Construct  (IC_Id_");
         Write_Int (Int (U_Rec.Last_Invocation_Construct));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  First_Invocation_Relation  (IR_Id_");
         Write_Int (Int (U_Rec.First_Invocation_Relation));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  Last_Invocation_Relation   (IR_Id_");
         Write_Int (Int (U_Rec.Last_Invocation_Relation));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  Invocation_Graph_Encoding = ");
         Write_Str (Invocation_Graph_Encoding (U_Id)'Img);
         Write_Eol;
         Write_Eol;

         For_Each_Invocation_Construct
           (U_Id      => U_Id,
            Processor => Write_Invocation_Construct'Access);

         For_Each_Invocation_Relation
           (U_Id      => U_Id,
            Processor => Write_Invocation_Relation'Access);
      end Write_Unit;

      -----------------------
      -- Write_Unit_Common --
      -----------------------

      procedure Write_Unit_Common (U_Id : Unit_Id) is
         pragma Assert (Present (U_Id));

         U_Rec : Unit_Record renames ALI.Units.Table (U_Id);

      begin
         Write_Str  ("unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (U_Rec.Uname);
         Write_Eol;

         if U_Rec.SAL_Interface then
            Write_Str ("  SAL_Interface = True");
            Write_Eol;
         end if;
      end Write_Unit_Common;
   end ALI_Writers;

   -------------------
   -- Cycle_Writers --
   -------------------

   package body Cycle_Writers is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Cycle
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id);
      pragma Inline (Write_Cycle);
      --  Write the path of cycle Cycle found in library graph G to standard
      --  output.

      procedure Write_Cyclic_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id);
      pragma Inline (Write_Cyclic_Edge);
      --  Write cyclic edge Edge of library graph G to standard

      -----------
      -- Debug --
      -----------

      procedure palgc (G : Library_Graph) renames Write_Cycles;
      pragma Unreferenced (palgc);

      procedure plgc
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id) renames Write_Cycle;
      pragma Unreferenced (plgc);

      -----------------
      -- Write_Cycle --
      -----------------

      procedure Write_Cycle
        (G     : Library_Graph;
         Cycle : Library_Graph_Cycle_Id)
      is
         Edge : Library_Graph_Edge_Id;
         Iter : Edges_Of_Cycle_Iterator;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Cycle));

         --  Nothing to do when switch -d_P (output cycle paths) is not in
         --  effect.

         if not Debug_Flag_Underscore_PP then
            return;
         end if;

         Write_Str ("cycle (LGC_Id_");
         Write_Int (Int (Cycle));
         Write_Str (")");
         Write_Eol;

         Iter := Iterate_Edges_Of_Cycle (G, Cycle);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            Write_Cyclic_Edge (G, Edge);
         end loop;

         Write_Eol;
      end Write_Cycle;

      ------------------
      -- Write_Cycles --
      ------------------

      procedure Write_Cycles (G : Library_Graph) is
         Cycle : Library_Graph_Cycle_Id;
         Iter  : All_Cycle_Iterator;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Cycles (G);
         while Has_Next (Iter) loop
            Next (Iter, Cycle);

            Write_Cycle (G, Cycle);
         end loop;
      end Write_Cycles;

      -----------------------
      -- Write_Cyclic_Edge --
      -----------------------

      procedure Write_Cyclic_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, Edge);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, Edge);

      begin
         Indent_By (Nested_Indentation);
         Write_Name (Name (G, Succ));
         Write_Str  (" --> ");
         Write_Name (Name (G, Pred));
         Write_Str  ("   ");

         if Is_Elaborate_All_Edge (G, Edge) then
            Write_Str ("Elaborate_All edge");

         elsif Is_Elaborate_Body_Edge (G, Edge) then
            Write_Str ("Elaborate_Body edge");

         elsif Is_Elaborate_Edge (G, Edge) then
            Write_Str ("Elaborate edge");

         elsif Is_Forced_Edge (G, Edge) then
            Write_Str ("forced edge");

         elsif Is_Invocation_Edge (G, Edge) then
            Write_Str ("invocation edge");

         else
            pragma Assert (Is_With_Edge (G, Edge));

            Write_Str ("with edge");
         end if;

         Write_Eol;
      end Write_Cyclic_Edge;
   end Cycle_Writers;

   ------------------------
   -- Dependency_Writers --
   ------------------------

   package body Dependency_Writers is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Dependencies_Of_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id);
      pragma Inline (Write_Dependencies_Of_Vertex);
      --  Write the dependencies of vertex Vertex of library graph G to
      --  standard output.

      procedure Write_Dependency_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id);
      pragma Inline (Write_Dependency_Edge);
      --  Write the dependency described by edge Edge of library graph G to
      --  standard output.

      ------------------------
      -- Write_Dependencies --
      ------------------------

      procedure Write_Dependencies (G : Library_Graph) is
         Use_Formatting : constant Boolean := not Zero_Formatting;

         Iter   : Library_Graphs.All_Vertex_Iterator;
         Vertex : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -e (output complete list of elaboration
         --  order dependencies) is not in effect.

         if not Elab_Dependency_Output then
            return;
         end if;

         if Use_Formatting then
            Write_Eol;
            Write_Line ("ELABORATION ORDER DEPENDENCIES");
            Write_Eol;
         end if;

         Info_Prefix_Suppress := True;

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, Vertex);

            Write_Dependencies_Of_Vertex (G, Vertex);
         end loop;

         Info_Prefix_Suppress := False;

         if Use_Formatting then
            Write_Eol;
         end if;
      end Write_Dependencies;

      ----------------------------------
      -- Write_Dependencies_Of_Vertex --
      ----------------------------------

      procedure Write_Dependencies_Of_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id)
      is
         Edge : Library_Graph_Edge_Id;
         Iter : Edges_To_Successors_Iterator;

      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         --  Nothing to do for internal and predefined units

         if Is_Internal_Unit (G, Vertex)
           or else Is_Predefined_Unit (G, Vertex)
         then
            return;
         end if;

         Iter := Iterate_Edges_To_Successors (G, Vertex);
         while Has_Next (Iter) loop
            Next (Iter, Edge);

            Write_Dependency_Edge (G, Edge);
         end loop;
      end Write_Dependencies_Of_Vertex;

      ---------------------------
      -- Write_Dependency_Edge --
      ---------------------------

      procedure Write_Dependency_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, Edge);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, Edge);

      begin
         --  Nothing to do for internal and predefined units

         if Is_Internal_Unit (G, Succ)
           or else Is_Predefined_Unit (G, Succ)
         then
            return;
         end if;

         Error_Msg_Unit_1 := Name (G, Pred);
         Error_Msg_Unit_2 := Name (G, Succ);
         Error_Msg_Output
           (Msg  => "   unit $ must be elaborated before unit $",
            Info => True);

         Error_Msg_Unit_1 := Name (G, Succ);
         Error_Msg_Unit_2 := Name (G, Pred);

         if Is_Elaborate_All_Edge (G, Edge) then
            Error_Msg_Output
              (Msg  =>
                 "     reason: unit $ has with clause and pragma "
                 & "Elaborate_All for unit $",
               Info => True);

         elsif Is_Elaborate_Body_Edge (G, Edge) then
            Error_Msg_Output
              (Msg  => "     reason: unit $ has with clause for unit $",
               Info => True);

         elsif Is_Elaborate_Edge (G, Edge) then
            Error_Msg_Output
              (Msg  =>
                 "     reason: unit $ has with clause and pragma Elaborate "
                 & "for unit $",
               Info => True);

         elsif Is_Forced_Edge (G, Edge) then
            Error_Msg_Output
              (Msg  =>
                 "     reason: unit $ has a dependency on unit $ forced by -f "
                 & "switch",
               Info => True);

         elsif Is_Invocation_Edge (G, Edge) then
            Error_Msg_Output
              (Msg  =>
                 "     reason: unit $ invokes a construct of unit $ at "
                 & "elaboration time",
               Info => True);

         elsif Is_Spec_Before_Body_Edge (G, Edge) then
            Error_Msg_Output
              (Msg  => "     reason: spec must be elaborated before body",
               Info => True);

         else
            pragma Assert (Is_With_Edge (G, Edge));

            Error_Msg_Output
              (Msg  => "     reason: unit $ has with clause for unit $",
               Info => True);
         end if;
      end Write_Dependency_Edge;
   end Dependency_Writers;

   -------------------------------
   -- Elaboration_Order_Writers --
   -------------------------------

   package body Elaboration_Order_Writers is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Unit (U_Id : Unit_Id);
      pragma Inline (Write_Unit);
      --  Write unit U_Id to standard output

      procedure Write_Units (Order : Unit_Id_Table);
      pragma Inline (Write_Units);
      --  Write all units found in elaboration order Order to standard output

      -----------------------------
      -- Write_Elaboration_Order --
      -----------------------------

      procedure Write_Elaboration_Order (Order : Unit_Id_Table) is
         Use_Formatting : constant Boolean := not Zero_Formatting;

      begin
         --  Nothing to do when switch -l (output chosen elaboration order) is
         --  not in effect.

         if not Elab_Order_Output then
            return;
         end if;

         if Use_Formatting then
            Write_Eol;
            Write_Str ("ELABORATION ORDER");
            Write_Eol;
         end if;

         Write_Units (Order);

         if Use_Formatting then
            Write_Eol;
         end if;
      end Write_Elaboration_Order;

      ----------------
      -- Write_Unit --
      ----------------

      procedure Write_Unit (U_Id : Unit_Id) is
         Use_Formatting : constant Boolean := not Zero_Formatting;

      begin
         pragma Assert (Present (U_Id));

         if Use_Formatting then
            Write_Str ("   ");
         end if;

         Write_Unit_Name (Name (U_Id));
         Write_Eol;
      end Write_Unit;

      -----------------
      -- Write_Units --
      -----------------

      procedure Write_Units (Order : Unit_Id_Table) is
      begin
         for Index in Unit_Id_Tables.First .. Unit_Id_Tables.Last (Order) loop
            Write_Unit (Order.Table (Index));
         end loop;
      end Write_Units;
   end Elaboration_Order_Writers;

   ---------------
   -- Indent_By --
   ---------------

   procedure Indent_By (Indent : Indentation_Level) is
   begin
      for Count in 1 .. Indent loop
         Write_Char (' ');
      end loop;
   end Indent_By;

   ------------------------------
   -- Invocation_Graph_Writers --
   ------------------------------

   package body Invocation_Graph_Writers is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Elaboration_Root
        (G    : Invocation_Graph;
         Root : Invocation_Graph_Vertex_Id);
      pragma Inline (Write_Elaboration_Root);
      --  Write elaboration root Root of invocation graph G to standard output

      procedure Write_Elaboration_Roots (G : Invocation_Graph);
      pragma Inline (Write_Elaboration_Roots);
      --  Write all elaboration roots of invocation graph G to standard output

      procedure Write_Invocation_Graph_Edge
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id);
      pragma Inline (Write_Invocation_Graph_Edge);
      --  Write edge Edge of invocation graph G to standard output

      procedure Write_Invocation_Graph_Edges
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id);
      pragma Inline (Write_Invocation_Graph_Edges);
      --  Write all edges to targets of vertex Vertex of invocation graph G to
      --  standard output.

      procedure Write_Invocation_Graph_Vertex
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id);
      pragma Inline (Write_Invocation_Graph_Vertex);
      --  Write vertex Vertex of invocation graph G to standard output

      procedure Write_Invocation_Graph_Vertices (G : Invocation_Graph);
      pragma Inline (Write_Invocation_Graph_Vertices);
      --  Write all vertices of invocation graph G to standard output

      procedure Write_Statistics (G : Invocation_Graph);
      pragma Inline (Write_Statistics);
      --  Write the statistical information of invocation graph G to standard
      --  output.

      -----------
      -- Debug --
      -----------

      procedure pige
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id) renames Write_Invocation_Graph_Edge;
      pragma Unreferenced (pige);

      procedure pigv
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id)
         renames Write_Invocation_Graph_Vertex;
      pragma Unreferenced (pigv);

      ----------------------------
      -- Write_Elaboration_Root --
      ----------------------------

      procedure Write_Elaboration_Root
        (G    : Invocation_Graph;
         Root : Invocation_Graph_Vertex_Id)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Root));

         Write_Str  ("elaboration root (IGV_Id_");
         Write_Int  (Int (Root));
         Write_Str  (") name = ");
         Write_Name (Name (G, Root));
         Write_Eol;
      end Write_Elaboration_Root;

      -----------------------------
      -- Write_Elaboration_Roots --
      -----------------------------

      procedure Write_Elaboration_Roots (G : Invocation_Graph) is
         pragma Assert (Present (G));

         Num_Of_Roots : constant Natural := Number_Of_Elaboration_Roots (G);

         Iter : Elaboration_Root_Iterator;
         Root : Invocation_Graph_Vertex_Id;

      begin
         Write_Str ("Elaboration roots: ");
         Write_Int (Int (Num_Of_Roots));
         Write_Eol;

         if Num_Of_Roots > 0 then
            Iter := Iterate_Elaboration_Roots (G);
            while Has_Next (Iter) loop
               Next (Iter, Root);

               Write_Elaboration_Root (G, Root);
            end loop;
         else
            Write_Eol;
         end if;
      end Write_Elaboration_Roots;

      ----------------------------
      -- Write_Invocation_Graph --
      ----------------------------

      procedure Write_Invocation_Graph (G : Invocation_Graph) is
      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -d_I (output invocation graph) is not in
         --  effect.

         if not Debug_Flag_Underscore_II then
            return;
         end if;

         Write_Str ("Invocation Graph");
         Write_Eol;
         Write_Eol;

         Write_Statistics (G);
         Write_Invocation_Graph_Vertices (G);
         Write_Elaboration_Roots (G);

         Write_Str ("Invocation Graph end");
         Write_Eol;

         Write_Eol;
      end Write_Invocation_Graph;

      ---------------------------------
      -- Write_Invocation_Graph_Edge --
      ---------------------------------

      procedure Write_Invocation_Graph_Edge
        (G    : Invocation_Graph;
         Edge : Invocation_Graph_Edge_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         Targ : constant Invocation_Graph_Vertex_Id := Target (G, Edge);

      begin
         Write_Str ("    invocation graph edge (IGE_Id_");
         Write_Int (Int (Edge));
         Write_Str (")");
         Write_Eol;

         Write_Str ("      Relation (IR_Id_");
         Write_Int (Int (Relation (G, Edge)));
         Write_Str (")");
         Write_Eol;

         Write_Str ("      Target (IGV_Id_");
         Write_Int (Int (Targ));
         Write_Str (") name = ");
         Write_Name (Name (G, Targ));
         Write_Eol;

         Write_Eol;
      end Write_Invocation_Graph_Edge;

      ----------------------------------
      -- Write_Invocation_Graph_Edges --
      ----------------------------------

      procedure Write_Invocation_Graph_Edges
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Num_Of_Edges : constant Natural :=
                          Number_Of_Edges_To_Targets (G, Vertex);

         Edge : Invocation_Graph_Edge_Id;
         Iter : Invocation_Graphs.Edges_To_Targets_Iterator;

      begin
         Write_Str ("  Edges to targets: ");
         Write_Int (Int (Num_Of_Edges));
         Write_Eol;

         if Num_Of_Edges > 0 then
            Iter := Iterate_Edges_To_Targets (G, Vertex);
            while Has_Next (Iter) loop
               Next (Iter, Edge);

               Write_Invocation_Graph_Edge (G, Edge);
            end loop;
         else
            Write_Eol;
         end if;
      end Write_Invocation_Graph_Edges;

      -----------------------------------
      -- Write_Invocation_Graph_Vertex --
      -----------------------------------

      procedure Write_Invocation_Graph_Vertex
        (G      : Invocation_Graph;
         Vertex : Invocation_Graph_Vertex_Id)
      is
         Lib_Graph : constant Library_Graph := Get_Lib_Graph (G);

         B : constant Library_Graph_Vertex_Id := Body_Vertex (G, Vertex);
         S : constant Library_Graph_Vertex_Id := Spec_Vertex (G, Vertex);
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Write_Str  ("invocation graph vertex (IGV_Id_");
         Write_Int  (Int (Vertex));
         Write_Str  (") name = ");
         Write_Name (Name (G, Vertex));
         Write_Eol;

         Write_Str ("  Body_Vertex (LGV_Id_");
         Write_Int (Int (B));
         Write_Str (") name = ");
         Write_Name (Name (Lib_Graph, B));
         Write_Eol;

         Write_Str ("  Construct (IC_Id_");
         Write_Int (Int (Construct (G, Vertex)));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  Spec_Vertex (LGV_Id_");
         Write_Int (Int (S));
         Write_Str (") name = ");
         Write_Name (Name (Lib_Graph, S));
         Write_Eol;

         Write_Invocation_Graph_Edges (G, Vertex);
      end Write_Invocation_Graph_Vertex;

      -------------------------------------
      -- Write_Invocation_Graph_Vertices --
      -------------------------------------

      procedure Write_Invocation_Graph_Vertices (G : Invocation_Graph) is
         Iter   : Invocation_Graphs.All_Vertex_Iterator;
         Vertex : Invocation_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, Vertex);

            Write_Invocation_Graph_Vertex (G, Vertex);
         end loop;
      end Write_Invocation_Graph_Vertices;

      ----------------------
      -- Write_Statistics --
      ----------------------

      procedure Write_Statistics (G : Invocation_Graph) is
      begin
         pragma Assert (Present (G));

         Write_Str ("Edges   : ");
         Write_Num (Int (Number_Of_Edges (G)));
         Write_Eol;

         Write_Str ("Roots   : ");
         Write_Num (Int (Number_Of_Elaboration_Roots (G)));
         Write_Eol;

         Write_Str ("Vertices: ");
         Write_Num (Int (Number_Of_Vertices (G)));
         Write_Eol;
         Write_Eol;

         for Kind in Invocation_Kind'Range loop
            Write_Str ("  ");
            Write_Num (Int (Invocation_Graph_Edge_Count (G, Kind)));
            Write_Str (" - ");
            Write_Str (Kind'Img);
            Write_Eol;
         end loop;

         Write_Eol;
      end Write_Statistics;
   end Invocation_Graph_Writers;

   ---------------------------
   -- Library_Graph_Writers --
   ---------------------------

   package body Library_Graph_Writers is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Component
        (G    : Library_Graph;
         Comp : Component_Id);
      pragma Inline (Write_Component);
      --  Write component Comp of library graph G to standard output

      procedure Write_Component_Vertices
        (G    : Library_Graph;
         Comp : Component_Id);
      pragma Inline (Write_Component_Vertices);
      --  Write all vertices of component Comp of library graph G to standard
      --  output.

      procedure Write_Components (G : Library_Graph);
      pragma Inline (Write_Components);
      --  Write all components of library graph G to standard output

      procedure Write_Edges_To_Successors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id);
      pragma Inline (Write_Edges_To_Successors);
      --  Write all edges to successors of predecessor Vertex of library graph
      --  G to standard output.

      procedure Write_Library_Graph_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id);
      pragma Inline (Write_Library_Graph_Edge);
      --  Write edge Edge of library graph G to standard output

      procedure Write_Library_Graph_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id);
      pragma Inline (Write_Library_Graph_Vertex);
      --  Write vertex Vertex of library graph G to standard output

      procedure Write_Library_Graph_Vertices (G : Library_Graph);
      pragma Inline (Write_Library_Graph_Vertices);
      --  Write all vertices of library graph G to standard output

      procedure Write_Statistics (G : Library_Graph);
      pragma Inline (Write_Statistics);
      --  Write the statistical information of library graph G to standard
      --  output.

      -----------
      -- Debug --
      -----------

      procedure pc
        (G    : Library_Graph;
         Comp : Component_Id) renames Write_Component;
      pragma Unreferenced (pc);

      procedure plge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id) renames Write_Library_Graph_Edge;
      pragma Unreferenced (plge);

      procedure plgv
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id) renames Write_Library_Graph_Vertex;
      pragma Unreferenced (plgv);

      ---------------------
      -- Write_Component --
      ---------------------

      procedure Write_Component
        (G    : Library_Graph;
         Comp : Component_Id)
      is
      begin
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Write_Str ("component (Comp_");
         Write_Int (Int (Comp));
         Write_Str (")");
         Write_Eol;

         Write_Str ("  Pending_Strong_Predecessors = ");
         Write_Int (Int (Pending_Strong_Predecessors (G, Comp)));
         Write_Eol;

         Write_Str ("  Pending_Weak_Predecessors   = ");
         Write_Int (Int (Pending_Weak_Predecessors (G, Comp)));
         Write_Eol;

         Write_Component_Vertices (G, Comp);

         Write_Eol;
      end Write_Component;

      ------------------------------
      -- Write_Component_Vertices --
      ------------------------------

      procedure Write_Component_Vertices
        (G    : Library_Graph;
         Comp : Component_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Comp));

         Num_Of_Vertices : constant Natural :=
                             Number_Of_Component_Vertices (G, Comp);

         Iter   : Component_Vertex_Iterator;
         Vertex : Library_Graph_Vertex_Id;

      begin
         Write_Str ("  Vertices: ");
         Write_Int (Int (Num_Of_Vertices));
         Write_Eol;

         if Num_Of_Vertices > 0 then
            Iter := Iterate_Component_Vertices (G, Comp);
            while Has_Next (Iter) loop
               Next (Iter, Vertex);

               Write_Str  ("    library graph vertex (LGV_Id_");
               Write_Int  (Int (Vertex));
               Write_Str  (") name = ");
               Write_Name (Name (G, Vertex));
               Write_Eol;
            end loop;
         else
            Write_Eol;
         end if;
      end Write_Component_Vertices;

      ----------------------
      -- Write_Components --
      ----------------------

      procedure Write_Components (G : Library_Graph) is
         pragma Assert (Present (G));

         Num_Of_Comps : constant Natural := Number_Of_Components (G);

         Comp : Component_Id;
         Iter : Component_Iterator;

      begin
         --  Nothing to do when switch -d_L (output library item graph) is not
         --  in effect.

         if not Debug_Flag_Underscore_LL then
            return;
         end if;

         Write_Str ("Library Graph components");
         Write_Eol;
         Write_Eol;

         if Num_Of_Comps > 0 then
            Write_Str ("Components: ");
            Write_Num (Int (Num_Of_Comps));
            Write_Eol;

            Iter := Iterate_Components (G);
            while Has_Next (Iter) loop
               Next (Iter, Comp);

               Write_Component (G, Comp);
            end loop;
         else
            Write_Eol;
         end if;

         Write_Str ("Library Graph components end");
         Write_Eol;

         Write_Eol;
      end Write_Components;

      -------------------------------
      -- Write_Edges_To_Successors --
      -------------------------------

      procedure Write_Edges_To_Successors
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Num_Of_Edges : constant Natural :=
                          Number_Of_Edges_To_Successors (G, Vertex);

         Edge : Library_Graph_Edge_Id;
         Iter : Edges_To_Successors_Iterator;

      begin
         Write_Str ("  Edges to successors: ");
         Write_Int (Int (Num_Of_Edges));
         Write_Eol;

         if Num_Of_Edges > 0 then
            Iter := Iterate_Edges_To_Successors (G, Vertex);
            while Has_Next (Iter) loop
               Next (Iter, Edge);

               Write_Library_Graph_Edge (G, Edge);
            end loop;
         else
            Write_Eol;
         end if;
      end Write_Edges_To_Successors;

      -------------------------
      -- Write_Library_Graph --
      -------------------------

      procedure Write_Library_Graph (G : Library_Graph) is
      begin
         pragma Assert (Present (G));

         --  Nothing to do when switch -d_L (output library item graph) is not
         --  in effect.

         if not Debug_Flag_Underscore_LL then
            return;
         end if;

         Write_Str ("Library Graph");
         Write_Eol;
         Write_Eol;

         Write_Statistics (G);
         Write_Library_Graph_Vertices (G);
         Write_Components (G);

         Write_Str ("Library Graph end");
         Write_Eol;

         Write_Eol;
      end Write_Library_Graph;

      ------------------------------
      -- Write_Library_Graph_Edge --
      ------------------------------

      procedure Write_Library_Graph_Edge
        (G    : Library_Graph;
         Edge : Library_Graph_Edge_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Edge));

         Pred : constant Library_Graph_Vertex_Id := Predecessor (G, Edge);
         Succ : constant Library_Graph_Vertex_Id := Successor   (G, Edge);

      begin
         Write_Str ("    library graph edge (LGE_Id_");
         Write_Int (Int (Edge));
         Write_Str (")");
         Write_Eol;

         Write_Str ("      Kind = ");
         Write_Str (Kind (G, Edge)'Img);
         Write_Eol;

         Write_Str  ("      Predecessor (LGV_Id_");
         Write_Int  (Int (Pred));
         Write_Str  (") name = ");
         Write_Name (Name (G, Pred));
         Write_Eol;

         Write_Str  ("      Successor   (LGV_Id_");
         Write_Int  (Int (Succ));
         Write_Str  (") name = ");
         Write_Name (Name (G, Succ));
         Write_Eol;

         Write_Eol;
      end Write_Library_Graph_Edge;

      --------------------------------
      -- Write_Library_Graph_Vertex --
      --------------------------------

      procedure Write_Library_Graph_Vertex
        (G      : Library_Graph;
         Vertex : Library_Graph_Vertex_Id)
      is
         pragma Assert (Present (G));
         pragma Assert (Present (Vertex));

         Item : constant Library_Graph_Vertex_Id :=
                  Corresponding_Item (G, Vertex);
         U_Id : constant Unit_Id := Unit (G, Vertex);

      begin
         Write_Str  ("library graph vertex (LGV_Id_");
         Write_Int  (Int (Vertex));
         Write_Str  (") name = ");
         Write_Name (Name (G, Vertex));
         Write_Eol;

         if Present (Item) then
            Write_Str  ("  Corresponding_Item (LGV_Id_");
            Write_Int  (Int (Item));
            Write_Str  (") name = ");
            Write_Name (Name (G, Item));
         else
            Write_Str ("  Corresponding_Item = none");
         end if;

         Write_Eol;
         Write_Str ("  In_Elaboration_Order = ");

         if In_Elaboration_Order (G, Vertex) then
            Write_Str ("True");
         else
            Write_Str ("False");
         end if;

         Write_Eol;
         Write_Str ("  Pending_Strong_Predecessors = ");
         Write_Int (Int (Pending_Strong_Predecessors (G, Vertex)));
         Write_Eol;

         Write_Str ("  Pending_Weak_Predecessors   = ");
         Write_Int (Int (Pending_Weak_Predecessors (G, Vertex)));
         Write_Eol;

         Write_Str ("  Component (Comp_Id_");
         Write_Int (Int (Component (G, Vertex)));
         Write_Str (")");
         Write_Eol;

         Write_Str  ("  Unit (U_Id_");
         Write_Int  (Int (U_Id));
         Write_Str  (") name = ");
         Write_Name (Name (U_Id));
         Write_Eol;

         Write_Edges_To_Successors (G, Vertex);
      end Write_Library_Graph_Vertex;

      ----------------------------------
      -- Write_Library_Graph_Vertices --
      ----------------------------------

      procedure Write_Library_Graph_Vertices (G : Library_Graph) is
         Iter   : Library_Graphs.All_Vertex_Iterator;
         Vertex : Library_Graph_Vertex_Id;

      begin
         pragma Assert (Present (G));

         Iter := Iterate_All_Vertices (G);
         while Has_Next (Iter) loop
            Next (Iter, Vertex);

            Write_Library_Graph_Vertex (G, Vertex);
         end loop;
      end Write_Library_Graph_Vertices;

      ----------------------
      -- Write_Statistics --
      ----------------------

      procedure Write_Statistics (G : Library_Graph) is
      begin
         Write_Str ("Components: ");
         Write_Num (Int (Number_Of_Components (G)));
         Write_Eol;

         Write_Str ("Edges     : ");
         Write_Num (Int (Number_Of_Edges (G)));
         Write_Eol;

         Write_Str ("Vertices  : ");
         Write_Num (Int (Number_Of_Vertices (G)));
         Write_Eol;
         Write_Eol;

         for Kind in Library_Graph_Edge_Kind'Range loop
            Write_Str ("  ");
            Write_Num (Int (Library_Graph_Edge_Count (G, Kind)));
            Write_Str (" - ");
            Write_Str (Kind'Img);
            Write_Eol;
         end loop;

         Write_Eol;
      end Write_Statistics;
   end Library_Graph_Writers;

   -------------------
   -- Phase_Writers --
   -------------------

   package body Phase_Writers is

      subtype Phase_Message is String (1 .. 32);

      --  The following table contains the phase-specific messages for phase
      --  completion.

      End_Messages : constant array (Elaboration_Phase) of Phase_Message :=
        (Component_Discovery           => "components discovered.          ",
         Cycle_Diagnostics             => "cycle diagnosed.                ",
         Cycle_Discovery               => "cycles discovered.              ",
         Cycle_Validation              => "cycles validated.               ",
         Elaboration_Order_Validation  => "elaboration order validated.    ",
         Invocation_Graph_Construction => "invocation graph constructed.   ",
         Invocation_Graph_Validation   => "invocation graph validated.     ",
         Library_Graph_Augmentation    => "library graph augmented.        ",
         Library_Graph_Construction    => "library graph constructed.      ",
         Library_Graph_Elaboration     => "library graph elaborated.       ",
         Library_Graph_Validation      => "library graph validated.        ",
         Unit_Collection               => "units collected.                ",
         Unit_Elaboration              => "units elaborated.               ");

      --  The following table contains the phase-specific messages for phase
      --  commencement.

      Start_Messages : constant array (Elaboration_Phase) of Phase_Message :=
        (Component_Discovery           => "discovering components...       ",
         Cycle_Diagnostics             => "diagnosing cycle...             ",
         Cycle_Discovery               => "discovering cycles...           ",
         Cycle_Validation              => "validating cycles...            ",
         Elaboration_Order_Validation  => "validating elaboration order... ",
         Invocation_Graph_Construction => "constructing invocation graph...",
         Invocation_Graph_Validation   => "validating invocation graph...  ",
         Library_Graph_Augmentation    => "augmenting library graph...     ",
         Library_Graph_Construction    => "constructing library graph...   ",
         Library_Graph_Elaboration     => "elaborating library graph...    ",
         Library_Graph_Validation      => "validating library graph...     ",
         Unit_Collection               => "collecting units...             ",
         Unit_Elaboration              => "elaborating units...            ");

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_Phase_Message (Msg : Phase_Message);
      pragma Inline (Write_Phase_Message);
      --  Write elaboration phase-related message Msg to standard output

      ---------------
      -- End_Phase --
      ---------------

      procedure End_Phase (Phase : Elaboration_Phase) is
      begin
         Write_Phase_Message (End_Messages (Phase));
      end End_Phase;

      -----------------
      -- Start_Phase --
      -----------------

      procedure Start_Phase (Phase : Elaboration_Phase) is
      begin
         Write_Phase_Message (Start_Messages (Phase));
      end Start_Phase;

      -------------------------
      -- Write_Phase_Message --
      -------------------------

      procedure Write_Phase_Message (Msg : Phase_Message) is
      begin
         --  Nothing to do when switch -d_S (output elaboration order status)
         --  is not in effect.

         if not Debug_Flag_Underscore_SS then
            return;
         end if;

         Write_Str (Msg);
         Write_Eol;
      end Write_Phase_Message;
   end Phase_Writers;

   --------------------------
   -- Unit_Closure_Writers --
   --------------------------

   package body Unit_Closure_Writers is
      function Hash_File_Name (Nam : File_Name_Type) return Bucket_Range_Type;
      pragma Inline (Hash_File_Name);
      --  Obtain the hash value of key Nam

      package File_Name_Tables is new Membership_Sets
        (Element_Type => File_Name_Type,
         "="          => "=",
         Hash         => Hash_File_Name);
      use File_Name_Tables;

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Write_File_Name (Nam : File_Name_Type);
      pragma Inline (Write_File_Name);
      --  Write file name Nam to standard output

      procedure Write_Subunit_Closure
        (Dep : Sdep_Id;
         Set : Membership_Set);
      pragma Inline (Write_Subunit_Closure);
      --  Write the subunit which corresponds to dependency Dep to standard
      --  output if it does not appear in set Set.

      procedure Write_Subunits_Closure (Set : Membership_Set);
      pragma Inline (Write_Subunits_Closure);
      --  Write all subunits to standard output if they do not appear in set
      --  Set.

      procedure Write_Unit_Closure
        (U_Id : Unit_Id;
         Set  : Membership_Set);
      pragma Inline (Write_Unit_Closure);
      --  Write unit U_Id to standard output if it does not appear in set Set

      procedure Write_Units_Closure
        (Order : Unit_Id_Table;
         Set   : Membership_Set);
      pragma Inline (Write_Units_Closure);
      --  Write all units of elaboration order Order to standard output if they
      --  do not appear in set Set.

      --------------------
      -- Hash_File_Name --
      --------------------

      function Hash_File_Name
        (Nam : File_Name_Type) return Bucket_Range_Type
      is
      begin
         pragma Assert (Present (Nam));

         return Bucket_Range_Type (abs Nam);
      end Hash_File_Name;

      ---------------------
      -- Write_File_Name --
      ---------------------

      procedure Write_File_Name (Nam : File_Name_Type) is
         Use_Formatting : constant Boolean := not Zero_Formatting;

      begin
         pragma Assert (Present (Nam));

         if Use_Formatting then
            Write_Str ("   ");
         end if;

         Write_Line (Get_Name_String (Nam));
      end Write_File_Name;

      ---------------------------
      -- Write_Subunit_Closure --
      ---------------------------

      procedure Write_Subunit_Closure
        (Dep : Sdep_Id;
         Set : Membership_Set)
      is
         pragma Assert (Present (Dep));
         pragma Assert (Present (Set));

         Dep_Rec : Sdep_Record renames Sdep.Table (Dep);
         Source  : constant File_Name_Type := Dep_Rec.Sfile;

         pragma Assert (Present (Source));

      begin
         --  Nothing to do when the source file has already been written

         if Contains (Set, Source) then
            return;

         --  Nothing to do when the source file does not denote a non-internal
         --  subunit.

         elsif not Present (Dep_Rec.Subunit_Name)
           or else Is_Internal_File_Name (Source)
         then
            return;
         end if;

         --  Mark the subunit as written

         Insert (Set, Source);
         Write_File_Name (Source);
      end Write_Subunit_Closure;

      ----------------------------
      -- Write_Subunits_Closure --
      ----------------------------

      procedure Write_Subunits_Closure (Set : Membership_Set) is
      begin
         pragma Assert (Present (Set));

         for Dep in Sdep.First .. Sdep.Last loop
            Write_Subunit_Closure (Dep, Set);
         end loop;
      end Write_Subunits_Closure;

      ------------------------
      -- Write_Unit_Closure --
      ------------------------

      procedure Write_Unit_Closure (Order : Unit_Id_Table) is
         Use_Formatting : constant Boolean := not Zero_Formatting;

         Set : Membership_Set;

      begin
         --  Nothing to do when switch -R (list sources referenced in closure)
         --  is not in effect.

         if not List_Closure then
            return;
         end if;

         if Use_Formatting then
            Write_Eol;
            Write_Line ("REFERENCED SOURCES");
         end if;

         --  Use a set to avoid writing duplicate units and subunits

         Set := Create (Number_Of_Elaborable_Units);

         Write_Units_Closure (Order, Set);
         Write_Subunits_Closure (Set);

         Destroy (Set);

         if Use_Formatting then
            Write_Eol;
         end if;
      end Write_Unit_Closure;

      ------------------------
      -- Write_Unit_Closure --
      ------------------------

      procedure Write_Unit_Closure
        (U_Id : Unit_Id;
         Set  : Membership_Set)
      is
         pragma Assert (Present (U_Id));
         pragma Assert (Present (Set));

         U_Rec  : Unit_Record renames ALI.Units.Table (U_Id);
         Source : constant File_Name_Type := U_Rec.Sfile;

         pragma Assert (Present (Source));

      begin
         --  Nothing to do when the source file has already been written

         if Contains (Set, Source) then
            return;

         --  Nothing to do for internal source files unless switch -Ra is in
         --  effect.

         elsif Is_Internal_File_Name (Source)
           and then not List_Closure_All
         then
            return;
         end if;

         --  Mark the source file as written

         Insert (Set, Source);
         Write_File_Name (Source);
      end Write_Unit_Closure;

      -------------------------
      -- Write_Units_Closure --
      -------------------------

      procedure Write_Units_Closure
        (Order : Unit_Id_Table;
         Set   : Membership_Set)
      is
      begin
         pragma Assert (Present (Set));

         for Index in reverse Unit_Id_Tables.First ..
                              Unit_Id_Tables.Last (Order)
         loop
            Write_Unit_Closure
              (U_Id => Order.Table (Index),
               Set  => Set);
         end loop;
      end Write_Units_Closure;
   end Unit_Closure_Writers;

   ---------------
   -- Write_Num --
   ---------------

   procedure Write_Num
     (Val        : Int;
      Val_Indent : Indentation_Level := Number_Column)
   is
      function Digits_Indentation return Indentation_Level;
      pragma Inline (Digits_Indentation);
      --  Determine the level of indentation the number requires in order to
      --  be right-justified by Val_Indent.

      ------------------------
      -- Digits_Indentation --
      ------------------------

      function Digits_Indentation return Indentation_Level is
         Indent : Indentation_Level;
         Num    : Int;

      begin
         --  Treat zero as a single digit

         if Val = 0 then
            Indent := 1;

         else
            Indent := 0;
            Num    := Val;

            --  Shrink the input value by dividing it until all of its digits
            --  are exhausted.

            while Num /= 0 loop
               Indent := Indent + 1;
               Num    := Num / 10;
            end loop;
         end if;

         return Val_Indent - Indent;
      end Digits_Indentation;

   --  Start of processing for Write_Num

   begin
      Indent_By (Digits_Indentation);
      Write_Int (Val);
   end Write_Num;

end Bindo.Writers;
