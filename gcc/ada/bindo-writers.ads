------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        B I N D O . W R I T E R S                         --
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

--  The following unit contains facilities to output the various graphs used in
--  determining the elaboration order, as well as the elaboration order itself
--  to standard output.

with Types; use Types;

with Bindo.Graphs;
use  Bindo.Graphs;
use  Bindo.Graphs.Invocation_Graphs;
use  Bindo.Graphs.Library_Graphs;

package Bindo.Writers is

   -----------------
   -- Indentation --
   -----------------

   --  The following type defines the level of indentation used in various
   --  output routines.

   type Indentation_Level is new Natural;
   No_Indentation : constant Indentation_Level := Indentation_Level'First;

   Nested_Indentation : constant Indentation_Level := 2;
   --  The level of indentation for a nested new line

   Number_Column : constant Indentation_Level := 6;
   --  The level of right justification of numbers

   Step_Column : constant Indentation_Level := 4;
   --  The level of right justification of the elaboration order step

   procedure Indent_By (Indent : Indentation_Level);
   pragma Inline (Indent_By);
   --  Indent the current line by Indent spaces

   procedure Write_Num
     (Val        : Int;
      Val_Indent : Indentation_Level := Number_Column);
   pragma Inline (Write_Num);
   --  Output integer value Val in a right-justified form based on the value of
   --  Val_Col.

   -----------------
   -- ALI_Writers --
   -----------------

   package ALI_Writers is
      procedure Write_ALI_Tables;
      --  Write the contents of the following tables to standard output:
      --
      --    * ALI.Invocation_Constructs
      --    * ALI.Invocation_Relations

   end ALI_Writers;

   -------------------
   -- Cycle_Writers --
   -------------------

   package Cycle_Writers is
      procedure Write_Cycles (G : Library_Graph);
      --  Write all cycles of library graph G to standard output

   end Cycle_Writers;

   ------------------------
   -- Dependency_Writers --
   ------------------------

   package Dependency_Writers is
      procedure Write_Dependencies (G : Library_Graph);
      --  Write all elaboration dependencies of the units represented by
      --  vertices of library graph G.

   end Dependency_Writers;

   -------------------------------
   -- Elaboration_Order_Writers --
   -------------------------------

   package Elaboration_Order_Writers is
      procedure Write_Elaboration_Order (Order : Unit_Id_Table);
      --  Write elaboration order Order to standard output

   end Elaboration_Order_Writers;

   ------------------------------
   -- Invocation_Graph_Writers --
   ------------------------------

   package Invocation_Graph_Writers is
      procedure Write_Invocation_Graph (G : Invocation_Graph);
      --  Write invocation graph G to standard output

   end Invocation_Graph_Writers;

   ---------------------------
   -- Library_Graph_Writers --
   ---------------------------

   package Library_Graph_Writers is
      procedure Write_Library_Graph (G : Library_Graph);
      --  Write library graph G to standard output

   end Library_Graph_Writers;

   -------------------
   -- Phase_Writers --
   -------------------

   package Phase_Writers is
      procedure End_Phase (Phase : Elaboration_Phase);
      pragma Inline (End_Phase);
      --  Write the end message associated with elaboration phase Phase to
      --  standard output.

      procedure Start_Phase (Phase : Elaboration_Phase);
      pragma Inline (Start_Phase);
      --  Write the start message associated with elaboration phase Phase to
      --  standard output.

   end Phase_Writers;

   --------------------------
   -- Unit_Closure_Writers --
   --------------------------

   package Unit_Closure_Writers is
      procedure Write_Unit_Closure (Order : Unit_Id_Table);
      --  Write all sources in the closure of the main unit as enumerated in
      --  elaboration order Order.

   end Unit_Closure_Writers;

end Bindo.Writers;
