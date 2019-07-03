------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D O                                 --
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

with Bindo.Elaborators;
use  Bindo.Elaborators.Invocation_And_Library_Graph_Elaborators;

package body Bindo is

   ---------------------------------
   -- Elaboration order mechanism --
   ---------------------------------

   --  The elaboration order (EO) mechanism implemented in this unit and its
   --  children has the following objectives:
   --
   --    * Find an ordering of all library items (historically referred to as
   --      "units") in the bind which require elaboration, taking into account:
   --
   --        - The dependencies between units expressed in the form of with
   --          clauses.
   --
   --        - Pragmas Elaborate, Elaborate_All, Elaborate_Body, Preelaborable,
   --          and Pure.
   --
   --        - The flow of execution at elaboration time.
   --
   --        - Additional dependencies between units supplied to the binder by
   --          means of a file.
   --
   --      The high-level idea is to construct two graphs:
   --
   --        - Invocation graph - Models the flow of execution at elaboration
   --          time.
   --
   --        - Library graph - Represents with clause and pragma dependencies
   --          between units.
   --
   --      The library graph is further augmented with additional information
   --      from the invocation graph by exploring the execution paths from a
   --      unit with elaboration code to other external units. All strongly
   --      connected components of the library graph are discovered. Finally,
   --      the order is obtained via a topological sort-like algorithm which
   --      attempts to order available units while enabling other units to be
   --      ordered.
   --
   --    * Diagnose elaboration circularities between units
   --
   --      The library graph may contain at least one cycle, in which case no
   --      ordering is possible.
   --
   --      ??? more on this later

   -----------------
   -- Terminology --
   -----------------

   --  * Component - A strongly connected component of a graph.
   --
   --  * Elaboration root - A special invocation construct which denotes the
   --    elaboration procedure of a unit.
   --
   --  * Invocation - The act of activating a task, calling a subprogram, or
   --    instantiating a generic.
   --
   --  * Invocation construct - An entry declaration, [single] protected type,
   --    subprogram declaration, subprogram instantiation, or a [single] task
   --    type declared in the visible, private, or body declarations of some
   --    unit. The construct is encoded in the ALI file of the related unit.
   --
   --  * Invocation graph - A directed graph which models the flow of execution
   --    at elaboration time.
   --
   --      - Vertices - Invocation constructs plus extra information. Certain
   --        vertices act as elaboration roots.
   --
   --      - Edges - Invocation relations plus extra information.
   --
   --  * Invocation relation - A flow link between two invocation constructs.
   --    This link is encoded in the ALI file of unit that houses the invoker.
   --
   --  * Invocation signature - A set of attributes that uniquely identify an
   --    invocation construct within the namespace of all ALI files.
   --
   --  * Invoker - The source construct of an invocation relation (the caller,
   --    instantiator, or task activator).
   --
   --  * Library graph - A directed graph which captures with clause and pragma
   --    dependencies between units.
   --
   --      - Vertices - Units plus extra information.
   --
   --      - Edges - With clause, pragma, and additional dependencies between
   --        units.
   --
   --  * Pending predecessor - A vertex that must be elaborated before another
   --    vertex can be elaborated.
   --
   --  * Target - The destination construct of an invocation relation (the
   --    generic, subprogram, or task type).

   ------------------
   -- Architecture --
   ------------------

   --     Find_Elaboration_Order
   --     |
   --     +--> Collect_Elaborable_Units
   --     +--> Write_ALI_Tables
   --     +--> Elaborate_Units
   --          |
   --  +------ | -------------- Construction phase ------------------------+
   --  |       |                                                           |
   --  |       +--> Build_Library_Graph                                    |
   --  |       +--> Validate_Library_Graph                                 |
   --  |       +--> Write_Library_Graph                                    |
   --  |       |                                                           |
   --  |       +--> Build_Invocation_Graph                                 |
   --  |       +--> Validate_Invocation_Graph                              |
   --  |       +--> Write_Invocation_Graph                                 |
   --  |       |                                                           |
   --  +------ | ----------------------------------------------------------+
   --          |
   --  +------ | -------------- Augmentation phase ------------------------+
   --  |       |                                                           |
   --  |       +--> Augment_Library_Graph                                  |
   --  |       |                                                           |
   --  +------ | ----------------------------------------------------------+
   --          |
   --  +------ | -------------- Ordering phase ----------------------------+
   --  |       |                                                           |
   --  |       +--> Find_Components                                        |
   --  |       |                                                           |
   --  |       +--> Elaborate_Library_Graph                                |
   --  |       +--> Validate_Elaboration_Order                             |
   --  |       +--> Write_Elaboration_Order                                |
   --  |       |                                                           |
   --  |       +--> Write_Unit_Closure                                     |
   --  |       |                                                           |
   --  +------ | ----------------------------------------------------------+
   --          |
   --  +------ | -------------- Diagnostics phase -------------------------+
   --  |       |                                                           |
   --  |       +--> ??? more on this later                                 |
   --  |                                                                   |
   --  +-------------------------------------------------------------------+

   ------------------------
   -- Construction phase --
   ------------------------

   --  The Construction phase has the following objectives:
   --
   --    * Build the library graph by inspecting the ALI file of each unit that
   --      requires elaboration.
   --
   --    * Validate the consistency of the library graph, only when switch -d_V
   --      is in effect.
   --
   --    * Write the contents of the invocation graph in human-readable form to
   --      standard output when switch -d_L is in effect.
   --
   --    * Build the invocation graph by inspecting invocation constructs and
   --      relations in the ALI file of each unit that requires elaboration.
   --
   --    * Validate the consistency of the invocation graph, only when switch
   --      -d_V is in effect.
   --
   --    * Write the contents of the invocation graph in human-readable form to
   --      standard output when switch -d_I is in effect.

   ------------------------
   -- Augmentation phase --
   ------------------------

   --  The Augmentation phase has the following objectives:
   --
   --    * Discover transitions of the elaboration flow from a unit with an
   --      elaboration root to other units. Augment the library graph with
   --      extra edges for each such transition.

   --------------------
   -- Ordering phase --
   --------------------

   --  The Ordering phase has the following objectives:
   --
   --    * Discover all components of the library graph by treating specs and
   --      bodies as single vertices.
   --
   --    * Try to order as many vertices of the library graph as possible by
   --      peforming a topological sort based on the pending predecessors of
   --      vertices across all components and within a single component.
   --
   --    * Validate the consistency of the order, only when switch -d_V is in
   --      effect.
   --
   --    * Write the contents of the order in human-readable form to standard
   --      output when switch -d_O is in effect.
   --
   --    * Write the sources of the order closure when switch -R is in effect.

   -----------------------
   -- Diagnostics phase --
   -----------------------

   --  ??? more on this later

   --------------
   -- Switches --
   --------------

   --  -d_A  Output ALI invocation tables
   --
   --        GNATbind outputs the contents of ALI table Invocation_Constructs
   --        and Invocation_Edges in textual format to standard output.
   --
   --  -d_I  Output invocation graph
   --
   --        GNATbind outputs the invocation graph in text format to standard
   --        output.
   --
   --  -d_L  Output library graph
   --
   --        GNATbind outputs the library graph in textual format to standard
   --        output.
   --
   --  -d_N  New bindo order
   --
   --        GNATbind utilizes the new bindo elaboration order
   --
   --  -d_O  Output elaboration order
   --
   --        GNATbind outputs the elaboration order in text format to standard
   --        output.
   --
   --  -d_T  Output elaboration order trace information
   --
   --        GNATbind outputs trace information on elaboration order activities
   --        to standard output.
   --
   --  -d_V  Validate bindo graphs and order
   --
   --        GNATbind validates the invocation graph, library graph, SCC graph
   --        and elaboration order by detecting inconsistencies and producing
   --        error reports.

   ----------------------------------------
   -- Debugging elaboration order issues --
   ----------------------------------------

   --  ??? more on this later

   ----------------------------
   -- Find_Elaboration_Order --
   ----------------------------

   procedure Find_Elaboration_Order
     (Order         : out Unit_Id_Table;
      Main_Lib_File : File_Name_Type)
   is
   begin
      Elaborate_Units (Order, Main_Lib_File);
   end Find_Elaboration_Order;

end Bindo;
