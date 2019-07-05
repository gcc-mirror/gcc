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

with Binde;
with Debug; use Debug;

with Bindo.Elaborators;
use  Bindo.Elaborators;

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
   --          means of a forced-elaboration-order file.
   --
   --      The high-level idea empoyed by the EO mechanism is to construct two
   --      graphs and use the information they represent to find an ordering of
   --      all units.
   --
   --      The invocation graph represents the flow of execution at elaboration
   --      time.
   --
   --      The library graph captures the dependencies between units expressed
   --      by with clause and elaboration-related pragmas. The library graph is
   --      further augmented with additional information from the invocation
   --      graph by exploring the execution paths from a unit with elaboration
   --      code to other external units.
   --
   --      The strongly connected components of the library graph are computed.
   --
   --      The order is obtained using a topological sort-like algorithm which
   --      traverses the library graph and its strongly connected components in
   --      an attempt to order available units while enabling other units to be
   --      ordered.
   --
   --    * Diagnose elaboration circularities between units
   --
   --      An elaboration circularity arrises when either
   --
   --        - At least one unit cannot be ordered, or
   --
   --        - All units can be ordered, but an edge with an Elaborate_All
   --          pragma links two vertices within the same component of the
   --          library graph.
   --
   --      The library graph is traversed to discover, collect, and sort all
   --      cycles that hinder the elaboration order.
   --
   --      The most important cycle is diagnosed by describing its effects on
   --      the elaboration order and listing all units comprising the circuit.
   --      Various suggestions on how to break the cycle are offered.

   -----------------
   -- Terminology --
   -----------------

   --  * Component - A strongly connected component of a graph.
   --
   --  * Elaboration circularity - A cycle involving units from the bind.
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
   --  |       +--> Find_Cycles                                            |
   --  |       +--> Validate_Cycles                                        |
   --  |       +--> Write_Cycles                                           |
   --  |       |                                                           |
   --  |       +--> Diagnose_Cycle / Diagnose_All_Cycles                   |
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

   --  The Diagnostics phase has the following objectives:
   --
   --    * Discover, save, and sort all cycles in the library graph. The cycles
   --      are sorted based on the following heiristics:
   --
   --        - A cycle with higher precedence is preferred.
   --
   --        - A cycle with fewer invocation edges is preferred.
   --
   --        - A cycle with a shorter length is preferred.
   --
   --    * Validate the consistency of cycles, only when switch -d_V is in
   --      effect.
   --
   --    * Write the contents of all cycles in human-readable form to standard
   --      output when switch -d_O is in effect.
   --
   --    * Diagnose the most important cycle, or all cycles when switch -d_C is
   --      in effect. The diagnostic consists of:
   --
   --        - The reason for the existance of the cycle, along with the unit
   --          whose elaboration cannot be guaranteed.
   --
   --        - A detailed traceback of the cycle, showcasing the transition
   --          between units, along with any other elaboration order-related
   --          information.
   --
   --        - A set of suggestions on how to break the cycle considering the
   --          the edges coprising the circuit, the elaboration model used to
   --          compile the units, the availability of invocation information,
   --          and the state of various relevant switches.

   --------------
   -- Switches --
   --------------

   --  -d_A  Output ALI invocation tables
   --
   --        GNATbind outputs the contents of ALI table Invocation_Constructs
   --        and Invocation_Edges in textual format to standard output.
   --
   --  -d_C  Diagnose all cycles
   --
   --        GNATbind outputs diagnostics for all unique cycles in the bind,
   --        rather than just the most important one.
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
   --  -d_P  Output cycle paths
   --
   --        GNATbind output the cycle paths in text format to standard output
   --
   --  -d_T  Output elaboration order trace information
   --
   --        GNATbind outputs trace information on elaboration order and cycle
   --        detection activities to standard output.
   --
   --  -d_V  Validate bindo cycles, graphs, and order
   --
   --        GNATbind validates the invocation graph, library graph along with
   --        its cycles, and elaboration order by detecting inconsistencies and
   --        producing error reports.

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
      --  Use the invocation and library graph-based elaboration order when
      --  switch -d_N (new bindo order) is in effect.

      if Debug_Flag_Underscore_NN then
         Invocation_And_Library_Graph_Elaborators.Elaborate_Units
           (Order         => Order,
            Main_Lib_File => Main_Lib_File);

      --  Otherwise use the library graph and heuristic-based elaboration
      --  order.

      else
         Binde.Find_Elab_Order (Order, Main_Lib_File);
      end if;
   end Find_Elaboration_Order;

end Bindo;
