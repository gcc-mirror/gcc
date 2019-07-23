------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      B I N D O . V A L I D A T O R S                     --
--                                                                          --
--                                 S p e c                                  --
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

--  For full architecture, see unit Bindo.

--  The following unit contains facilities to verify the validity of the
--  various graphs used in determining the elaboration order of units.

with Bindo.Graphs;
use  Bindo.Graphs;
use  Bindo.Graphs.Invocation_Graphs;
use  Bindo.Graphs.Library_Graphs;

package Bindo.Validators is

   ----------------------
   -- Cycle_Validators --
   ----------------------

   package Cycle_Validators is
      Invalid_Cycle : exception;
      --  Exception raised when the library graph contains an invalid cycle

      procedure Validate_Cycles (G : Library_Graph);
      --  Ensure that all cycles of library graph G meet the following
      --  requirements:
      --
      --    * Are of proper kind
      --    * Have enough edges to form a circuit
      --    * No edge is repeated
      --
      --  Diagnose issues and raise Invalid_Cycle if this is not the case.

   end Cycle_Validators;

   ----------------------------------
   -- Elaboration_Order_Validators --
   ----------------------------------

   package Elaboration_Order_Validators is
      Invalid_Elaboration_Order : exception;
      --  Exception raised when the elaboration order contains invalid data

      procedure Validate_Elaboration_Order (Order : Unit_Id_Table);
      --  Ensure that elaboration order Order meets the following requirements:
      --
      --    * All units that must be elaborated appear in the order
      --    * No other units appear in the order
      --
      --  Diagnose issues and raise Invalid_Elaboration_Order if this is not
      --  the case.

   end Elaboration_Order_Validators;

   ---------------------------------
   -- Invocation_Graph_Validators --
   ---------------------------------

   package Invocation_Graph_Validators is
      Invalid_Invocation_Graph : exception;
      --  Exception raised when the invocation graph contains invalid data

      procedure Validate_Invocation_Graph (G : Invocation_Graph);
      --  Ensure that invocation graph G meets the following requirements:
      --
      --    * All attributes of edges are properly set
      --    * All attributes of vertices are properly set
      --
      --  Diagnose issues and raise Invalid_Invocation_Graph if this is not the
      --  case.

   end Invocation_Graph_Validators;

   ------------------------------
   -- Library_Graph_Validators --
   ------------------------------

   package Library_Graph_Validators is
      Invalid_Library_Graph : exception;
      --  Exception raised when the library graph contains invalid data

      procedure Validate_Library_Graph (G : Library_Graph);
      --  Ensure that library graph G meets the following requirements:
      --
      --    * All attributes edges are properly set
      --    * All attributes of vertices are properly set
      --
      --  Diagnose issues and raise Invalid_Library_Graph if this is not the
      --  case.

   end Library_Graph_Validators;

end Bindo.Validators;
