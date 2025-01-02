------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     B I N D O . D I A G N O S T I C S                    --
--                                                                          --
--                                 S p e c                                  --
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

--  For full architecture, see unit Bindo.

--  The following unit contains facilities to diagnose various issues with the
--  elaboration order.

with Bindo.Graphs;
use  Bindo.Graphs;
use  Bindo.Graphs.Invocation_Graphs;
use  Bindo.Graphs.Library_Graphs;

package Bindo.Diagnostics is

   -----------
   -- Types --
   -----------

   --  The following type enumerates all possible statuses of the elaboration
   --  order.

   type Elaboration_Order_Status is
     (Order_Has_Circularity,
      Order_Has_Elaborate_All_Circularity,
      Order_OK);

   ---------
   -- API --
   ---------

   procedure Diagnose_Circularities (Inv_Graph : Invocation_Graph);
   pragma Inline (Diagnose_Circularities);
   --  Diagnose all cycles of the library graph of Inv_Graph with matching
   --  invocation graph Inv_Graph.

end Bindo.Diagnostics;
