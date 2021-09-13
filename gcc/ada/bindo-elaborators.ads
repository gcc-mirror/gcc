------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     B I N D O . E L A B O R A T O R S                    --
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

--  The following unit contains facilities to find the elaboration order of
--  units based on various graphs.

with Bindo.Graphs;
use  Bindo.Graphs;
use  Bindo.Graphs.Invocation_Graphs;
use  Bindo.Graphs.Library_Graphs;

package Bindo.Elaborators is

   ----------------------------------------------
   -- Invocation_And_Library_Graph_Elaborators --
   ----------------------------------------------

   package Invocation_And_Library_Graph_Elaborators is
      procedure Elaborate_Units
        (Order         : out Unit_Id_Table;
         Main_Lib_File : File_Name_Type);
      --  Find an order of all units in the bind that need to be elaborated
      --  such that elaboration code flow, pragmas Elaborate, Elaborate_All,
      --  and Elaborate_Body, and with clause dependencies are all honoured.
      --  Main_Lib_File is the argument of the bind. If a satisfactory order
      --  exists, it is returned in Order, otherwise Unrecoverable_Error is
      --  raised.

   end Invocation_And_Library_Graph_Elaborators;

end Bindo.Elaborators;
