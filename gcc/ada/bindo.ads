------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D O                                 --
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

--  The following unit contains the main entry point into the elaboration order
--  mechanism. See the body for details.

with ALI;   use ALI;
with Namet; use Namet;

package Bindo is

   procedure Find_Elaboration_Order
     (Order         : out Unit_Id_Table;
      Main_Lib_File : File_Name_Type);
   --  Find an order of all units in the bind that need to be elaborated
   --  such that elaboration code flow, pragmas Elaborate, Elaborate_All,
   --  and Elaborate_Body, and with clause dependencies are all honoured.
   --  Main_Lib_File is the argument of the bind. If a satisfactory order
   --  exists, it is returned in Order, otherwise Unrecoverable_Error is
   --  raised.

private

   --  The following type represents the various kinds of precedence between
   --  two items.

   type Precedence_Kind is
     (Lower_Precedence,
      Equal_Precedence,
      Higher_Precedence);

end Bindo;
