------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A D A B K E N D                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  Generic package implementing the common parts of back_end.adb for back ends
--  written in Ada, thereby reducing code duplication.

with Types;

generic
   Product_Name    : String;
   Copyright_Years : String;

   with procedure Driver (Root : Types.Node_Id);
   --  Main driver procedure for back end

   with function Is_Back_End_Switch (Switch : String) return Boolean;
   --  Back-end specific function to determine validity of switches

package Adabkend is

   procedure Call_Back_End;
   --  Call back end, i.e. make call to the Driver passing the root
   --  node for this compilation unit.

   procedure Scan_Compiler_Arguments;
   --  Acquires command-line parameters passed to the compiler and processes
   --  them. Calls Scan_Front_End_Switches for any front-end switches
   --  encountered. See spec of Back_End for more details.

end Adabkend;
