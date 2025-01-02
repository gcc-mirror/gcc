------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B C H E C K                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

package Bcheck is

--  This package contains the routines to perform binder consistency checks

   procedure Check_Duplicated_Subunits;
   --  Check that no subunit names duplicate names of other packages in
   --  the partition (check required by RM 10.2(19)).

   procedure Check_Versions;
   --  Check correct library and standard versions used

   procedure Check_Consistency;
   --  This procedure performs checks that the ALI files are consistent
   --  with the corresponding source files and with one another. At the
   --  time this is called, the Source table has been completely built and
   --  contains either the time stamp from the actual source file if the
   --  Check_Source_Files mode is set, or the latest stamp found in any of
   --  the ALI files in the program.

   procedure Check_Configuration_Consistency;
   --  This procedure performs a similar check that configuration pragma
   --  set items that are required to be consistent are in fact consistent

end Bcheck;
