------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M L I B                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1999-2001, Ada Core Technologies, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the core high level routines used by GNATMLIB
--  and GNATMAKE to build libraries

with GNAT.OS_Lib; use GNAT.OS_Lib;

package MLib is

   Tools_Error : exception;
   --  ??? needs comment

   Max_Characters_In_Library_Name : constant := 20;
   --  ??? needs comment

   procedure Check_Library_Name (Name : String);
   --  Verify that the name of a library has the following characteristics
   --   - starts with a letter
   --   - includes only letters and digits
   --   - contains not more than Max_Characters_In_Library_Name characters

   procedure Build_Library
     (Ofiles      : Argument_List;
      Afiles      : Argument_List;
      Output_File : String;
      Output_Dir  : String);
   --  Build a static library from a set of object files

end MLib;
