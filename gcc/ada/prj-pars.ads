------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . P A R S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2003 Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Implements the parsing of project files.

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Prj.Pars is

   procedure Set_Verbosity (To : Verbosity);
   --  Set the verbosity when parsing the project files.

   procedure Parse
     (Project           : out Project_Id;
      Project_File_Name : String;
      Packages_To_Check : String_List_Access := All_Packages);
   --  Parse a project files and all its imported project files.
   --  If parsing is successful, Project_Id is the project ID
   --  of the main project file; otherwise, Project_Id is set
   --  to No_Project.
   --  Packages_To_Check indicates the packages where any unknown attribute
   --  produces an error. For other packages, an unknown attribute produces
   --  a warning.

end Prj.Pars;
