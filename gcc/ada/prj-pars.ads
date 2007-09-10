------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . P A R S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2007, Free Software Foundation, Inc.         --
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

--  Implements the parsing of project files

package Prj.Pars is

   procedure Set_Verbosity (To : Verbosity);
   --  Set the verbosity when parsing the project files

   procedure Parse
     (In_Tree           : Project_Tree_Ref;
      Project           : out Project_Id;
      Project_File_Name : String;
      Packages_To_Check : String_List_Access := All_Packages;
      When_No_Sources   : Error_Warning := Error;
      Reset_Tree        : Boolean := True);
   --  Parse a project files and all its imported project files, in the
   --  project tree In_Tree.
   --
   --  If parsing is successful, Project_Id is the project ID
   --  of the main project file; otherwise, Project_Id is set
   --  to No_Project.
   --
   --  Packages_To_Check indicates the packages where any unknown attribute
   --  produces an error. For other packages, an unknown attribute produces
   --  a warning.
   --
   --  When_No_Sources indicates what should be done when no sources
   --  are found in a project for a specified or implied language.
   --
   --  When Reset_Tree is True, all the project data are removed from the
   --  project table before processing.

end Prj.Pars;
