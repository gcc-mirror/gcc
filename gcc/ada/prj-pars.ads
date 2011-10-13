------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . P A R S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2011, Free Software Foundation, Inc.         --
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

--  General wrapper for the parsing of project files

with Prj.Tree;

package Prj.Pars is

   procedure Set_Verbosity (To : Verbosity);
   --  Set the verbosity when parsing the project files

   procedure Parse
     (In_Tree           : Project_Tree_Ref;
      Project           : out Project_Id;
      Project_File_Name : String;
      Packages_To_Check : String_List_Access;
      Reset_Tree        : Boolean := True;
      In_Node_Tree      : Prj.Tree.Project_Node_Tree_Ref := null;
      Env               : in out Prj.Tree.Environment);
   --  Parse and process a project files and all its imported project files, in
   --  the project tree In_Tree.
   --
   --  All the project files are parsed (through Prj.Tree) to create a tree in
   --  memory. That tree is then processed (through Prj.Proc) to create a
   --  expanded representation of the tree based on the current external
   --  references. This function is only a convenient wrapper over other
   --  services provided in the Prj.* package hierarchy.
   --
   --  If parsing is successful, Project is the project ID of the root project
   --  file; otherwise, Project_Id is set to No_Project. Project_Node_Tree is
   --  set to the tree (unprocessed) representation of the project file. This
   --  tree is permanently correct, whereas Project will need to be recomputed
   --  if the external references change.
   --
   --  Packages_To_Check indicates the packages where any unknown attribute
   --  produces an error. For other packages, an unknown attribute produces a
   --  warning.
   --
   --  When Reset_Tree is True, all the project data are removed from the
   --  project table before processing.
   --
   --  In_Node_Tree (if given) must have been Initialized. The main reason to
   --  pass an existing tree, is to pass the external references that will then
   --  be used to process the tree.

end Prj.Pars;
