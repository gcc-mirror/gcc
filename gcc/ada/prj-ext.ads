------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2009, Free Software Foundation, Inc.         --
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

--  Subprograms to set, get and cache external references, to be used as
--  External functions in project files.

with Prj.Tree;

package Prj.Ext is

   ------------------
   -- Project Path --
   ------------------

   procedure Add_Search_Project_Directory
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Path : String);
   --  Add a directory to the project path. Directories added with this
   --  procedure are added in order after the current directory and before
   --  the path given by the environment variable GPR_PROJECT_PATH. A value
   --  of "-" will remove the default project directory from the project path.
   --
   --  Calls to this subprogram must be performed before the first call to
   --  Project_Path below, or PATH will be added at the end of the search
   --  path.

   function Project_Path (Tree : Prj.Tree.Project_Node_Tree_Ref) return String;
   --  Return the current value of the project path, either the value set
   --  during elaboration of the package or, if procedure Set_Project_Path has
   --  been called, the value set by the last call to Set_Project_Path.

   procedure Set_Project_Path
     (Tree     : Prj.Tree.Project_Node_Tree_Ref;
      New_Path : String);
   --  Give a new value to the project path. The new value New_Path should
   --  always start with the current directory (".") and the path separators
   --  should be the correct ones for the platform.

   -------------------------
   -- External References --
   -------------------------

   --  External references influence the way a project tree is processed (in
   --  particular they provide the values for the typed string variables that
   --  are then used in case constructions).

   --  External references are project-tree specific, so that when multiple
   --  trees are loaded in parallel we can have different scenarios (or even
   --  load the same tree twice and see different views of it).

   procedure Add
     (Tree          : Prj.Tree.Project_Node_Tree_Ref;
      External_Name : String;
      Value         : String);
   --  Add an external reference (or modify an existing one)

   function Value_Of
     (Tree          : Prj.Tree.Project_Node_Tree_Ref;
      External_Name : Name_Id;
      With_Default  : Name_Id := No_Name)
      return          Name_Id;
   --  Get the value of an external reference, and cache it for future uses

   function Check
     (Tree        : Prj.Tree.Project_Node_Tree_Ref;
      Declaration : String) return Boolean;
   --  Check that an external declaration <external>=<value> is correct.
   --  If it is correct, the external reference is Added.

   procedure Reset (Tree : Prj.Tree.Project_Node_Tree_Ref);
   --  Clear the internal data structure that stores the external references
   --  and free any allocated memory.

end Prj.Ext;
