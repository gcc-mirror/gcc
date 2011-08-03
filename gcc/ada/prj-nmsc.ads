------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
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

--  Find source dirs and source files for a project

with Prj.Tree;

private package Prj.Nmsc is

   procedure Process_Naming_Scheme
     (Tree         : Project_Tree_Ref;
      Root_Project : Project_Id;
      Node_Tree    : Prj.Tree.Project_Node_Tree_Ref;
      Flags        : Processing_Flags);
   --  Perform consistency and semantic checks on all the projects in the tree.
   --  This procedure interprets the various case statements in the project
   --  based on the current external references. After checking the validity of
   --  the naming scheme, it searches for all the source files of the project.
   --  The result of this procedure is a filled-in data structure for
   --  Project_Id which contains all the information about the project. This
   --  information is only valid while the external references are preserved.

   procedure Process_Aggregated_Projects
     (Tree      : Project_Tree_Ref;
      Project   : Project_Id;
      Node_Tree : Prj.Tree.Project_Node_Tree_Ref;
      Flags     : Processing_Flags);
   --  Assuming Project is an aggregate project, find out (based on the
   --  current external references) what are the projects it aggregates.
   --  This has to be done in phase 1 of the processing, so that we know the
   --  full list of languages required for root_project and its aggregated
   --  projects. As a result, it cannot be done as part of
   --  Process_Naming_Scheme.

end Prj.Nmsc;
