------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . D E C T                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2001-2005 Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Parse a list of declarative items in a project file

with Prj.Tree;

private package Prj.Dect is

   procedure Parse
     (In_Tree           : Prj.Tree.Project_Node_Tree_Ref;
      Declarations      : out Prj.Tree.Project_Node_Id;
      Current_Project   : Prj.Tree.Project_Node_Id;
      Extends           : Prj.Tree.Project_Node_Id;
      Packages_To_Check : String_List_Access);
   --  Parse project declarative items
   --
   --  In_Tree is the project node tree
   --
   --  Declarations is the resulting project node
   --
   --  Current_Project is the project node of the project for which the
   --  declarative items are parsed.
   --
   --  Extends is the project node of the project that project Current_Project
   --  extends. If project Current-Project does not extend any project,
   --  Extends has the value Empty_Node.
   --
   --  Packages_To_Check is the list of packages that needs to be checked.
   --  For legal packages declared in project Current_Project that are not in
   --  Packages_To_Check, only the syntax of the declarations are checked, not
   --  the attribute names and kinds.

end Prj.Dect;
