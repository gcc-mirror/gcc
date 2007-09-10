------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E X T                               --
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

--  Subprograms to set, get and cache external references, to be used as
--  External functions in project files.

package Prj.Ext is

   Gpr_Project_Path : constant String := "GPR_PROJECT_PATH";
   --  Name of primary env. variable that contain path name(s) of directories
   --  where project files may reside.

   procedure Add_Search_Project_Directory (Path : String);
   --  Add a directory to the project path. Directories added with this
   --  procedure are added in order after the current directory and before
   --  the path given by the environment variable GPR_PROJECT_PATH. A value
   --  of "-" will remove the default project directory from the project path.

   function Project_Path return String;
   --  Return the current value of the project path, either the value set
   --  during elaboration of the package or, if procedure Set_Project_Path has
   --  been called, the value set by the last call to Set_Project_Path.

   procedure Set_Project_Path (New_Path : String);
   --  Give a new value to the project path. The new value New_Path should
   --  always start with the current directory (".") and the path separators
   --  should be the correct ones for the platform.

   procedure Add
     (External_Name : String;
      Value         : String);
   --  Add an external reference (or modify an existing one)

   function Value_Of
     (External_Name : Name_Id;
      With_Default  : Name_Id := No_Name)
      return          Name_Id;
   --  Get the value of an external reference, and cache it for future uses

   function Check (Declaration : String) return Boolean;
   --  Check that an external declaration <external>=<value> is correct.
   --  If it is correct, the external reference is Added.

   procedure Reset;
   --  Clear the internal data structure that stores the external references
   --  and free any allocated memory.

end Prj.Ext;
