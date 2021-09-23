------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.DIRECTORIES.HIERARCHICAL_FILE_NAMES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with the --
-- GNAT Pro compiler, including any required library run-time units,  using --
-- any licensing terms  of your choosing.  See the AdaCore Software License --
-- for full details.                                                        --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Directories.Hierarchical_File_Names is

   function Is_Simple_Name (Name : String) return Boolean;
   --  Returns True if Name is a simple name, and returns False otherwise.

   function Is_Root_Directory_Name (Name : String) return Boolean;
   --  Returns True if Name is syntactically a root (a directory that cannot
   --  be decomposed further), and returns False otherwise.

   function Is_Parent_Directory_Name (Name : String) return Boolean;
   --  Returns True if Name can be used to indicate symbolically the parent
   --  directory of any directory, and returns False otherwise.

   function Is_Current_Directory_Name (Name : String) return Boolean;
   --  Returns True if Name can be used to indicate symbolically the directory
   --  itself for any directory, and returns False otherwise.

   function Is_Full_Name (Name : String) return Boolean;
   --  Returns True if the leftmost directory part of Name is a root, and
   --  returns False otherwise.

   function Is_Relative_Name (Name : String) return Boolean;
   --  Returns True if Name allows the identification of an external file
   --  (including directories and special files) but is not a full name, and
   --  returns False otherwise.

   function Simple_Name (Name : String) return String
     renames Ada.Directories.Simple_Name;
   --  Returns the simple name portion of the file name specified by Name. The
   --  exception Name_Error is propagated if the string given as Name does not
   --  allow the identification of an external file (including directories and
   --  special files).

   function Containing_Directory (Name : String) return String
     renames Ada.Directories.Containing_Directory;
   --  Returns the name of the containing directory of the external file
   --  (including directories) identified by Name. If more than one directory
   --  can contain Name, the directory name returned is implementation-defined.
   --  The exception Name_Error is propagated if the string given as Name does
   --  not allow the identification of an external file. The exception
   --  Use_Error is propagated if the external file does not have a containing
   --  directory.

   function Initial_Directory (Name : String) return String;
   --  Returns the leftmost directory part in Name. That is, it returns a root
   --  directory name (for a full name), or one of a parent directory name, a
   --  current directory name, or a simple name (for a relative name). The
   --  exception Name_Error is propagated if the string given as Name does not
   --  allow the identification of an external file (including directories and
   --  special files).

   function Relative_Name (Name : String) return String;
   --  Returns the entire file name except the Initial_Directory portion. The
   --  exception Name_Error is propagated if the string given as Name does not
   --  allow the identification of an external file (including directories and
   --  special files), or if Name has a single part (this includes if any of
   --  Is_Simple_Name, Is_Root_Directory_Name, Is_Parent_Directory_Name, or
   --  Is_Current_Directory_Name are True).

   function Compose
     (Directory      : String := "";
      Relative_Name  : String;
      Extension      : String := "") return String;
   --  Returns the name of the external file with the specified Directory,
   --  Relative_Name, and Extension. The exception Name_Error is propagated if
   --  the string given as Directory is not the null string and does not allow
   --  the identification of a directory, or if Is_Relative_Name
   --  (Relative_Name) is False, or if the string given as Extension is not
   --  the null string and is not a possible extension, or if Extension is not
   --  the null string and Simple_Name (Relative_Name) is not a base name.
   --
   --  The result of Compose is a full name if Is_Full_Name (Directory) is
   --  True; result is a relative name otherwise.

end Ada.Directories.Hierarchical_File_Names;
