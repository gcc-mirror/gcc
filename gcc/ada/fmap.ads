------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 F M A P                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2021, Free Software Foundation, Inc.         --
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

--  This package keeps two mappings: from unit names to file names,
--  and from file names to path names.
--
--  This mapping is used to communicate between the builder (gnatmake or
--  gprbuild) and the compiler. The format of this mapping file is the
--  following:
--  For each source file, there are three lines in the mapping file:
--    Unit name with %b or %s added depending on whether it is a body or a spec
--              This line is omitted for file-based languages
--    File name
--    Path name (set to '/' if the file should be ignored in fact, ie for
--               a Locally_Removed_File in a project)

with Namet; use Namet;

package Fmap is

   procedure Initialize (File_Name : String);
   --  Initialize the mappings from the mapping file File_Name.
   --  If the mapping file is incorrect (nonexistent file, truncated file,
   --  duplicate entries), output a warning and do not initialize the mappings.
   --  Record the state of the mapping tables in case Update is called
   --  later on.

   function Mapped_Path_Name (File : File_Name_Type) return File_Name_Type;
   --  Return the path name mapped to the file name File.
   --  Return No_File if File is not mapped.

   function Mapped_File_Name (Unit : Unit_Name_Type) return File_Name_Type;
   --  Return the file name mapped to the unit name Unit.
   --  Return No_File if Unit is not mapped.
   --  Return Error_Name if it is forbidden.

   procedure Add_To_File_Map
     (Unit_Name : Unit_Name_Type;
      File_Name : File_Name_Type;
      Path_Name : File_Name_Type);
   --  Add mapping of Unit_Name to File_Name and of File_Name to Path_Name

   procedure Update_Mapping_File (File_Name : String);
   --  If Add_To_File_Map has been called (after Initialize or any time
   --  if Initialize has not been called), append the new entries to the
   --  mapping file whose file name is File_Name.

   procedure Reset_Tables;
   --  Initialize all the internal data structures

   procedure Add_Forbidden_File_Name (Name : File_Name_Type);
   --  Indicate that a source file name is forbidden. This is used when there
   --  are excluded sources in projects (attributes Excluded_Source_Files or
   --  Locally_Removed_Files).

end Fmap;
