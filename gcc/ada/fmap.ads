------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 F M A P                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--            Copyright (C) 2001, Free Software Foundation, Inc.            --
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

--  This package keeps two mappings: from unit names to file names,
--  and from file names to path names.

with Types; use Types;

package Fmap is

   procedure Initialize (File_Name : String);
   --  Initialize the mappings from the mapping file File_Name.
   --  If the mapping file is incorrect (non existent file, truncated file,
   --  duplicate entries), output a warning and do not initialize the mappings.
   --  Record the state of the mapping tables in case Update is called
   --  later on.

   function Mapped_Path_Name (File : File_Name_Type) return File_Name_Type;
   --  Return the path name mapped to the file name File.
   --  Return No_File if File is not mapped.

   function Mapped_File_Name (Unit : Unit_Name_Type) return File_Name_Type;
   --  Return the file name mapped to the unit name Unit.
   --  Return No_File if Unit is not mapped.

   procedure Add_To_File_Map
     (Unit_Name : Unit_Name_Type;
      File_Name : File_Name_Type;
      Path_Name : File_Name_Type);
   --  Add mapping of Unit_Name to File_Name and of File_Name to Path_Name

   procedure Update_Mapping_File (File_Name : String);
   --  If Add_To_File_Map has been called (after Initialize or any time
   --  if Initialize has not been called), append the new entries to the
   --  to the mapping file.
   --  What is the significance of the parameter File_Name ???

end Fmap;
