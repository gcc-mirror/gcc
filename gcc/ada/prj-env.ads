------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E N V                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--             Copyright (C) 2001-2002 Free Software Foundation, Inc        --
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

--  This package implements services for Project-aware tools, related
--  to the environment (gnat.adc, ADA_INCLUDE_PATH, ADA_OBJECTS_PATH)

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Prj.Env is

   procedure Initialize;
   --  Put Standard_Naming_Data into Namings table (called by Prj.Initialize)

   procedure Print_Sources;
   --  Output the list of sources, after Project files have been scanned

   procedure Create_Mapping_File (Name : in out Temp_File_Name);
   --  Create a temporary mapping file. For each unit, put the mapping of
   --  its spec and or body to its file name and path name in this file.

   procedure Set_Mapping_File_Initial_State_To_Empty;
   --  When creating a mapping file, create an empty map. This case occurs
   --  when run time source files are found in the project files.

   procedure Create_Config_Pragmas_File
     (For_Project  : Project_Id;
      Main_Project : Project_Id);
   --  If there needs to have SFN pragmas, either for non standard naming
   --  schemes or for individual units, or if Global_Configuration_Pragmas
   --  has been specified in package gnatmake of the main project, or if
   --  Local_Configuration_Pragmas has been specified in package Compiler
   --  of the main project, build (if needed) a temporary file that contains
   --  all configuration pragmas, and specify the configuration pragmas file
   --  in the project data.

   function Ada_Include_Path (Project : Project_Id) return String_Access;
   --  Get the ADA_INCLUDE_PATH of a Project file. For the first call, compute
   --  it and cache it.

   function Ada_Include_Path
     (Project   : Project_Id;
      Recursive : Boolean)
      return      String;
   --  Get the ADA_INCLUDE_PATH of a Project file. If Recursive it True,
   --  get all the source directories of the imported and modified project
   --  files (recursively). If Recursive is False, just get the path for the
   --  source directories of Project. Note: the resulting String may be empty
   --  if there is no source directory in the project file.

   function Ada_Objects_Path
     (Project             : Project_Id;
      Including_Libraries : Boolean := True)
      return                String_Access;
   --  Get the ADA_OBJECTS_PATH of a Project file. For the first call, compute
   --  it and cache it. When Including_Libraries is False, do not include the
   --  object directories of the library projects, and do not cache the result.

   function Path_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id)
      return    String;
   --  Returns the Path of a library unit.

   function File_Name_Of_Library_Unit_Body
     (Name    : String;
      Project : Project_Id)
      return    String;
   --  Returns the file name of a library unit, in canonical case. Name may or
   --  may not have an extension (corresponding to the naming scheme of the
   --  project). If there is no body with this name, but there is a spec, the
   --  name of the spec is returned. If neither a body or a spec can be found,
   --  return an empty string.

   procedure Get_Reference
     (Source_File_Name : String;
      Project          : out Project_Id;
      Path             : out Name_Id);
   --  Returns the project of a source.

   generic
      with procedure Action (Path : String);
   procedure For_All_Source_Dirs (Project : Project_Id);
   --  Iterate through all the source directories of a project,
   --  including those of imported or modified projects.

   generic
      with procedure Action (Path : String);
   procedure For_All_Object_Dirs (Project : Project_Id);
   --  Iterate through all the object directories of a project,
   --  including those of imported or modified projects.

end Prj.Env;
