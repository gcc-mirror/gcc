------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . E N V                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2013, Free Software Foundation, Inc.         --
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

--  This package implements services for Project-aware tools, mostly related
--  to the environment (configuration pragma files, path files, mapping files).

with GNAT.Dynamic_HTables;
with GNAT.OS_Lib;

package Prj.Env is

   procedure Initialize (In_Tree : Project_Tree_Ref);
   --  Initialize global components relative to environment variables

   procedure Print_Sources (In_Tree : Project_Tree_Ref);
   --  Output the list of sources after Project files have been scanned

   procedure Create_Mapping (In_Tree : Project_Tree_Ref);
   --  Create in memory mapping from the sources of all the projects (in body
   --  of package Fmap), so that Osint.Find_File will find the correct path
   --  corresponding to a source.

   procedure Create_Temp_File
     (Shared    : Shared_Project_Tree_Data_Access;
      Path_FD   : out File_Descriptor;
      Path_Name : out Path_Name_Type;
      File_Use  : String);
   --  Create temporary file, fail with an error if it could not be created

   procedure Create_Mapping_File
     (Project  : Project_Id;
      Language : Name_Id;
      In_Tree  : Project_Tree_Ref;
      Name     : out Path_Name_Type);
   --  Create a temporary mapping file for project Project. For each source or
   --  template of Language in the Project, put the mapping of its file name
   --  and path name in this file. See fmap for a description of the format
   --  of the mapping file.
   --
   --  Implementation note: we pass a language name, not a language_index here,
   --  since the latter would have to match exactly the index of that language
   --  for the specified project, and that is not information available in
   --  buildgpr.adb.

   procedure Create_Config_Pragmas_File
     (For_Project : Project_Id;
      In_Tree     : Project_Tree_Ref);
   --  If we need SFN pragmas, either for non standard naming schemes or for
   --  individual units.

   procedure Create_New_Path_File
     (Shared    : Shared_Project_Tree_Data_Access;
      Path_FD   : out File_Descriptor;
      Path_Name : out Path_Name_Type);
   --  Create a new temporary path file, placing file name in Path_Name

   function Ada_Include_Path
     (Project   : Project_Id;
      In_Tree   : Project_Tree_Ref;
      Recursive : Boolean := False) return String;
   --  Get the source search path of a Project file. If Recursive it True, get
   --  all the source directories of the imported and modified project files
   --  (recursively). If Recursive is False, just get the path for the source
   --  directories of Project. Note: the resulting String may be empty if there
   --  is no source directory in the project file.

   function Ada_Objects_Path
     (Project             : Project_Id;
      In_Tree             : Project_Tree_Ref;
      Including_Libraries : Boolean := True) return String_Access;
   --  Get the ADA_OBJECTS_PATH of a Project file. For the first call with the
   --  exact same parameters, compute it and cache it. When Including_Libraries
   --  is False, the object directory of a library project is replaced with the
   --  library ALI directory of this project (usually the library directory of
   --  the project, except when attribute Library_ALI_Dir is declared) except
   --  when the library ALI directory does not contain any ALI file.

   procedure Set_Ada_Paths
     (Project             : Project_Id;
      In_Tree             : Project_Tree_Ref;
      Including_Libraries : Boolean;
      Include_Path        : Boolean := True;
      Objects_Path        : Boolean := True);
   --  Set the environment variables for additional project path files, after
   --  creating the path files if necessary.

   function File_Name_Of_Library_Unit_Body
     (Name              : String;
      Project           : Project_Id;
      In_Tree           : Project_Tree_Ref;
      Main_Project_Only : Boolean := True;
      Full_Path         : Boolean := False) return String;
   --  Returns the file name of a library unit, in canonical case. Name may or
   --  may not have an extension (corresponding to the naming scheme of the
   --  project). If there is no body with this name, but there is a spec, the
   --  name of the spec is returned.
   --
   --  If Full_Path is False (the default), the simple file name is returned.
   --  If Full_Path is True, the absolute path name is returned.
   --
   --  If neither a body nor a spec can be found, an empty string is returned.
   --  If Main_Project_Only is True, the unit must be an immediate source of
   --  Project. If it is False, it may be a source of one of its imported
   --  projects.

   function Project_Of
     (Name         : String;
      Main_Project : Project_Id;
      In_Tree      : Project_Tree_Ref) return Project_Id;
   --  Get the project of a source. The source file name may be truncated
   --  (".adb" or ".ads" may be missing). If the source is in a project being
   --  extended, return the ultimate extending project. If it is not a source
   --  of any project, return No_Project.

   procedure Get_Reference
     (Source_File_Name : String;
      In_Tree          : Project_Tree_Ref;
      Project          : out Project_Id;
      Path             : out Path_Name_Type);
   --  Returns the project of a source and its path in displayable form

   generic
      with procedure Action (Path : String);
   procedure For_All_Source_Dirs
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref);
   --  Iterate through all the source directories of a project, including those
   --  of imported or modified projects. Only returns those directories that
   --  potentially contain Ada sources (ie ignore projects that have no Ada
   --  sources

   generic
      with procedure Action (Path : String);
   procedure For_All_Object_Dirs
     (Project : Project_Id;
      Tree    : Project_Tree_Ref);
   --  Iterate through all the object directories of a project, including those
   --  of imported or modified projects.

   ------------------
   -- Project Path --
   ------------------

   type Project_Search_Path is private;
   --  An abstraction of the project path. This object provides subprograms
   --  to search for projects on the path (and caches the results to improve
   --  efficiency).

   No_Project_Search_Path : constant Project_Search_Path;

   procedure Initialize_Default_Project_Path
     (Self        : in out Project_Search_Path;
      Target_Name : String);
   --  Initialize Self. It will then contain the default project path on the
   --  given target (including directories specified by the environment
   --  variables ADA_PROJECT_PATH and GPR_PROJECT_PATH). This does nothing if
   --  Self has already been initialized.

   procedure Copy (From : Project_Search_Path; To : out Project_Search_Path);
   --  Copy From into To

   procedure Initialize_Empty (Self : in out Project_Search_Path);
   --  Initialize self with an empty list of directories. If Self had already
   --  been set, it is reset.

   function Is_Initialized (Self : Project_Search_Path) return Boolean;
   --  Whether Self has been initialized

   procedure Free (Self : in out Project_Search_Path);
   --  Free the memory used by Self

   procedure Add_Directories
     (Self    : in out Project_Search_Path;
      Path    : String;
      Prepend : Boolean := False);
   --  Add one or more directories to the path. Directories added with this
   --  procedure are added in order after the current directory and before the
   --  path given by the environment variable GPR_PROJECT_PATH. A value of "-"
   --  will remove the default project directory from the project path.
   --
   --  Calls to this subprogram must be performed before the first call to
   --  Find_Project below, or PATH will be added at the end of the search path.

   procedure Get_Path (Self : Project_Search_Path; Path : out String_Access);
   --  Return the current value of the project path, either the value set
   --  during elaboration of the package or, if procedure Set_Project_Path has
   --  been called, the value set by the last call to Set_Project_Path. The
   --  returned value must not be modified.
   --  Self must have been initialized first.

   procedure Set_Path (Self : in out Project_Search_Path; Path : String);
   --  Override the value of the project path. This also removes the implicit
   --  default search directories.

   generic
      with function Check_Filename (Name : String) return Boolean;
   function Find_Name_In_Path
     (Self : Project_Search_Path;
      Path : String) return String_Access;
   --  Find a name in the project search path of Self. Check_Filename is
   --  the predicate to valid the search.  If Path is an absolute filename,
   --  simply calls the predicate with Path. Otherwise, calls the predicate
   --  for each component of the path. Stops as soon as the predicate
   --  returns True and returns the name, or returns null in case of failure.

   procedure Find_Project
     (Self               : in out Project_Search_Path;
      Project_File_Name  : String;
      Directory          : String;
      Path               : out Namet.Path_Name_Type);
   --  Search for a project with the given name either in Directory (which
   --  often will be the directory contain the project we are currently parsing
   --  and which we found a reference to another project), or in the project
   --  path Self. Self must have been initialized first.
   --
   --  Project_File_Name can optionally contain directories, and the extension
   --  (.gpr) for the file name is optional.
   --
   --  Returns No_Name if no such project was found

   function Get_Runtime_Path
     (Self : Project_Search_Path;
      Name : String) return String_Access;
   --  Compute the full path for the project-based runtime name.  It first
   --  checks that name is not a simple name (must has a path separator in it),
   --  and returns null in case of failure.  This check might be removed in the
   --  future.  The name is simply searched on the project path.

private
   package Projects_Paths is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Path_Name_Type,
      No_Element => No_Path,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

   type Project_Search_Path is record
      Path : GNAT.OS_Lib.String_Access;
      --  As a special case, if the first character is '#:" or this variable
      --  is unset, this means that the PATH has not been fully initialized
      --  yet (although subprograms above will properly take care of that).

      Cache : Projects_Paths.Instance;
   end record;

   No_Project_Search_Path : constant Project_Search_Path :=
                              (Path  => null,
                               Cache => Projects_Paths.Nil);

end Prj.Env;
