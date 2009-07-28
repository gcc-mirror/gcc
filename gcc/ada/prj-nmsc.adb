------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Dynamic_HTables;

with Err_Vars; use Err_Vars;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Err;  use Prj.Err;
with Prj.Util; use Prj.Util;
with Sinput.P;
with Snames;   use Snames;
with Targparm; use Targparm;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Directories;            use Ada.Directories;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

package body Prj.Nmsc is

   No_Continuation_String : aliased String := "";
   Continuation_String    : aliased String := "\";
   --  Used in Check_Library for continuation error messages at the same
   --  location.

   type Name_Location is record
      Name     : File_Name_Type;  --  ??? duplicates the key
      Location : Source_Ptr;
      Source   : Source_Id := No_Source;
      Found    : Boolean := False;
   end record;
   No_Name_Location : constant Name_Location :=
     (No_File, No_Location, No_Source, False);
   package Source_Names_Htable is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_Location,
      No_Element => No_Name_Location,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  Information about file names found in string list attribute
   --  (Source_Files or Source_List_File).
   --  Except is set to True if source is a naming exception in the project.
   --  This is used to check that all referenced files were indeed found on the
   --  disk.

   type Unit_Exception is record
      Name : Name_Id;  --  ??? duplicates the key
      Spec : File_Name_Type;
      Impl : File_Name_Type;
   end record;

   No_Unit_Exception : constant Unit_Exception := (No_Name, No_File, No_File);

   package Unit_Exceptions_Htable is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Unit_Exception,
      No_Element => No_Unit_Exception,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Record special naming schemes for Ada units (name of spec file and name
   --  of implementation file). The elements in this list come from the naming
   --  exceptions specified in the project files.

   type File_Found is record
      File     : File_Name_Type  := No_File;
      Found    : Boolean         := False;
      Location : Source_Ptr      := No_Location;
   end record;

   No_File_Found : constant File_Found := (No_File, False, No_Location);

   package Excluded_Sources_Htable is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => File_Found,
      No_Element => No_File_Found,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store the base names of excluded files, if any

   package Object_File_Names_Htable is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Source_Id,
      No_Element => No_Source,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store the object file names for a project, to check that
   --  two different sources have different object file names.

   type Project_Processing_Data is record
      Project         : Project_Id;
      Source_Names    : Source_Names_Htable.Instance;
      Unit_Exceptions : Unit_Exceptions_Htable.Instance;
      Excluded        : Excluded_Sources_Htable.Instance;

      Source_List_File_Location : Source_Ptr;
      --  Location of the Source_List_File attribute, for error messages
   end record;
   --  This is similar to Tree_Processing_Data, but contains project-specific
   --  information which is only useful while processing the project, and can
   --  be discarded as soon as we have finished processing the project

   package Files_Htable is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Source_Id,
      No_Element => No_Source,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping from base file names to Source_Id (containing full info about
   --  the source).

   type Tree_Processing_Data is record
      Tree           : Project_Tree_Ref;
      File_To_Source : Files_Htable.Instance;
      Flags          : Prj.Processing_Flags;
   end record;
   --  Temporary data which is needed while parsing a project. It does not need
   --  to be kept in memory once a project has been fully loaded, but is
   --  necessary while performing consistency checks (duplicate sources,...)
   --  This data must be initialized before processing any project, and the
   --  same data is used for processing all projects in the tree.

   procedure Initialize
     (Data  : out Tree_Processing_Data;
      Tree  : Project_Tree_Ref;
      Flags : Prj.Processing_Flags);
   --  Initialize Data

   procedure Free (Data : in out Tree_Processing_Data);
   --  Free the memory occupied by Data

   procedure Check
     (Project     : Project_Id;
      Data        : in out Tree_Processing_Data);
   --  Process the naming scheme for a single project

   procedure Initialize
     (Data    : in out Project_Processing_Data;
      Project : Project_Id);
   procedure Free (Data : in out Project_Processing_Data);
   --  Initialize or free memory for a project-specific data

   procedure Find_Excluded_Sources
     (Project : in out Project_Processing_Data;
      Data    : in out Tree_Processing_Data);
   --  Find the list of files that should not be considered as source files
   --  for this project. Sets the list in the Project.Excluded_Sources_Htable.

   procedure Override_Kind (Source : Source_Id; Kind : Source_Kind);
   --  Override the reference kind for a source file. This properly updates
   --  the unit data if necessary.

   procedure Load_Naming_Exceptions
     (Project : in out Project_Processing_Data;
      Data    : in out Tree_Processing_Data);
   --  All source files in Data.First_Source are considered as naming
   --  exceptions, and copied into the Source_Names and Unit_Exceptions tables
   --  as appropriate.

   procedure Add_Source
     (Id                  : out Source_Id;
      Data                : in out Tree_Processing_Data;
      Project             : Project_Id;
      Lang_Id             : Language_Ptr;
      Kind                : Source_Kind;
      File_Name           : File_Name_Type;
      Display_File        : File_Name_Type;
      Naming_Exception    : Boolean          := False;
      Path                : Path_Information := No_Path_Information;
      Alternate_Languages : Language_List    := null;
      Unit                : Name_Id          := No_Name;
      Index               : Int              := 0;
      Locally_Removed     : Boolean          := False;
      Location            : Source_Ptr       := No_Location);
   --  Add a new source to the different lists: list of all sources in the
   --  project tree, list of source of a project and list of sources of a
   --  language.
   --
   --  If Path is specified, the file is also added to Source_Paths_HT.
   --
   --  Location is used for error messages

   function Canonical_Case_File_Name (Name : Name_Id) return File_Name_Type;
   --  Same as Osint.Canonical_Case_File_Name but applies to Name_Id.
   --  This alters Name_Buffer

   function Suffix_Matches
     (Filename : String;
      Suffix   : File_Name_Type) return Boolean;
   --  True if the file name ends with the given suffix. Always returns False
   --  if Suffix is No_Name.

   procedure Replace_Into_Name_Buffer
     (Str         : String;
      Pattern     : String;
      Replacement : Character);
   --  Copy Str into Name_Buffer, replacing Pattern with Replacement. Str is
   --  converted to lower-case at the same time.

   procedure Check_Ada_Name (Name : String; Unit : out Name_Id);
   --  Check that a name is a valid Ada unit name

   procedure Check_Package_Naming
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data;
      Bodies  : out Array_Element_Id;
      Specs   : out Array_Element_Id);
   --  Check the naming scheme part of Data, and initialize the naming scheme
   --  data in the config of the various languages. This also returns the
   --  naming scheme exceptions for unit-based languages (Bodies and Specs are
   --  associative arrays mapping individual unit names to source file names).

   procedure Check_Configuration
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data);
   --  Check the configuration attributes for the project

   procedure Check_If_Externally_Built
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data);
   --  Check attribute Externally_Built of project Project in project tree
   --  Data.Tree and modify its data Data if it has the value "true".

   procedure Check_Interfaces
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data);
   --  If a list of sources is specified in attribute Interfaces, set
   --  In_Interfaces only for the sources specified in the list.

   procedure Check_Library_Attributes
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data);
   --  Check the library attributes of project Project in project tree
   --  and modify its data Data accordingly.

   procedure Check_Programming_Languages
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data);
   --  Check attribute Languages for the project with data Data in project
   --  tree Data.Tree and set the components of Data for all the programming
   --  languages indicated in attribute Languages, if any.

   procedure Check_Stand_Alone_Library
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data);
   --  Check if project Project in project tree Data.Tree is a Stand-Alone
   --  Library project, and modify its data Data accordingly if it is one.

   function Compute_Directory_Last (Dir : String) return Natural;
   --  Return the index of the last significant character in Dir. This is used
   --  to avoid duplicate '/' (slash) characters at the end of directory names.

   procedure Search_Directories
     (Project         : in out Project_Processing_Data;
      Data            : in out Tree_Processing_Data;
      For_All_Sources : Boolean);
   --  Search the source directories to find the sources. If For_All_Sources is
   --  True, check each regular file name against the naming schemes of the
   --  various languages. Otherwise consider only the file names in hash table
   --  Source_Names. If Allow_Duplicate_Basenames then files with identical
   --  base names are permitted within a project for source-based languages
   --  (never for unit based languages).

   procedure Check_File
     (Project           : in out Project_Processing_Data;
      Data              : in out Tree_Processing_Data;
      Path              : Path_Name_Type;
      File_Name         : File_Name_Type;
      Display_File_Name : File_Name_Type;
      Locally_Removed   : Boolean;
      For_All_Sources   : Boolean);
   --  Check if file File_Name is a valid source of the project. This is used
   --  in multi-language mode only. When the file matches one of the naming
   --  schemes, it is added to various htables through Add_Source and to
   --  Source_Paths_Htable.
   --
   --  Name is the name of the candidate file. It hasn't been normalized yet
   --  and is the direct result of readdir().
   --
   --  File_Name is the same as Name, but has been normalized.
   --  Display_File_Name, however, has not been normalized.
   --
   --  Source_Directory is the directory in which the file was found. It is
   --  neither normalized nor has had links resolved, and must not end with a
   --  a directory separator, to avoid duplicates later on.
   --
   --  If For_All_Sources is True, then all possible file names are analyzed
   --  otherwise only those currently set in the Source_Names hash table.

   procedure Check_File_Naming_Schemes
     (In_Tree               : Project_Tree_Ref;
      Project               : Project_Processing_Data;
      File_Name             : File_Name_Type;
      Alternate_Languages   : out Language_List;
      Language              : out Language_Ptr;
      Display_Language_Name : out Name_Id;
      Unit                  : out Name_Id;
      Lang_Kind             : out Language_Kind;
      Kind                  : out Source_Kind);
   --  Check if the file name File_Name conforms to one of the naming schemes
   --  of the project. If the file does not match one of the naming schemes,
   --  set Language to No_Language_Index. Filename is the name of the file
   --  being investigated. It has been normalized (case-folded). File_Name is
   --  the same value.

   procedure Get_Directories
     (Project     : Project_Id;
      Data        : in out Tree_Processing_Data);
   --  Get the object directory, the exec directory and the source directories
   --  of a project.

   procedure Get_Mains
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data);
   --  Get the mains of a project from attribute Main, if it exists, and put
   --  them in the project data.

   procedure Get_Sources_From_File
     (Path     : String;
      Location : Source_Ptr;
      Project  : in out Project_Processing_Data;
      Data     : in out Tree_Processing_Data);
   --  Get the list of sources from a text file and put them in hash table
   --  Source_Names.

   procedure Find_Sources
     (Project : in out Project_Processing_Data;
      Data    : in out Tree_Processing_Data);
   --  Process the Source_Files and Source_List_File attributes, and store the
   --  list of source files into the Source_Names htable. When these attributes
   --  are not defined, find all files matching the naming schemes in the
   --  source directories. If Allow_Duplicate_Basenames, then files with the
   --  same base names are authorized within a project for source-based
   --  languages (never for unit based languages)

   procedure Compute_Unit_Name
     (File_Name : File_Name_Type;
      Naming    : Lang_Naming_Data;
      Kind      : out Source_Kind;
      Unit      : out Name_Id;
      Project   : Project_Processing_Data;
      In_Tree   : Project_Tree_Ref);
   --  Check whether the file matches the naming scheme. If it does,
   --  compute its unit name. If Unit is set to No_Name on exit, none of the
   --  other out parameters are relevant.

   procedure Check_Illegal_Suffix
     (Project         : Project_Id;
      Suffix          : File_Name_Type;
      Dot_Replacement : File_Name_Type;
      Attribute_Name  : String;
      Location        : Source_Ptr;
      Data            : in out Tree_Processing_Data);
   --  Display an error message if the given suffix is illegal for some reason.
   --  The name of the attribute we are testing is specified in Attribute_Name,
   --  which is used in the error message. Location is the location where the
   --  suffix is defined.

   procedure Locate_Directory
     (Project          : Project_Id;
      Name             : File_Name_Type;
      Path             : out Path_Information;
      Dir_Exists       : out Boolean;
      Data             : in out Tree_Processing_Data;
      Create           : String := "";
      Location         : Source_Ptr := No_Location;
      Must_Exist       : Boolean := True;
      Externally_Built : Boolean := False);
   --  Locate a directory. Name is the directory name. Relative paths are
   --  resolved relative to the project's directory. If the directory does not
   --  exist and Setup_Projects is True and Create is a non null string, an
   --  attempt is made to create the directory. If the directory does not
   --  exist, it is either created if Setup_Projects is False (and then
   --  returned), or simply returned without checking for its existence (if
   --  Must_Exist is False) or No_Path_Information is returned. In all cases,
   --  Dir_Exists indicates whether the directory now exists. Create is also
   --  used for debugging traces to show which path we are computing.

   procedure Look_For_Sources
     (Project : in out Project_Processing_Data;
      Data    : in out Tree_Processing_Data);
   --  Find all the sources of project Project in project tree Data.Tree and
   --  update its Data accordingly. This assumes that Data.First_Source has
   --  been initialized with the list of excluded sources and special naming
   --  exceptions.

   function Path_Name_Of
     (File_Name : File_Name_Type;
      Directory : Path_Name_Type) return String;
   --  Returns the path name of a (non project) file. Returns an empty string
   --  if file cannot be found.

   procedure Remove_Source
     (Id          : Source_Id;
      Replaced_By : Source_Id);
   --  Remove a file from the list of sources of a project. This might be
   --  because the file is replaced by another one in an extending project,
   --  or because a file was added as a naming exception but was not found
   --  in the end.

   procedure Report_No_Sources
     (Project      : Project_Id;
      Lang_Name    : String;
      Data         : Tree_Processing_Data;
      Location     : Source_Ptr;
      Continuation : Boolean := False);
   --  Report an error or a warning depending on the value of When_No_Sources
   --  when there are no sources for language Lang_Name.

   procedure Show_Source_Dirs
     (Project : Project_Id; In_Tree : Project_Tree_Ref);
   --  List all the source directories of a project

   procedure Write_Attr (Name, Value : String);
   --  Debug print a value for a specific property. Does nothing when not in
   --  debug mode

   ------------------------------
   -- Replace_Into_Name_Buffer --
   ------------------------------

   procedure Replace_Into_Name_Buffer
     (Str         : String;
      Pattern     : String;
      Replacement : Character)
   is
      Max : constant Integer := Str'Last - Pattern'Length + 1;
      J   : Positive;

   begin
      Name_Len := 0;

      J := Str'First;
      while J <= Str'Last loop
         Name_Len := Name_Len + 1;

         if J <= Max
           and then Str (J .. J + Pattern'Length - 1) = Pattern
         then
            Name_Buffer (Name_Len) := Replacement;
            J := J + Pattern'Length;

         else
            Name_Buffer (Name_Len) := GNAT.Case_Util.To_Lower (Str (J));
            J := J + 1;
         end if;
      end loop;
   end Replace_Into_Name_Buffer;

   --------------------
   -- Suffix_Matches --
   --------------------

   function Suffix_Matches
     (Filename : String;
      Suffix   : File_Name_Type) return Boolean
   is
      Min_Prefix_Length : Natural := 0;

   begin
      if Suffix = No_File or else Suffix = Empty_File then
         return False;
      end if;

      declare
         Suf : String := Get_Name_String (Suffix);

      begin
         --  On non case-sensitive systems, use proper suffix casing

         Canonical_Case_File_Name (Suf);

         --  The file name must end with the suffix (which is not an extension)
         --  For instance a suffix "configure.in" must match a file with the
         --  same name. To avoid dummy cases, though, a suffix starting with
         --  '.' requires a file that is at least one character longer ('.cpp'
         --  should not match a file with the same name)

         if Suf (Suf'First) = '.' then
            Min_Prefix_Length := 1;
         end if;

         return Filename'Length >= Suf'Length + Min_Prefix_Length
           and then Filename
             (Filename'Last - Suf'Length + 1 .. Filename'Last) = Suf;
      end;
   end Suffix_Matches;

   ----------------
   -- Write_Attr --
   ----------------

   procedure Write_Attr (Name, Value : String) is
   begin
      if Current_Verbosity = High then
         Write_Str  ("  " & Name & " = """);
         Write_Str  (Value);
         Write_Char ('"');
         Write_Eol;
      end if;
   end Write_Attr;

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source
     (Id                  : out Source_Id;
      Data                : in out Tree_Processing_Data;
      Project             : Project_Id;
      Lang_Id             : Language_Ptr;
      Kind                : Source_Kind;
      File_Name           : File_Name_Type;
      Display_File        : File_Name_Type;
      Naming_Exception    : Boolean          := False;
      Path                : Path_Information := No_Path_Information;
      Alternate_Languages : Language_List    := null;
      Unit                : Name_Id          := No_Name;
      Index               : Int              := 0;
      Locally_Removed     : Boolean          := False;
      Location            : Source_Ptr       := No_Location)
   is
      Config    : constant Language_Config := Lang_Id.Config;
      UData     : Unit_Index;
      Add_Src   : Boolean;
      Source    : Source_Id;
      Prev_Unit : Unit_Index := No_Unit_Index;

      Source_To_Replace : Source_Id := No_Source;

   begin
      --  Check if the same file name or unit is used in the prj tree

      Add_Src := True;

      if Unit /= No_Name then
         Prev_Unit := Units_Htable.Get (Data.Tree.Units_HT, Unit);
      end if;

      if Prev_Unit /= No_Unit_Index
        and then (Kind = Impl or else Kind = Spec)
        and then Prev_Unit.File_Names (Kind) /= null
      then
         --  Suspicious, we need to check later whether this is authorized

         Add_Src := False;
         Source := Prev_Unit.File_Names (Kind);

      else
         Source  := Files_Htable.Get (Data.File_To_Source, File_Name);

         if Source /= No_Source
           and then Source.Index = Index
         then
            Add_Src := False;
         end if;
      end if;

      --  Duplication of file/unit in same project is allowed if order of
      --  source directories is known.

      if Add_Src = False then
         Add_Src := True;

         if Project = Source.Project then
            if Prev_Unit = No_Unit_Index then
               if Data.Flags.Allow_Duplicate_Basenames then
                  Add_Src := True;

               elsif Project.Known_Order_Of_Source_Dirs then
                  Add_Src := False;

               else
                  Error_Msg_File_1 := File_Name;
                  Error_Msg
                    (Data.Flags, "duplicate source file name {",
                     Location, Project);
                  Add_Src := False;
               end if;

            else
               if Project.Known_Order_Of_Source_Dirs then
                  Add_Src := False;

               --  We might be seeing the same file through a different path
               --  (for instance because of symbolic links).

               elsif Source.Path.Name /= Path.Name then
                  Error_Msg_Name_1 := Unit;
                  Error_Msg
                    (Data.Flags, "duplicate unit %%", Location, Project);
                  Add_Src := False;
               end if;
            end if;

            --  Do not allow the same unit name in different projects, except
            --  if one is extending the other.

            --  For a file based language, the same file name replaces a file
            --  in a project being extended, but it is allowed to have the same
            --  file name in unrelated projects.

         elsif Is_Extending (Project, Source.Project) then
            if not Locally_Removed then
               Source_To_Replace := Source;
            end if;

         elsif Prev_Unit /= No_Unit_Index
           and then not Source.Locally_Removed
         then
            --  Path is set if this is a source we found on the disk, in which
            --  case we can provide more explicit error message. Path is unset
            --  when the source is added from one of the naming exceptions in
            --  the project.

            if Path /= No_Path_Information then
               Error_Msg_Name_1 := Unit;
               Error_Msg
                 (Data.Flags,
                  "unit %% cannot belong to several projects",
                  Location, Project);

               Error_Msg_Name_1 := Project.Name;
               Error_Msg_Name_2 := Name_Id (Path.Name);
               Error_Msg
                 (Data.Flags, "\  project %%, %%", Location, Project);

               Error_Msg_Name_1 := Source.Project.Name;
               Error_Msg_Name_2 := Name_Id (Source.Path.Display_Name);
               Error_Msg
                 (Data.Flags, "\  project %%, %%", Location, Project);

            else
               Error_Msg_Name_1 := Unit;
               Error_Msg_Name_2 := Source.Project.Name;
               Error_Msg
                 (Data.Flags, "unit %% already belongs to project %%",
                  Location, Project);
            end if;

            Add_Src := False;

         elsif not Source.Locally_Removed
           and then not Data.Flags.Allow_Duplicate_Basenames
           and then Lang_Id.Config.Kind = Unit_Based
         then
            Error_Msg_File_1 := File_Name;
            Error_Msg_File_2 := File_Name_Type (Source.Project.Name);
            Error_Msg
              (Data.Flags,
               "{ is already a source of project {", Location, Project);

            --  Add the file anyway, to avoid further warnings like "language
            --  unknown".

            Add_Src := True;
         end if;
      end if;

      if not Add_Src then
         return;
      end if;

      --  Add the new file

      Id := new Source_Data;

      if Current_Verbosity = High then
         Write_Str ("Adding source File: ");
         Write_Str (Get_Name_String (File_Name));

         if Index /= 0 then
            Write_Str (" at" & Index'Img);
         end if;

         if Lang_Id.Config.Kind = Unit_Based then
            Write_Str (" Unit: ");

            --  ??? in gprclean, it seems we sometimes pass an empty Unit name
            --  (see test extended_projects).

            if Unit /= No_Name then
               Write_Str (Get_Name_String (Unit));
            end if;

            Write_Str (" Kind: ");
            Write_Str (Source_Kind'Image (Kind));
         end if;

         Write_Eol;
      end if;

      Id.Project             := Project;
      Id.Language            := Lang_Id;
      Id.Kind                := Kind;
      Id.Alternate_Languages := Alternate_Languages;
      Id.Locally_Removed     := Locally_Removed;
      Id.Index               := Index;
      Id.File                := File_Name;
      Id.Display_File        := Display_File;
      Id.Dep_Name            := Dependency_Name
                                  (File_Name, Lang_Id.Config.Dependency_Kind);
      Id.Naming_Exception    := Naming_Exception;

      --  Add the source id to the Unit_Sources_HT hash table, if the unit name
      --  is not null.

      if Unit /= No_Name then

         --  Note: we might be creating a dummy unit here, when we in fact have
         --  a separate. For instance, file file-bar.adb will initially be
         --  assumed to be the IMPL of unit "file.bar". Only later on (in
         --  Check_Object_Files) will we parse those units that only have an
         --  impl and no spec to make sure whether we have a Separate in fact
         --  (that significantly reduces the number of times we need to parse
         --  the files, since we are then only interested in those with no
         --  spec). We still need those dummy units in the table, since that's
         --  the name we find in the ALI file

         UData := Units_Htable.Get (Data.Tree.Units_HT, Unit);

         if UData = No_Unit_Index then
            UData := new Unit_Data;
            UData.Name := Unit;
            Units_Htable.Set (Data.Tree.Units_HT, Unit, UData);
         end if;

         Id.Unit := UData;

         --  Note that this updates Unit information as well

         Override_Kind (Id, Kind);
      end if;

      if Is_Compilable (Id) and then Config.Object_Generated then
         Id.Object   := Object_Name (File_Name, Config.Object_File_Suffix);
         Id.Switches := Switches_Name (File_Name);
      end if;

      if Path /= No_Path_Information then
         Id.Path := Path;
         Source_Paths_Htable.Set (Data.Tree.Source_Paths_HT, Path.Name, Id);
      end if;

      --  Add the source to the language list

      Id.Next_In_Lang := Lang_Id.First_Source;
      Lang_Id.First_Source := Id;

      if Source_To_Replace /= No_Source then
         Remove_Source (Source_To_Replace, Id);
      end if;

      Files_Htable.Set (Data.File_To_Source, File_Name, Id);
   end Add_Source;

   ------------------------------
   -- Canonical_Case_File_Name --
   ------------------------------

   function Canonical_Case_File_Name (Name : Name_Id) return File_Name_Type is
   begin
      if Osint.File_Names_Case_Sensitive then
         return File_Name_Type (Name);
      else
         Get_Name_String (Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
         return Name_Find;
      end if;
   end Canonical_Case_File_Name;

   -----------
   -- Check --
   -----------

   procedure Check
     (Project     : Project_Id;
      Data        : in out Tree_Processing_Data)
   is
      Specs     : Array_Element_Id;
      Bodies    : Array_Element_Id;
      Extending : Boolean := False;
      Prj_Data  : Project_Processing_Data;

   begin
      Initialize (Prj_Data, Project);

      Check_If_Externally_Built (Project, Data);

      --  Object, exec and source directories

      Get_Directories (Project, Data);

      --  Get the programming languages

      Check_Programming_Languages (Project, Data);

      if Project.Qualifier = Dry
        and then Project.Source_Dirs /= Nil_String
      then
         declare
            Source_Dirs      : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Source_Dirs,
                                    Project.Decl.Attributes, Data.Tree);
            Source_Files     : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Source_Files,
                                    Project.Decl.Attributes, Data.Tree);
            Source_List_File : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Source_List_File,
                                    Project.Decl.Attributes, Data.Tree);
            Languages        : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Languages,
                                    Project.Decl.Attributes, Data.Tree);

         begin
            if Source_Dirs.Values  = Nil_String
              and then Source_Files.Values = Nil_String
              and then Languages.Values = Nil_String
              and then Source_List_File.Default
            then
               Project.Source_Dirs := Nil_String;

            else
               Error_Msg
                 (Data.Flags,
                  "at least one of Source_Files, Source_Dirs or Languages "
                    & "must be declared empty for an abstract project",
                  Project.Location, Project);
            end if;
         end;
      end if;

      --  Check configuration. This must be done even for gnatmake (even though
      --  no user configuration file was provided) since the default config we
      --  generate indicates whether libraries are supported for instance.

      Check_Configuration (Project, Data);

      --  Library attributes

      Check_Library_Attributes (Project, Data);

      if Current_Verbosity = High then
         Show_Source_Dirs (Project, Data.Tree);
      end if;

      Extending := Project.Extends /= No_Project;

      Check_Package_Naming (Project, Data, Bodies => Bodies, Specs => Specs);

      --  Find the sources

      if Project.Source_Dirs /= Nil_String then
         Look_For_Sources (Prj_Data, Data);

         if not Project.Externally_Built
           and then not Extending
         then
            declare
               Language     : Language_Ptr;
               Source       : Source_Id;
               Alt_Lang     : Language_List;
               Continuation : Boolean := False;
               Iter         : Source_Iterator;

            begin
               Language := Project.Languages;
               while Language /= No_Language_Index loop

                  --  If there are no sources for this language, check if there
                  --  are sources for which this is an alternate language.

                  if Language.First_Source = No_Source
                    and then (Data.Flags.Require_Sources_Other_Lang
                               or else Language.Name = Name_Ada)
                  then
                     Iter := For_Each_Source (In_Tree => Data.Tree,
                                              Project => Project);
                     Source_Loop : loop
                        Source := Element (Iter);
                        exit Source_Loop when Source = No_Source
                          or else Source.Language = Language;

                        Alt_Lang := Source.Alternate_Languages;
                        while Alt_Lang /= null loop
                           exit Source_Loop when Alt_Lang.Language = Language;
                           Alt_Lang := Alt_Lang.Next;
                        end loop;

                        Next (Iter);
                     end loop Source_Loop;

                     if Source = No_Source then

                        Report_No_Sources
                          (Project,
                           Get_Name_String (Language.Display_Name),
                           Data,
                           Prj_Data.Source_List_File_Location,
                           Continuation);
                        Continuation := True;
                     end if;
                  end if;

                  Language := Language.Next;
               end loop;
            end;
         end if;
      end if;

      --  If a list of sources is specified in attribute Interfaces, set
      --  In_Interfaces only for the sources specified in the list.

      Check_Interfaces (Project, Data);

      --  If it is a library project file, check if it is a standalone library

      if Project.Library then
         Check_Stand_Alone_Library (Project, Data);
      end if;

      --  Put the list of Mains, if any, in the project data

      Get_Mains (Project, Data);

      Free (Prj_Data);
   end Check;

   --------------------
   -- Check_Ada_Name --
   --------------------

   procedure Check_Ada_Name (Name : String; Unit : out Name_Id) is
      The_Name        : String := Name;
      Real_Name       : Name_Id;
      Need_Letter     : Boolean := True;
      Last_Underscore : Boolean := False;
      OK              : Boolean := The_Name'Length > 0;
      First           : Positive;

      function Is_Reserved (Name : Name_Id) return Boolean;
      function Is_Reserved (S    : String)  return Boolean;
      --  Check that the given name is not an Ada 95 reserved word. The reason
      --  for the Ada 95 here is that we do not want to exclude the case of an
      --  Ada 95 unit called Interface (for example). In Ada 2005, such a unit
      --  name would be rejected anyway by the compiler. That means there is no
      --  requirement that the project file parser reject this.

      -----------------
      -- Is_Reserved --
      -----------------

      function Is_Reserved (S : String) return Boolean is
      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (S);
         return Is_Reserved (Name_Find);
      end Is_Reserved;

      -----------------
      -- Is_Reserved --
      -----------------

      function Is_Reserved (Name : Name_Id) return Boolean is
      begin
         if Get_Name_Table_Byte (Name) /= 0
           and then Name /= Name_Project
           and then Name /= Name_Extends
           and then Name /= Name_External
           and then Name not in Ada_2005_Reserved_Words
         then
            Unit := No_Name;

            if Current_Verbosity = High then
               Write_Str (The_Name);
               Write_Line (" is an Ada reserved word.");
            end if;

            return True;

         else
            return False;
         end if;
      end Is_Reserved;

   --  Start of processing for Check_Ada_Name

   begin
      To_Lower (The_Name);

      Name_Len := The_Name'Length;
      Name_Buffer (1 .. Name_Len) := The_Name;

      --  Special cases of children of packages A, G, I and S on VMS

      if OpenVMS_On_Target
        and then Name_Len > 3
        and then Name_Buffer (2 .. 3) = "__"
        and then
          ((Name_Buffer (1) = 'a') or else
           (Name_Buffer (1) = 'g') or else
           (Name_Buffer (1) = 'i') or else
           (Name_Buffer (1) = 's'))
      then
         Name_Buffer (2) := '.';
         Name_Buffer (3 .. Name_Len - 1) := Name_Buffer (4 .. Name_Len);
         Name_Len := Name_Len - 1;
      end if;

      Real_Name := Name_Find;

      if Is_Reserved (Real_Name) then
         return;
      end if;

      First := The_Name'First;

      for Index in The_Name'Range loop
         if Need_Letter then

            --  We need a letter (at the beginning, and following a dot),
            --  but we don't have one.

            if Is_Letter (The_Name (Index)) then
               Need_Letter := False;

            else
               OK := False;

               if Current_Verbosity = High then
                  Write_Int  (Types.Int (Index));
                  Write_Str  (": '");
                  Write_Char (The_Name (Index));
                  Write_Line ("' is not a letter.");
               end if;

               exit;
            end if;

         elsif Last_Underscore
           and then (The_Name (Index) = '_' or else The_Name (Index) = '.')
         then
            --  Two underscores are illegal, and a dot cannot follow
            --  an underscore.

            OK := False;

            if Current_Verbosity = High then
               Write_Int  (Types.Int (Index));
               Write_Str  (": '");
               Write_Char (The_Name (Index));
               Write_Line ("' is illegal here.");
            end if;

            exit;

         elsif The_Name (Index) = '.' then

            --  First, check if the name before the dot is not a reserved word

            if Is_Reserved (The_Name (First .. Index - 1)) then
               return;
            end if;

            First := Index + 1;

            --  We need a letter after a dot

            Need_Letter := True;

         elsif The_Name (Index) = '_' then
            Last_Underscore := True;

         else
            --  We need an letter or a digit

            Last_Underscore := False;

            if not Is_Alphanumeric (The_Name (Index)) then
               OK := False;

               if Current_Verbosity = High then
                  Write_Int  (Types.Int (Index));
                  Write_Str  (": '");
                  Write_Char (The_Name (Index));
                  Write_Line ("' is not alphanumeric.");
               end if;

               exit;
            end if;
         end if;
      end loop;

      --  Cannot end with an underscore or a dot

      OK := OK and then not Need_Letter and then not Last_Underscore;

      if OK then
         if First /= Name'First and then
           Is_Reserved (The_Name (First .. The_Name'Last))
         then
            return;
         end if;

         Unit := Real_Name;

      else
         --  Signal a problem with No_Name

         Unit := No_Name;
      end if;
   end Check_Ada_Name;

   -------------------------
   -- Check_Configuration --
   -------------------------

   procedure Check_Configuration
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data)
   is
      Dot_Replacement : File_Name_Type := No_File;
      Casing          : Casing_Type    := All_Lower_Case;
      Separate_Suffix : File_Name_Type := No_File;

      Lang_Index : Language_Ptr := No_Language_Index;
      --  The index of the language data being checked

      Prev_Index : Language_Ptr := No_Language_Index;
      --  The index of the previous language

      procedure Process_Project_Level_Simple_Attributes;
      --  Process the simple attributes at the project level

      procedure Process_Project_Level_Array_Attributes;
      --  Process the associate array attributes at the project level

      procedure Process_Packages;
      --  Read the packages of the project

      ----------------------
      -- Process_Packages --
      ----------------------

      procedure Process_Packages is
         Packages : Package_Id;
         Element  : Package_Element;

         procedure Process_Binder (Arrays : Array_Id);
         --  Process the associate array attributes of package Binder

         procedure Process_Builder (Attributes : Variable_Id);
         --  Process the simple attributes of package Builder

         procedure Process_Compiler (Arrays : Array_Id);
         --  Process the associate array attributes of package Compiler

         procedure Process_Naming (Attributes : Variable_Id);
         --  Process the simple attributes of package Naming

         procedure Process_Naming (Arrays : Array_Id);
         --  Process the associate array attributes of package Naming

         procedure Process_Linker (Attributes : Variable_Id);
         --  Process the simple attributes of package Linker of a
         --  configuration project.

         --------------------
         -- Process_Binder --
         --------------------

         procedure Process_Binder (Arrays : Array_Id) is
            Current_Array_Id : Array_Id;
            Current_Array    : Array_Data;
            Element_Id       : Array_Element_Id;
            Element          : Array_Element;

         begin
            --  Process the associative array attribute of package Binder

            Current_Array_Id := Arrays;
            while Current_Array_Id /= No_Array loop
               Current_Array := Data.Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := Data.Tree.Array_Elements.Table (Element_Id);

                  if Element.Index /= All_Other_Names then

                     --  Get the name of the language

                     Lang_Index :=
                       Get_Language_From_Name
                         (Project, Get_Name_String (Element.Index));

                     if Lang_Index /= No_Language_Index then
                        case Current_Array.Name is
                           when Name_Driver =>

                              --  Attribute Driver (<language>)

                              Lang_Index.Config.Binder_Driver :=
                                File_Name_Type (Element.Value.Value);

                           when Name_Required_Switches =>
                              Put
                                (Into_List =>
                                   Lang_Index.Config.Binder_Required_Switches,
                                 From_List => Element.Value.Values,
                                 In_Tree   => Data.Tree);

                           when Name_Prefix =>

                              --  Attribute Prefix (<language>)

                              Lang_Index.Config.Binder_Prefix :=
                                Element.Value.Value;

                           when Name_Objects_Path =>

                              --  Attribute Objects_Path (<language>)

                              Lang_Index.Config.Objects_Path :=
                                Element.Value.Value;

                           when Name_Objects_Path_File =>

                              --  Attribute Objects_Path (<language>)

                              Lang_Index.Config.Objects_Path_File :=
                                Element.Value.Value;

                           when others =>
                              null;
                        end case;
                     end if;
                  end if;

                  Element_Id := Element.Next;
               end loop;

               Current_Array_Id := Current_Array.Next;
            end loop;
         end Process_Binder;

         ---------------------
         -- Process_Builder --
         ---------------------

         procedure Process_Builder (Attributes : Variable_Id) is
            Attribute_Id : Variable_Id;
            Attribute    : Variable;

         begin
            --  Process non associated array attribute from package Builder

            Attribute_Id := Attributes;
            while Attribute_Id /= No_Variable loop
               Attribute :=
                 Data.Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Executable_Suffix then

                     --  Attribute Executable_Suffix: the suffix of the
                     --  executables.

                     Project.Config.Executable_Suffix :=
                       Attribute.Value.Value;
                  end if;
               end if;

               Attribute_Id := Attribute.Next;
            end loop;
         end Process_Builder;

         ----------------------
         -- Process_Compiler --
         ----------------------

         procedure Process_Compiler (Arrays : Array_Id) is
            Current_Array_Id : Array_Id;
            Current_Array    : Array_Data;
            Element_Id       : Array_Element_Id;
            Element          : Array_Element;
            List             : String_List_Id;

         begin
            --  Process the associative array attribute of package Compiler

            Current_Array_Id := Arrays;
            while Current_Array_Id /= No_Array loop
               Current_Array := Data.Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := Data.Tree.Array_Elements.Table (Element_Id);

                  if Element.Index /= All_Other_Names then

                     --  Get the name of the language

                     Lang_Index := Get_Language_From_Name
                       (Project, Get_Name_String (Element.Index));

                     if Lang_Index /= No_Language_Index then
                        case Current_Array.Name is
                        when Name_Dependency_Switches =>

                           --  Attribute Dependency_Switches (<language>)

                           if Lang_Index.Config.Dependency_Kind = None then
                              Lang_Index.Config.Dependency_Kind := Makefile;
                           end if;

                           List := Element.Value.Values;

                           if List /= Nil_String then
                              Put (Into_List =>
                                     Lang_Index.Config.Dependency_Option,
                                   From_List => List,
                                   In_Tree   => Data.Tree);
                           end if;

                        when Name_Dependency_Driver =>

                           --  Attribute Dependency_Driver (<language>)

                           if Lang_Index.Config.Dependency_Kind = None then
                              Lang_Index.Config.Dependency_Kind := Makefile;
                           end if;

                           List := Element.Value.Values;

                           if List /= Nil_String then
                              Put (Into_List =>
                                     Lang_Index.Config.Compute_Dependency,
                                   From_List => List,
                                   In_Tree   => Data.Tree);
                           end if;

                        when Name_Include_Switches =>

                           --  Attribute Include_Switches (<language>)

                           List := Element.Value.Values;

                           if List = Nil_String then
                              Error_Msg
                                (Data.Flags, "include option cannot be null",
                                 Element.Value.Location, Project);
                           end if;

                           Put (Into_List => Lang_Index.Config.Include_Option,
                                From_List => List,
                                In_Tree   => Data.Tree);

                        when Name_Include_Path =>

                           --  Attribute Include_Path (<language>)

                           Lang_Index.Config.Include_Path :=
                             Element.Value.Value;

                        when Name_Include_Path_File =>

                           --  Attribute Include_Path_File (<language>)

                           Lang_Index.Config.Include_Path_File :=
                               Element.Value.Value;

                        when Name_Driver =>

                           --  Attribute Driver (<language>)

                           Lang_Index.Config.Compiler_Driver :=
                             File_Name_Type (Element.Value.Value);

                        when Name_Required_Switches |
                             Name_Leading_Required_Switches =>
                           Put (Into_List =>
                                  Lang_Index.Config.
                                    Compiler_Leading_Required_Switches,
                                From_List => Element.Value.Values,
                                In_Tree   => Data.Tree);

                        when Name_Trailing_Required_Switches =>
                           Put (Into_List =>
                                  Lang_Index.Config.
                                    Compiler_Trailing_Required_Switches,
                                From_List => Element.Value.Values,
                                In_Tree   => Data.Tree);

                        when Name_Path_Syntax =>
                           begin
                              Lang_Index.Config.Path_Syntax :=
                                  Path_Syntax_Kind'Value
                                    (Get_Name_String (Element.Value.Value));

                           exception
                              when Constraint_Error =>
                                 Error_Msg
                                   (Data.Flags,
                                    "invalid value for Path_Syntax",
                                    Element.Value.Location, Project);
                           end;

                        when Name_Object_File_Suffix =>
                           if Get_Name_String (Element.Value.Value) = "" then
                              Error_Msg
                                (Data.Flags,
                                 "object file suffix cannot be empty",
                                 Element.Value.Location, Project);

                           else
                              Lang_Index.Config.Object_File_Suffix :=
                                Element.Value.Value;
                           end if;

                        when Name_Object_File_Switches =>
                           Put (Into_List =>
                                  Lang_Index.Config.Object_File_Switches,
                                From_List => Element.Value.Values,
                                In_Tree   => Data.Tree);

                        when Name_Pic_Option =>

                           --  Attribute Compiler_Pic_Option (<language>)

                           List := Element.Value.Values;

                           if List = Nil_String then
                              Error_Msg
                                (Data.Flags,
                                 "compiler PIC option cannot be null",
                                 Element.Value.Location, Project);
                           end if;

                           Put (Into_List =>
                                  Lang_Index.Config.Compilation_PIC_Option,
                                From_List => List,
                                In_Tree   => Data.Tree);

                        when Name_Mapping_File_Switches =>

                           --  Attribute Mapping_File_Switches (<language>)

                           List := Element.Value.Values;

                           if List = Nil_String then
                              Error_Msg
                                (Data.Flags,
                                 "mapping file switches cannot be null",
                                 Element.Value.Location, Project);
                           end if;

                           Put (Into_List =>
                                Lang_Index.Config.Mapping_File_Switches,
                                From_List => List,
                                In_Tree   => Data.Tree);

                        when Name_Mapping_Spec_Suffix =>

                           --  Attribute Mapping_Spec_Suffix (<language>)

                           Lang_Index.Config.Mapping_Spec_Suffix :=
                             File_Name_Type (Element.Value.Value);

                        when Name_Mapping_Body_Suffix =>

                           --  Attribute Mapping_Body_Suffix (<language>)

                           Lang_Index.Config.Mapping_Body_Suffix :=
                             File_Name_Type (Element.Value.Value);

                        when Name_Config_File_Switches =>

                           --  Attribute Config_File_Switches (<language>)

                           List := Element.Value.Values;

                           if List = Nil_String then
                              Error_Msg
                                (Data.Flags,
                                 "config file switches cannot be null",
                                 Element.Value.Location, Project);
                           end if;

                           Put (Into_List =>
                                  Lang_Index.Config.Config_File_Switches,
                                From_List => List,
                                In_Tree   => Data.Tree);

                        when Name_Objects_Path =>

                           --  Attribute Objects_Path (<language>)

                           Lang_Index.Config.Objects_Path :=
                             Element.Value.Value;

                        when Name_Objects_Path_File =>

                           --  Attribute Objects_Path_File (<language>)

                           Lang_Index.Config.Objects_Path_File :=
                             Element.Value.Value;

                        when Name_Config_Body_File_Name =>

                           --  Attribute Config_Body_File_Name (<language>)

                           Lang_Index.Config.Config_Body :=
                             Element.Value.Value;

                        when Name_Config_Body_File_Name_Pattern =>

                           --  Attribute Config_Body_File_Name_Pattern
                           --  (<language>)

                           Lang_Index.Config.Config_Body_Pattern :=
                             Element.Value.Value;

                        when Name_Config_Spec_File_Name =>

                           --  Attribute Config_Spec_File_Name (<language>)

                           Lang_Index.Config.Config_Spec :=
                             Element.Value.Value;

                        when Name_Config_Spec_File_Name_Pattern =>

                           --  Attribute Config_Spec_File_Name_Pattern
                           --  (<language>)

                           Lang_Index.Config.Config_Spec_Pattern :=
                             Element.Value.Value;

                        when Name_Config_File_Unique =>

                           --  Attribute Config_File_Unique (<language>)

                           begin
                              Lang_Index.Config.Config_File_Unique :=
                                Boolean'Value
                                  (Get_Name_String (Element.Value.Value));
                           exception
                              when Constraint_Error =>
                                 Error_Msg
                                   (Data.Flags,
                                    "illegal value for Config_File_Unique",
                                    Element.Value.Location, Project);
                           end;

                        when others =>
                           null;
                        end case;
                     end if;
                  end if;

                  Element_Id := Element.Next;
               end loop;

               Current_Array_Id := Current_Array.Next;
            end loop;
         end Process_Compiler;

         --------------------
         -- Process_Naming --
         --------------------

         procedure Process_Naming (Attributes : Variable_Id) is
            Attribute_Id : Variable_Id;
            Attribute    : Variable;

         begin
            --  Process non associated array attribute from package Naming

            Attribute_Id := Attributes;
            while Attribute_Id /= No_Variable loop
               Attribute := Data.Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Separate_Suffix then

                     --  Attribute Separate_Suffix

                     Get_Name_String (Attribute.Value.Value);
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Separate_Suffix := Name_Find;

                  elsif Attribute.Name = Name_Casing then

                     --  Attribute Casing

                     begin
                        Casing :=
                          Value (Get_Name_String (Attribute.Value.Value));

                     exception
                        when Constraint_Error =>
                           Error_Msg
                             (Data.Flags,
                              "invalid value for Casing",
                              Attribute.Value.Location, Project);
                     end;

                  elsif Attribute.Name = Name_Dot_Replacement then

                     --  Attribute Dot_Replacement

                     Dot_Replacement := File_Name_Type (Attribute.Value.Value);

                  end if;
               end if;

               Attribute_Id := Attribute.Next;
            end loop;
         end Process_Naming;

         procedure Process_Naming (Arrays : Array_Id) is
            Current_Array_Id : Array_Id;
            Current_Array    : Array_Data;
            Element_Id       : Array_Element_Id;
            Element          : Array_Element;

         begin
            --  Process the associative array attribute of package Naming

            Current_Array_Id := Arrays;
            while Current_Array_Id /= No_Array loop
               Current_Array := Data.Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := Data.Tree.Array_Elements.Table (Element_Id);

                  --  Get the name of the language

                  Lang_Index := Get_Language_From_Name
                    (Project, Get_Name_String (Element.Index));

                  if Lang_Index /= No_Language_Index then
                     case Current_Array.Name is
                        when Name_Spec_Suffix | Name_Specification_Suffix =>

                           --  Attribute Spec_Suffix (<language>)

                           Get_Name_String (Element.Value.Value);
                           Canonical_Case_File_Name
                             (Name_Buffer (1 .. Name_Len));
                           Lang_Index.Config.Naming_Data.Spec_Suffix :=
                             Name_Find;

                        when Name_Implementation_Suffix | Name_Body_Suffix =>

                           Get_Name_String (Element.Value.Value);
                           Canonical_Case_File_Name
                             (Name_Buffer (1 .. Name_Len));

                           --  Attribute Body_Suffix (<language>)

                           Lang_Index.Config.Naming_Data.Body_Suffix :=
                             Name_Find;
                           Lang_Index.Config.Naming_Data.Separate_Suffix :=
                             Lang_Index.Config.Naming_Data.Body_Suffix;

                        when others =>
                           null;
                     end case;
                  end if;

                  Element_Id := Element.Next;
               end loop;

               Current_Array_Id := Current_Array.Next;
            end loop;
         end Process_Naming;

         --------------------
         -- Process_Linker --
         --------------------

         procedure Process_Linker (Attributes : Variable_Id) is
            Attribute_Id : Variable_Id;
            Attribute    : Variable;

         begin
            --  Process non associated array attribute from package Linker

            Attribute_Id := Attributes;
            while Attribute_Id /= No_Variable loop
               Attribute :=
                 Data.Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Driver then

                     --  Attribute Linker'Driver: the default linker to use

                     Project.Config.Linker :=
                       Path_Name_Type (Attribute.Value.Value);

                     --  Linker'Driver is also used to link shared libraries
                     --  if the obsolescent attribute Library_GCC has not been
                     --  specified.

                     if Project.Config.Shared_Lib_Driver = No_File then
                        Project.Config.Shared_Lib_Driver :=
                          File_Name_Type (Attribute.Value.Value);
                     end if;

                  elsif Attribute.Name = Name_Required_Switches then

                     --  Attribute Required_Switches: the minimum
                     --  options to use when invoking the linker

                     Put (Into_List => Project.Config.Minimum_Linker_Options,
                          From_List => Attribute.Value.Values,
                          In_Tree   => Data.Tree);

                  elsif Attribute.Name = Name_Map_File_Option then
                     Project.Config.Map_File_Option := Attribute.Value.Value;

                  elsif Attribute.Name = Name_Max_Command_Line_Length then
                     begin
                        Project.Config.Max_Command_Line_Length :=
                          Natural'Value (Get_Name_String
                                         (Attribute.Value.Value));

                     exception
                        when Constraint_Error =>
                           Error_Msg
                             (Data.Flags,
                              "value must be positive or equal to 0",
                              Attribute.Value.Location, Project);
                     end;

                  elsif Attribute.Name = Name_Response_File_Format then
                     declare
                        Name  : Name_Id;

                     begin
                        Get_Name_String (Attribute.Value.Value);
                        To_Lower (Name_Buffer (1 .. Name_Len));
                        Name := Name_Find;

                        if Name = Name_None then
                           Project.Config.Resp_File_Format := None;

                        elsif Name = Name_Gnu then
                           Project.Config.Resp_File_Format := GNU;

                        elsif Name = Name_Object_List then
                           Project.Config.Resp_File_Format := Object_List;

                        elsif Name = Name_Option_List then
                           Project.Config.Resp_File_Format := Option_List;

                        else
                           Error_Msg
                             (Data.Flags,
                              "illegal response file format",
                              Attribute.Value.Location, Project);
                        end if;
                     end;

                  elsif Attribute.Name = Name_Response_File_Switches then
                     Put (Into_List => Project.Config.Resp_File_Options,
                          From_List => Attribute.Value.Values,
                          In_Tree   => Data.Tree);
                  end if;
               end if;

               Attribute_Id := Attribute.Next;
            end loop;
         end Process_Linker;

      --  Start of processing for Process_Packages

      begin
         Packages := Project.Decl.Packages;
         while Packages /= No_Package loop
            Element := Data.Tree.Packages.Table (Packages);

            case Element.Name is
               when Name_Binder =>

                  --  Process attributes of package Binder

                  Process_Binder (Element.Decl.Arrays);

               when Name_Builder =>

                  --  Process attributes of package Builder

                  Process_Builder (Element.Decl.Attributes);

               when Name_Compiler =>

                  --  Process attributes of package Compiler

                  Process_Compiler (Element.Decl.Arrays);

               when Name_Linker =>

                  --  Process attributes of package Linker

                  Process_Linker (Element.Decl.Attributes);

               when Name_Naming =>

                  --  Process attributes of package Naming

                  Process_Naming (Element.Decl.Attributes);
                  Process_Naming (Element.Decl.Arrays);

               when others =>
                  null;
            end case;

            Packages := Element.Next;
         end loop;
      end Process_Packages;

      ---------------------------------------------
      -- Process_Project_Level_Simple_Attributes --
      ---------------------------------------------

      procedure Process_Project_Level_Simple_Attributes is
         Attribute_Id : Variable_Id;
         Attribute    : Variable;
         List         : String_List_Id;

      begin
         --  Process non associated array attribute at project level

         Attribute_Id := Project.Decl.Attributes;
         while Attribute_Id /= No_Variable loop
            Attribute :=
              Data.Tree.Variable_Elements.Table (Attribute_Id);

            if not Attribute.Value.Default then
               if Attribute.Name = Name_Target then

                  --  Attribute Target: the target specified

                  Project.Config.Target := Attribute.Value.Value;

               elsif Attribute.Name = Name_Library_Builder then

                  --  Attribute Library_Builder: the application to invoke
                  --  to build libraries.

                  Project.Config.Library_Builder :=
                    Path_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Archive_Builder then

                  --  Attribute Archive_Builder: the archive builder
                  --  (usually "ar") and its minimum options (usually "cr").

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       (Data.Flags,
                        "archive builder cannot be null",
                        Attribute.Value.Location, Project);
                  end if;

                  Put (Into_List => Project.Config.Archive_Builder,
                       From_List => List,
                       In_Tree   => Data.Tree);

               elsif Attribute.Name = Name_Archive_Builder_Append_Option then

                  --  Attribute Archive_Builder: the archive builder
                  --  (usually "ar") and its minimum options (usually "cr").

                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put
                       (Into_List =>
                          Project.Config.Archive_Builder_Append_Option,
                        From_List => List,
                        In_Tree   => Data.Tree);
                  end if;

               elsif Attribute.Name = Name_Archive_Indexer then

                  --  Attribute Archive_Indexer: the optional archive
                  --  indexer (usually "ranlib") with its minimum options
                  --  (usually none).

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       (Data.Flags,
                        "archive indexer cannot be null",
                        Attribute.Value.Location, Project);
                  end if;

                  Put (Into_List => Project.Config.Archive_Indexer,
                       From_List => List,
                       In_Tree   => Data.Tree);

               elsif Attribute.Name = Name_Library_Partial_Linker then

                  --  Attribute Library_Partial_Linker: the optional linker
                  --  driver with its minimum options, to partially link
                  --  archives.

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       (Data.Flags,
                        "partial linker cannot be null",
                        Attribute.Value.Location, Project);
                  end if;

                  Put (Into_List => Project.Config.Lib_Partial_Linker,
                       From_List => List,
                       In_Tree   => Data.Tree);

               elsif Attribute.Name = Name_Library_GCC then
                  Project.Config.Shared_Lib_Driver :=
                    File_Name_Type (Attribute.Value.Value);
                  Error_Msg
                    (Data.Flags,
                     "?Library_'G'C'C is an obsolescent attribute, " &
                     "use Linker''Driver instead",
                     Attribute.Value.Location, Project);

               elsif Attribute.Name = Name_Archive_Suffix then
                  Project.Config.Archive_Suffix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Linker_Executable_Option then

                  --  Attribute Linker_Executable_Option: optional options
                  --  to specify an executable name. Defaults to "-o".

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       (Data.Flags,
                        "linker executable option cannot be null",
                        Attribute.Value.Location, Project);
                  end if;

                  Put (Into_List => Project.Config.Linker_Executable_Option,
                       From_List => List,
                       In_Tree   => Data.Tree);

               elsif Attribute.Name = Name_Linker_Lib_Dir_Option then

                  --  Attribute Linker_Lib_Dir_Option: optional options
                  --  to specify a library search directory. Defaults to
                  --  "-L".

                  Get_Name_String (Attribute.Value.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Data.Flags,
                        "linker library directory option cannot be empty",
                        Attribute.Value.Location, Project);
                  end if;

                  Project.Config.Linker_Lib_Dir_Option :=
                    Attribute.Value.Value;

               elsif Attribute.Name = Name_Linker_Lib_Name_Option then

                  --  Attribute Linker_Lib_Name_Option: optional options
                  --  to specify the name of a library to be linked in.
                  --  Defaults to "-l".

                  Get_Name_String (Attribute.Value.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Data.Flags,
                        "linker library name option cannot be empty",
                        Attribute.Value.Location, Project);
                  end if;

                  Project.Config.Linker_Lib_Name_Option :=
                    Attribute.Value.Value;

               elsif Attribute.Name = Name_Run_Path_Option then

                  --  Attribute Run_Path_Option: optional options to
                  --  specify a path for libraries.

                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List => Project.Config.Run_Path_Option,
                          From_List => List,
                          In_Tree   => Data.Tree);
                  end if;

               elsif Attribute.Name = Name_Separate_Run_Path_Options then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project.Config.Separate_Run_Path_Options :=
                       Boolean'Value (Get_Name_String (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Data.Flags,
                           "invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Separate_Run_Path_Options",
                           Attribute.Value.Location, Project);
                  end;

               elsif Attribute.Name = Name_Library_Support then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project.Config.Lib_Support :=
                       Library_Support'Value (Get_Name_String
                                              (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Data.Flags,
                           "invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Library_Support",
                           Attribute.Value.Location, Project);
                  end;

               elsif Attribute.Name = Name_Shared_Library_Prefix then
                  Project.Config.Shared_Lib_Prefix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Shared_Library_Suffix then
                  Project.Config.Shared_Lib_Suffix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Symbolic_Link_Supported then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project.Config.Symbolic_Link_Supported :=
                       Boolean'Value (Get_Name_String
                                      (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Data.Flags,
                           "invalid value """
                             & Get_Name_String (Attribute.Value.Value)
                             & """ for Symbolic_Link_Supported",
                           Attribute.Value.Location, Project);
                  end;

               elsif
                 Attribute.Name = Name_Library_Major_Minor_Id_Supported
               then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project.Config.Lib_Maj_Min_Id_Supported :=
                       Boolean'Value (Get_Name_String
                                      (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Data.Flags,
                           "invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Library_Major_Minor_Id_Supported",
                           Attribute.Value.Location, Project);
                  end;

               elsif Attribute.Name = Name_Library_Auto_Init_Supported then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project.Config.Auto_Init_Supported :=
                       Boolean'Value (Get_Name_String (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Data.Flags,
                           "invalid value """
                             & Get_Name_String (Attribute.Value.Value)
                             & """ for Library_Auto_Init_Supported",
                           Attribute.Value.Location, Project);
                  end;

               elsif Attribute.Name = Name_Shared_Library_Minimum_Switches then
                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List => Project.Config.Shared_Lib_Min_Options,
                          From_List => List,
                          In_Tree   => Data.Tree);
                  end if;

               elsif Attribute.Name = Name_Library_Version_Switches then
                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List => Project.Config.Lib_Version_Options,
                          From_List => List,
                          In_Tree   => Data.Tree);
                  end if;
               end if;
            end if;

            Attribute_Id := Attribute.Next;
         end loop;
      end Process_Project_Level_Simple_Attributes;

      --------------------------------------------
      -- Process_Project_Level_Array_Attributes --
      --------------------------------------------

      procedure Process_Project_Level_Array_Attributes is
         Current_Array_Id : Array_Id;
         Current_Array    : Array_Data;
         Element_Id       : Array_Element_Id;
         Element          : Array_Element;
         List             : String_List_Id;

      begin
         --  Process the associative array attributes at project level

         Current_Array_Id := Project.Decl.Arrays;
         while Current_Array_Id /= No_Array loop
            Current_Array := Data.Tree.Arrays.Table (Current_Array_Id);

            Element_Id := Current_Array.Value;
            while Element_Id /= No_Array_Element loop
               Element := Data.Tree.Array_Elements.Table (Element_Id);

               --  Get the name of the language

               Lang_Index :=
                 Get_Language_From_Name
                   (Project, Get_Name_String (Element.Index));

               if Lang_Index /= No_Language_Index then
                  case Current_Array.Name is
                     when Name_Inherit_Source_Path =>
                        List := Element.Value.Values;

                        if List /= Nil_String then
                           Put
                             (Into_List  =>
                                Lang_Index.Config.Include_Compatible_Languages,
                              From_List  => List,
                              In_Tree    => Data.Tree,
                              Lower_Case => True);
                        end if;

                     when Name_Toolchain_Description =>

                        --  Attribute Toolchain_Description (<language>)

                        Lang_Index.Config.Toolchain_Description :=
                          Element.Value.Value;

                     when Name_Toolchain_Version =>

                        --  Attribute Toolchain_Version (<language>)

                        Lang_Index.Config.Toolchain_Version :=
                          Element.Value.Value;

                     when Name_Runtime_Library_Dir =>

                        --  Attribute Runtime_Library_Dir (<language>)

                        Lang_Index.Config.Runtime_Library_Dir :=
                          Element.Value.Value;

                     when Name_Runtime_Source_Dir =>

                        --  Attribute Runtime_Library_Dir (<language>)

                        Lang_Index.Config.Runtime_Source_Dir :=
                          Element.Value.Value;

                     when Name_Object_Generated =>
                        declare
                           pragma Unsuppress (All_Checks);
                           Value : Boolean;

                        begin
                           Value :=
                             Boolean'Value
                               (Get_Name_String (Element.Value.Value));

                           Lang_Index.Config.Object_Generated := Value;

                           --  If no object is generated, no object may be
                           --  linked.

                           if not Value then
                              Lang_Index.Config.Objects_Linked := False;
                           end if;

                        exception
                           when Constraint_Error =>
                              Error_Msg
                                (Data.Flags,
                                 "invalid value """
                                 & Get_Name_String (Element.Value.Value)
                                 & """ for Object_Generated",
                                 Element.Value.Location, Project);
                        end;

                     when Name_Objects_Linked =>
                        declare
                           pragma Unsuppress (All_Checks);
                           Value : Boolean;

                        begin
                           Value :=
                             Boolean'Value
                               (Get_Name_String (Element.Value.Value));

                           --  No change if Object_Generated is False, as this
                           --  forces Objects_Linked to be False too.

                           if Lang_Index.Config.Object_Generated then
                              Lang_Index.Config.Objects_Linked := Value;
                           end if;

                        exception
                           when Constraint_Error =>
                              Error_Msg
                                (Data.Flags,
                                 "invalid value """
                                 & Get_Name_String (Element.Value.Value)
                                 & """ for Objects_Linked",
                                 Element.Value.Location, Project);
                        end;
                     when others =>
                        null;
                  end case;
               end if;

               Element_Id := Element.Next;
            end loop;

            Current_Array_Id := Current_Array.Next;
         end loop;
      end Process_Project_Level_Array_Attributes;

   --  Start of processing for Check_Configuration

   begin
      Process_Project_Level_Simple_Attributes;
      Process_Project_Level_Array_Attributes;
      Process_Packages;

      --  For unit based languages, set Casing, Dot_Replacement and
      --  Separate_Suffix in Naming_Data.

      Lang_Index := Project.Languages;
      while Lang_Index /= No_Language_Index loop
         if Lang_Index.Name = Name_Ada then
            Lang_Index.Config.Naming_Data.Casing := Casing;
            Lang_Index.Config.Naming_Data.Dot_Replacement := Dot_Replacement;

            if Separate_Suffix /= No_File then
               Lang_Index.Config.Naming_Data.Separate_Suffix :=
                 Separate_Suffix;
            end if;

            exit;
         end if;

         Lang_Index := Lang_Index.Next;
      end loop;

      --  Give empty names to various prefixes/suffixes, if they have not
      --  been specified in the configuration.

      if Project.Config.Archive_Suffix = No_File then
         Project.Config.Archive_Suffix := Empty_File;
      end if;

      if Project.Config.Shared_Lib_Prefix = No_File then
         Project.Config.Shared_Lib_Prefix := Empty_File;
      end if;

      if Project.Config.Shared_Lib_Suffix = No_File then
         Project.Config.Shared_Lib_Suffix := Empty_File;
      end if;

      Lang_Index := Project.Languages;
      while Lang_Index /= No_Language_Index loop

         --  For all languages, Compiler_Driver needs to be specified. This is
         --  only needed if we do intend to compile (not in GPS for instance).

         if Data.Flags.Compiler_Driver_Mandatory
           and then Lang_Index.Config.Compiler_Driver = No_File
         then
            Error_Msg_Name_1 := Lang_Index.Display_Name;
            Error_Msg
              (Data.Flags,
               "?no compiler specified for language %%" &
                 ", ignoring all its sources",
               No_Location, Project);

            if Lang_Index = Project.Languages then
               Project.Languages := Lang_Index.Next;
            else
               Prev_Index.Next := Lang_Index.Next;
            end if;

         elsif Lang_Index.Name = Name_Ada then
            Prev_Index := Lang_Index;

            --  For unit based languages, Dot_Replacement, Spec_Suffix and
            --  Body_Suffix need to be specified.

            if Lang_Index.Config.Naming_Data.Dot_Replacement = No_File then
               Error_Msg
                 (Data.Flags,
                  "Dot_Replacement not specified for Ada",
                  No_Location, Project);
            end if;

            if Lang_Index.Config.Naming_Data.Spec_Suffix = No_File then
               Error_Msg
                 (Data.Flags,
                  "Spec_Suffix not specified for Ada",
                  No_Location, Project);
            end if;

            if Lang_Index.Config.Naming_Data.Body_Suffix = No_File then
               Error_Msg
                 (Data.Flags,
                  "Body_Suffix not specified for Ada",
                  No_Location, Project);
            end if;

         else
            Prev_Index := Lang_Index;

            --  For file based languages, either Spec_Suffix or Body_Suffix
            --  need to be specified.

            if Data.Flags.Require_Sources_Other_Lang
              and then Lang_Index.Config.Naming_Data.Spec_Suffix = No_File
              and then Lang_Index.Config.Naming_Data.Body_Suffix = No_File
            then
               Error_Msg_Name_1 := Lang_Index.Display_Name;
               Error_Msg
                 (Data.Flags,
                  "no suffixes specified for %%",
                  No_Location, Project);
            end if;
         end if;

         Lang_Index := Lang_Index.Next;
      end loop;
   end Check_Configuration;

   -------------------------------
   -- Check_If_Externally_Built --
   -------------------------------

   procedure Check_If_Externally_Built
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data)
   is
      Externally_Built : constant Variable_Value :=
                           Util.Value_Of
                            (Name_Externally_Built,
                             Project.Decl.Attributes, Data.Tree);

   begin
      if not Externally_Built.Default then
         Get_Name_String (Externally_Built.Value);
         To_Lower (Name_Buffer (1 .. Name_Len));

         if Name_Buffer (1 .. Name_Len) = "true" then
            Project.Externally_Built := True;

         elsif Name_Buffer (1 .. Name_Len) /= "false" then
            Error_Msg (Data.Flags,
                       "Externally_Built may only be true or false",
                       Externally_Built.Location, Project);
         end if;
      end if;

      --  A virtual project extending an externally built project is itself
      --  externally built.

      if Project.Virtual and then Project.Extends /= No_Project then
         Project.Externally_Built := Project.Extends.Externally_Built;
      end if;

      if Current_Verbosity = High then
         Write_Str ("Project is ");

         if not Project.Externally_Built then
            Write_Str ("not ");
         end if;

         Write_Line ("externally built.");
      end if;
   end Check_If_Externally_Built;

   ----------------------
   -- Check_Interfaces --
   ----------------------

   procedure Check_Interfaces
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data)
   is
      Interfaces : constant Prj.Variable_Value :=
                     Prj.Util.Value_Of
                       (Snames.Name_Interfaces,
                        Project.Decl.Attributes,
                        Data.Tree);

      List      : String_List_Id;
      Element   : String_Element;
      Name      : File_Name_Type;
      Iter      : Source_Iterator;
      Source    : Source_Id;
      Project_2 : Project_Id;
      Other     : Source_Id;

   begin
      if not Interfaces.Default then

         --  Set In_Interfaces to False for all sources. It will be set to True
         --  later for the sources in the Interfaces list.

         Project_2 := Project;
         while Project_2 /= No_Project loop
            Iter := For_Each_Source (Data.Tree, Project_2);
            loop
               Source := Prj.Element (Iter);
               exit when Source = No_Source;
               Source.In_Interfaces := False;
               Next (Iter);
            end loop;

            Project_2 := Project_2.Extends;
         end loop;

         List := Interfaces.Values;
         while List /= Nil_String loop
            Element := Data.Tree.String_Elements.Table (List);
            Name := Canonical_Case_File_Name (Element.Value);

            Project_2 := Project;
            Big_Loop :
            while Project_2 /= No_Project loop
               Iter := For_Each_Source (Data.Tree, Project_2);

               loop
                  Source := Prj.Element (Iter);
                  exit when Source = No_Source;

                  if Source.File = Name then
                     if not Source.Locally_Removed then
                        Source.In_Interfaces := True;
                        Source.Declared_In_Interfaces := True;

                        Other := Other_Part (Source);

                        if Other /= No_Source then
                           Other.In_Interfaces := True;
                           Other.Declared_In_Interfaces := True;
                        end if;

                        if Current_Verbosity = High then
                           Write_Str ("   interface: ");
                           Write_Line (Get_Name_String (Source.Path.Name));
                        end if;
                     end if;

                     exit Big_Loop;
                  end if;

                  Next (Iter);
               end loop;

               Project_2 := Project_2.Extends;
            end loop Big_Loop;

            if Source = No_Source then
               Error_Msg_File_1 := File_Name_Type (Element.Value);
               Error_Msg_Name_1 := Project.Name;

               Error_Msg
                 (Data.Flags,
                  "{ cannot be an interface of project %% "
                  & "as it is not one of its sources",
                  Element.Location, Project);
            end if;

            List := Element.Next;
         end loop;

         Project.Interfaces_Defined := True;

      elsif Project.Extends /= No_Project then
         Project.Interfaces_Defined := Project.Extends.Interfaces_Defined;

         if Project.Interfaces_Defined then
            Iter := For_Each_Source (Data.Tree, Project);
            loop
               Source := Prj.Element (Iter);
               exit when Source = No_Source;

               if not Source.Declared_In_Interfaces then
                  Source.In_Interfaces := False;
               end if;

               Next (Iter);
            end loop;
         end if;
      end if;
   end Check_Interfaces;

   --------------------------
   -- Check_Package_Naming --
   --------------------------

   procedure Check_Package_Naming
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data;
      Bodies  : out Array_Element_Id;
      Specs   : out Array_Element_Id)
   is
      Naming_Id : constant Package_Id :=
                    Util.Value_Of
                      (Name_Naming, Project.Decl.Packages, Data.Tree);
      Naming    : Package_Element;

      Ada_Body_Suffix_Loc : Source_Ptr := No_Location;

      procedure Check_Naming;
      --  Check the validity of the Naming package (suffixes valid, ...)

      procedure Check_Common
        (Dot_Replacement : in out File_Name_Type;
         Casing          : in out Casing_Type;
         Casing_Defined  : out Boolean;
         Separate_Suffix : in out File_Name_Type;
         Sep_Suffix_Loc  : out Source_Ptr);
      --  Check attributes common

      procedure Process_Exceptions_File_Based
        (Lang_Id : Language_Ptr;
         Kind    : Source_Kind);
      procedure Process_Exceptions_Unit_Based
        (Lang_Id : Language_Ptr;
         Kind    : Source_Kind);
      --  Process the naming exceptions for the two types of languages

      procedure Initialize_Naming_Data;
      --  Initialize internal naming data for the various languages

      ------------------
      -- Check_Common --
      ------------------

      procedure Check_Common
        (Dot_Replacement : in out File_Name_Type;
         Casing          : in out Casing_Type;
         Casing_Defined  : out Boolean;
         Separate_Suffix : in out File_Name_Type;
         Sep_Suffix_Loc  : out Source_Ptr)
      is
         Dot_Repl      : constant Variable_Value :=
                           Util.Value_Of
                             (Name_Dot_Replacement,
                              Naming.Decl.Attributes,
                              Data.Tree);
         Casing_String : constant Variable_Value :=
                           Util.Value_Of
                             (Name_Casing,
                              Naming.Decl.Attributes,
                              Data.Tree);
         Sep_Suffix    : constant Variable_Value :=
                           Util.Value_Of
                             (Name_Separate_Suffix,
                              Naming.Decl.Attributes,
                              Data.Tree);
         Dot_Repl_Loc  : Source_Ptr;

      begin
         Sep_Suffix_Loc := No_Location;

         if not Dot_Repl.Default then
            pragma Assert
              (Dot_Repl.Kind = Single, "Dot_Replacement is not a string");

            if Length_Of_Name (Dot_Repl.Value) = 0 then
               Error_Msg
                 (Data.Flags, "Dot_Replacement cannot be empty",
                  Dot_Repl.Location, Project);
            end if;

            Dot_Replacement := Canonical_Case_File_Name (Dot_Repl.Value);
            Dot_Repl_Loc    := Dot_Repl.Location;

            declare
               Repl : constant String := Get_Name_String (Dot_Replacement);

            begin
               --  Dot_Replacement cannot
               --   - be empty
               --   - start or end with an alphanumeric
               --   - be a single '_'
               --   - start with an '_' followed by an alphanumeric
               --   - contain a '.' except if it is "."

               if Repl'Length = 0
                 or else Is_Alphanumeric (Repl (Repl'First))
                 or else Is_Alphanumeric (Repl (Repl'Last))
                 or else (Repl (Repl'First) = '_'
                           and then
                             (Repl'Length = 1
                               or else
                                 Is_Alphanumeric (Repl (Repl'First + 1))))
                 or else (Repl'Length > 1
                           and then
                             Index (Source => Repl, Pattern => ".") /= 0)
               then
                  Error_Msg
                    (Data.Flags,
                     '"' & Repl &
                     """ is illegal for Dot_Replacement.",
                     Dot_Repl_Loc, Project);
               end if;
            end;
         end if;

         if Dot_Replacement /= No_File then
            Write_Attr
              ("Dot_Replacement", Get_Name_String (Dot_Replacement));
         end if;

         Casing_Defined := False;

         if not Casing_String.Default then
            pragma Assert
              (Casing_String.Kind = Single, "Casing is not a string");

            declare
               Casing_Image : constant String :=
                                Get_Name_String (Casing_String.Value);

            begin
               if Casing_Image'Length = 0 then
                  Error_Msg
                    (Data.Flags,
                     "Casing cannot be an empty string",
                     Casing_String.Location, Project);
               end if;

               Casing := Value (Casing_Image);
               Casing_Defined := True;

            exception
               when Constraint_Error =>
                  Name_Len := Casing_Image'Length;
                  Name_Buffer (1 .. Name_Len) := Casing_Image;
                  Err_Vars.Error_Msg_Name_1 := Name_Find;
                  Error_Msg
                    (Data.Flags,
                     "%% is not a correct Casing",
                     Casing_String.Location, Project);
            end;
         end if;

         Write_Attr ("Casing", Image (Casing));

         if not Sep_Suffix.Default then
            if Length_Of_Name (Sep_Suffix.Value) = 0 then
               Error_Msg
                 (Data.Flags,
                  "Separate_Suffix cannot be empty",
                  Sep_Suffix.Location, Project);

            else
               Separate_Suffix := Canonical_Case_File_Name (Sep_Suffix.Value);
               Sep_Suffix_Loc  := Sep_Suffix.Location;

               Check_Illegal_Suffix
                 (Project, Separate_Suffix,
                  Dot_Replacement, "Separate_Suffix", Sep_Suffix.Location,
                  Data);
            end if;
         end if;

         if Separate_Suffix /= No_File then
            Write_Attr
              ("Separate_Suffix", Get_Name_String (Separate_Suffix));
         end if;
      end Check_Common;

      -----------------------------------
      -- Process_Exceptions_File_Based --
      -----------------------------------

      procedure Process_Exceptions_File_Based
        (Lang_Id : Language_Ptr;
         Kind    : Source_Kind)
      is
         Lang           : constant Name_Id := Lang_Id.Name;
         Exceptions     : Array_Element_Id;
         Exception_List : Variable_Value;
         Element_Id     : String_List_Id;
         Element        : String_Element;
         File_Name      : File_Name_Type;
         Source         : Source_Id;
         Iter           : Source_Iterator;

      begin
         case Kind is
            when Impl | Sep =>
               Exceptions :=
                 Value_Of
                   (Name_Implementation_Exceptions,
                    In_Arrays => Naming.Decl.Arrays,
                    In_Tree   => Data.Tree);

            when Spec =>
               Exceptions :=
                 Value_Of
                   (Name_Specification_Exceptions,
                    In_Arrays => Naming.Decl.Arrays,
                    In_Tree   => Data.Tree);
         end case;

         Exception_List := Value_Of
           (Index    => Lang,
            In_Array => Exceptions,
            In_Tree  => Data.Tree);

         if Exception_List /= Nil_Variable_Value then
            Element_Id := Exception_List.Values;
            while Element_Id /= Nil_String loop
               Element   := Data.Tree.String_Elements.Table (Element_Id);
               File_Name := Canonical_Case_File_Name (Element.Value);

               Iter := For_Each_Source (Data.Tree, Project);
               loop
                  Source := Prj.Element (Iter);
                  exit when Source = No_Source or else Source.File = File_Name;
                  Next (Iter);
               end loop;

               if Source = No_Source then
                  Add_Source
                    (Id               => Source,
                     Data             => Data,
                     Project          => Project,
                     Lang_Id          => Lang_Id,
                     Kind             => Kind,
                     File_Name        => File_Name,
                     Display_File     => File_Name_Type (Element.Value),
                     Naming_Exception => True);

               else
                  --  Check if the file name is already recorded for another
                  --  language or another kind.

                  if Source.Language /= Lang_Id then
                     Error_Msg
                       (Data.Flags,
                        "the same file cannot be a source of two languages",
                        Element.Location, Project);

                  elsif Source.Kind /= Kind then
                     Error_Msg
                       (Data.Flags,
                        "the same file cannot be a source and a template",
                        Element.Location, Project);
                  end if;

                  --  If the file is already recorded for the same
                  --  language and the same kind, it means that the file
                  --  name appears several times in the *_Exceptions
                  --  attribute; so there is nothing to do.
               end if;

               Element_Id := Element.Next;
            end loop;
         end if;
      end Process_Exceptions_File_Based;

      -----------------------------------
      -- Process_Exceptions_Unit_Based --
      -----------------------------------

      procedure Process_Exceptions_Unit_Based
        (Lang_Id : Language_Ptr;
         Kind    : Source_Kind)
      is
         Lang       : constant Name_Id := Lang_Id.Name;
         Exceptions : Array_Element_Id;
         Element    : Array_Element;
         Unit       : Name_Id;
         Index      : Int;
         File_Name  : File_Name_Type;
         Source     : Source_Id;

      begin
         case Kind is
            when Impl | Sep =>
               Exceptions :=
                 Value_Of
                   (Name_Body,
                    In_Arrays => Naming.Decl.Arrays,
                    In_Tree   => Data.Tree);

               if Exceptions = No_Array_Element then
                  Exceptions :=
                    Value_Of
                      (Name_Implementation,
                       In_Arrays => Naming.Decl.Arrays,
                       In_Tree   => Data.Tree);
               end if;

            when Spec =>
               Exceptions :=
                 Value_Of
                   (Name_Spec,
                    In_Arrays => Naming.Decl.Arrays,
                    In_Tree   => Data.Tree);

               if Exceptions = No_Array_Element then
                  Exceptions :=
                    Value_Of
                      (Name_Spec,
                       In_Arrays => Naming.Decl.Arrays,
                       In_Tree   => Data.Tree);
               end if;
         end case;

         while Exceptions /= No_Array_Element loop
            Element   := Data.Tree.Array_Elements.Table (Exceptions);
            File_Name := Canonical_Case_File_Name (Element.Value.Value);

            Get_Name_String (Element.Index);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Unit  := Name_Find;
            Index := Element.Value.Index;

            --  For Ada, check if it is a valid unit name

            if Lang = Name_Ada then
               Get_Name_String (Element.Index);
               Check_Ada_Name (Name_Buffer (1 .. Name_Len), Unit);

               if Unit = No_Name then
                  Err_Vars.Error_Msg_Name_1 := Element.Index;
                  Error_Msg
                    (Data.Flags,
                     "%% is not a valid unit name.",
                     Element.Value.Location, Project);
               end if;
            end if;

            if Unit /= No_Name then
               Add_Source
                 (Id           => Source,
                  Data         => Data,
                  Project      => Project,
                  Lang_Id      => Lang_Id,
                  Kind         => Kind,
                  File_Name    => File_Name,
                  Display_File => File_Name_Type (Element.Value.Value),
                  Unit         => Unit,
                  Index        => Index,
                  Location     => Element.Value.Location,
                  Naming_Exception => True);
            end if;

            Exceptions := Element.Next;
         end loop;
      end Process_Exceptions_Unit_Based;

      ------------------
      -- Check_Naming --
      ------------------

      procedure Check_Naming is
         Dot_Replacement : File_Name_Type :=
                             File_Name_Type
                               (First_Name_Id + Character'Pos ('-'));
         Separate_Suffix : File_Name_Type := No_File;
         Casing          : Casing_Type    := All_Lower_Case;
         Casing_Defined  : Boolean;
         Lang_Id         : Language_Ptr;
         Sep_Suffix_Loc  : Source_Ptr;
         Suffix          : Variable_Value;
         Lang            : Name_Id;

      begin
         Check_Common
           (Dot_Replacement => Dot_Replacement,
            Casing          => Casing,
            Casing_Defined  => Casing_Defined,
            Separate_Suffix => Separate_Suffix,
            Sep_Suffix_Loc  => Sep_Suffix_Loc);

         --  For all unit based languages, if any, set the specified value
         --  of Dot_Replacement, Casing and/or Separate_Suffix. Do not
         --  systematically overwrite, since the defaults come from the
         --  configuration file.

         if Dot_Replacement /= No_File
           or else Casing_Defined
           or else Separate_Suffix /= No_File
         then
            Lang_Id := Project.Languages;
            while Lang_Id /= No_Language_Index loop
               if Lang_Id.Config.Kind = Unit_Based then
                  if Dot_Replacement /= No_File then
                     Lang_Id.Config.Naming_Data.Dot_Replacement :=
                         Dot_Replacement;
                  end if;

                  if Casing_Defined then
                     Lang_Id.Config.Naming_Data.Casing := Casing;
                  end if;
               end if;

               Lang_Id := Lang_Id.Next;
            end loop;
         end if;

         --  Next, get the spec and body suffixes

         Lang_Id := Project.Languages;
         while Lang_Id /= No_Language_Index loop
            Lang := Lang_Id.Name;

            --  Spec_Suffix

            Suffix := Value_Of
              (Name                    => Lang,
               Attribute_Or_Array_Name => Name_Spec_Suffix,
               In_Package              => Naming_Id,
               In_Tree                 => Data.Tree);

            if Suffix = Nil_Variable_Value then
               Suffix := Value_Of
                 (Name                    => Lang,
                  Attribute_Or_Array_Name => Name_Specification_Suffix,
                  In_Package              => Naming_Id,
                  In_Tree                 => Data.Tree);
            end if;

            if Suffix /= Nil_Variable_Value then
               Lang_Id.Config.Naming_Data.Spec_Suffix :=
                   File_Name_Type (Suffix.Value);

               Check_Illegal_Suffix
                 (Project,
                  Lang_Id.Config.Naming_Data.Spec_Suffix,
                  Lang_Id.Config.Naming_Data.Dot_Replacement,
                  "Spec_Suffix", Suffix.Location, Data);

               Write_Attr
                 ("Spec_Suffix",
                  Get_Name_String (Lang_Id.Config.Naming_Data.Spec_Suffix));
            end if;

            --  Body_Suffix

            Suffix :=
              Value_Of
                (Name                    => Lang,
                 Attribute_Or_Array_Name => Name_Body_Suffix,
                 In_Package              => Naming_Id,
                 In_Tree                 => Data.Tree);

            if Suffix = Nil_Variable_Value then
               Suffix :=
                 Value_Of
                   (Name                    => Lang,
                    Attribute_Or_Array_Name => Name_Implementation_Suffix,
                    In_Package              => Naming_Id,
                    In_Tree                 => Data.Tree);
            end if;

            if Suffix /= Nil_Variable_Value then
               Lang_Id.Config.Naming_Data.Body_Suffix :=
                 File_Name_Type (Suffix.Value);

               --  The default value of separate suffix should be the same as
               --  the body suffix, so we need to compute that first.

               if Separate_Suffix = No_File then
                  Lang_Id.Config.Naming_Data.Separate_Suffix :=
                    Lang_Id.Config.Naming_Data.Body_Suffix;
                  Write_Attr
                    ("Sep_Suffix",
                     Get_Name_String
                       (Lang_Id.Config.Naming_Data.Separate_Suffix));
               else
                  Lang_Id.Config.Naming_Data.Separate_Suffix :=
                    Separate_Suffix;
               end if;

               Check_Illegal_Suffix
                 (Project,
                  Lang_Id.Config.Naming_Data.Body_Suffix,
                  Lang_Id.Config.Naming_Data.Dot_Replacement,
                  "Body_Suffix", Suffix.Location, Data);

               Write_Attr
                 ("Body_Suffix",
                  Get_Name_String (Lang_Id.Config.Naming_Data.Body_Suffix));

            elsif Separate_Suffix /= No_File then
               Lang_Id.Config.Naming_Data.Separate_Suffix := Separate_Suffix;
            end if;

            --  Spec_Suffix cannot be equal to Body_Suffix or Separate_Suffix,
            --  since that would cause a clear ambiguity. Note that we do allow
            --  a Spec_Suffix to have the same termination as one of these,
            --  which causes a potential ambiguity, but we resolve that my
            --  matching the longest possible suffix.

            if Lang_Id.Config.Naming_Data.Spec_Suffix /= No_File
              and then Lang_Id.Config.Naming_Data.Spec_Suffix =
                       Lang_Id.Config.Naming_Data.Body_Suffix
            then
               Error_Msg
                 (Data.Flags,
                  "Body_Suffix ("""
                  & Get_Name_String (Lang_Id.Config.Naming_Data.Body_Suffix)
                  & """) cannot be the same as Spec_Suffix.",
                  Ada_Body_Suffix_Loc, Project);
            end if;

            if Lang_Id.Config.Naming_Data.Body_Suffix /=
               Lang_Id.Config.Naming_Data.Separate_Suffix
              and then Lang_Id.Config.Naming_Data.Spec_Suffix =
                       Lang_Id.Config.Naming_Data.Separate_Suffix
            then
               Error_Msg
                 (Data.Flags,
                  "Separate_Suffix ("""
                  & Get_Name_String
                    (Lang_Id.Config.Naming_Data.Separate_Suffix)
                  & """) cannot be the same as Spec_Suffix.",
                  Sep_Suffix_Loc, Project);
            end if;

            Lang_Id := Lang_Id.Next;
         end loop;

         --  Get the naming exceptions for all languages

         for Kind in Spec .. Impl loop
            Lang_Id := Project.Languages;
            while Lang_Id /= No_Language_Index loop
               case Lang_Id.Config.Kind is
                  when File_Based =>
                     Process_Exceptions_File_Based (Lang_Id, Kind);

                  when Unit_Based =>
                     Process_Exceptions_Unit_Based (Lang_Id, Kind);
               end case;

               Lang_Id := Lang_Id.Next;
            end loop;
         end loop;
      end Check_Naming;

      ----------------------------
      -- Initialize_Naming_Data --
      ----------------------------

      procedure Initialize_Naming_Data is
         Specs : Array_Element_Id :=
                   Util.Value_Of
                     (Name_Spec_Suffix,
                      Naming.Decl.Arrays,
                      Data.Tree);

         Impls : Array_Element_Id :=
                   Util.Value_Of
                     (Name_Body_Suffix,
                      Naming.Decl.Arrays,
                      Data.Tree);

         Lang      : Language_Ptr;
         Lang_Name : Name_Id;
         Value     : Variable_Value;
         Extended  : Project_Id;

      begin
         --  At this stage, the project already contains the default extensions
         --  for the various languages. We now merge those suffixes read in the
         --  user project, and they override the default.

         while Specs /= No_Array_Element loop
            Lang_Name := Data.Tree.Array_Elements.Table (Specs).Index;
            Lang :=
              Get_Language_From_Name
                (Project, Name => Get_Name_String (Lang_Name));

            --  An extending project inherits its parent projects' languages
            --  so if needed we should create entries for those languages

            if Lang = null  then
               Extended := Project.Extends;
               while Extended /= null loop
                  Lang := Get_Language_From_Name
                    (Extended, Name => Get_Name_String (Lang_Name));
                  exit when Lang /= null;

                  Extended := Extended.Extends;
               end loop;

               if Lang /= null then
                  Lang := new Language_Data'(Lang.all);
                  Lang.First_Source := null;
                  Lang.Next := Project.Languages;
                  Project.Languages := Lang;
               end if;
            end if;

            --  If language was not found in project or the projects it extends

            if Lang = null then
               if Current_Verbosity = High then
                  Write_Line
                    ("Ignoring spec naming data for "
                     & Get_Name_String (Lang_Name)
                     & " since language is not defined for this project");
               end if;

            else
               Value := Data.Tree.Array_Elements.Table (Specs).Value;

               if Value.Kind = Single then
                  Lang.Config.Naming_Data.Spec_Suffix :=
                    Canonical_Case_File_Name (Value.Value);
               end if;
            end if;

            Specs := Data.Tree.Array_Elements.Table (Specs).Next;
         end loop;

         while Impls /= No_Array_Element loop
            Lang_Name := Data.Tree.Array_Elements.Table (Impls).Index;
            Lang :=
              Get_Language_From_Name
                (Project, Name => Get_Name_String (Lang_Name));

            if Lang = null then
               if Current_Verbosity = High then
                  Write_Line
                    ("Ignoring impl naming data for "
                     & Get_Name_String (Lang_Name)
                     & " since language is not defined for this project");
               end if;
            else
               Value := Data.Tree.Array_Elements.Table (Impls).Value;

               if Lang.Name = Name_Ada then
                  Ada_Body_Suffix_Loc := Value.Location;
               end if;

               if Value.Kind = Single then
                  Lang.Config.Naming_Data.Body_Suffix :=
                    Canonical_Case_File_Name (Value.Value);
               end if;
            end if;

            Impls := Data.Tree.Array_Elements.Table (Impls).Next;
         end loop;
      end Initialize_Naming_Data;

   --  Start of processing for Check_Naming_Schemes

   begin
      Specs  := No_Array_Element;
      Bodies := No_Array_Element;

      --  No Naming package or parsing a configuration file? nothing to do

      if Naming_Id /= No_Package
        and then Project.Qualifier /= Configuration
      then
         Naming := Data.Tree.Packages.Table (Naming_Id);

         if Current_Verbosity = High then
            Write_Line ("Checking package Naming for project "
                        & Get_Name_String (Project.Name));
         end if;

         Initialize_Naming_Data;
         Check_Naming;
      end if;
   end Check_Package_Naming;

   ------------------------------
   -- Check_Library_Attributes --
   ------------------------------

   procedure Check_Library_Attributes
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data)
   is
      Attributes   : constant Prj.Variable_Id := Project.Decl.Attributes;

      Lib_Dir      : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Dir, Attributes, Data.Tree);

      Lib_Name     : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Name, Attributes, Data.Tree);

      Lib_Version  : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Version, Attributes, Data.Tree);

      Lib_ALI_Dir  : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Ali_Dir, Attributes, Data.Tree);

      Lib_GCC      : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_GCC, Attributes, Data.Tree);

      The_Lib_Kind : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Kind, Attributes, Data.Tree);

      Imported_Project_List : Project_List;

      Continuation : String_Access := No_Continuation_String'Access;

      Support_For_Libraries : Library_Support;

      Library_Directory_Present : Boolean;

      procedure Check_Library (Proj : Project_Id; Extends : Boolean);
      --  Check if an imported or extended project if also a library project

      -------------------
      -- Check_Library --
      -------------------

      procedure Check_Library (Proj : Project_Id; Extends : Boolean) is
         Src_Id : Source_Id;
         Iter   : Source_Iterator;

      begin
         if Proj /= No_Project then
            if not Proj.Library then

               --  The only not library projects that are OK are those that
               --  have no sources. However, header files from non-Ada
               --  languages are OK, as there is nothing to compile.

               Iter := For_Each_Source (Data.Tree, Proj);
               loop
                  Src_Id := Prj.Element (Iter);
                  exit when Src_Id = No_Source
                    or else Src_Id.Language.Config.Kind /= File_Based
                    or else Src_Id.Kind /= Spec;
                  Next (Iter);
               end loop;

               if Src_Id /= No_Source then
                  Error_Msg_Name_1 := Project.Name;
                  Error_Msg_Name_2 := Proj.Name;

                  if Extends then
                     if Project.Library_Kind /= Static then
                        Error_Msg
                          (Data.Flags,
                           Continuation.all &
                           "shared library project %% cannot extend " &
                           "project %% that is not a library project",
                           Project.Location, Project);
                        Continuation := Continuation_String'Access;
                     end if;

                  elsif (not Unchecked_Shared_Lib_Imports)
                        and then Project.Library_Kind /= Static
                  then
                     Error_Msg
                       (Data.Flags,
                        Continuation.all &
                        "shared library project %% cannot import project %% " &
                        "that is not a shared library project",
                        Project.Location, Project);
                     Continuation := Continuation_String'Access;
                  end if;
               end if;

            elsif Project.Library_Kind /= Static and then
                  Proj.Library_Kind = Static
            then
               Error_Msg_Name_1 := Project.Name;
               Error_Msg_Name_2 := Proj.Name;

               if Extends then
                  Error_Msg
                    (Data.Flags,
                     Continuation.all &
                     "shared library project %% cannot extend static " &
                     "library project %%",
                     Project.Location, Project);
                  Continuation := Continuation_String'Access;

               elsif not Unchecked_Shared_Lib_Imports then
                  Error_Msg
                    (Data.Flags,
                     Continuation.all &
                     "shared library project %% cannot import static " &
                     "library project %%",
                     Project.Location, Project);
                  Continuation := Continuation_String'Access;
               end if;

            end if;
         end if;
      end Check_Library;

      Dir_Exists : Boolean;

   --  Start of processing for Check_Library_Attributes

   begin
      Library_Directory_Present := Lib_Dir.Value /= Empty_String;

      --  Special case of extending project

      if Project.Extends /= No_Project then

         --  If the project extended is a library project, we inherit the
         --  library name, if it is not redefined; we check that the library
         --  directory is specified.

         if Project.Extends.Library then
            if Project.Qualifier = Standard then
               Error_Msg
                 (Data.Flags,
                  "a standard project cannot extend a library project",
                  Project.Location, Project);

            else
               if Lib_Name.Default then
                  Project.Library_Name := Project.Extends.Library_Name;
               end if;

               if Lib_Dir.Default then
                  if not Project.Virtual then
                     Error_Msg
                       (Data.Flags,
                        "a project extending a library project must " &
                        "specify an attribute Library_Dir",
                        Project.Location, Project);

                  else
                     --  For a virtual project extending a library project,
                     --  inherit library directory.

                     Project.Library_Dir := Project.Extends.Library_Dir;
                     Library_Directory_Present := True;
                  end if;
               end if;
            end if;
         end if;
      end if;

      pragma Assert (Lib_Name.Kind = Single);

      if Lib_Name.Value = Empty_String then
         if Current_Verbosity = High
           and then Project.Library_Name = No_Name
         then
            Write_Line ("No library name");
         end if;

      else
         --  There is no restriction on the syntax of library names

         Project.Library_Name := Lib_Name.Value;
      end if;

      if Project.Library_Name /= No_Name then
         if Current_Verbosity = High then
            Write_Attr
              ("Library name", Get_Name_String (Project.Library_Name));
         end if;

         pragma Assert (Lib_Dir.Kind = Single);

         if not Library_Directory_Present then
            if Current_Verbosity = High then
               Write_Line ("No library directory");
            end if;

         else
            --  Find path name (unless inherited), check that it is a directory

            if Project.Library_Dir = No_Path_Information then
               Locate_Directory
                 (Project,
                  File_Name_Type (Lib_Dir.Value),
                  Path             => Project.Library_Dir,
                  Dir_Exists       => Dir_Exists,
                  Data             => Data,
                  Create           => "library",
                  Must_Exist       => False,
                  Location         => Lib_Dir.Location,
                  Externally_Built => Project.Externally_Built);

            else
               Dir_Exists :=
                 Is_Directory
                   (Get_Name_String
                        (Project.Library_Dir.Display_Name));
            end if;

            if not Dir_Exists then

               --  Get the absolute name of the library directory that
               --  does not exist, to report an error.

               Err_Vars.Error_Msg_File_1 :=
                 File_Name_Type (Project.Library_Dir.Display_Name);
               Error_Msg
                 (Data.Flags,
                  "library directory { does not exist",
                  Lib_Dir.Location, Project);

               --  The library directory cannot be the same as the Object
               --  directory.

            elsif Project.Library_Dir.Name = Project.Object_Directory.Name then
               Error_Msg
                 (Data.Flags,
                  "library directory cannot be the same " &
                  "as object directory",
                  Lib_Dir.Location, Project);
               Project.Library_Dir := No_Path_Information;

            else
               declare
                  OK       : Boolean := True;
                  Dirs_Id  : String_List_Id;
                  Dir_Elem : String_Element;
                  Pid      : Project_List;

               begin
                  --  The library directory cannot be the same as a source
                  --  directory of the current project.

                  Dirs_Id := Project.Source_Dirs;
                  while Dirs_Id /= Nil_String loop
                     Dir_Elem := Data.Tree.String_Elements.Table (Dirs_Id);
                     Dirs_Id  := Dir_Elem.Next;

                     if Project.Library_Dir.Name =
                       Path_Name_Type (Dir_Elem.Value)
                     then
                        Err_Vars.Error_Msg_File_1 :=
                          File_Name_Type (Dir_Elem.Value);
                        Error_Msg
                          (Data.Flags,
                           "library directory cannot be the same " &
                           "as source directory {",
                           Lib_Dir.Location, Project);
                        OK := False;
                        exit;
                     end if;
                  end loop;

                  if OK then

                     --  The library directory cannot be the same as a source
                     --  directory of another project either.

                     Pid := Data.Tree.Projects;
                     Project_Loop : loop
                        exit Project_Loop when Pid = null;

                        if Pid.Project /= Project then
                           Dirs_Id := Pid.Project.Source_Dirs;

                           Dir_Loop : while Dirs_Id /= Nil_String loop
                              Dir_Elem :=
                                Data.Tree.String_Elements.Table (Dirs_Id);
                              Dirs_Id  := Dir_Elem.Next;

                              if Project.Library_Dir.Name =
                                Path_Name_Type (Dir_Elem.Value)
                              then
                                 Err_Vars.Error_Msg_File_1 :=
                                   File_Name_Type (Dir_Elem.Value);
                                 Err_Vars.Error_Msg_Name_1 := Pid.Project.Name;

                                 Error_Msg
                                   (Data.Flags,
                                    "library directory cannot be the same " &
                                    "as source directory { of project %%",
                                    Lib_Dir.Location, Project);
                                 OK := False;
                                 exit Project_Loop;
                              end if;
                           end loop Dir_Loop;
                        end if;

                        Pid := Pid.Next;
                     end loop Project_Loop;
                  end if;

                  if not OK then
                     Project.Library_Dir := No_Path_Information;

                  elsif Current_Verbosity = High then

                     --  Display the Library directory in high verbosity

                     Write_Attr
                       ("Library directory",
                        Get_Name_String (Project.Library_Dir.Display_Name));
                  end if;
               end;
            end if;
         end if;

      end if;

      Project.Library :=
        Project.Library_Dir /= No_Path_Information
          and then Project.Library_Name /= No_Name;

      if Project.Extends = No_Project then
         case Project.Qualifier is
            when Standard =>
               if Project.Library then
                  Error_Msg
                    (Data.Flags,
                     "a standard project cannot be a library project",
                     Lib_Name.Location, Project);
               end if;

            when Library =>
               if not Project.Library then
                  if Project.Library_Dir = No_Path_Information then
                     Error_Msg
                       (Data.Flags,
                        "\attribute Library_Dir not declared",
                        Project.Location, Project);
                  end if;

                  if Project.Library_Name = No_Name then
                     Error_Msg
                       (Data.Flags,
                        "\attribute Library_Name not declared",
                        Project.Location, Project);
                  end if;
               end if;

            when others =>
               null;

         end case;
      end if;

      if Project.Library then
         Support_For_Libraries := Project.Config.Lib_Support;

         if Support_For_Libraries = Prj.None then
            Error_Msg
              (Data.Flags,
               "?libraries are not supported on this platform",
               Lib_Name.Location, Project);
            Project.Library := False;

         else
            if Lib_ALI_Dir.Value = Empty_String then
               if Current_Verbosity = High then
                  Write_Line ("No library ALI directory specified");
               end if;

               Project.Library_ALI_Dir := Project.Library_Dir;

            else
               --  Find path name, check that it is a directory

               Locate_Directory
                 (Project,
                  File_Name_Type (Lib_ALI_Dir.Value),
                  Path             => Project.Library_ALI_Dir,
                  Create           => "library ALI",
                  Dir_Exists       => Dir_Exists,
                  Data             => Data,
                  Must_Exist       => False,
                  Location         => Lib_ALI_Dir.Location,
                  Externally_Built => Project.Externally_Built);

               if not Dir_Exists then

                  --  Get the absolute name of the library ALI directory that
                  --  does not exist, to report an error.

                  Err_Vars.Error_Msg_File_1 :=
                    File_Name_Type (Project.Library_ALI_Dir.Display_Name);
                  Error_Msg
                    (Data.Flags,
                     "library 'A'L'I directory { does not exist",
                     Lib_ALI_Dir.Location, Project);
               end if;

               if Project.Library_ALI_Dir /= Project.Library_Dir then

                  --  The library ALI directory cannot be the same as the
                  --  Object directory.

                  if Project.Library_ALI_Dir = Project.Object_Directory then
                     Error_Msg
                       (Data.Flags,
                        "library 'A'L'I directory cannot be the same " &
                        "as object directory",
                        Lib_ALI_Dir.Location, Project);
                     Project.Library_ALI_Dir := No_Path_Information;

                  else
                     declare
                        OK       : Boolean := True;
                        Dirs_Id  : String_List_Id;
                        Dir_Elem : String_Element;
                        Pid      : Project_List;

                     begin
                        --  The library ALI directory cannot be the same as
                        --  a source directory of the current project.

                        Dirs_Id := Project.Source_Dirs;
                        while Dirs_Id /= Nil_String loop
                           Dir_Elem :=
                             Data.Tree.String_Elements.Table (Dirs_Id);
                           Dirs_Id  := Dir_Elem.Next;

                           if Project.Library_ALI_Dir.Name =
                             Path_Name_Type (Dir_Elem.Value)
                           then
                              Err_Vars.Error_Msg_File_1 :=
                                File_Name_Type (Dir_Elem.Value);
                              Error_Msg
                                (Data.Flags,
                                 "library 'A'L'I directory cannot be " &
                                 "the same as source directory {",
                                 Lib_ALI_Dir.Location, Project);
                              OK := False;
                              exit;
                           end if;
                        end loop;

                        if OK then

                           --  The library ALI directory cannot be the same as
                           --  a source directory of another project either.

                           Pid := Data.Tree.Projects;
                           ALI_Project_Loop : loop
                              exit ALI_Project_Loop when Pid = null;

                              if Pid.Project /= Project then
                                 Dirs_Id := Pid.Project.Source_Dirs;

                                 ALI_Dir_Loop :
                                 while Dirs_Id /= Nil_String loop
                                    Dir_Elem :=
                                      Data.Tree.String_Elements.Table
                                        (Dirs_Id);
                                    Dirs_Id  := Dir_Elem.Next;

                                    if Project.Library_ALI_Dir.Name =
                                        Path_Name_Type (Dir_Elem.Value)
                                    then
                                       Err_Vars.Error_Msg_File_1 :=
                                         File_Name_Type (Dir_Elem.Value);
                                       Err_Vars.Error_Msg_Name_1 :=
                                         Pid.Project.Name;

                                       Error_Msg
                                         (Data.Flags,
                                          "library 'A'L'I directory cannot " &
                                          "be the same as source directory " &
                                          "{ of project %%",
                                          Lib_ALI_Dir.Location, Project);
                                       OK := False;
                                       exit ALI_Project_Loop;
                                    end if;
                                 end loop ALI_Dir_Loop;
                              end if;
                              Pid := Pid.Next;
                           end loop ALI_Project_Loop;
                        end if;

                        if not OK then
                           Project.Library_ALI_Dir := No_Path_Information;

                        elsif Current_Verbosity = High then

                           --  Display Library ALI directory in high verbosity

                           Write_Attr
                             ("Library ALI dir",
                              Get_Name_String
                                (Project.Library_ALI_Dir.Display_Name));
                        end if;
                     end;
                  end if;
               end if;
            end if;

            pragma Assert (Lib_Version.Kind = Single);

            if Lib_Version.Value = Empty_String then
               if Current_Verbosity = High then
                  Write_Line ("No library version specified");
               end if;

            else
               Project.Lib_Internal_Name := Lib_Version.Value;
            end if;

            pragma Assert (The_Lib_Kind.Kind = Single);

            if The_Lib_Kind.Value = Empty_String then
               if Current_Verbosity = High then
                  Write_Line ("No library kind specified");
               end if;

            else
               Get_Name_String (The_Lib_Kind.Value);

               declare
                  Kind_Name : constant String :=
                                To_Lower (Name_Buffer (1 .. Name_Len));

                  OK : Boolean := True;

               begin
                  if Kind_Name = "static" then
                     Project.Library_Kind := Static;

                  elsif Kind_Name = "dynamic" then
                     Project.Library_Kind := Dynamic;

                  elsif Kind_Name = "relocatable" then
                     Project.Library_Kind := Relocatable;

                  else
                     Error_Msg
                       (Data.Flags,
                        "illegal value for Library_Kind",
                        The_Lib_Kind.Location, Project);
                     OK := False;
                  end if;

                  if Current_Verbosity = High and then OK then
                     Write_Attr ("Library kind", Kind_Name);
                  end if;

                  if Project.Library_Kind /= Static then
                     if Support_For_Libraries = Prj.Static_Only then
                        Error_Msg
                          (Data.Flags,
                           "only static libraries are supported " &
                           "on this platform",
                           The_Lib_Kind.Location, Project);
                        Project.Library := False;

                     else
                        --  Check if (obsolescent) attribute Library_GCC or
                        --  Linker'Driver is declared.

                        if Lib_GCC.Value /= Empty_String then
                           Error_Msg
                             (Data.Flags,
                              "?Library_'G'C'C is an obsolescent attribute, " &
                              "use Linker''Driver instead",
                              Lib_GCC.Location, Project);
                           Project.Config.Shared_Lib_Driver :=
                             File_Name_Type (Lib_GCC.Value);

                        else
                           declare
                              Linker : constant Package_Id :=
                                         Value_Of
                                           (Name_Linker,
                                            Project.Decl.Packages,
                                            Data.Tree);
                              Driver : constant Variable_Value :=
                                         Value_Of
                                           (Name                 => No_Name,
                                            Attribute_Or_Array_Name =>
                                              Name_Driver,
                                            In_Package           => Linker,
                                            In_Tree              => Data.Tree);

                           begin
                              if Driver /= Nil_Variable_Value
                                 and then Driver.Value /= Empty_String
                              then
                                 Project.Config.Shared_Lib_Driver :=
                                   File_Name_Type (Driver.Value);
                              end if;
                           end;
                        end if;
                     end if;
                  end if;
               end;
            end if;

            if Project.Library then
               if Current_Verbosity = High then
                  Write_Line ("This is a library project file");
               end if;

               Check_Library (Project.Extends, Extends => True);

               Imported_Project_List := Project.Imported_Projects;
               while Imported_Project_List /= null loop
                  Check_Library
                    (Imported_Project_List.Project,
                     Extends => False);
                  Imported_Project_List := Imported_Project_List.Next;
               end loop;
            end if;

         end if;
      end if;

      --  Check if Linker'Switches or Linker'Default_Switches are declared.
      --  Warn if they are declared, as it is a common error to think that
      --  library are "linked" with Linker switches.

      if Project.Library then
         declare
            Linker_Package_Id : constant Package_Id :=
                                  Util.Value_Of
                                    (Name_Linker,
                                     Project.Decl.Packages, Data.Tree);
            Linker_Package    : Package_Element;
            Switches          : Array_Element_Id := No_Array_Element;

         begin
            if Linker_Package_Id /= No_Package then
               Linker_Package := Data.Tree.Packages.Table (Linker_Package_Id);

               Switches :=
                 Value_Of
                   (Name      => Name_Switches,
                    In_Arrays => Linker_Package.Decl.Arrays,
                    In_Tree   => Data.Tree);

               if Switches = No_Array_Element then
                  Switches :=
                    Value_Of
                      (Name      => Name_Default_Switches,
                       In_Arrays => Linker_Package.Decl.Arrays,
                       In_Tree   => Data.Tree);
               end if;

               if Switches /= No_Array_Element then
                  Error_Msg
                    (Data.Flags,
                     "?Linker switches not taken into account in library " &
                     "projects",
                     No_Location, Project);
               end if;
            end if;
         end;
      end if;

      if Project.Extends /= No_Project then
         Project.Extends.Library := False;
      end if;
   end Check_Library_Attributes;

   ---------------------------------
   -- Check_Programming_Languages --
   ---------------------------------

   procedure Check_Programming_Languages
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data)
   is
      Languages   : Variable_Value := Nil_Variable_Value;
      Def_Lang    : Variable_Value := Nil_Variable_Value;
      Def_Lang_Id : Name_Id;

      procedure Add_Language (Name, Display_Name : Name_Id);
      --  Add a new language to the list of languages for the project.
      --  Nothing is done if the language has already been defined

      ------------------
      -- Add_Language --
      ------------------

      procedure Add_Language (Name, Display_Name : Name_Id) is
         Lang : Language_Ptr;

      begin
         Lang := Project.Languages;
         while Lang /= No_Language_Index loop
            if Name = Lang.Name then
               return;
            end if;

            Lang := Lang.Next;
         end loop;

         Lang              := new Language_Data'(No_Language_Data);
         Lang.Next         := Project.Languages;
         Project.Languages := Lang;
         Lang.Name         := Name;
         Lang.Display_Name := Display_Name;

         if Name = Name_Ada then
            Lang.Config.Kind            := Unit_Based;
            Lang.Config.Dependency_Kind := ALI_File;
         else
            Lang.Config.Kind := File_Based;
         end if;
      end Add_Language;

   --  Start of processing for Check_Programming_Languages

   begin
      Project.Languages := null;
      Languages :=
        Prj.Util.Value_Of (Name_Languages, Project.Decl.Attributes, Data.Tree);
      Def_Lang :=
        Prj.Util.Value_Of
          (Name_Default_Language, Project.Decl.Attributes, Data.Tree);

      if Project.Source_Dirs /= Nil_String then

         --  Check if languages are specified in this project

         if Languages.Default then

            --  Fail if there is no default language defined

            if Def_Lang.Default then
               Error_Msg
                 (Data.Flags,
                  "no languages defined for this project",
                  Project.Location, Project);
               Def_Lang_Id := No_Name;

            else
               Get_Name_String (Def_Lang.Value);
               To_Lower (Name_Buffer (1 .. Name_Len));
               Def_Lang_Id := Name_Find;
            end if;

            if Def_Lang_Id /= No_Name then
               Get_Name_String (Def_Lang_Id);
               Name_Buffer (1) := GNAT.Case_Util.To_Upper (Name_Buffer (1));
               Add_Language
                 (Name         => Def_Lang_Id,
                  Display_Name => Name_Find);
            end if;

         else
            declare
               Current : String_List_Id := Languages.Values;
               Element : String_Element;

            begin
               --  If there are no languages declared, there are no sources

               if Current = Nil_String then
                  Project.Source_Dirs := Nil_String;

                  if Project.Qualifier = Standard then
                     Error_Msg
                       (Data.Flags,
                        "a standard project must have at least one language",
                        Languages.Location, Project);
                  end if;

               else
                  --  Look through all the languages specified in attribute
                  --  Languages.

                  while Current /= Nil_String loop
                     Element := Data.Tree.String_Elements.Table (Current);
                     Get_Name_String (Element.Value);
                     To_Lower (Name_Buffer (1 .. Name_Len));

                     Add_Language
                       (Name         => Name_Find,
                        Display_Name => Element.Value);

                     Current := Element.Next;
                  end loop;
               end if;
            end;
         end if;
      end if;
   end Check_Programming_Languages;

   -------------------------------
   -- Check_Stand_Alone_Library --
   -------------------------------

   procedure Check_Stand_Alone_Library
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data)
   is
      Lib_Interfaces      : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Interface,
                                 Project.Decl.Attributes,
                                 Data.Tree);

      Lib_Auto_Init       : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Auto_Init,
                                 Project.Decl.Attributes,
                                 Data.Tree);

      Lib_Src_Dir         : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Src_Dir,
                                 Project.Decl.Attributes,
                                 Data.Tree);

      Lib_Symbol_File     : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Symbol_File,
                                 Project.Decl.Attributes,
                                 Data.Tree);

      Lib_Symbol_Policy   : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Symbol_Policy,
                                 Project.Decl.Attributes,
                                 Data.Tree);

      Lib_Ref_Symbol_File : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Reference_Symbol_File,
                                 Project.Decl.Attributes,
                                 Data.Tree);

      Auto_Init_Supported : Boolean;
      OK                  : Boolean := True;
      Source              : Source_Id;
      Next_Proj           : Project_Id;
      Iter                : Source_Iterator;

   begin
      Auto_Init_Supported := Project.Config.Auto_Init_Supported;

      pragma Assert (Lib_Interfaces.Kind = List);

      --  It is a stand-alone library project file if attribute
      --  Library_Interface is defined.

      if not Lib_Interfaces.Default then
         declare
            Interfaces     : String_List_Id := Lib_Interfaces.Values;
            Interface_ALIs : String_List_Id := Nil_String;
            Unit           : Name_Id;

         begin
            Project.Standalone_Library := True;

            --  Library_Interface cannot be an empty list

            if Interfaces = Nil_String then
               Error_Msg
                 (Data.Flags,
                  "Library_Interface cannot be an empty list",
                  Lib_Interfaces.Location, Project);
            end if;

            --  Process each unit name specified in the attribute
            --  Library_Interface.

            while Interfaces /= Nil_String loop
               Get_Name_String
                 (Data.Tree.String_Elements.Table (Interfaces).Value);
               To_Lower (Name_Buffer (1 .. Name_Len));

               if Name_Len = 0 then
                  Error_Msg
                    (Data.Flags,
                     "an interface cannot be an empty string",
                     Data.Tree.String_Elements.Table (Interfaces).Location,
                     Project);

               else
                  Unit := Name_Find;
                  Error_Msg_Name_1 := Unit;

                  Next_Proj := Project.Extends;
                  Iter := For_Each_Source (Data.Tree, Project);
                  loop
                     while Prj.Element (Iter) /= No_Source
                       and then
                         (Prj.Element (Iter).Unit = null
                           or else Prj.Element (Iter).Unit.Name /= Unit)
                     loop
                        Next (Iter);
                     end loop;

                     Source := Prj.Element (Iter);
                     exit when Source /= No_Source
                       or else Next_Proj = No_Project;

                     Iter := For_Each_Source (Data.Tree, Next_Proj);
                     Next_Proj := Next_Proj.Extends;
                  end loop;

                  if Source /= No_Source then
                     if Source.Kind = Sep then
                        Source := No_Source;

                     elsif Source.Kind = Spec
                       and then Other_Part (Source) /= No_Source
                     then
                        Source := Other_Part (Source);
                     end if;
                  end if;

                  if Source /= No_Source then
                     if Source.Project /= Project
                       and then not Is_Extending (Project, Source.Project)
                     then
                        Source := No_Source;
                     end if;
                  end if;

                  if Source = No_Source then
                     Error_Msg
                       (Data.Flags,
                        "%% is not a unit of this project",
                        Data.Tree.String_Elements.Table
                          (Interfaces).Location, Project);

                  else
                     if Source.Kind = Spec
                       and then Other_Part (Source) /= No_Source
                     then
                        Source := Other_Part (Source);
                     end if;

                     String_Element_Table.Increment_Last
                       (Data.Tree.String_Elements);

                     Data.Tree.String_Elements.Table
                       (String_Element_Table.Last
                          (Data.Tree.String_Elements)) :=
                       (Value         => Name_Id (Source.Dep_Name),
                        Index         => 0,
                        Display_Value => Name_Id (Source.Dep_Name),
                        Location      =>
                          Data.Tree.String_Elements.Table
                            (Interfaces).Location,
                        Flag          => False,
                        Next          => Interface_ALIs);

                     Interface_ALIs :=
                       String_Element_Table.Last
                         (Data.Tree.String_Elements);
                  end if;
               end if;

               Interfaces := Data.Tree.String_Elements.Table (Interfaces).Next;
            end loop;

            --  Put the list of Interface ALIs in the project data

            Project.Lib_Interface_ALIs := Interface_ALIs;

            --  Check value of attribute Library_Auto_Init and set
            --  Lib_Auto_Init accordingly.

            if Lib_Auto_Init.Default then

               --  If no attribute Library_Auto_Init is declared, then set auto
               --  init only if it is supported.

               Project.Lib_Auto_Init := Auto_Init_Supported;

            else
               Get_Name_String (Lib_Auto_Init.Value);
               To_Lower (Name_Buffer (1 .. Name_Len));

               if Name_Buffer (1 .. Name_Len) = "false" then
                  Project.Lib_Auto_Init := False;

               elsif Name_Buffer (1 .. Name_Len) = "true" then
                  if Auto_Init_Supported then
                     Project.Lib_Auto_Init := True;

                  else
                     --  Library_Auto_Init cannot be "true" if auto init is not
                     --  supported.

                     Error_Msg
                       (Data.Flags,
                        "library auto init not supported " &
                        "on this platform",
                        Lib_Auto_Init.Location, Project);
                  end if;

               else
                  Error_Msg
                    (Data.Flags,
                     "invalid value for attribute Library_Auto_Init",
                     Lib_Auto_Init.Location, Project);
               end if;
            end if;
         end;

         --  If attribute Library_Src_Dir is defined and not the empty string,
         --  check if the directory exist and is not the object directory or
         --  one of the source directories. This is the directory where copies
         --  of the interface sources will be copied. Note that this directory
         --  may be the library directory.

         if Lib_Src_Dir.Value /= Empty_String then
            declare
               Dir_Id     : constant File_Name_Type :=
                              File_Name_Type (Lib_Src_Dir.Value);
               Dir_Exists : Boolean;

            begin
               Locate_Directory
                 (Project,
                  Dir_Id,
                  Path             => Project.Library_Src_Dir,
                  Dir_Exists       => Dir_Exists,
                  Data             => Data,
                  Must_Exist       => False,
                  Create           => "library source copy",
                  Location         => Lib_Src_Dir.Location,
                  Externally_Built => Project.Externally_Built);

               --  If directory does not exist, report an error

               if not Dir_Exists then

                  --  Get the absolute name of the library directory that does
                  --  not exist, to report an error.

                  Err_Vars.Error_Msg_File_1 :=
                    File_Name_Type (Project.Library_Src_Dir.Display_Name);
                  Error_Msg
                    (Data.Flags,
                     "Directory { does not exist",
                     Lib_Src_Dir.Location, Project);

                  --  Report error if it is the same as the object directory

               elsif Project.Library_Src_Dir = Project.Object_Directory then
                  Error_Msg
                    (Data.Flags,
                     "directory to copy interfaces cannot be " &
                     "the object directory",
                     Lib_Src_Dir.Location, Project);
                  Project.Library_Src_Dir := No_Path_Information;

               else
                  declare
                     Src_Dirs : String_List_Id;
                     Src_Dir  : String_Element;
                     Pid      : Project_List;

                  begin
                     --  Interface copy directory cannot be one of the source
                     --  directory of the current project.

                     Src_Dirs := Project.Source_Dirs;
                     while Src_Dirs /= Nil_String loop
                        Src_Dir := Data.Tree.String_Elements.Table (Src_Dirs);

                        --  Report error if it is one of the source directories

                        if Project.Library_Src_Dir.Name =
                             Path_Name_Type (Src_Dir.Value)
                        then
                           Error_Msg
                             (Data.Flags,
                              "directory to copy interfaces cannot " &
                              "be one of the source directories",
                              Lib_Src_Dir.Location, Project);
                           Project.Library_Src_Dir := No_Path_Information;
                           exit;
                        end if;

                        Src_Dirs := Src_Dir.Next;
                     end loop;

                     if Project.Library_Src_Dir /= No_Path_Information then

                        --  It cannot be a source directory of any other
                        --  project either.

                        Pid := Data.Tree.Projects;
                        Project_Loop : loop
                           exit Project_Loop when Pid = null;

                           Src_Dirs := Pid.Project.Source_Dirs;
                           Dir_Loop : while Src_Dirs /= Nil_String loop
                              Src_Dir :=
                                Data.Tree.String_Elements.Table (Src_Dirs);

                              --  Report error if it is one of the source
                              --  directories.

                              if Project.Library_Src_Dir.Name =
                                Path_Name_Type (Src_Dir.Value)
                              then
                                 Error_Msg_File_1 :=
                                   File_Name_Type (Src_Dir.Value);
                                 Error_Msg_Name_1 := Pid.Project.Name;
                                 Error_Msg
                                   (Data.Flags,
                                    "directory to copy interfaces cannot " &
                                    "be the same as source directory { of " &
                                    "project %%",
                                    Lib_Src_Dir.Location, Project);
                                 Project.Library_Src_Dir :=
                                   No_Path_Information;
                                 exit Project_Loop;
                              end if;

                              Src_Dirs := Src_Dir.Next;
                           end loop Dir_Loop;

                           Pid := Pid.Next;
                        end loop Project_Loop;
                     end if;
                  end;

                  --  In high verbosity, if there is a valid Library_Src_Dir,
                  --  display its path name.

                  if Project.Library_Src_Dir /= No_Path_Information
                    and then Current_Verbosity = High
                  then
                     Write_Attr
                       ("Directory to copy interfaces",
                        Get_Name_String (Project.Library_Src_Dir.Name));
                  end if;
               end if;
            end;
         end if;

         --  Check the symbol related attributes

         --  First, the symbol policy

         if not Lib_Symbol_Policy.Default then
            declare
               Value : constant String :=
                         To_Lower
                           (Get_Name_String (Lib_Symbol_Policy.Value));

            begin
               --  Symbol policy must hove one of a limited number of values

               if Value = "autonomous" or else Value = "default" then
                  Project.Symbol_Data.Symbol_Policy := Autonomous;

               elsif Value = "compliant" then
                  Project.Symbol_Data.Symbol_Policy := Compliant;

               elsif Value = "controlled" then
                  Project.Symbol_Data.Symbol_Policy := Controlled;

               elsif Value = "restricted" then
                  Project.Symbol_Data.Symbol_Policy := Restricted;

               elsif Value = "direct" then
                  Project.Symbol_Data.Symbol_Policy := Direct;

               else
                  Error_Msg
                    (Data.Flags,
                     "illegal value for Library_Symbol_Policy",
                     Lib_Symbol_Policy.Location, Project);
               end if;
            end;
         end if;

         --  If attribute Library_Symbol_File is not specified, symbol policy
         --  cannot be Restricted.

         if Lib_Symbol_File.Default then
            if Project.Symbol_Data.Symbol_Policy = Restricted then
               Error_Msg
                 (Data.Flags,
                  "Library_Symbol_File needs to be defined when " &
                  "symbol policy is Restricted",
                  Lib_Symbol_Policy.Location, Project);
            end if;

         else
            --  Library_Symbol_File is defined

            Project.Symbol_Data.Symbol_File :=
              Path_Name_Type (Lib_Symbol_File.Value);

            Get_Name_String (Lib_Symbol_File.Value);

            if Name_Len = 0 then
               Error_Msg
                 (Data.Flags,
                  "symbol file name cannot be an empty string",
                  Lib_Symbol_File.Location, Project);

            else
               OK := not Is_Absolute_Path (Name_Buffer (1 .. Name_Len));

               if OK then
                  for J in 1 .. Name_Len loop
                     if Name_Buffer (J) = '/'
                       or else Name_Buffer (J) = Directory_Separator
                     then
                        OK := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if not OK then
                  Error_Msg_File_1 := File_Name_Type (Lib_Symbol_File.Value);
                  Error_Msg
                    (Data.Flags,
                     "symbol file name { is illegal. " &
                     "Name cannot include directory info.",
                     Lib_Symbol_File.Location, Project);
               end if;
            end if;
         end if;

         --  If attribute Library_Reference_Symbol_File is not defined,
         --  symbol policy cannot be Compliant or Controlled.

         if Lib_Ref_Symbol_File.Default then
            if Project.Symbol_Data.Symbol_Policy = Compliant
              or else Project.Symbol_Data.Symbol_Policy = Controlled
            then
               Error_Msg
                 (Data.Flags,
                  "a reference symbol file needs to be defined",
                  Lib_Symbol_Policy.Location, Project);
            end if;

         else
            --  Library_Reference_Symbol_File is defined, check file exists

            Project.Symbol_Data.Reference :=
              Path_Name_Type (Lib_Ref_Symbol_File.Value);

            Get_Name_String (Lib_Ref_Symbol_File.Value);

            if Name_Len = 0 then
               Error_Msg
                 (Data.Flags,
                  "reference symbol file name cannot be an empty string",
                  Lib_Symbol_File.Location, Project);

            else
               if not Is_Absolute_Path (Name_Buffer (1 .. Name_Len)) then
                  Name_Len := 0;
                  Add_Str_To_Name_Buffer
                    (Get_Name_String (Project.Directory.Name));
                  Add_Str_To_Name_Buffer
                    (Get_Name_String (Lib_Ref_Symbol_File.Value));
                  Project.Symbol_Data.Reference := Name_Find;
               end if;

               if not Is_Regular_File
                        (Get_Name_String (Project.Symbol_Data.Reference))
               then
                  Error_Msg_File_1 :=
                    File_Name_Type (Lib_Ref_Symbol_File.Value);

                  --  For controlled and direct symbol policies, it is an error
                  --  if the reference symbol file does not exist. For other
                  --  symbol policies, this is just a warning

                  Error_Msg_Warn :=
                    Project.Symbol_Data.Symbol_Policy /= Controlled
                    and then Project.Symbol_Data.Symbol_Policy /= Direct;

                  Error_Msg
                    (Data.Flags,
                     "<library reference symbol file { does not exist",
                     Lib_Ref_Symbol_File.Location, Project);

                  --  In addition in the non-controlled case, if symbol policy
                  --  is Compliant, it is changed to Autonomous, because there
                  --  is no reference to check against, and we don't want to
                  --  fail in this case.

                  if Project.Symbol_Data.Symbol_Policy /= Controlled then
                     if Project.Symbol_Data.Symbol_Policy = Compliant then
                        Project.Symbol_Data.Symbol_Policy := Autonomous;
                     end if;
                  end if;
               end if;

               --  If both the reference symbol file and the symbol file are
               --  defined, then check that they are not the same file.

               if Project.Symbol_Data.Symbol_File /= No_Path then
                  Get_Name_String (Project.Symbol_Data.Symbol_File);

                  if Name_Len > 0 then
                     declare
                        --  We do not need to pass a Directory to
                        --  Normalize_Pathname, since the path_information
                        --  already contains absolute information.

                        Symb_Path : constant String :=
                                      Normalize_Pathname
                                        (Get_Name_String
                                           (Project.Object_Directory.Name) &
                                         Name_Buffer (1 .. Name_Len),
                                         Directory     => "/",
                                         Resolve_Links =>
                                           Opt.Follow_Links_For_Files);
                        Ref_Path  : constant String :=
                                      Normalize_Pathname
                                        (Get_Name_String
                                           (Project.Symbol_Data.Reference),
                                         Directory     => "/",
                                         Resolve_Links =>
                                           Opt.Follow_Links_For_Files);
                     begin
                        if Symb_Path = Ref_Path then
                           Error_Msg
                             (Data.Flags,
                              "library reference symbol file and library" &
                              " symbol file cannot be the same file",
                              Lib_Ref_Symbol_File.Location, Project);
                        end if;
                     end;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Check_Stand_Alone_Library;

   ----------------------------
   -- Compute_Directory_Last --
   ----------------------------

   function Compute_Directory_Last (Dir : String) return Natural is
   begin
      if Dir'Length > 1
        and then (Dir (Dir'Last - 1) = Directory_Separator
                    or else
                  Dir (Dir'Last - 1) = '/')
      then
         return Dir'Last - 1;
      else
         return Dir'Last;
      end if;
   end Compute_Directory_Last;

   ---------------------
   -- Get_Directories --
   ---------------------

   procedure Get_Directories
     (Project     : Project_Id;
      Data        : in out Tree_Processing_Data)
   is
      package Recursive_Dirs is new GNAT.Dynamic_HTables.Simple_HTable
        (Header_Num => Header_Num,
         Element    => Boolean,
         No_Element => False,
         Key        => Name_Id,
         Hash       => Hash,
         Equal      => "=");
      --  Hash table stores recursive source directories, to avoid looking
      --  several times, and to avoid cycles that may be introduced by symbolic
      --  links.

      Visited : Recursive_Dirs.Instance;

      Object_Dir  : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Object_Dir, Project.Decl.Attributes, Data.Tree);

      Exec_Dir : constant Variable_Value :=
                   Util.Value_Of
                     (Name_Exec_Dir, Project.Decl.Attributes, Data.Tree);

      Source_Dirs : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Source_Dirs, Project.Decl.Attributes, Data.Tree);

      Excluded_Source_Dirs : constant Variable_Value :=
                              Util.Value_Of
                                (Name_Excluded_Source_Dirs,
                                 Project.Decl.Attributes,
                                 Data.Tree);

      Source_Files : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Source_Files,
                         Project.Decl.Attributes, Data.Tree);

      Last_Source_Dir : String_List_Id  := Nil_String;

      Languages : constant Variable_Value :=
                      Prj.Util.Value_Of
                        (Name_Languages, Project.Decl.Attributes, Data.Tree);

      procedure Find_Source_Dirs
        (From     : File_Name_Type;
         Location : Source_Ptr;
         Removed  : Boolean := False);
      --  Find one or several source directories, and add (or remove, if
      --  Removed is True) them to list of source directories of the project.

      ----------------------
      -- Find_Source_Dirs --
      ----------------------

      procedure Find_Source_Dirs
        (From     : File_Name_Type;
         Location : Source_Ptr;
         Removed  : Boolean := False)
      is
         Directory : constant String := Get_Name_String (From);
         Element   : String_Element;

         procedure Recursive_Find_Dirs (Path : Name_Id);
         --  Find all the subdirectories (recursively) of Path and add them
         --  to the list of source directories of the project.

         -------------------------
         -- Recursive_Find_Dirs --
         -------------------------

         procedure Recursive_Find_Dirs (Path : Name_Id) is
            Dir     : Dir_Type;
            Name    : String (1 .. 250);
            Last    : Natural;
            List    : String_List_Id;
            Prev    : String_List_Id;
            Element : String_Element;
            Found   : Boolean := False;

            Non_Canonical_Path : Name_Id := No_Name;
            Canonical_Path     : Name_Id := No_Name;

            The_Path : constant String :=
                         Normalize_Pathname
                           (Get_Name_String (Path),
                            Directory     =>
                              Get_Name_String (Project.Directory.Display_Name),
                            Resolve_Links => Opt.Follow_Links_For_Dirs) &
                         Directory_Separator;

            The_Path_Last : constant Natural :=
                              Compute_Directory_Last (The_Path);

         begin
            Name_Len := The_Path_Last - The_Path'First + 1;
            Name_Buffer (1 .. Name_Len) :=
              The_Path (The_Path'First .. The_Path_Last);
            Non_Canonical_Path := Name_Find;
            Canonical_Path :=
              Name_Id (Canonical_Case_File_Name (Non_Canonical_Path));

            --  To avoid processing the same directory several times, check
            --  if the directory is already in Recursive_Dirs. If it is, then
            --  there is nothing to do, just return. If it is not, put it there
            --  and continue recursive processing.

            if not Removed then
               if Recursive_Dirs.Get (Visited, Canonical_Path) then
                  return;
               else
                  Recursive_Dirs.Set (Visited, Canonical_Path, True);
               end if;
            end if;

            --  Check if directory is already in list

            List := Project.Source_Dirs;
            Prev := Nil_String;
            while List /= Nil_String loop
               Element := Data.Tree.String_Elements.Table (List);

               if Element.Value /= No_Name then
                  Found := Element.Value = Canonical_Path;
                  exit when Found;
               end if;

               Prev := List;
               List := Element.Next;
            end loop;

            --  If directory is not already in list, put it there

            if (not Removed) and (not Found) then
               if Current_Verbosity = High then
                  Write_Str  ("   ");
                  Write_Line (The_Path (The_Path'First .. The_Path_Last));
               end if;

               String_Element_Table.Increment_Last (Data.Tree.String_Elements);
               Element :=
                 (Value         => Canonical_Path,
                  Display_Value => Non_Canonical_Path,
                  Location      => No_Location,
                  Flag          => False,
                  Next          => Nil_String,
                  Index         => 0);

               --  Case of first source directory

               if Last_Source_Dir = Nil_String then
                  Project.Source_Dirs :=
                    String_Element_Table.Last (Data.Tree.String_Elements);

                  --  Here we already have source directories

               else
                  --  Link the previous last to the new one

                  Data.Tree.String_Elements.Table
                    (Last_Source_Dir).Next :=
                      String_Element_Table.Last (Data.Tree.String_Elements);
               end if;

               --  And register this source directory as the new last

               Last_Source_Dir :=
                 String_Element_Table.Last (Data.Tree.String_Elements);
               Data.Tree.String_Elements.Table (Last_Source_Dir) := Element;

            elsif Removed and Found then
               if Prev = Nil_String then
                  Project.Source_Dirs :=
                    Data.Tree.String_Elements.Table (List).Next;
               else
                  Data.Tree.String_Elements.Table (Prev).Next :=
                    Data.Tree.String_Elements.Table (List).Next;
               end if;
            end if;

            --  Now look for subdirectories. We do that even when this
            --  directory is already in the list, because some of its
            --  subdirectories may not be in the list yet.

            Open (Dir, The_Path (The_Path'First .. The_Path_Last));

            loop
               Read (Dir, Name, Last);
               exit when Last = 0;

               if Name (1 .. Last) /= "."
                 and then Name (1 .. Last) /= ".."
               then
                  --  Avoid . and .. directories

                  if Current_Verbosity = High then
                     Write_Str  ("   Checking ");
                     Write_Line (Name (1 .. Last));
                  end if;

                  declare
                     Path_Name : constant String :=
                       Normalize_Pathname
                         (Name      => Name (1 .. Last),
                          Directory =>
                            The_Path (The_Path'First .. The_Path_Last),
                          Resolve_Links  => Opt.Follow_Links_For_Dirs,
                          Case_Sensitive => True);

                  begin
                     if Is_Directory (Path_Name) then

                        --  We have found a new subdirectory, call self

                        Name_Len := Path_Name'Length;
                        Name_Buffer (1 .. Name_Len) := Path_Name;
                        Recursive_Find_Dirs (Name_Find);
                     end if;
                  end;
               end if;
            end loop;

            Close (Dir);

         exception
            when Directory_Error =>
               null;
         end Recursive_Find_Dirs;

      --  Start of processing for Find_Source_Dirs

      begin
         if Current_Verbosity = High and then not Removed then
            Write_Str ("Find_Source_Dirs (""");
            Write_Str (Directory);
            Write_Line (""")");
         end if;

         --  First, check if we are looking for a directory tree, indicated
         --  by "/**" at the end.

         if Directory'Length >= 3
           and then Directory (Directory'Last - 1 .. Directory'Last) = "**"
           and then (Directory (Directory'Last - 2) = '/'
                       or else
                     Directory (Directory'Last - 2) = Directory_Separator)
         then
            if not Removed then
               Project.Known_Order_Of_Source_Dirs := False;
            end if;

            Name_Len := Directory'Length - 3;

            if Name_Len = 0 then

               --  Case of "/**": all directories in file system

               Name_Len := 1;
               Name_Buffer (1) := Directory (Directory'First);

            else
               Name_Buffer (1 .. Name_Len) :=
                 Directory (Directory'First .. Directory'Last - 3);
            end if;

            if Current_Verbosity = High then
               Write_Str ("Looking for all subdirectories of """);
               Write_Str (Name_Buffer (1 .. Name_Len));
               Write_Line ("""");
            end if;

            declare
               Base_Dir : constant File_Name_Type := Name_Find;
               Root_Dir : constant String :=
                            Normalize_Pathname
                              (Name      => Get_Name_String (Base_Dir),
                               Directory =>
                                 Get_Name_String
                                   (Project.Directory.Display_Name),
                               Resolve_Links  => False,
                               Case_Sensitive => True);

            begin
               if Root_Dir'Length = 0 then
                  Err_Vars.Error_Msg_File_1 := Base_Dir;

                  if Location = No_Location then
                     Error_Msg
                       (Data.Flags,
                        "{ is not a valid directory.",
                        Project.Location, Project);
                  else
                     Error_Msg
                       (Data.Flags,
                        "{ is not a valid directory.",
                        Location, Project);
                  end if;

               else
                  --  We have an existing directory, we register it and all of
                  --  its subdirectories.

                  if Current_Verbosity = High then
                     Write_Line ("Looking for source directories:");
                  end if;

                  Name_Len := Root_Dir'Length;
                  Name_Buffer (1 .. Name_Len) := Root_Dir;
                  Recursive_Find_Dirs (Name_Find);

                  if Current_Verbosity = High then
                     Write_Line ("End of looking for source directories.");
                  end if;
               end if;
            end;

         --  We have a single directory

         else
            declare
               Path_Name  : Path_Information;
               List       : String_List_Id;
               Prev       : String_List_Id;
               Dir_Exists : Boolean;

            begin
               Locate_Directory
                 (Project     => Project,
                  Name        => From,
                  Path        => Path_Name,
                  Dir_Exists  => Dir_Exists,
                  Data        => Data,
                  Must_Exist  => False);

               if not Dir_Exists then
                  Err_Vars.Error_Msg_File_1 := From;

                  if Location = No_Location then
                     Error_Msg
                       (Data.Flags,
                        "{ is not a valid directory",
                        Project.Location, Project);
                  else
                     Error_Msg
                       (Data.Flags,
                        "{ is not a valid directory",
                        Location, Project);
                  end if;

               else
                  declare
                     Path              : constant String :=
                                           Get_Name_String (Path_Name.Name);
                     Last_Path         : constant Natural :=
                                           Compute_Directory_Last (Path);
                     Path_Id           : Name_Id;
                     Display_Path      : constant String :=
                                           Get_Name_String
                                             (Path_Name.Display_Name);
                     Last_Display_Path : constant Natural :=
                                           Compute_Directory_Last
                                             (Display_Path);
                     Display_Path_Id   : Name_Id;

                  begin
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer (Path (Path'First .. Last_Path));
                     Path_Id := Name_Find;
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer
                       (Display_Path
                          (Display_Path'First .. Last_Display_Path));
                     Display_Path_Id := Name_Find;

                     if not Removed then

                        --  As it is an existing directory, we add it to the
                        --  list of directories.

                        String_Element_Table.Increment_Last
                          (Data.Tree.String_Elements);
                        Element :=
                          (Value         => Path_Id,
                           Index         => 0,
                           Display_Value => Display_Path_Id,
                           Location      => No_Location,
                           Flag          => False,
                           Next          => Nil_String);

                        if Last_Source_Dir = Nil_String then

                           --  This is the first source directory

                           Project.Source_Dirs := String_Element_Table.Last
                             (Data.Tree.String_Elements);

                        else
                           --  We already have source directories, link the
                           --  previous last to the new one.

                           Data.Tree.String_Elements.Table
                             (Last_Source_Dir).Next :=
                             String_Element_Table.Last
                               (Data.Tree.String_Elements);
                        end if;

                        --  And register this source directory as the new last

                        Last_Source_Dir := String_Element_Table.Last
                          (Data.Tree.String_Elements);
                        Data.Tree.String_Elements.Table
                          (Last_Source_Dir) := Element;

                     else
                        --  Remove source dir, if present

                        Prev := Nil_String;

                        --  Look for source dir in current list

                        List := Project.Source_Dirs;
                        while List /= Nil_String loop
                           Element := Data.Tree.String_Elements.Table (List);
                           exit when Element.Value = Path_Id;
                           Prev := List;
                           List := Element.Next;
                        end loop;

                        if List /= Nil_String then
                           --  Source dir was found, remove it from the list

                           if Prev = Nil_String then
                              Project.Source_Dirs :=
                                Data.Tree.String_Elements.Table (List).Next;

                           else
                              Data.Tree.String_Elements.Table (Prev).Next :=
                                Data.Tree.String_Elements.Table (List).Next;
                           end if;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;

         Recursive_Dirs.Reset (Visited);
      end Find_Source_Dirs;

   --  Start of processing for Get_Directories

      Dir_Exists : Boolean;

   begin
      if Current_Verbosity = High then
         Write_Line ("Starting to look for directories");
      end if;

      --  Set the object directory to its default which may be nil, if there
      --  is no sources in the project.

      if (((not Source_Files.Default)
             and then Source_Files.Values = Nil_String)
          or else
           ((not Source_Dirs.Default) and then Source_Dirs.Values = Nil_String)
              or else
           ((not Languages.Default) and then Languages.Values = Nil_String))
        and then Project.Extends = No_Project
      then
         Project.Object_Directory := No_Path_Information;
      else
         Project.Object_Directory := Project.Directory;
      end if;

      --  Check the object directory

      if Object_Dir.Value /= Empty_String then
         Get_Name_String (Object_Dir.Value);

         if Name_Len = 0 then
            Error_Msg
              (Data.Flags,
               "Object_Dir cannot be empty",
               Object_Dir.Location, Project);

         else
            --  We check that the specified object directory does exist.
            --  However, even when it doesn't exist, we set it to a default
            --  value. This is for the benefit of tools that recover from
            --  errors; for example, these tools could create the non existent
            --  directory. We always return an absolute directory name though.

            Locate_Directory
              (Project,
               File_Name_Type (Object_Dir.Value),
               Path             => Project.Object_Directory,
               Create           => "object",
               Dir_Exists       => Dir_Exists,
               Data             => Data,
               Location         => Object_Dir.Location,
               Must_Exist       => False,
               Externally_Built => Project.Externally_Built);

            if not Dir_Exists
              and then not Project.Externally_Built
            then
               --  The object directory does not exist, report an error if
               --  the project is not externally built.

               Err_Vars.Error_Msg_File_1 :=
                 File_Name_Type (Object_Dir.Value);
               Error_Msg
                 (Data.Flags,
                  "object directory { not found",
                  Project.Location, Project);
            end if;
         end if;

      elsif Project.Object_Directory /= No_Path_Information
        and then Subdirs /= null
      then
         Name_Len := 1;
         Name_Buffer (1) := '.';
         Locate_Directory
           (Project,
            Name_Find,
            Path             => Project.Object_Directory,
            Create           => "object",
            Dir_Exists       => Dir_Exists,
            Data             => Data,
            Location         => Object_Dir.Location,
            Externally_Built => Project.Externally_Built);
      end if;

      if Current_Verbosity = High then
         if Project.Object_Directory = No_Path_Information then
            Write_Line ("No object directory");
         else
            Write_Attr
              ("Object directory",
               Get_Name_String (Project.Object_Directory.Display_Name));
         end if;
      end if;

      --  Check the exec directory

      --  We set the object directory to its default

      Project.Exec_Directory   := Project.Object_Directory;

      if Exec_Dir.Value /= Empty_String then
         Get_Name_String (Exec_Dir.Value);

         if Name_Len = 0 then
            Error_Msg
              (Data.Flags,
               "Exec_Dir cannot be empty",
               Exec_Dir.Location, Project);

         else
            --  We check that the specified exec directory does exist

            Locate_Directory
              (Project,
               File_Name_Type (Exec_Dir.Value),
               Path             => Project.Exec_Directory,
               Dir_Exists       => Dir_Exists,
               Data             => Data,
               Create           => "exec",
               Location         => Exec_Dir.Location,
               Externally_Built => Project.Externally_Built);

            if not Dir_Exists then
               Err_Vars.Error_Msg_File_1 := File_Name_Type (Exec_Dir.Value);
               Error_Msg
                 (Data.Flags,
                  "exec directory { not found",
                  Project.Location, Project);
            end if;
         end if;
      end if;

      if Current_Verbosity = High then
         if Project.Exec_Directory = No_Path_Information then
            Write_Line ("No exec directory");
         else
            Write_Str ("Exec directory: """);
            Write_Str (Get_Name_String (Project.Exec_Directory.Display_Name));
            Write_Line ("""");
         end if;
      end if;

      --  Look for the source directories

      if Current_Verbosity = High then
         Write_Line ("Starting to look for source directories");
      end if;

      pragma Assert (Source_Dirs.Kind = List, "Source_Dirs is not a list");

      if (not Source_Files.Default)
        and then Source_Files.Values = Nil_String
      then
         Project.Source_Dirs := Nil_String;

         if Project.Qualifier = Standard then
            Error_Msg
              (Data.Flags,
               "a standard project cannot have no sources",
               Source_Files.Location, Project);
         end if;

      elsif Source_Dirs.Default then

         --  No Source_Dirs specified: the single source directory is the one
         --  containing the project file.

         String_Element_Table.Append (Data.Tree.String_Elements,
           (Value         => Name_Id (Project.Directory.Name),
            Display_Value => Name_Id (Project.Directory.Display_Name),
            Location      => No_Location,
            Flag          => False,
            Next          => Nil_String,
            Index         => 0));

         Project.Source_Dirs :=
           String_Element_Table.Last (Data.Tree.String_Elements);

         if Current_Verbosity = High then
            Write_Attr
              ("Default source directory",
               Get_Name_String (Project.Directory.Display_Name));
         end if;

      elsif Source_Dirs.Values = Nil_String then
         if Project.Qualifier = Standard then
            Error_Msg
              (Data.Flags,
               "a standard project cannot have no source directories",
               Source_Dirs.Location, Project);
         end if;

         Project.Source_Dirs := Nil_String;

      else
         declare
            Source_Dir : String_List_Id;
            Element    : String_Element;

         begin
            --  Process the source directories for each element of the list

            Source_Dir := Source_Dirs.Values;
            while Source_Dir /= Nil_String loop
               Element := Data.Tree.String_Elements.Table (Source_Dir);
               Find_Source_Dirs
                 (File_Name_Type (Element.Value), Element.Location);
               Source_Dir := Element.Next;
            end loop;
         end;
      end if;

      if not Excluded_Source_Dirs.Default
        and then Excluded_Source_Dirs.Values /= Nil_String
      then
         declare
            Source_Dir : String_List_Id;
            Element    : String_Element;

         begin
            --  Process the source directories for each element of the list

            Source_Dir := Excluded_Source_Dirs.Values;
            while Source_Dir /= Nil_String loop
               Element := Data.Tree.String_Elements.Table (Source_Dir);
               Find_Source_Dirs
                 (File_Name_Type (Element.Value),
                  Element.Location,
                  Removed => True);
               Source_Dir := Element.Next;
            end loop;
         end;
      end if;

      if Current_Verbosity = High then
         Write_Line ("Putting source directories in canonical cases");
      end if;

      declare
         Current : String_List_Id := Project.Source_Dirs;
         Element : String_Element;

      begin
         while Current /= Nil_String loop
            Element := Data.Tree.String_Elements.Table (Current);
            if Element.Value /= No_Name then
               Element.Value :=
                 Name_Id (Canonical_Case_File_Name (Name_Id (Element.Value)));
               Data.Tree.String_Elements.Table (Current) := Element;
            end if;

            Current := Element.Next;
         end loop;
      end;
   end Get_Directories;

   ---------------
   -- Get_Mains --
   ---------------

   procedure Get_Mains
     (Project : Project_Id;
      Data    : in out Tree_Processing_Data)
   is
      Mains : constant Variable_Value :=
               Prj.Util.Value_Of
                 (Name_Main, Project.Decl.Attributes, Data.Tree);
      List  : String_List_Id;
      Elem  : String_Element;

   begin
      Project.Mains := Mains.Values;

      --  If no Mains were specified, and if we are an extending project,
      --  inherit the Mains from the project we are extending.

      if Mains.Default then
         if not Project.Library and then Project.Extends /= No_Project then
            Project.Mains := Project.Extends.Mains;
         end if;

      --  In a library project file, Main cannot be specified

      elsif Project.Library then
         Error_Msg
           (Data.Flags,
            "a library project file cannot have Main specified",
            Mains.Location, Project);

      else
         List := Mains.Values;
         while List /= Nil_String loop
            Elem := Data.Tree.String_Elements.Table (List);

            if Length_Of_Name (Elem.Value) = 0 then
               Error_Msg
                 (Data.Flags,
                  "?a main cannot have an empty name",
                  Elem.Location, Project);
               exit;
            end if;

            List := Elem.Next;
         end loop;
      end if;
   end Get_Mains;

   ---------------------------
   -- Get_Sources_From_File --
   ---------------------------

   procedure Get_Sources_From_File
     (Path     : String;
      Location : Source_Ptr;
      Project  : in out Project_Processing_Data;
      Data     : in out Tree_Processing_Data)
   is
      File        : Prj.Util.Text_File;
      Line        : String (1 .. 250);
      Last        : Natural;
      Source_Name : File_Name_Type;
      Name_Loc    : Name_Location;

   begin
      if Current_Verbosity = High then
         Write_Str  ("Opening """);
         Write_Str  (Path);
         Write_Line (""".");
      end if;

      --  Open the file

      Prj.Util.Open (File, Path);

      if not Prj.Util.Is_Valid (File) then
         Error_Msg
           (Data.Flags, "file does not exist", Location, Project.Project);

      else
         --  Read the lines one by one

         while not Prj.Util.End_Of_File (File) loop
            Prj.Util.Get_Line (File, Line, Last);

            --  A non empty, non comment line should contain a file name

            if Last /= 0
              and then (Last = 1 or else Line (1 .. 2) /= "--")
            then
               Name_Len := Last;
               Name_Buffer (1 .. Name_Len) := Line (1 .. Last);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Source_Name := Name_Find;

               --  Check that there is no directory information

               for J in 1 .. Last loop
                  if Line (J) = '/' or else Line (J) = Directory_Separator then
                     Error_Msg_File_1 := Source_Name;
                     Error_Msg
                       (Data.Flags,
                        "file name cannot include directory information ({)",
                        Location, Project.Project);
                     exit;
                  end if;
               end loop;

               Name_Loc := Source_Names_Htable.Get
                 (Project.Source_Names, Source_Name);

               if Name_Loc = No_Name_Location then
                  Name_Loc :=
                    (Name     => Source_Name,
                     Location => Location,
                     Source   => No_Source,
                     Found    => False);
               end if;

               Source_Names_Htable.Set
                 (Project.Source_Names, Source_Name, Name_Loc);
            end if;
         end loop;

         Prj.Util.Close (File);

      end if;
   end Get_Sources_From_File;

   -----------------------
   -- Compute_Unit_Name --
   -----------------------

   procedure Compute_Unit_Name
     (File_Name : File_Name_Type;
      Naming    : Lang_Naming_Data;
      Kind      : out Source_Kind;
      Unit      : out Name_Id;
      Project   : Project_Processing_Data;
      In_Tree   : Project_Tree_Ref)
   is
      Filename : constant String  := Get_Name_String (File_Name);
      Last     : Integer          := Filename'Last;
      Sep_Len  : Integer;
      Body_Len : Integer;
      Spec_Len : Integer;

      Unit_Except : Unit_Exception;
      Masked      : Boolean  := False;

   begin
      Unit := No_Name;
      Kind := Spec;

      if Naming.Separate_Suffix = No_File
        or else Naming.Body_Suffix = No_File
        or else Naming.Spec_Suffix = No_File
      then
         return;
      end if;

      if Naming.Dot_Replacement = No_File then
         if Current_Verbosity = High then
            Write_Line ("  No dot_replacement specified");
         end if;

         return;
      end if;

      Sep_Len  := Integer (Length_Of_Name (Naming.Separate_Suffix));
      Spec_Len := Integer (Length_Of_Name (Naming.Spec_Suffix));
      Body_Len := Integer (Length_Of_Name (Naming.Body_Suffix));

      --  Choose the longest suffix that matches. If there are several matches,
      --  give priority to specs, then bodies, then separates.

      if Naming.Separate_Suffix /= Naming.Body_Suffix
        and then Suffix_Matches (Filename, Naming.Separate_Suffix)
      then
         Last := Filename'Last - Sep_Len;
         Kind := Sep;
      end if;

      if Filename'Last - Body_Len <= Last
        and then Suffix_Matches (Filename, Naming.Body_Suffix)
      then
         Last := Natural'Min (Last, Filename'Last - Body_Len);
         Kind := Impl;
      end if;

      if Filename'Last - Spec_Len <= Last
        and then Suffix_Matches (Filename, Naming.Spec_Suffix)
      then
         Last := Natural'Min (Last, Filename'Last - Spec_Len);
         Kind := Spec;
      end if;

      if Last = Filename'Last then
         if Current_Verbosity = High then
            Write_Line ("     no matching suffix");
         end if;

         return;
      end if;

      --  Check that the casing matches

      if File_Names_Case_Sensitive then
         case Naming.Casing is
            when All_Lower_Case =>
               for J in Filename'First .. Last loop
                  if Is_Letter (Filename (J))
                    and then not Is_Lower (Filename (J))
                  then
                     if Current_Verbosity = High then
                        Write_Line ("  Invalid casing");
                     end if;

                     return;
                  end if;
               end loop;

            when All_Upper_Case =>
               for J in Filename'First .. Last loop
                  if Is_Letter (Filename (J))
                    and then not Is_Upper (Filename (J))
                  then
                     if Current_Verbosity = High then
                        Write_Line ("  Invalid casing");
                     end if;

                     return;
                  end if;
               end loop;

            when Mixed_Case | Unknown =>
               null;
         end case;
      end if;

      --  If Dot_Replacement is not a single dot, then there should not
      --  be any dot in the name.

      declare
         Dot_Repl : constant String :=
                      Get_Name_String (Naming.Dot_Replacement);

      begin
         if Dot_Repl /= "." then
            for Index in Filename'First .. Last loop
               if Filename (Index) = '.' then
                  if Current_Verbosity = High then
                     Write_Line ("   Invalid name, contains dot");
                  end if;

                  return;
               end if;
            end loop;

            Replace_Into_Name_Buffer
              (Filename (Filename'First .. Last), Dot_Repl, '.');

         else
            Name_Len := Last - Filename'First + 1;
            Name_Buffer (1 .. Name_Len) := Filename (Filename'First .. Last);
            Fixed.Translate
              (Source  => Name_Buffer (1 .. Name_Len),
               Mapping => Lower_Case_Map);
         end if;
      end;

      --  In the standard GNAT naming scheme, check for special cases: children
      --  or separates of A, G, I or S, and run time sources.

      if Is_Standard_GNAT_Naming (Naming)
        and then Name_Len >= 3
      then
         declare
            S1 : constant Character := Name_Buffer (1);
            S2 : constant Character := Name_Buffer (2);
            S3 : constant Character := Name_Buffer (3);

         begin
            if        S1 = 'a'
              or else S1 = 'g'
              or else S1 = 'i'
              or else S1 = 's'
            then
               --  Children or separates of packages A, G, I or S. These names
               --  are x__ ... or x~... (where x is a, g, i, or s). Both
               --  versions (x__... and x~...) are allowed in all platforms,
               --  because it is not possible to know the platform before
               --  processing of the project files.

               if S2 = '_' and then S3 = '_' then
                  Name_Buffer (2) := '.';
                  Name_Buffer (3 .. Name_Len - 1) :=
                    Name_Buffer (4 .. Name_Len);
                  Name_Len := Name_Len - 1;

               elsif S2 = '~' then
                  Name_Buffer (2) := '.';

               elsif S2 = '.' then

                  --  If it is potentially a run time source

                  null;
               end if;
            end if;
         end;
      end if;

      --  Name_Buffer contains the name of the the unit in lower-cases. Check
      --  that this is a valid unit name

      Check_Ada_Name (Name_Buffer (1 .. Name_Len), Unit);

      --  If there is a naming exception for the same unit, the file is not
      --  a source for the unit.

      if Unit /= No_Name then
         Unit_Except :=
           Unit_Exceptions_Htable.Get (Project.Unit_Exceptions, Unit);

         if Kind = Spec then
            Masked := Unit_Except.Spec /= No_File
                        and then
                      Unit_Except.Spec /= File_Name;
         else
            Masked := Unit_Except.Impl /= No_File
                        and then
                      Unit_Except.Impl /= File_Name;
         end if;

         if Masked then
            if Current_Verbosity = High then
               Write_Str ("   """ & Filename & """ contains the ");

               if Kind = Spec then
                  Write_Str ("spec of a unit found in """);
                  Write_Str (Get_Name_String (Unit_Except.Spec));
               else
                  Write_Str ("body of a unit found in """);
                  Write_Str (Get_Name_String (Unit_Except.Impl));
               end if;

               Write_Line (""" (ignored)");
            end if;

            Unit := No_Name;
         end if;
      end if;

      if Unit /= No_Name
        and then Current_Verbosity = High
      then
         case Kind is
            when Spec => Write_Str ("   spec of ");
            when Impl => Write_Str ("   body of ");
            when Sep  => Write_Str ("   sep of ");
         end case;

         Write_Line (Get_Name_String (Unit));
      end if;
   end Compute_Unit_Name;

   --------------------------
   -- Check_Illegal_Suffix --
   --------------------------

   procedure Check_Illegal_Suffix
     (Project         : Project_Id;
      Suffix          : File_Name_Type;
      Dot_Replacement : File_Name_Type;
      Attribute_Name  : String;
      Location        : Source_Ptr;
      Data            : in out Tree_Processing_Data)
   is
      Suffix_Str : constant String := Get_Name_String (Suffix);

   begin
      if Suffix_Str'Length = 0 then

         --  Always valid

         return;

      elsif Index (Suffix_Str, ".") = 0 then
         Err_Vars.Error_Msg_File_1 := Suffix;
         Error_Msg
           (Data.Flags,
            "{ is illegal for " & Attribute_Name & ": must have a dot",
            Location, Project);
         return;
      end if;

      --  Case of dot replacement is a single dot, and first character of
      --  suffix is also a dot.

      if Dot_Replacement /= No_File
        and then Get_Name_String (Dot_Replacement) = "."
        and then Suffix_Str (Suffix_Str'First) = '.'
      then
         for Index in Suffix_Str'First + 1 .. Suffix_Str'Last loop

            --  If there are multiple dots in the name

            if Suffix_Str (Index) = '.' then

               --  It is illegal to have a letter following the initial dot

               if Is_Letter (Suffix_Str (Suffix_Str'First + 1)) then
                  Err_Vars.Error_Msg_File_1 := Suffix;
                  Error_Msg
                    (Data.Flags,
                     "{ is illegal for " & Attribute_Name
                     & ": ambiguous prefix when Dot_Replacement is a dot",
                     Location, Project);
               end if;
               return;
            end if;
         end loop;
      end if;
   end Check_Illegal_Suffix;

   ----------------------
   -- Locate_Directory --
   ----------------------

   procedure Locate_Directory
     (Project          : Project_Id;
      Name             : File_Name_Type;
      Path             : out Path_Information;
      Dir_Exists       : out Boolean;
      Data             : in out Tree_Processing_Data;
      Create           : String := "";
      Location         : Source_Ptr := No_Location;
      Must_Exist       : Boolean := True;
      Externally_Built : Boolean := False)
   is
      Parent          : constant Path_Name_Type :=
                          Project.Directory.Display_Name;
      The_Parent      : constant String :=
                          Get_Name_String (Parent);
      The_Parent_Last : constant Natural :=
                          Compute_Directory_Last (The_Parent);
      Full_Name       : File_Name_Type;
      The_Name        : File_Name_Type;

   begin
      Get_Name_String (Name);

      --  Add Subdirs.all if it is a directory that may be created and
      --  Subdirs is not null;

      if Create /= "" and then Subdirs /= null then
         if Name_Buffer (Name_Len) /= Directory_Separator then
            Add_Char_To_Name_Buffer (Directory_Separator);
         end if;

         Add_Str_To_Name_Buffer (Subdirs.all);
      end if;

      --  Convert '/' to directory separator (for Windows)

      for J in 1 .. Name_Len loop
         if Name_Buffer (J) = '/' then
            Name_Buffer (J) := Directory_Separator;
         end if;
      end loop;

      The_Name := Name_Find;

      if Current_Verbosity = High then
         Write_Str ("Locate_Directory (""");
         Write_Str (Get_Name_String (The_Name));
         Write_Str (""", """);
         Write_Str (The_Parent);
         Write_Line (""")");
      end if;

      Path := No_Path_Information;
      Dir_Exists := False;

      if Is_Absolute_Path (Get_Name_String (The_Name)) then
         Full_Name := The_Name;

      else
         Name_Len := 0;
         Add_Str_To_Name_Buffer
           (The_Parent (The_Parent'First .. The_Parent_Last));
         Add_Str_To_Name_Buffer (Get_Name_String (The_Name));
         Full_Name := Name_Find;
      end if;

      declare
         Full_Path_Name : String_Access :=
                            new String'(Get_Name_String (Full_Name));

      begin
         if (Setup_Projects or else Subdirs /= null)
           and then Create'Length > 0
         then
            if not Is_Directory (Full_Path_Name.all) then

               --  If project is externally built, do not create a subdir,
               --  use the specified directory, without the subdir.

               if Externally_Built then
                  if Is_Absolute_Path (Get_Name_String (Name)) then
                     Get_Name_String (Name);

                  else
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer
                       (The_Parent (The_Parent'First .. The_Parent_Last));
                     Add_Str_To_Name_Buffer (Get_Name_String (Name));
                  end if;

                  Full_Path_Name := new String'(Name_Buffer (1 .. Name_Len));

               else
                  begin
                     Create_Path (Full_Path_Name.all);

                     if not Quiet_Output then
                        Write_Str (Create);
                        Write_Str (" directory """);
                        Write_Str (Full_Path_Name.all);
                        Write_Str (""" created for project ");
                        Write_Line (Get_Name_String (Project.Name));
                     end if;

                  exception
                     when Use_Error =>
                        Error_Msg
                          (Data.Flags,
                           "could not create " & Create &
                           " directory " & Full_Path_Name.all,
                           Location, Project);
                  end;
               end if;
            end if;
         end if;

         Dir_Exists := Is_Directory (Full_Path_Name.all);

         if not Must_Exist or else Dir_Exists then
            declare
               Normed : constant String :=
                          Normalize_Pathname
                            (Full_Path_Name.all,
                             Directory      =>
                              The_Parent (The_Parent'First .. The_Parent_Last),
                             Resolve_Links  => False,
                             Case_Sensitive => True);

               Canonical_Path : constant String :=
                                  Normalize_Pathname
                                    (Normed,
                                     Directory      =>
                                       The_Parent
                                         (The_Parent'First .. The_Parent_Last),
                                     Resolve_Links  =>
                                        Opt.Follow_Links_For_Dirs,
                                     Case_Sensitive => False);

            begin
               Name_Len := Normed'Length;
               Name_Buffer (1 .. Name_Len) := Normed;

               --  Directories should always end with a directory separator

               if Name_Buffer (Name_Len) /= Directory_Separator then
                  Add_Char_To_Name_Buffer (Directory_Separator);
               end if;

               Path.Display_Name := Name_Find;

               Name_Len := Canonical_Path'Length;
               Name_Buffer (1 .. Name_Len) := Canonical_Path;

               if Name_Buffer (Name_Len) /= Directory_Separator then
                  Add_Char_To_Name_Buffer (Directory_Separator);
               end if;

               Path.Name := Name_Find;
            end;
         end if;

         Free (Full_Path_Name);
      end;
   end Locate_Directory;

   ---------------------------
   -- Find_Excluded_Sources --
   ---------------------------

   procedure Find_Excluded_Sources
     (Project : in out Project_Processing_Data;
      Data    : in out Tree_Processing_Data)
   is
      Excluded_Source_List_File : constant Variable_Value :=
                                    Util.Value_Of
                                      (Name_Excluded_Source_List_File,
                                       Project.Project.Decl.Attributes,
                                       Data.Tree);
      Excluded_Sources          : Variable_Value := Util.Value_Of
                                    (Name_Excluded_Source_Files,
                                     Project.Project.Decl.Attributes,
                                     Data.Tree);

      Current         : String_List_Id;
      Element         : String_Element;
      Location        : Source_Ptr;
      Name            : File_Name_Type;
      File            : Prj.Util.Text_File;
      Line            : String (1 .. 300);
      Last            : Natural;
      Locally_Removed : Boolean := False;

   begin
      --  If Excluded_Source_Files is not declared, check Locally_Removed_Files

      if Excluded_Sources.Default then
         Locally_Removed := True;
         Excluded_Sources :=
           Util.Value_Of
             (Name_Locally_Removed_Files,
              Project.Project.Decl.Attributes, Data.Tree);
      end if;

      --  If there are excluded sources, put them in the table

      if not Excluded_Sources.Default then
         if not Excluded_Source_List_File.Default then
            if Locally_Removed then
               Error_Msg
                 (Data.Flags,
                  "?both attributes Locally_Removed_Files and " &
                  "Excluded_Source_List_File are present",
                  Excluded_Source_List_File.Location, Project.Project);
            else
               Error_Msg
                 (Data.Flags,
                  "?both attributes Excluded_Source_Files and " &
                  "Excluded_Source_List_File are present",
                  Excluded_Source_List_File.Location, Project.Project);
            end if;
         end if;

         Current := Excluded_Sources.Values;
         while Current /= Nil_String loop
            Element := Data.Tree.String_Elements.Table (Current);
            Name := Canonical_Case_File_Name (Element.Value);

            --  If the element has no location, then use the location of
            --  Excluded_Sources to report possible errors.

            if Element.Location = No_Location then
               Location := Excluded_Sources.Location;
            else
               Location := Element.Location;
            end if;

            Excluded_Sources_Htable.Set
              (Project.Excluded, Name, (Name, False, Location));
            Current := Element.Next;
         end loop;

      elsif not Excluded_Source_List_File.Default then
         Location := Excluded_Source_List_File.Location;

         declare
            Source_File_Path_Name : constant String :=
                                      Path_Name_Of
                                        (File_Name_Type
                                           (Excluded_Source_List_File.Value),
                                         Project.Project.Directory.Name);

         begin
            if Source_File_Path_Name'Length = 0 then
               Err_Vars.Error_Msg_File_1 :=
                 File_Name_Type (Excluded_Source_List_File.Value);
               Error_Msg
                 (Data.Flags,
                  "file with excluded sources { does not exist",
                  Excluded_Source_List_File.Location, Project.Project);

            else
               --  Open the file

               Prj.Util.Open (File, Source_File_Path_Name);

               if not Prj.Util.Is_Valid (File) then
                  Error_Msg
                    (Data.Flags, "file does not exist",
                     Location, Project.Project);
               else
                  --  Read the lines one by one

                  while not Prj.Util.End_Of_File (File) loop
                     Prj.Util.Get_Line (File, Line, Last);

                     --  Non empty, non comment line should contain a file name

                     if Last /= 0
                       and then (Last = 1 or else Line (1 .. 2) /= "--")
                     then
                        Name_Len := Last;
                        Name_Buffer (1 .. Name_Len) := Line (1 .. Last);
                        Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                        Name := Name_Find;

                        --  Check that there is no directory information

                        for J in 1 .. Last loop
                           if Line (J) = '/'
                             or else Line (J) = Directory_Separator
                           then
                              Error_Msg_File_1 := Name;
                              Error_Msg
                                (Data.Flags,
                                 "file name cannot include " &
                                 "directory information ({)",
                                 Location, Project.Project);
                              exit;
                           end if;
                        end loop;

                        Excluded_Sources_Htable.Set
                          (Project.Excluded, Name, (Name, False, Location));
                     end if;
                  end loop;

                  Prj.Util.Close (File);
               end if;
            end if;
         end;
      end if;
   end Find_Excluded_Sources;

   ------------------
   -- Find_Sources --
   ------------------

   procedure Find_Sources
     (Project   : in out Project_Processing_Data;
      Data      : in out Tree_Processing_Data)
   is
      Sources : constant Variable_Value :=
                  Util.Value_Of
                    (Name_Source_Files,
                    Project.Project.Decl.Attributes,
                    Data.Tree);

      Source_List_File : constant Variable_Value :=
                           Util.Value_Of
                             (Name_Source_List_File,
                              Project.Project.Decl.Attributes,
                              Data.Tree);

      Name_Loc             : Name_Location;
      Has_Explicit_Sources : Boolean;

   begin
      pragma Assert (Sources.Kind = List, "Source_Files is not a list");
      pragma Assert
        (Source_List_File.Kind = Single,
         "Source_List_File is not a single string");

      Project.Source_List_File_Location := Source_List_File.Location;

      --  If the user has specified a Source_Files attribute

      if not Sources.Default then
         if not Source_List_File.Default then
            Error_Msg
              (Data.Flags,
               "?both attributes source_files and " &
               "source_list_file are present",
               Source_List_File.Location, Project.Project);
         end if;

         --  Sources is a list of file names

         declare
            Current  : String_List_Id := Sources.Values;
            Element  : String_Element;
            Location : Source_Ptr;
            Name     : File_Name_Type;

         begin
            if Current = Nil_String then
               Project.Project.Languages := No_Language_Index;

               --  This project contains no source. For projects that don't
               --  extend other projects, this also means that there is no
               --  need for an object directory, if not specified.

               if Project.Project.Extends = No_Project
                 and then Project.Project.Object_Directory =
                   Project.Project.Directory
               then
                  Project.Project.Object_Directory := No_Path_Information;
               end if;
            end if;

            while Current /= Nil_String loop
               Element := Data.Tree.String_Elements.Table (Current);
               Name := Canonical_Case_File_Name (Element.Value);
               Get_Name_String (Element.Value);

               --  If the element has no location, then use the location of
               --  Sources to report possible errors.

               if Element.Location = No_Location then
                  Location := Sources.Location;
               else
                  Location := Element.Location;
               end if;

               --  Check that there is no directory information

               for J in 1 .. Name_Len loop
                  if Name_Buffer (J) = '/'
                    or else Name_Buffer (J) = Directory_Separator
                  then
                     Error_Msg_File_1 := Name;
                     Error_Msg
                       (Data.Flags,
                        "file name cannot include directory " &
                        "information ({)",
                        Location, Project.Project);
                     exit;
                  end if;
               end loop;

               --  Check whether the file is already there: the same file name
               --  may be in the list. If the source is missing, the error will
               --  be on the first mention of the source file name.

               Name_Loc := Source_Names_Htable.Get
                 (Project.Source_Names, Name);

               if Name_Loc = No_Name_Location then
                  Name_Loc :=
                    (Name     => Name,
                     Location => Location,
                     Source   => No_Source,
                     Found    => False);
                  Source_Names_Htable.Set
                    (Project.Source_Names, Name, Name_Loc);
               end if;

               Current := Element.Next;
            end loop;

            Has_Explicit_Sources := True;
         end;

         --  If we have no Source_Files attribute, check the Source_List_File
         --  attribute.

      elsif not Source_List_File.Default then

         --  Source_List_File is the name of the file that contains the source
         --  file names.

         declare
            Source_File_Path_Name : constant String :=
              Path_Name_Of
                (File_Name_Type (Source_List_File.Value),
                 Project.Project.Directory.Name);

         begin
            Has_Explicit_Sources := True;

            if Source_File_Path_Name'Length = 0 then
               Err_Vars.Error_Msg_File_1 :=
                 File_Name_Type (Source_List_File.Value);
               Error_Msg
                 (Data.Flags,
                  "file with sources { does not exist",
                  Source_List_File.Location, Project.Project);

            else
               Get_Sources_From_File
                 (Source_File_Path_Name, Source_List_File.Location,
                  Project, Data);
            end if;
         end;

      else
         --  Neither Source_Files nor Source_List_File has been specified. Find
         --  all the files that satisfy the naming scheme in all the source
         --  directories.

         Has_Explicit_Sources := False;
      end if;

      Search_Directories
        (Project,
         Data            => Data,
         For_All_Sources => Sources.Default and then Source_List_File.Default);

      --  Check if all exceptions have been found

      declare
         Source : Source_Id;
         Iter   : Source_Iterator;

      begin
         Iter := For_Each_Source (Data.Tree, Project.Project);
         loop
            Source := Prj.Element (Iter);
            exit when Source = No_Source;

            if Source.Naming_Exception
              and then Source.Path = No_Path_Information
            then
               if Source.Unit /= No_Unit_Index then

                  --  For multi-unit source files, source_id gets duplicated
                  --  once for every unit. Only the first source_id got its
                  --  full path set. So if it isn't set for that first one,
                  --  the file wasn't found. Otherwise we need to update for
                  --  units after the first one.

                  if Source.Index = 0
                    or else Source.Index = 1
                  then
                     Error_Msg_Name_1 := Name_Id (Source.Display_File);
                     Error_Msg_Name_2 := Name_Id (Source.Unit.Name);
                     Error_Msg
                       (Data.Flags,
                        "source file %% for unit %% not found",
                        No_Location, Project.Project);

                  else
                     Source.Path := Files_Htable.Get
                       (Data.File_To_Source, Source.File).Path;

                     if Current_Verbosity = High then
                        if Source.Path /= No_Path_Information then
                           Write_Line ("Setting full path for "
                                       & Get_Name_String (Source.File)
                                       & " at" & Source.Index'Img
                                       & " to "
                                       & Get_Name_String (Source.Path.Name));
                        end if;
                     end if;
                  end if;
               end if;

               if Source.Path = No_Path_Information then
                  Remove_Source (Source, No_Source);
               end if;
            end if;

            Next (Iter);
         end loop;
      end;

      --  It is an error if a source file name in a source list or in a source
      --  list file is not found.

      if Has_Explicit_Sources then
         declare
            NL          : Name_Location;
            First_Error : Boolean;

         begin
            NL := Source_Names_Htable.Get_First (Project.Source_Names);
            First_Error := True;
            while NL /= No_Name_Location loop
               if not NL.Found then
                  Err_Vars.Error_Msg_File_1 := NL.Name;

                  if First_Error then
                     Error_Msg
                       (Data.Flags,
                        "source file { not found",
                        NL.Location, Project.Project);
                     First_Error := False;

                  else
                     Error_Msg
                       (Data.Flags,
                        "\source file { not found",
                        NL.Location, Project.Project);
                  end if;
               end if;

               NL := Source_Names_Htable.Get_Next (Project.Source_Names);
            end loop;
         end;
      end if;
   end Find_Sources;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Data  : out Tree_Processing_Data;
      Tree  : Project_Tree_Ref;
      Flags : Prj.Processing_Flags)
   is
   begin
      Files_Htable.Reset (Data.File_To_Source);
      Data.Tree  := Tree;
      Data.Flags := Flags;
   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Tree_Processing_Data) is
   begin
      Files_Htable.Reset (Data.File_To_Source);
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Data    : in out Project_Processing_Data;
      Project : Project_Id)
   is
   begin
      Data.Project := Project;
   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Project_Processing_Data) is
   begin
      Source_Names_Htable.Reset      (Data.Source_Names);
      Unit_Exceptions_Htable.Reset   (Data.Unit_Exceptions);
      Excluded_Sources_Htable.Reset  (Data.Excluded);
   end Free;

   -------------------------------
   -- Check_File_Naming_Schemes --
   -------------------------------

   procedure Check_File_Naming_Schemes
     (In_Tree               : Project_Tree_Ref;
      Project               : Project_Processing_Data;
      File_Name             : File_Name_Type;
      Alternate_Languages   : out Language_List;
      Language              : out Language_Ptr;
      Display_Language_Name : out Name_Id;
      Unit                  : out Name_Id;
      Lang_Kind             : out Language_Kind;
      Kind                  : out Source_Kind)
   is
      Filename : constant String := Get_Name_String (File_Name);
      Config   : Language_Config;
      Tmp_Lang : Language_Ptr;

      Header_File : Boolean := False;
      --  True if we found at least one language for which the file is a header
      --  In such a case, we search for all possible languages where this is
      --  also a header (C and C++ for instance), since the file might be used
      --  for several such languages.

      procedure Check_File_Based_Lang;
      --  Does the naming scheme test for file-based languages. For those,
      --  there is no Unit. Just check if the file name has the implementation
      --  or, if it is specified, the template suffix of the language.
      --
      --  Returns True if the file belongs to the current language and we
      --  should stop searching for matching languages. Not that a given header
      --  file could belong to several languages (C and C++ for instance). Thus
      --  if we found a header we'll check whether it matches other languages.

      ---------------------------
      -- Check_File_Based_Lang --
      ---------------------------

      procedure Check_File_Based_Lang is
      begin
         if not Header_File
           and then Suffix_Matches (Filename, Config.Naming_Data.Body_Suffix)
         then
            Unit     := No_Name;
            Kind     := Impl;
            Language := Tmp_Lang;

            if Current_Verbosity = High then
               Write_Str ("     implementation of language ");
               Write_Line (Get_Name_String (Display_Language_Name));
            end if;

         elsif Suffix_Matches (Filename, Config.Naming_Data.Spec_Suffix) then
            if Current_Verbosity = High then
               Write_Str ("     header of language ");
               Write_Line (Get_Name_String (Display_Language_Name));
            end if;

            if Header_File then
               Alternate_Languages := new Language_List_Element'
                 (Language => Language,
                  Next     => Alternate_Languages);

            else
               Header_File := True;
               Kind        := Spec;
               Unit        := No_Name;
               Language    := Tmp_Lang;
            end if;
         end if;
      end Check_File_Based_Lang;

   --  Start of processing for Check_File_Naming_Schemes

   begin
      Language              := No_Language_Index;
      Alternate_Languages   := null;
      Display_Language_Name := No_Name;
      Unit                  := No_Name;
      Lang_Kind             := File_Based;
      Kind                  := Spec;

      Tmp_Lang := Project.Project.Languages;
      while Tmp_Lang /= No_Language_Index loop
         if Current_Verbosity = High then
            Write_Line
              ("     Testing language "
               & Get_Name_String (Tmp_Lang.Name)
               & " Header_File=" & Header_File'Img);
         end if;

         Display_Language_Name := Tmp_Lang.Display_Name;
         Config := Tmp_Lang.Config;
         Lang_Kind := Config.Kind;

         case Config.Kind is
            when File_Based =>
               Check_File_Based_Lang;
               exit when Kind = Impl;

            when Unit_Based =>

               --  We know it belongs to a least a file_based language, no
               --  need to check unit-based ones.

               if not Header_File then
                  Compute_Unit_Name
                    (File_Name       => File_Name,
                     Naming          => Config.Naming_Data,
                     Kind            => Kind,
                     Unit            => Unit,
                     Project         => Project,
                     In_Tree         => In_Tree);

                  if Unit /= No_Name then
                     Language    := Tmp_Lang;
                     exit;
                  end if;
               end if;
         end case;

         Tmp_Lang := Tmp_Lang.Next;
      end loop;

      if Language = No_Language_Index
        and then Current_Verbosity = High
      then
         Write_Line ("     not a source of any language");
      end if;
   end Check_File_Naming_Schemes;

   -------------------
   -- Override_Kind --
   -------------------

   procedure Override_Kind (Source : Source_Id; Kind : Source_Kind) is
   begin
      --  If the file was previously already associated with a unit, change it

      if Source.Unit /= null
        and then Source.Kind in Spec_Or_Body
        and then Source.Unit.File_Names (Source.Kind) /= null
      then
         --  If we had another file referencing the same unit (for instance it
         --  was in an extended project), that source file is in fact invisible
         --  from now on, and in particular doesn't belong to the same unit.

         if Source.Unit.File_Names (Source.Kind) /= Source then
            Source.Unit.File_Names (Source.Kind).Unit := No_Unit_Index;
         end if;

         Source.Unit.File_Names (Source.Kind) := null;
      end if;

      Source.Kind := Kind;

      if Current_Verbosity = High
        and then Source.File /= No_File
      then
         Write_Line ("Override kind for "
                     & Get_Name_String (Source.File)
                     & " kind=" & Source.Kind'Img);
      end if;

      if Source.Kind in Spec_Or_Body and then Source.Unit /= null then
         Source.Unit.File_Names (Source.Kind) := Source;
      end if;
   end Override_Kind;

   ----------------
   -- Check_File --
   ----------------

   procedure Check_File
     (Project           : in out Project_Processing_Data;
      Data              : in out Tree_Processing_Data;
      Path              : Path_Name_Type;
      File_Name         : File_Name_Type;
      Display_File_Name : File_Name_Type;
      Locally_Removed   : Boolean;
      For_All_Sources   : Boolean)
   is
      Canonical_Path : constant Path_Name_Type :=
                         Path_Name_Type
                           (Canonical_Case_File_Name (Name_Id (Path)));

      Name_Loc              : Name_Location :=
                                Source_Names_Htable.Get
                                  (Project.Source_Names, File_Name);
      Check_Name            : Boolean := False;
      Alternate_Languages   : Language_List;
      Language              : Language_Ptr;
      Source                : Source_Id;
      Src_Ind               : Source_File_Index;
      Unit                  : Name_Id;
      Display_Language_Name : Name_Id;
      Lang_Kind             : Language_Kind;
      Kind                  : Source_Kind := Spec;

   begin
      if Name_Loc = No_Name_Location then
         Check_Name := For_All_Sources;

      else
         if Name_Loc.Found then

            --  Check if it is OK to have the same file name in several
            --  source directories.

            if not Project.Project.Known_Order_Of_Source_Dirs then
               Error_Msg_File_1 := File_Name;
               Error_Msg
                 (Data.Flags,
                  "{ is found in several source directories",
                  Name_Loc.Location, Project.Project);
            end if;

         else
            Name_Loc.Found := True;

            Source_Names_Htable.Set
              (Project.Source_Names, File_Name, Name_Loc);

            if Name_Loc.Source = No_Source then
               Check_Name := True;

            else
               Name_Loc.Source.Path := (Canonical_Path, Path);

               Source_Paths_Htable.Set
                 (Data.Tree.Source_Paths_HT,
                  Canonical_Path,
                  Name_Loc.Source);

               --  Check if this is a subunit

               if Name_Loc.Source.Unit /= No_Unit_Index
                 and then Name_Loc.Source.Kind = Impl
               then
                  Src_Ind := Sinput.P.Load_Project_File
                    (Get_Name_String (Canonical_Path));

                  if Sinput.P.Source_File_Is_Subunit (Src_Ind) then
                     Override_Kind (Name_Loc.Source, Sep);
                  end if;
               end if;

               Files_Htable.Set
                 (Data.File_To_Source, File_Name, Name_Loc.Source);
            end if;
         end if;
      end if;

      if Check_Name then
         Check_File_Naming_Schemes
           (In_Tree               => Data.Tree,
            Project               => Project,
            File_Name             => File_Name,
            Alternate_Languages   => Alternate_Languages,
            Language              => Language,
            Display_Language_Name => Display_Language_Name,
            Unit                  => Unit,
            Lang_Kind             => Lang_Kind,
            Kind                  => Kind);

         if Language = No_Language_Index then

            --  A file name in a list must be a source of a language

            if Data.Flags.Error_On_Unknown_Language
              and then Name_Loc.Found
            then
               Error_Msg_File_1 := File_Name;
               Error_Msg
                 (Data.Flags,
                  "language unknown for {",
                  Name_Loc.Location, Project.Project);
            end if;

         else
            Add_Source
              (Id                  => Source,
               Project             => Project.Project,
               Lang_Id             => Language,
               Kind                => Kind,
               Data                => Data,
               Alternate_Languages => Alternate_Languages,
               File_Name           => File_Name,
               Display_File        => Display_File_Name,
               Unit                => Unit,
               Locally_Removed     => Locally_Removed,
               Path                => (Canonical_Path, Path));
         end if;
      end if;
   end Check_File;

   ------------------------
   -- Search_Directories --
   ------------------------

   procedure Search_Directories
     (Project         : in out Project_Processing_Data;
      Data            : in out Tree_Processing_Data;
      For_All_Sources : Boolean)
   is
      Source_Dir        : String_List_Id;
      Element           : String_Element;
      Dir               : Dir_Type;
      Name              : String (1 .. 1_000);
      Last              : Natural;
      File_Name         : File_Name_Type;
      Display_File_Name : File_Name_Type;

   begin
      if Current_Verbosity = High then
         Write_Line ("Looking for sources:");
      end if;

      --  Loop through subdirectories

      Source_Dir := Project.Project.Source_Dirs;
      while Source_Dir /= Nil_String loop
         begin
            Element := Data.Tree.String_Elements.Table (Source_Dir);
            if Element.Value /= No_Name then
               Get_Name_String (Element.Display_Value);

               declare
                  Source_Directory : constant String :=
                                       Name_Buffer (1 .. Name_Len) &
                                         Directory_Separator;

                  Dir_Last : constant Natural :=
                                       Compute_Directory_Last
                                         (Source_Directory);

               begin
                  if Current_Verbosity = High then
                     Write_Attr ("Source_Dir", Source_Directory);
                  end if;

                  --  We look to every entry in the source directory

                  Open (Dir, Source_Directory);

                  loop
                     Read (Dir, Name, Last);

                     exit when Last = 0;

                     --  ??? Duplicate system call here, we just did a a
                     --  similar one. Maybe Ada.Directories would be more
                     --  appropriate here.

                     if Is_Regular_File
                          (Source_Directory & Name (1 .. Last))
                     then
                        if Current_Verbosity = High then
                           Write_Str  ("   Checking ");
                           Write_Line (Name (1 .. Last));
                        end if;

                        Name_Len := Last;
                        Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
                        Display_File_Name := Name_Find;

                        if Osint.File_Names_Case_Sensitive then
                           File_Name := Display_File_Name;
                        else
                           Canonical_Case_File_Name
                             (Name_Buffer (1 .. Name_Len));
                           File_Name := Name_Find;
                        end if;

                        declare
                           Path_Name : constant String :=
                                         Normalize_Pathname
                                           (Name (1 .. Last),
                                            Directory       =>
                                              Source_Directory
                                                (Source_Directory'First ..
                                                 Dir_Last),
                                            Resolve_Links   =>
                                              Opt.Follow_Links_For_Files,
                                            Case_Sensitive => True);
                           --  Case_Sensitive set True (no folding)

                           Path : Path_Name_Type;
                           FF   : File_Found := Excluded_Sources_Htable.Get
                                                 (Project.Excluded, File_Name);
                           To_Remove : Boolean := False;

                        begin
                           Name_Len := Path_Name'Length;
                           Name_Buffer (1 .. Name_Len) := Path_Name;
                           Path := Name_Find;

                           if FF /= No_File_Found then
                              if not FF.Found then
                                 FF.Found := True;
                                 Excluded_Sources_Htable.Set
                                   (Project.Excluded, File_Name, FF);

                                 if Current_Verbosity = High then
                                    Write_Str ("     excluded source """);
                                    Write_Str (Get_Name_String (File_Name));
                                    Write_Line ("""");
                                 end if;

                                 --  Will mark the file as removed, but we
                                 --  still need to add it to the list: if we
                                 --  don't, the file will not appear in the
                                 --  mapping file and will cause the compiler
                                 --  to fail

                                 To_Remove := True;
                              end if;
                           end if;

                           Check_File
                             (Project           => Project,
                              Data              => Data,
                              Path              => Path,
                              File_Name         => File_Name,
                              Locally_Removed   => To_Remove,
                              Display_File_Name => Display_File_Name,
                              For_All_Sources   => For_All_Sources);
                        end;
                     end if;
                  end loop;

                  Close (Dir);
               end;
            end if;

         exception
            when Directory_Error =>
               null;
         end;

         Source_Dir := Element.Next;
      end loop;

      if Current_Verbosity = High then
         Write_Line ("end Looking for sources.");
      end if;
   end Search_Directories;

   ----------------------------
   -- Load_Naming_Exceptions --
   ----------------------------

   procedure Load_Naming_Exceptions
     (Project : in out Project_Processing_Data;
      Data    : in out Tree_Processing_Data)
   is
      Source : Source_Id;
      Iter   : Source_Iterator;

   begin
      Iter := For_Each_Source (Data.Tree, Project.Project);
      loop
         Source := Prj.Element (Iter);
         exit when Source = No_Source;

         --  An excluded file cannot also be an exception file name

         if Excluded_Sources_Htable.Get (Project.Excluded, Source.File) /=
                                                                 No_File_Found
         then
            Error_Msg_File_1 := Source.File;
            Error_Msg
              (Data.Flags,
               "{ cannot be both excluded and an exception file name",
               No_Location, Project.Project);
         end if;

         if Current_Verbosity = High then
            Write_Str ("Naming exception: Putting source file ");
            Write_Str (Get_Name_String (Source.File));
            Write_Line (" in Source_Names");
         end if;

         Source_Names_Htable.Set
           (Project.Source_Names,
            K => Source.File,
            E => Name_Location'
                  (Name     => Source.File,
                   Location => No_Location,
                   Source   => Source,
                   Found    => False));

         --  If this is an Ada exception, record in table Unit_Exceptions

         if Source.Unit /= No_Unit_Index then
            declare
               Unit_Except : Unit_Exception :=
                 Unit_Exceptions_Htable.Get
                   (Project.Unit_Exceptions, Source.Unit.Name);

            begin
               Unit_Except.Name := Source.Unit.Name;

               if Source.Kind = Spec then
                  Unit_Except.Spec := Source.File;
               else
                  Unit_Except.Impl := Source.File;
               end if;

               Unit_Exceptions_Htable.Set
                 (Project.Unit_Exceptions, Source.Unit.Name, Unit_Except);
            end;
         end if;

         Next (Iter);
      end loop;
   end Load_Naming_Exceptions;

   ----------------------
   -- Look_For_Sources --
   ----------------------

   procedure Look_For_Sources
     (Project : in out Project_Processing_Data;
      Data    : in out Tree_Processing_Data)
   is
      Object_Files : Object_File_Names_Htable.Instance;
      Iter : Source_Iterator;
      Src  : Source_Id;

      procedure Check_Object (Src : Source_Id);
      --  Check if object file name of Src is already used in the project tree,
      --  and report an error if so.

      procedure Check_Object_Files;
      --  Check that no two sources of this project have the same object file

      procedure Mark_Excluded_Sources;
      --  Mark as such the sources that are declared as excluded

      ------------------
      -- Check_Object --
      ------------------

      procedure Check_Object (Src : Source_Id) is
         Source : Source_Id;

      begin
         Source := Object_File_Names_Htable.Get (Object_Files, Src.Object);

         --  We cannot just check on "Source /= Src", since we might have
         --  two different entries for the same file (and since that's
         --  the same file it is expected that it has the same object)

         if Source /= No_Source
           and then Source.Path /= Src.Path
         then
            Error_Msg_File_1 := Src.File;
            Error_Msg_File_2 := Source.File;
            Error_Msg
              (Data.Flags,
               "{ and { have the same object file name",
               No_Location, Project.Project);

         else
            Object_File_Names_Htable.Set (Object_Files, Src.Object, Src);
         end if;
      end Check_Object;

      ---------------------------
      -- Mark_Excluded_Sources --
      ---------------------------

      procedure Mark_Excluded_Sources is
         Source   : Source_Id := No_Source;
         Excluded : File_Found;
         Proj     : Project_Id;

      begin
         --  Minor optimization: if there are no excluded files, no need to
         --  traverse the list of sources. We cannot however also check whether
         --  the existing exceptions have ".Found" set to True (indicating we
         --  found them before) because we need to do some final processing on
         --  them in any case.

         if Excluded_Sources_Htable.Get_First (Project.Excluded) /=
                                                             No_File_Found
         then
            Proj := Project.Project;
            while Proj /= No_Project loop
               Iter := For_Each_Source (Data.Tree, Proj);
               while Prj.Element (Iter) /= No_Source loop
                  Source   := Prj.Element (Iter);
                  Excluded := Excluded_Sources_Htable.Get
                    (Project.Excluded, Source.File);

                  if Excluded /= No_File_Found then
                     Source.Locally_Removed := True;
                     Source.In_Interfaces   := False;

                     if Current_Verbosity = High then
                        Write_Str ("Removing file ");
                        Write_Line
                          (Get_Name_String (Excluded.File)
                           & " " & Get_Name_String (Source.Project.Name));
                     end if;

                     Excluded_Sources_Htable.Remove
                       (Project.Excluded, Source.File);
                  end if;

                  Next (Iter);
               end loop;

               Proj := Proj.Extends;
            end loop;
         end if;

         --  If we have any excluded element left, that means we did not find
         --  the source file

         Excluded := Excluded_Sources_Htable.Get_First (Project.Excluded);
         while Excluded /= No_File_Found loop
            if not Excluded.Found then

               --  Check if the file belongs to another imported project to
               --  provide a better error message.

               Src := Find_Source
                 (In_Tree          => Data.Tree,
                  Project          => Project.Project,
                  In_Imported_Only => True,
                  Base_Name        => Excluded.File);

               Err_Vars.Error_Msg_File_1 := Excluded.File;

               if Src = No_Source then
                  Error_Msg
                    (Data.Flags,
                     "unknown file {", Excluded.Location, Project.Project);
               else
                  Error_Msg
                    (Data.Flags,
                     "cannot remove a source from an imported project: {",
                     Excluded.Location, Project.Project);
               end if;
            end if;

            Excluded := Excluded_Sources_Htable.Get_Next (Project.Excluded);
         end loop;
      end Mark_Excluded_Sources;

      ------------------------
      -- Check_Object_Files --
      ------------------------

      procedure Check_Object_Files is
         Iter    : Source_Iterator;
         Src_Id  : Source_Id;
         Src_Ind : Source_File_Index;

      begin
         Iter := For_Each_Source (Data.Tree);
         loop
            Src_Id := Prj.Element (Iter);
            exit when Src_Id = No_Source;

            if Is_Compilable (Src_Id)
              and then Src_Id.Language.Config.Object_Generated
              and then Is_Extending (Project.Project, Src_Id.Project)
            then
               if Src_Id.Unit = No_Unit_Index then
                  if Src_Id.Kind = Impl then
                     Check_Object (Src_Id);
                  end if;

               else
                  case Src_Id.Kind is
                     when Spec =>
                        if Other_Part (Src_Id) = No_Source then
                           Check_Object (Src_Id);
                        end if;

                     when Sep =>
                        null;

                     when Impl =>
                        if Other_Part (Src_Id) /= No_Source then
                           Check_Object (Src_Id);

                        else
                           --  Check if it is a subunit

                           Src_Ind :=
                             Sinput.P.Load_Project_File
                               (Get_Name_String (Src_Id.Path.Name));

                           if Sinput.P.Source_File_Is_Subunit (Src_Ind) then
                              Override_Kind (Src_Id, Sep);
                           else
                              Check_Object (Src_Id);
                           end if;
                        end if;
                  end case;
               end if;
            end if;

            Next (Iter);
         end loop;
      end Check_Object_Files;

   --  Start of processing for Look_For_Sources

   begin
      Find_Excluded_Sources (Project, Data);

      if Project.Project.Languages /= No_Language_Index then
         Load_Naming_Exceptions (Project, Data);
         Find_Sources (Project, Data);
         Mark_Excluded_Sources;
         Check_Object_Files;
      end if;

      Object_File_Names_Htable.Reset (Object_Files);
   end Look_For_Sources;

   ------------------
   -- Path_Name_Of --
   ------------------

   function Path_Name_Of
     (File_Name : File_Name_Type;
      Directory : Path_Name_Type) return String
   is
      Result        : String_Access;
      The_Directory : constant String := Get_Name_String (Directory);

   begin
      Get_Name_String (File_Name);
      Result :=
        Locate_Regular_File
          (File_Name => Name_Buffer (1 .. Name_Len),
           Path      => The_Directory);

      if Result = null then
         return "";
      else
         declare
            R : String := Result.all;
         begin
            Free (Result);
            Canonical_Case_File_Name (R);
            return R;
         end;
      end if;
   end Path_Name_Of;

   -------------------
   -- Remove_Source --
   -------------------

   procedure Remove_Source
     (Id          : Source_Id;
      Replaced_By : Source_Id)
   is
      Source : Source_Id;

   begin
      if Current_Verbosity = High then
         Write_Str ("Removing source ");
         Write_Line (Get_Name_String (Id.File) & " at" & Id.Index'Img);
      end if;

      if Replaced_By /= No_Source then
         Id.Replaced_By := Replaced_By;
         Replaced_By.Declared_In_Interfaces := Id.Declared_In_Interfaces;
      end if;

      Id.In_Interfaces := False;
      Id.Locally_Removed := True;

      --  ??? Should we remove the source from the unit ? The file is not used,
      --  so probably should not be referenced from the unit. On the other hand
      --  it might give useful additional info
      --        if Id.Unit /= null then
      --           Id.Unit.File_Names (Id.Kind) := null;
      --        end if;

      Source := Id.Language.First_Source;

      if Source = Id then
         Id.Language.First_Source := Id.Next_In_Lang;

      else
         while Source.Next_In_Lang /= Id loop
            Source := Source.Next_In_Lang;
         end loop;

         Source.Next_In_Lang := Id.Next_In_Lang;
      end if;
   end Remove_Source;

   -----------------------
   -- Report_No_Sources --
   -----------------------

   procedure Report_No_Sources
     (Project      : Project_Id;
      Lang_Name    : String;
      Data         : Tree_Processing_Data;
      Location     : Source_Ptr;
      Continuation : Boolean := False)
   is
   begin
      case Data.Flags.When_No_Sources is
         when Silent =>
            null;

         when Warning | Error =>
            declare
               Msg : constant String :=
                       "<there are no " &
                       Lang_Name &
                       " sources in this project";

            begin
               Error_Msg_Warn := Data.Flags.When_No_Sources = Warning;

               if Continuation then
                  Error_Msg (Data.Flags, "\" & Msg, Location, Project);
               else
                  Error_Msg (Data.Flags, Msg, Location, Project);
               end if;
            end;
      end case;
   end Report_No_Sources;

   ----------------------
   -- Show_Source_Dirs --
   ----------------------

   procedure Show_Source_Dirs
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref)
   is
      Current : String_List_Id;
      Element : String_Element;

   begin
      Write_Line ("Source_Dirs:");

      Current := Project.Source_Dirs;
      while Current /= Nil_String loop
         Element := In_Tree.String_Elements.Table (Current);
         Write_Str  ("   ");
         Write_Line (Get_Name_String (Element.Value));
         Current := Element.Next;
      end loop;

      Write_Line ("end Source_Dirs.");
   end Show_Source_Dirs;

   ---------------------------
   -- Process_Naming_Scheme --
   ---------------------------

   procedure Process_Naming_Scheme
     (Tree         : Project_Tree_Ref;
      Root_Project : Project_Id;
      Flags        : Processing_Flags)
   is
      procedure Recursive_Check
        (Project : Project_Id;
         Data    : in out Tree_Processing_Data);
      --  Check_Naming_Scheme for the project

      ---------------------
      -- Recursive_Check --
      ---------------------

      procedure Recursive_Check
        (Project : Project_Id;
         Data    : in out Tree_Processing_Data)
      is
      begin
         if Verbose_Mode then
            Write_Str ("Processing_Naming_Scheme for project """);
            Write_Str (Get_Name_String (Project.Name));
            Write_Line ("""");
         end if;

         Prj.Nmsc.Check (Project, Data);
      end Recursive_Check;

      procedure Check_All_Projects is new
        For_Every_Project_Imported (Tree_Processing_Data, Recursive_Check);

      Data : Tree_Processing_Data;

   --  Start of processing for Process_Naming_Scheme
   begin
      Initialize (Data, Tree => Tree, Flags => Flags);
      Check_All_Projects (Root_Project, Data, Imported_First => True);
      Free (Data);
   end Process_Naming_Scheme;

end Prj.Nmsc;
