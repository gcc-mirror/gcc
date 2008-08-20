------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2008, Free Software Foundation, Inc.         --
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
with GNAT.HTable;

with Err_Vars; use Err_Vars;
with Fmap;     use Fmap;
with Hostparm;
with MLib.Tgt;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Env;  use Prj.Env;
with Prj.Err;
with Prj.Util; use Prj.Util;
with Sinput.P;
with Snames;   use Snames;
with Table;    use Table;
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

   Error_Report : Put_Line_Access := null;
   --  Set to point to error reporting procedure

   When_No_Sources : Error_Warning := Error;
   --  Indicates what should be done when there is no Ada sources in a non
   --  extending Ada project.

   ALI_Suffix   : constant String := ".ali";
   --  File suffix for ali files

   type Name_Location is record
      Name     : File_Name_Type;
      Location : Source_Ptr;
      Source   : Source_Id := No_Source;
      Except   : Boolean := False;
      Found    : Boolean := False;
   end record;
   --  Information about file names found in string list attribute
   --  Source_Files or in a source list file, stored in hash table
   --  Source_Names, used by procedure Get_Path_Names_And_Record_Sources.

   No_Name_Location : constant Name_Location :=
                        (Name     => No_File,
                         Location => No_Location,
                         Source   => No_Source,
                         Except   => False,
                         Found    => False);

   package Source_Names is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_Location,
      No_Element => No_Name_Location,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table to store file names found in string list attribute
   --  Source_Files or in a source list file, stored in hash table
   --  Source_Names, used by procedure Get_Path_Names_And_Record_Sources.

   --  More documentation needed on what unit exceptions are about ???

   type Unit_Exception is record
      Name : Name_Id;
      Spec : File_Name_Type;
      Impl : File_Name_Type;
   end record;

   No_Unit_Exception : constant Unit_Exception :=
                         (Name => No_Name,
                          Spec => No_File,
                          Impl => No_File);

   package Unit_Exceptions is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Unit_Exception,
      No_Element => No_Unit_Exception,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table to store the unit exceptions

   package Recursive_Dirs is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table to store recursive source directories, to avoid looking
   --  several times, and to avoid cycles that may be introduced by symbolic
   --  links.

   type Ada_Naming_Exception_Id is new Nat;
   No_Ada_Naming_Exception : constant Ada_Naming_Exception_Id := 0;

   type Unit_Info is record
      Kind : Spec_Or_Body;
      Unit : Name_Id;
      Next : Ada_Naming_Exception_Id := No_Ada_Naming_Exception;
   end record;
   --  Comment needed???

   --  Why is the following commented out ???
   --  No_Unit : constant Unit_Info :=
   --              (Specification, No_Name, No_Ada_Naming_Exception);

   package Ada_Naming_Exception_Table is new Table.Table
     (Table_Component_Type => Unit_Info,
      Table_Index_Type     => Ada_Naming_Exception_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Prj.Nmsc.Ada_Naming_Exception_Table");

   package Ada_Naming_Exceptions is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Ada_Naming_Exception_Id,
      No_Element => No_Ada_Naming_Exception,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store naming exceptions for Ada. For each file name
   --  there is one or several unit in table Ada_Naming_Exception_Table.

   package Object_File_Names is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => File_Name_Type,
      No_Element => No_File,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store the object file names for a project, to check that
   --  two different sources have different object file names.

   type File_Found is record
      File     : File_Name_Type  := No_File;
      Found    : Boolean         := False;
      Location : Source_Ptr      := No_Location;
   end record;
   No_File_Found : constant File_Found := (No_File, False, No_Location);
   --  Comments needed ???

   package Excluded_Sources_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => File_Found,
      No_Element => No_File_Found,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store the excluded files, if any. This is filled by
   --  Find_Excluded_Sources below.

   procedure Find_Excluded_Sources
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : Project_Data);
   --  Find the list of files that should not be considered as source files
   --  for this project. Sets the list in the Excluded_Sources_Htable.

   function Hash (Unit : Unit_Info) return Header_Num;

   type Name_And_Index is record
      Name  : Name_Id := No_Name;
      Index : Int     := 0;
   end record;
   No_Name_And_Index : constant Name_And_Index :=
                         (Name => No_Name, Index => 0);

   package Reverse_Ada_Naming_Exceptions is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_And_Index,
      No_Element => No_Name_And_Index,
      Key        => Unit_Info,
      Hash       => Hash,
      Equal      => "=");
   --  A table to check if a unit with an exceptional name will hide a source
   --  with a file name following the naming convention.

   procedure Add_Source
     (Id                  : out Source_Id;
      Data                : in out Project_Data;
      In_Tree             : Project_Tree_Ref;
      Project             : Project_Id;
      Lang                : Name_Id;
      Lang_Id             : Language_Index;
      Kind                : Source_Kind;
      File_Name           : File_Name_Type;
      Display_File        : File_Name_Type;
      Lang_Kind           : Language_Kind;
      Naming_Exception    : Boolean := False;
      Path                : Path_Name_Type := No_Path;
      Display_Path        : Path_Name_Type := No_Path;
      Alternate_Languages : Alternate_Language_Id := No_Alternate_Language;
      Other_Part          : Source_Id := No_Source;
      Unit                : Name_Id   := No_Name;
      Index               : Int       := 0;
      Source_To_Replace   : Source_Id := No_Source);
   --  Add a new source to the different lists: list of all sources in the
   --  project tree, list of source of a project and list of sources of a
   --  language.
   --
   --  If Path is specified, the file is also added to Source_Paths_HT.
   --  If Source_To_Replace is specified, it points to the source in the
   --  extended project that the new file is overriding.

   function ALI_File_Name (Source : String) return String;
   --  Return the ALI file name corresponding to a source

   procedure Check_Ada_Name (Name : String; Unit : out Name_Id);
   --  Check that a name is a valid Ada unit name

   procedure Check_Naming_Schemes
     (Data    : in out Project_Data;
      Project : Project_Id;
      In_Tree : Project_Tree_Ref);
   --  Check the naming scheme part of Data

   procedure Check_Ada_Naming_Scheme_Validity
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Naming  : Naming_Data);
   --  Check that the package Naming is correct

   procedure Check_Configuration
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Check the configuration attributes for the project

   procedure Check_If_Externally_Built
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Check attribute Externally_Built of project Project in project tree
   --  In_Tree and modify its data Data if it has the value "true".

   procedure Check_Interfaces
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  If a list of sources is specified in attribute Interfaces, set
   --  In_Interfaces only for the sources specified in the list.

   procedure Check_Library_Attributes
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Current_Dir : String;
      Data        : in out Project_Data);
   --  Check the library attributes of project Project in project tree In_Tree
   --  and modify its data Data accordingly.
   --  Current_Dir should represent the current directory, and is passed for
   --  efficiency to avoid system calls to recompute it.

   procedure Check_Package_Naming
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Check package Naming of project Project in project tree In_Tree and
   --  modify its data Data accordingly.

   procedure Check_Programming_Languages
     (In_Tree : Project_Tree_Ref;
      Project : Project_Id;
      Data    : in out Project_Data);
   --  Check attribute Languages for the project with data Data in project
   --  tree In_Tree and set the components of Data for all the programming
   --  languages indicated in attribute Languages, if any.

   function Check_Project
     (P            : Project_Id;
      Root_Project : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Extending    : Boolean) return Boolean;
   --  Returns True if P is Root_Project or, if Extending is True, a project
   --  extended by Root_Project.

   procedure Check_Stand_Alone_Library
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Data        : in out Project_Data;
      Current_Dir : String;
      Extending   : Boolean);
   --  Check if project Project in project tree In_Tree is a Stand-Alone
   --  Library project, and modify its data Data accordingly if it is one.
   --  Current_Dir should represent the current directory, and is passed for
   --  efficiency to avoid system calls to recompute it.

   procedure Get_Path_Names_And_Record_Ada_Sources
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Data        : in out Project_Data;
      Current_Dir : String);
   --  Find the path names of the source files in the Source_Names table
   --  in the source directories and record those that are Ada sources.

   function Compute_Directory_Last (Dir : String) return Natural;
   --  Return the index of the last significant character in Dir. This is used
   --  to avoid duplicate '/' (slash) characters at the end of directory names.

   procedure Error_Msg
     (Project       : Project_Id;
      In_Tree       : Project_Tree_Ref;
      Msg           : String;
      Flag_Location : Source_Ptr);
   --  Output an error message. If Error_Report is null, simply call
   --  Prj.Err.Error_Msg. Otherwise, disregard Flag_Location and use
   --  Error_Report.

   procedure Find_Ada_Sources
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Data         : in out Project_Data;
      Current_Dir  : String);
   --  Find all the Ada sources in all of the source directories of a project
   --  Current_Dir should represent the current directory, and is passed for
   --  efficiency to avoid system calls to recompute it.

   procedure Search_Directories
     (Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Data            : in out Project_Data;
      For_All_Sources : Boolean);
   --  Search the source directories to find the sources.
   --  If For_All_Sources is True, check each regular file name against the
   --  naming schemes of the different languages. Otherwise consider only the
   --  file names in the hash table Source_Names.

   procedure Check_File
     (Project           : Project_Id;
      In_Tree           : Project_Tree_Ref;
      Data              : in out Project_Data;
      Name              : String;
      File_Name         : File_Name_Type;
      Display_File_Name : File_Name_Type;
      Source_Directory  : String;
      For_All_Sources   : Boolean);
   --  Check if file File_Name is a valid source of the project. This is used
   --  in multi-language mode only.
   --  When the file matches one of the naming schemes, it is added to
   --  various htables through Add_Source and to Source_Paths_Htable.
   --
   --  Name is the name of the candidate file. It hasn't been normalized yet
   --  and is the direct result of readdir().
   --
   --  File_Name is the same as Name, but has been normalized.
   --  Display_File_Name, however, has not been normalized.
   --
   --  Source_Directory is the directory in which the file
   --  was found. It hasn't been normalized (nor has had links resolved).
   --  It should not end with a directory separator, to avoid duplicates
   --  later on.
   --
   --  If For_All_Sources is True, then all possible file names are analyzed
   --  otherwise only those currently set in the Source_Names htable.

   procedure Check_Naming_Schemes
     (In_Tree               : Project_Tree_Ref;
      Data                  : in out Project_Data;
      Filename              : String;
      File_Name             : File_Name_Type;
      Alternate_Languages   : out Alternate_Language_Id;
      Language              : out Language_Index;
      Language_Name         : out Name_Id;
      Display_Language_Name : out Name_Id;
      Unit                  : out Name_Id;
      Lang_Kind             : out Language_Kind;
      Kind                  : out Source_Kind);
   --  Check if the file name File_Name conforms to one of the naming
   --  schemes of the project.
   --
   --  If the file does not match one of the naming schemes, set Language
   --  to No_Language_Index.
   --
   --  Filename is the name of the file being investigated. It has been
   --  normalized (case-folded). File_Name is the same value.

   procedure Free_Ada_Naming_Exceptions;
   --  Free the internal hash tables used for checking naming exceptions

   procedure Get_Directories
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Current_Dir : String;
      Data    : in out Project_Data);
   --  Get the object directory, the exec directory and the source directories
   --  of a project.
   --
   --  Current_Dir should represent the current directory, and is passed for
   --  efficiency to avoid system calls to recompute it.

   procedure Get_Mains
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Get the mains of a project from attribute Main, if it exists, and put
   --  them in the project data.

   procedure Get_Sources_From_File
     (Path     : String;
      Location : Source_Ptr;
      Project  : Project_Id;
      In_Tree  : Project_Tree_Ref);
   --  Get the list of sources from a text file and put them in hash table
   --  Source_Names.

   procedure Find_Explicit_Sources
     (Current_Dir : String;
      Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Data        : in out Project_Data);
   --  Process the Source_Files and Source_List_File attributes, and store
   --  the list of source files into the Source_Names htable.
   --
   --  Lang indicates which language is being processed when in Ada_Only mode
   --  (all languages are processed anyway when in Multi_Language mode).

   procedure Get_Unit
     (In_Tree             : Project_Tree_Ref;
      Canonical_File_Name : File_Name_Type;
      Naming              : Naming_Data;
      Exception_Id        : out Ada_Naming_Exception_Id;
      Unit_Name           : out Name_Id;
      Unit_Kind           : out Spec_Or_Body;
      Needs_Pragma        : out Boolean);
   --  Find out, from a file name, the unit name, the unit kind and if a
   --  specific SFN pragma is needed. If the file name corresponds to no unit,
   --  then Unit_Name will be No_Name. If the file is a multi-unit source or an
   --  exception to the naming scheme, then Exception_Id is set to the unit or
   --  units that the source contains.

   function Is_Illegal_Suffix
     (Suffix                          : String;
      Dot_Replacement_Is_A_Single_Dot : Boolean) return Boolean;
   --  Returns True if the string Suffix cannot be used as a spec suffix, a
   --  body suffix or a separate suffix.

   procedure Locate_Directory
     (Project  : Project_Id;
      In_Tree  : Project_Tree_Ref;
      Name     : File_Name_Type;
      Parent   : Path_Name_Type;
      Dir      : out Path_Name_Type;
      Display  : out Path_Name_Type;
      Create   : String := "";
      Current_Dir : String;
      Location : Source_Ptr := No_Location);
   --  Locate a directory. Name is the directory name. Parent is the root
   --  directory, if Name a relative path name. Dir is set to the canonical
   --  case path name of the directory, and Display is the directory path name
   --  for display purposes. If the directory does not exist and Project_Setup
   --  is True and Create is a non null string, an attempt is made to create
   --  the directory. If the directory does not exist and Project_Setup is
   --  false, then Dir and Display are set to No_Name.
   --
   --  Current_Dir should represent the current directory, and is passed for
   --  efficiency to avoid system calls to recompute it.

   procedure Look_For_Sources
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Data        : in out Project_Data;
      Current_Dir : String);
   --  Find all the sources of project Project in project tree In_Tree and
   --  update its Data accordingly.
   --
   --  Current_Dir should represent the current directory, and is passed for
   --  efficiency to avoid system calls to recompute it.

   function Path_Name_Of
     (File_Name : File_Name_Type;
      Directory : Path_Name_Type) return String;
   --  Returns the path name of a (non project) file. Returns an empty string
   --  if file cannot be found.

   procedure Prepare_Ada_Naming_Exceptions
     (List    : Array_Element_Id;
      In_Tree : Project_Tree_Ref;
      Kind    : Spec_Or_Body);
   --  Prepare the internal hash tables used for checking naming exceptions
   --  for Ada. Insert all elements of List in the tables.

   function Project_Extends
     (Extending : Project_Id;
      Extended  : Project_Id;
      In_Tree   : Project_Tree_Ref) return Boolean;
   --  Returns True if Extending is extending Extended either directly or
   --  indirectly.

   procedure Record_Ada_Source
     (File_Name       : File_Name_Type;
      Path_Name       : Path_Name_Type;
      Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Data            : in out Project_Data;
      Location        : Source_Ptr;
      Current_Source  : in out String_List_Id;
      Source_Recorded : in out Boolean;
      Current_Dir     : String);
   --  Put a unit in the list of units of a project, if the file name
   --  corresponds to a valid unit name.
   --
   --  Current_Dir should represent the current directory, and is passed for
   --  efficiency to avoid system calls to recompute it.

   procedure Remove_Source
     (Id          : Source_Id;
      Replaced_By : Source_Id;
      Project     : Project_Id;
      Data        : in out Project_Data;
      In_Tree     : Project_Tree_Ref);
   --  ??? needs comment

   procedure Report_No_Sources
     (Project      : Project_Id;
      Lang_Name    : String;
      In_Tree      : Project_Tree_Ref;
      Location     : Source_Ptr;
      Continuation : Boolean := False);
   --  Report an error or a warning depending on the value of When_No_Sources
   --  when there are no sources for language Lang_Name.

   procedure Show_Source_Dirs
     (Data : Project_Data; In_Tree : Project_Tree_Ref);
   --  List all the source directories of a project

   procedure Warn_If_Not_Sources
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Conventions : Array_Element_Id;
      Specs       : Boolean;
      Extending   : Boolean);
   --  Check that individual naming conventions apply to immediate sources of
   --  the project. If not, issue a warning.

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source
     (Id                  : out Source_Id;
      Data                : in out Project_Data;
      In_Tree             : Project_Tree_Ref;
      Project             : Project_Id;
      Lang                : Name_Id;
      Lang_Id             : Language_Index;
      Kind                : Source_Kind;
      File_Name           : File_Name_Type;
      Display_File        : File_Name_Type;
      Lang_Kind           : Language_Kind;
      Naming_Exception    : Boolean := False;
      Path                : Path_Name_Type := No_Path;
      Display_Path        : Path_Name_Type := No_Path;
      Alternate_Languages : Alternate_Language_Id := No_Alternate_Language;
      Other_Part          : Source_Id := No_Source;
      Unit                : Name_Id   := No_Name;
      Index               : Int       := 0;
      Source_To_Replace   : Source_Id := No_Source)
   is
      Source   : constant Source_Id := Data.Last_Source;
      Src_Data : Source_Data := No_Source_Data;
      Config   : constant Language_Config :=
                   In_Tree.Languages_Data.Table (Lang_Id).Config;

   begin
      --  This is a new source so create an entry for it in the Sources table

      Source_Data_Table.Increment_Last (In_Tree.Sources);
      Id := Source_Data_Table.Last (In_Tree.Sources);

      if Current_Verbosity = High then
         Write_Str ("Adding source #");
         Write_Str (Id'Img);
         Write_Str (", File : ");
         Write_Str (Get_Name_String (File_Name));

         if Lang_Kind = Unit_Based then
            Write_Str (", Unit : ");
            Write_Str (Get_Name_String (Unit));
         end if;

         Write_Eol;
      end if;

      Src_Data.Project             := Project;
      Src_Data.Language_Name       := Lang;
      Src_Data.Language            := Lang_Id;
      Src_Data.Lang_Kind           := Lang_Kind;
      Src_Data.Compiled            := In_Tree.Languages_Data.Table
                                        (Lang_Id).Config.Compiler_Driver /=
                                                              Empty_File_Name;
      Src_Data.Kind                := Kind;
      Src_Data.Alternate_Languages := Alternate_Languages;
      Src_Data.Other_Part          := Other_Part;

      Src_Data.Object_Exists := Config.Object_Generated;
      Src_Data.Object_Linked := Config.Objects_Linked;

      if Other_Part /= No_Source then
         In_Tree.Sources.Table (Other_Part).Other_Part := Id;
      end if;

      Src_Data.Unit                := Unit;
      Src_Data.Index               := Index;
      Src_Data.File                := File_Name;
      Src_Data.Display_File        := Display_File;
      Src_Data.Dependency          := In_Tree.Languages_Data.Table
                                        (Lang_Id).Config.Dependency_Kind;
      Src_Data.Naming_Exception    := Naming_Exception;

      if Src_Data.Compiled and then Src_Data.Object_Exists then
         Src_Data.Object   := Object_Name (File_Name);
         Src_Data.Dep_Name :=
           Dependency_Name (File_Name, Src_Data.Dependency);
         Src_Data.Switches := Switches_Name (File_Name);
      end if;

      if Path /= No_Path then
         Src_Data.Path := (Path, Display_Path);
         Source_Paths_Htable.Set (In_Tree.Source_Paths_HT, Path, Id);
      end if;

      --  Add the source to the global list

      Src_Data.Next_In_Sources := In_Tree.First_Source;
      In_Tree.First_Source := Id;

      --  Add the source to the project list

      if Source = No_Source then
         Data.First_Source := Id;
      else
         In_Tree.Sources.Table (Source).Next_In_Project := Id;
      end if;

      Data.Last_Source := Id;

      --  Add the source to the language list

      Src_Data.Next_In_Lang :=
        In_Tree.Languages_Data.Table (Lang_Id).First_Source;
      In_Tree.Languages_Data.Table (Lang_Id).First_Source := Id;

      In_Tree.Sources.Table (Id) := Src_Data;

      if Source_To_Replace /= No_Source then
         Remove_Source (Source_To_Replace, Id, Project, Data, In_Tree);
      end if;
   end Add_Source;

   -------------------
   -- ALI_File_Name --
   -------------------

   function ALI_File_Name (Source : String) return String is
   begin
      --  If the source name has an extension, then replace it with
      --  the ALI suffix.

      for Index in reverse Source'First + 1 .. Source'Last loop
         if Source (Index) = '.' then
            return Source (Source'First .. Index - 1) & ALI_Suffix;
         end if;
      end loop;

      --  If there is no dot, or if it is the first character, just add the
      --  ALI suffix.

      return Source & ALI_Suffix;
   end ALI_File_Name;

   -----------
   -- Check --
   -----------

   procedure Check
     (Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Report_Error    : Put_Line_Access;
      When_No_Sources : Error_Warning;
      Current_Dir     : String)
   is
      Data      : Project_Data := In_Tree.Projects.Table (Project);
      Extending : Boolean := False;

   begin
      Nmsc.When_No_Sources := When_No_Sources;
      Error_Report := Report_Error;

      Recursive_Dirs.Reset;

      Check_If_Externally_Built (Project, In_Tree, Data);

      --  Object, exec and source directories

      Get_Directories (Project, In_Tree, Current_Dir, Data);

      --  Get the programming languages

      Check_Programming_Languages (In_Tree, Project, Data);

      if Data.Qualifier = Dry and then Data.Source_Dirs /= Nil_String then
         Error_Msg
           (Project, In_Tree,
            "an abstract project need to have no language, no sources or no " &
            "source directories",
            Data.Location);
      end if;

      --  Check configuration in multi language mode

      if Must_Check_Configuration then
         Check_Configuration (Project, In_Tree, Data);
      end if;

      --  Library attributes

      Check_Library_Attributes (Project, In_Tree, Current_Dir, Data);

      if Current_Verbosity = High then
         Show_Source_Dirs (Data, In_Tree);
      end if;

      Check_Package_Naming (Project, In_Tree, Data);

      Extending := Data.Extends /= No_Project;

      Check_Naming_Schemes (Data, Project, In_Tree);

      if Get_Mode = Ada_Only then
         Prepare_Ada_Naming_Exceptions
           (Data.Naming.Bodies, In_Tree, Body_Part);
         Prepare_Ada_Naming_Exceptions
           (Data.Naming.Specs, In_Tree, Specification);
      end if;

      --  Find the sources

      if Data.Source_Dirs /= Nil_String then
         Look_For_Sources (Project, In_Tree, Data, Current_Dir);

         if Get_Mode = Ada_Only then

            --  Check that all individual naming conventions apply to sources
            --  of this project file.

            Warn_If_Not_Sources
              (Project, In_Tree, Data.Naming.Bodies,
               Specs     => False,
               Extending => Extending);
            Warn_If_Not_Sources
              (Project, In_Tree, Data.Naming.Specs,
               Specs     => True,
               Extending => Extending);

         elsif Get_Mode = Multi_Language and then
               (not Data.Externally_Built) and then
               (not Extending)
         then
            declare
               Language      : Language_Index;
               Source        : Source_Id;
               Src_Data      : Source_Data;
               Alt_Lang      : Alternate_Language_Id;
               Alt_Lang_Data : Alternate_Language_Data;
               Continuation  : Boolean := False;

            begin
               Language := Data.First_Language_Processing;
               while Language /= No_Language_Index loop
                  Source := Data.First_Source;
                  Source_Loop : while Source /= No_Source loop
                     Src_Data := In_Tree.Sources.Table (Source);

                     exit Source_Loop when Src_Data.Language = Language;

                     Alt_Lang := Src_Data.Alternate_Languages;

                     Alternate_Loop :
                     while Alt_Lang /= No_Alternate_Language loop
                        Alt_Lang_Data :=
                          In_Tree.Alt_Langs.Table (Alt_Lang);
                        exit Source_Loop
                               when Alt_Lang_Data.Language = Language;
                        Alt_Lang := Alt_Lang_Data.Next;
                     end loop Alternate_Loop;

                     Source := Src_Data.Next_In_Project;
                  end loop Source_Loop;

                  if Source = No_Source then
                     Report_No_Sources
                       (Project,
                        Get_Name_String
                          (In_Tree.Languages_Data.Table
                             (Language).Display_Name),
                        In_Tree,
                        Data.Location,
                        Continuation);
                     Continuation := True;
                  end if;

                  Language := In_Tree.Languages_Data.Table (Language).Next;
               end loop;
            end;
         end if;
      end if;

      if Get_Mode = Multi_Language then

         --  If a list of sources is specified in attribute Interfaces, set
         --  In_Interfaces only for the sources specified in the list.

         Check_Interfaces (Project, In_Tree, Data);
      end if;

      --  If it is a library project file, check if it is a standalone library

      if Data.Library then
         Check_Stand_Alone_Library
           (Project, In_Tree, Data, Current_Dir, Extending);
      end if;

      --  Put the list of Mains, if any, in the project data

      Get_Mains (Project, In_Tree, Data);

      --  Update the project data in the Projects table

      In_Tree.Projects.Table (Project) := Data;

      Free_Ada_Naming_Exceptions;
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

   --------------------------------------
   -- Check_Ada_Naming_Scheme_Validity --
   --------------------------------------

   procedure Check_Ada_Naming_Scheme_Validity
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Naming  : Naming_Data)
   is
   begin
      --  Only check if we are not using the Default naming scheme

      if Naming /= In_Tree.Private_Part.Default_Naming then
         declare
            Dot_Replacement : constant String :=
                                Get_Name_String
                                  (Naming.Dot_Replacement);

            Spec_Suffix : constant String :=
                                  Spec_Suffix_Of (In_Tree, "ada", Naming);

            Body_Suffix : constant String :=
                                  Body_Suffix_Of (In_Tree, "ada", Naming);

            Separate_Suffix : constant String :=
                                Get_Name_String
                                  (Naming.Separate_Suffix);

         begin
            --  Dot_Replacement cannot

            --   - be empty
            --   - start or end with an alphanumeric
            --   - be a single '_'
            --   - start with an '_' followed by an alphanumeric
            --   - contain a '.' except if it is "."

            if Dot_Replacement'Length = 0
              or else Is_Alphanumeric
                        (Dot_Replacement (Dot_Replacement'First))
              or else Is_Alphanumeric
                        (Dot_Replacement (Dot_Replacement'Last))
              or else (Dot_Replacement (Dot_Replacement'First) = '_'
                        and then
                        (Dot_Replacement'Length = 1
                          or else
                           Is_Alphanumeric
                             (Dot_Replacement (Dot_Replacement'First + 1))))
              or else (Dot_Replacement'Length > 1
                         and then
                           Index (Source => Dot_Replacement,
                                  Pattern => ".") /= 0)
            then
               Error_Msg
                 (Project, In_Tree,
                  '"' & Dot_Replacement &
                  """ is illegal for Dot_Replacement.",
                  Naming.Dot_Repl_Loc);
            end if;

            --  Suffixes cannot
            --   - be empty

            if Is_Illegal_Suffix
                 (Spec_Suffix, Dot_Replacement = ".")
            then
               Err_Vars.Error_Msg_File_1 :=
                 Spec_Suffix_Id_Of (In_Tree, Name_Ada, Naming);
               Error_Msg
                 (Project, In_Tree,
                  "{ is illegal for Spec_Suffix",
                  Naming.Ada_Spec_Suffix_Loc);
            end if;

            if Is_Illegal_Suffix
                 (Body_Suffix, Dot_Replacement = ".")
            then
               Err_Vars.Error_Msg_File_1 :=
                 Body_Suffix_Id_Of (In_Tree, Name_Ada, Naming);
               Error_Msg
                 (Project, In_Tree,
                  "{ is illegal for Body_Suffix",
                  Naming.Ada_Body_Suffix_Loc);
            end if;

            if Body_Suffix /= Separate_Suffix then
               if Is_Illegal_Suffix
                    (Separate_Suffix, Dot_Replacement = ".")
               then
                  Err_Vars.Error_Msg_File_1 := Naming.Separate_Suffix;
                  Error_Msg
                    (Project, In_Tree,
                     "{ is illegal for Separate_Suffix",
                     Naming.Sep_Suffix_Loc);
               end if;
            end if;

            --  Spec_Suffix cannot be equal to Body_Suffix Separate_Suffix,
            --  since that would cause a clear ambiguity. Note that we do
            --  allow a Spec_Suffix to have the same termination as one of
            --  these, which causes a potential ambiguity, but we resolve
            --  that my matching the longest possible suffix.

            if Spec_Suffix = Body_Suffix then
               Error_Msg
                 (Project, In_Tree,
                  "Body_Suffix (""" &
                  Body_Suffix &
                  """) cannot be the same as Spec_Suffix.",
                  Naming.Ada_Body_Suffix_Loc);
            end if;

            if Body_Suffix /= Separate_Suffix
              and then Spec_Suffix = Separate_Suffix
            then
               Error_Msg
                 (Project, In_Tree,
                  "Separate_Suffix (""" &
                  Separate_Suffix &
                  """) cannot be the same as Spec_Suffix.",
                  Naming.Sep_Suffix_Loc);
            end if;
         end;
      end if;
   end Check_Ada_Naming_Scheme_Validity;

   -------------------------
   -- Check_Configuration --
   -------------------------

   procedure Check_Configuration
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Dot_Replacement : File_Name_Type := No_File;
      Casing          : Casing_Type    := All_Lower_Case;
      Separate_Suffix : File_Name_Type := No_File;

      Lang_Index : Language_Index := No_Language_Index;
      --  The index of the language data being checked

      Prev_Index : Language_Index := No_Language_Index;
      --  The index of the previous language

      Current_Language : Name_Id := No_Name;
      --  The name of the language

      Lang_Data : Language_Data;
      --  The data of the language being checked

      procedure Get_Language_Index_Of (Language : Name_Id);
      --  Get the language index of Language, if Language is one of the
      --  languages of the project.

      procedure Process_Project_Level_Simple_Attributes;
      --  Process the simple attributes at the project level

      procedure Process_Project_Level_Array_Attributes;
      --  Process the associate array attributes at the project level

      procedure Process_Packages;
      --  Read the packages of the project

      ---------------------------
      -- Get_Language_Index_Of --
      ---------------------------

      procedure Get_Language_Index_Of (Language : Name_Id) is
         Real_Language : Name_Id;

      begin
         Get_Name_String (Language);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Real_Language := Name_Find;

         --  Nothing to do if the language is the same as the current language

         if Current_Language /= Real_Language then
            Lang_Index := Data.First_Language_Processing;
            while Lang_Index /= No_Language_Index loop
               exit when In_Tree.Languages_Data.Table (Lang_Index).Name =
                 Real_Language;
               Lang_Index :=
                 In_Tree.Languages_Data.Table (Lang_Index).Next;
            end loop;

            if Lang_Index = No_Language_Index then
               Current_Language := No_Name;
            else
               Current_Language := Real_Language;
            end if;
         end if;
      end Get_Language_Index_Of;

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
               Current_Array := In_Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := In_Tree.Array_Elements.Table (Element_Id);

                  if Element.Index /= All_Other_Names then

                     --  Get the name of the language

                     Get_Language_Index_Of (Element.Index);

                     if Lang_Index /= No_Language_Index then
                        case Current_Array.Name is
                        when Name_Driver =>

                           --  Attribute Driver (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Binder_Driver :=
                             File_Name_Type (Element.Value.Value);

                        when Name_Required_Switches =>
                           Put (Into_List =>
                                In_Tree.Languages_Data.Table
                                  (Lang_Index).Config.Binder_Required_Switches,
                                From_List => Element.Value.Values,
                                In_Tree   => In_Tree);

                        when Name_Prefix =>

                           --  Attribute Prefix (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Binder_Prefix :=
                             Element.Value.Value;

                        when Name_Objects_Path =>

                           --  Attribute Objects_Path (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Objects_Path :=
                             Element.Value.Value;

                        when Name_Objects_Path_File =>

                           --  Attribute Objects_Path (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Objects_Path_File :=
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
                 In_Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Executable_Suffix then

                     --  Attribute Executable_Suffix: the suffix of the
                     --  executables.

                     Data.Config.Executable_Suffix :=
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
               Current_Array := In_Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := In_Tree.Array_Elements.Table (Element_Id);

                  if Element.Index /= All_Other_Names then

                     --  Get the name of the language

                     Get_Language_Index_Of (Element.Index);

                     if Lang_Index /= No_Language_Index then
                        case Current_Array.Name is
                        when Name_Dependency_Switches =>

                           --  Attribute Dependency_Switches (<language>)

                           if In_Tree.Languages_Data.Table
                               (Lang_Index).Config.Dependency_Kind = None
                           then
                              In_Tree.Languages_Data.Table
                                (Lang_Index).Config.Dependency_Kind :=
                                  Makefile;
                           end if;

                           List := Element.Value.Values;

                           if List /= Nil_String then
                              Put (Into_List =>
                                     In_Tree.Languages_Data.Table
                                       (Lang_Index).Config.Dependency_Option,
                                   From_List => List,
                                   In_Tree   => In_Tree);
                           end if;

                        when Name_Dependency_Driver =>

                           --  Attribute Dependency_Driver (<language>)

                           if In_Tree.Languages_Data.Table
                               (Lang_Index).Config.Dependency_Kind = None
                           then
                              In_Tree.Languages_Data.Table
                                (Lang_Index).Config.Dependency_Kind :=
                                  Makefile;
                           end if;

                           List := Element.Value.Values;

                           if List /= Nil_String then
                              Put (Into_List =>
                                     In_Tree.Languages_Data.Table
                                       (Lang_Index).Config.Compute_Dependency,
                                   From_List => List,
                                   In_Tree   => In_Tree);
                           end if;

                        when Name_Include_Switches =>

                           --  Attribute Include_Switches (<language>)

                           List := Element.Value.Values;

                           if List = Nil_String then
                              Error_Msg
                                (Project,
                                 In_Tree,
                                 "include option cannot be null",
                                 Element.Value.Location);
                           end if;

                           Put (Into_List =>
                                In_Tree.Languages_Data.Table
                                  (Lang_Index).Config.Include_Option,
                                From_List => List,
                                In_Tree   => In_Tree);

                        when Name_Include_Path =>

                           --  Attribute Include_Path (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Include_Path :=
                             Element.Value.Value;

                        when Name_Include_Path_File =>

                           --  Attribute Include_Path_File (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Include_Path_File :=
                               Element.Value.Value;

                        when Name_Driver =>

                           --  Attribute Driver (<language>)

                           Get_Name_String (Element.Value.Value);

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Compiler_Driver :=
                               File_Name_Type (Element.Value.Value);

                        when Name_Required_Switches =>
                           Put (Into_List =>
                                In_Tree.Languages_Data.Table
                                  (Lang_Index).Config.
                                    Compiler_Required_Switches,
                                From_List => Element.Value.Values,
                                In_Tree   => In_Tree);

                        when Name_Path_Syntax =>
                           begin
                              In_Tree.Languages_Data.Table
                                (Lang_Index).Config.Path_Syntax :=
                                  Path_Syntax_Kind'Value
                                    (Get_Name_String (Element.Value.Value));

                           exception
                              when Constraint_Error =>
                                 Error_Msg
                                   (Project,
                                    In_Tree,
                                    "invalid value for Path_Syntax",
                                    Element.Value.Location);
                           end;

                        when Name_Pic_Option =>

                           --  Attribute Compiler_Pic_Option (<language>)

                           List := Element.Value.Values;

                           if List = Nil_String then
                              Error_Msg
                                (Project,
                                 In_Tree,
                                 "compiler PIC option cannot be null",
                                 Element.Value.Location);
                           end if;

                           Put (Into_List =>
                                In_Tree.Languages_Data.Table
                                  (Lang_Index).Config.Compilation_PIC_Option,
                                From_List => List,
                                In_Tree   => In_Tree);

                        when Name_Mapping_File_Switches =>

                           --  Attribute Mapping_File_Switches (<language>)

                           List := Element.Value.Values;

                           if List = Nil_String then
                              Error_Msg
                                (Project,
                                 In_Tree,
                                 "mapping file switches cannot be null",
                                 Element.Value.Location);
                           end if;

                           Put (Into_List =>
                                In_Tree.Languages_Data.Table
                                  (Lang_Index).Config.Mapping_File_Switches,
                                From_List => List,
                                In_Tree   => In_Tree);

                        when Name_Mapping_Spec_Suffix =>

                           --  Attribute Mapping_Spec_Suffix (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Mapping_Spec_Suffix :=
                               File_Name_Type (Element.Value.Value);

                        when Name_Mapping_Body_Suffix =>

                           --  Attribute Mapping_Body_Suffix (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Mapping_Body_Suffix :=
                               File_Name_Type (Element.Value.Value);

                        when Name_Config_File_Switches =>

                           --  Attribute Config_File_Switches (<language>)

                           List := Element.Value.Values;

                           if List = Nil_String then
                              Error_Msg
                                (Project,
                                 In_Tree,
                                 "config file switches cannot be null",
                                 Element.Value.Location);
                           end if;

                           Put (Into_List =>
                                  In_Tree.Languages_Data.Table
                                    (Lang_Index).Config.Config_File_Switches,
                                From_List => List,
                                In_Tree   => In_Tree);

                        when Name_Objects_Path =>

                           --  Attribute Objects_Path (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Objects_Path :=
                               Element.Value.Value;

                        when Name_Objects_Path_File =>

                           --  Attribute Objects_Path_File (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Objects_Path_File :=
                               Element.Value.Value;

                        when Name_Config_Body_File_Name =>

                           --  Attribute Config_Body_File_Name (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Config_Body :=
                               Element.Value.Value;

                        when Name_Config_Body_File_Name_Pattern =>

                           --  Attribute Config_Body_File_Name_Pattern
                           --  (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Config_Body_Pattern :=
                               Element.Value.Value;

                        when Name_Config_Spec_File_Name =>

                           --  Attribute Config_Spec_File_Name (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Config_Spec :=
                               Element.Value.Value;

                        when Name_Config_Spec_File_Name_Pattern =>

                           --  Attribute Config_Spec_File_Name_Pattern
                           --  (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Config_Spec_Pattern :=
                             Element.Value.Value;

                        when Name_Config_File_Unique =>

                           --  Attribute Config_File_Unique (<language>)

                           begin
                              In_Tree.Languages_Data.Table
                                (Lang_Index).Config.Config_File_Unique :=
                                  Boolean'Value
                                    (Get_Name_String (Element.Value.Value));
                           exception
                              when Constraint_Error =>
                                 Error_Msg
                                   (Project,
                                    In_Tree,
                                    "illegal value for Config_File_Unique",
                                    Element.Value.Location);
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
               Attribute := In_Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Separate_Suffix then

                     --  Attribute Separate_Suffix

                     Separate_Suffix := File_Name_Type (Attribute.Value.Value);

                  elsif Attribute.Name = Name_Casing then

                     --  Attribute Casing

                     begin
                        Casing :=
                          Value (Get_Name_String (Attribute.Value.Value));

                     exception
                        when Constraint_Error =>
                           Error_Msg
                             (Project,
                              In_Tree,
                              "invalid value for Casing",
                              Attribute.Value.Location);
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
               Current_Array := In_Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := In_Tree.Array_Elements.Table (Element_Id);

                  --  Get the name of the language

                  Get_Language_Index_Of (Element.Index);

                  if Lang_Index /= No_Language_Index then
                     case Current_Array.Name is
                        when Name_Specification_Suffix | Name_Spec_Suffix =>

                           --  Attribute Spec_Suffix (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Naming_Data.Spec_Suffix :=
                               File_Name_Type (Element.Value.Value);

                        when Name_Implementation_Suffix | Name_Body_Suffix =>

                           --  Attribute Body_Suffix (<language>)

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Naming_Data.Body_Suffix :=
                               File_Name_Type (Element.Value.Value);

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Naming_Data.Separate_Suffix :=
                               File_Name_Type (Element.Value.Value);

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
                 In_Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Driver then

                     --  Attribute Linker'Driver: the default linker to use

                     Data.Config.Linker :=
                       Path_Name_Type (Attribute.Value.Value);

                  elsif Attribute.Name = Name_Required_Switches then

                     --  Attribute Required_Switches: the minimum
                     --  options to use when invoking the linker

                     Put (Into_List =>
                            Data.Config.Minimum_Linker_Options,
                          From_List => Attribute.Value.Values,
                          In_Tree   => In_Tree);

                  elsif Attribute.Name = Name_Map_File_Option then
                     Data.Config.Map_File_Option := Attribute.Value.Value;
                  end if;
               end if;

               Attribute_Id := Attribute.Next;
            end loop;
         end Process_Linker;

      --  Start of processing for Process_Packages

      begin
         Packages := Data.Decl.Packages;
         while Packages /= No_Package loop
            Element := In_Tree.Packages.Table (Packages);

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

         Attribute_Id := Data.Decl.Attributes;
         while Attribute_Id /= No_Variable loop
            Attribute :=
              In_Tree.Variable_Elements.Table (Attribute_Id);

            if not Attribute.Value.Default then
               if Attribute.Name = Name_Library_Builder then

                  --  Attribute Library_Builder: the application to invoke
                  --  to build libraries.

                  Data.Config.Library_Builder :=
                    Path_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Archive_Builder then

                  --  Attribute Archive_Builder: the archive builder
                  --  (usually "ar") and its minimum options (usually "cr").

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       (Project,
                        In_Tree,
                        "archive builder cannot be null",
                        Attribute.Value.Location);
                  end if;

                  Put (Into_List => Data.Config.Archive_Builder,
                       From_List => List,
                       In_Tree   => In_Tree);

               elsif Attribute.Name = Name_Archive_Builder_Append_Option then

                  --  Attribute Archive_Builder: the archive builder
                  --  (usually "ar") and its minimum options (usually "cr").

                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put
                       (Into_List => Data.Config.Archive_Builder_Append_Option,
                        From_List => List,
                        In_Tree   => In_Tree);
                  end if;

               elsif Attribute.Name = Name_Archive_Indexer then

                  --  Attribute Archive_Indexer: the optional archive
                  --  indexer (usually "ranlib") with its minimum options
                  --  (usually none).

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       (Project,
                        In_Tree,
                        "archive indexer cannot be null",
                        Attribute.Value.Location);
                  end if;

                  Put (Into_List => Data.Config.Archive_Indexer,
                       From_List => List,
                       In_Tree   => In_Tree);

               elsif Attribute.Name = Name_Library_Partial_Linker then

                  --  Attribute Library_Partial_Linker: the optional linker
                  --  driver with its minimum options, to partially link
                  --  archives.

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       (Project,
                        In_Tree,
                        "partial linker cannot be null",
                        Attribute.Value.Location);
                  end if;

                  Put (Into_List => Data.Config.Lib_Partial_Linker,
                       From_List => List,
                       In_Tree   => In_Tree);

               elsif Attribute.Name = Name_Library_GCC then
                  Data.Config.Shared_Lib_Driver :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Archive_Suffix then
                  Data.Config.Archive_Suffix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Linker_Executable_Option then

                  --  Attribute Linker_Executable_Option: optional options
                  --  to specify an executable name. Defaults to "-o".

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       (Project,
                        In_Tree,
                        "linker executable option cannot be null",
                        Attribute.Value.Location);
                  end if;

                  Put (Into_List => Data.Config.Linker_Executable_Option,
                       From_List => List,
                       In_Tree   => In_Tree);

               elsif Attribute.Name = Name_Linker_Lib_Dir_Option then

                  --  Attribute Linker_Lib_Dir_Option: optional options
                  --  to specify a library search directory. Defaults to
                  --  "-L".

                  Get_Name_String (Attribute.Value.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Project,
                        In_Tree,
                        "linker library directory option cannot be empty",
                        Attribute.Value.Location);
                  end if;

                  Data.Config.Linker_Lib_Dir_Option := Attribute.Value.Value;

               elsif Attribute.Name = Name_Linker_Lib_Name_Option then

                  --  Attribute Linker_Lib_Name_Option: optional options
                  --  to specify the name of a library to be linked in.
                  --  Defaults to "-l".

                  Get_Name_String (Attribute.Value.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Project,
                        In_Tree,
                        "linker library name option cannot be empty",
                        Attribute.Value.Location);
                  end if;

                  Data.Config.Linker_Lib_Name_Option := Attribute.Value.Value;

               elsif Attribute.Name = Name_Run_Path_Option then

                  --  Attribute Run_Path_Option: optional options to
                  --  specify a path for libraries.

                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List => Data.Config.Run_Path_Option,
                          From_List => List,
                          In_Tree   => In_Tree);
                  end if;

               elsif Attribute.Name = Name_Library_Support then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Data.Config.Lib_Support :=
                       Library_Support'Value (Get_Name_String
                                              (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Project,
                           In_Tree,
                           "invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Library_Support",
                           Attribute.Value.Location);
                  end;

               elsif Attribute.Name = Name_Shared_Library_Prefix then
                  Data.Config.Shared_Lib_Prefix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Shared_Library_Suffix then
                  Data.Config.Shared_Lib_Suffix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Symbolic_Link_Supported then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Data.Config.Symbolic_Link_Supported :=
                       Boolean'Value (Get_Name_String
                                      (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Project,
                           In_Tree,
                           "invalid value """
                             & Get_Name_String (Attribute.Value.Value)
                             & """ for Symbolic_Link_Supported",
                           Attribute.Value.Location);
                  end;

               elsif
                 Attribute.Name = Name_Library_Major_Minor_Id_Supported
               then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Data.Config.Lib_Maj_Min_Id_Supported :=
                       Boolean'Value (Get_Name_String
                                      (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Project,
                           In_Tree,
                           "invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Library_Major_Minor_Id_Supported",
                           Attribute.Value.Location);
                  end;

               elsif Attribute.Name = Name_Library_Auto_Init_Supported then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Data.Config.Auto_Init_Supported :=
                       Boolean'Value (Get_Name_String (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          (Project,
                           In_Tree,
                           "invalid value """
                             & Get_Name_String (Attribute.Value.Value)
                             & """ for Library_Auto_Init_Supported",
                           Attribute.Value.Location);
                  end;

               elsif Attribute.Name = Name_Shared_Library_Minimum_Switches then
                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List => Data.Config.Shared_Lib_Min_Options,
                          From_List => List,
                          In_Tree   => In_Tree);
                  end if;

               elsif Attribute.Name = Name_Library_Version_Switches then
                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List => Data.Config.Lib_Version_Options,
                          From_List => List,
                          In_Tree   => In_Tree);
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

         Current_Array_Id := Data.Decl.Arrays;
         while Current_Array_Id /= No_Array loop
            Current_Array := In_Tree.Arrays.Table (Current_Array_Id);

            Element_Id := Current_Array.Value;
            while Element_Id /= No_Array_Element loop
               Element := In_Tree.Array_Elements.Table (Element_Id);

               --  Get the name of the language

               Get_Language_Index_Of (Element.Index);

               if Lang_Index /= No_Language_Index then
                  case Current_Array.Name is
                     when Name_Inherit_Source_Path =>
                        List := Element.Value.Values;

                        if List /= Nil_String then
                           Put
                             (Into_List  =>
                                In_Tree.Languages_Data.Table (Lang_Index).
                                  Config.Include_Compatible_Languages,
                              From_List  => List,
                              In_Tree    => In_Tree,
                              Lower_Case => True);
                        end if;

                     when Name_Toolchain_Description =>

                        --  Attribute Toolchain_Description (<language>)

                        In_Tree.Languages_Data.Table
                          (Lang_Index).Config.Toolchain_Description :=
                          Element.Value.Value;

                     when Name_Toolchain_Version =>

                        --  Attribute Toolchain_Version (<language>)

                        In_Tree.Languages_Data.Table
                          (Lang_Index).Config.Toolchain_Version :=
                          Element.Value.Value;

                     when Name_Runtime_Library_Dir =>

                        --  Attribute Runtime_Library_Dir (<language>)

                        In_Tree.Languages_Data.Table
                          (Lang_Index).Config.Runtime_Library_Dir :=
                          Element.Value.Value;

                     when Name_Object_Generated =>
                        declare
                           pragma Unsuppress (All_Checks);
                           Value : Boolean;

                        begin
                           Value :=
                             Boolean'Value
                               (Get_Name_String (Element.Value.Value));

                           In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Object_Generated := Value;

                           --  If no object is generated, no object may be
                           --  linked.

                           if not Value then
                              In_Tree.Languages_Data.Table
                                (Lang_Index).Config.Objects_Linked := False;
                           end if;

                        exception
                           when Constraint_Error =>
                              Error_Msg
                                (Project,
                                 In_Tree,
                                 "invalid value """
                                 & Get_Name_String (Element.Value.Value)
                                 & """ for Object_Generated",
                                 Element.Value.Location);
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

                           if In_Tree.Languages_Data.Table
                             (Lang_Index).Config.Object_Generated
                           then
                              In_Tree.Languages_Data.Table
                                (Lang_Index).Config.Objects_Linked :=
                                Value;
                           end if;

                        exception
                           when Constraint_Error =>
                              Error_Msg
                                (Project,
                                 In_Tree,
                                 "invalid value """
                                 & Get_Name_String (Element.Value.Value)
                                 & """ for Objects_Linked",
                                 Element.Value.Location);
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

   begin
      Process_Project_Level_Simple_Attributes;
      Process_Project_Level_Array_Attributes;
      Process_Packages;

      --  For unit based languages, set Casing, Dot_Replacement and
      --  Separate_Suffix in Naming_Data.

      Lang_Index := Data.First_Language_Processing;
      while Lang_Index /= No_Language_Index loop
         if In_Tree.Languages_Data.Table
           (Lang_Index).Name = Name_Ada
         then
            In_Tree.Languages_Data.Table
              (Lang_Index).Config.Naming_Data.Casing := Casing;
            In_Tree.Languages_Data.Table
              (Lang_Index).Config.Naming_Data.Dot_Replacement :=
              Dot_Replacement;

            if Separate_Suffix /= No_File then
               In_Tree.Languages_Data.Table
                 (Lang_Index).Config.Naming_Data.Separate_Suffix :=
                 Separate_Suffix;
            end if;

            exit;
         end if;

         Lang_Index := In_Tree.Languages_Data.Table (Lang_Index).Next;
      end loop;

      --  Give empty names to various prefixes/suffixes, if they have not
      --  been specified in the configuration.

      if Data.Config.Archive_Suffix = No_File then
         Data.Config.Archive_Suffix := Empty_File;
      end if;

      if Data.Config.Shared_Lib_Prefix = No_File then
         Data.Config.Shared_Lib_Prefix := Empty_File;
      end if;

      if Data.Config.Shared_Lib_Suffix = No_File then
         Data.Config.Shared_Lib_Suffix := Empty_File;
      end if;

      Lang_Index := Data.First_Language_Processing;
      while Lang_Index /= No_Language_Index loop
         Lang_Data := In_Tree.Languages_Data.Table (Lang_Index);

         Current_Language := Lang_Data.Display_Name;

         --  For all languages, Compiler_Driver needs to be specified

         if Lang_Data.Config.Compiler_Driver = No_File then
            Error_Msg_Name_1 := Current_Language;
            Error_Msg
              (Project,
               In_Tree,
               "?no compiler specified for language %%" &
               ", ignoring all its sources",
               No_Location);

            if Lang_Index = Data.First_Language_Processing then
               Data.First_Language_Processing :=
                 Lang_Data.Next;
            else
               In_Tree.Languages_Data.Table (Prev_Index).Next :=
                 Lang_Data.Next;
            end if;

         elsif Lang_Data.Name = Name_Ada then
            Prev_Index := Lang_Index;

            --  For unit based languages, Dot_Replacement, Spec_Suffix and
            --  Body_Suffix need to be specified.

            if Lang_Data.Config.Naming_Data.Dot_Replacement = No_File then
               Error_Msg
                 (Project,
                  In_Tree,
                  "Dot_Replacement not specified for Ada",
                  No_Location);
            end if;

            if Lang_Data.Config.Naming_Data.Spec_Suffix = No_File then
               Error_Msg
                 (Project,
                  In_Tree,
                  "Spec_Suffix not specified for Ada",
                  No_Location);
            end if;

            if Lang_Data.Config.Naming_Data.Body_Suffix = No_File then
               Error_Msg
                 (Project,
                  In_Tree,
                  "Body_Suffix not specified for Ada",
                  No_Location);
            end if;

         else
            Prev_Index := Lang_Index;

            --  For file based languages, either Spec_Suffix or Body_Suffix
            --  need to be specified.

            if Lang_Data.Config.Naming_Data.Spec_Suffix = No_File and then
              Lang_Data.Config.Naming_Data.Body_Suffix = No_File
            then
               Error_Msg_Name_1 := Current_Language;
               Error_Msg
                 (Project,
                  In_Tree,
                  "no suffixes specified for %%",
                  No_Location);
            end if;
         end if;

         Lang_Index := Lang_Data.Next;
      end loop;
   end Check_Configuration;

   -------------------------------
   -- Check_If_Externally_Built --
   -------------------------------

   procedure Check_If_Externally_Built
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Externally_Built : constant Variable_Value :=
                           Util.Value_Of
                            (Name_Externally_Built,
                             Data.Decl.Attributes, In_Tree);

   begin
      if not Externally_Built.Default then
         Get_Name_String (Externally_Built.Value);
         To_Lower (Name_Buffer (1 .. Name_Len));

         if Name_Buffer (1 .. Name_Len) = "true" then
            Data.Externally_Built := True;

         elsif Name_Buffer (1 .. Name_Len) /= "false" then
            Error_Msg (Project, In_Tree,
                       "Externally_Built may only be true or false",
                       Externally_Built.Location);
         end if;
      end if;

      --  A virtual project extending an externally built project is itself
      --  externally built.

      if Data.Virtual and then Data.Extends /= No_Project then
         Data.Externally_Built :=
           In_Tree.Projects.Table (Data.Extends).Externally_Built;
      end if;

      if Current_Verbosity = High then
         Write_Str ("Project is ");

         if not Data.Externally_Built then
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
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Interfaces : constant Prj.Variable_Value :=
                     Prj.Util.Value_Of
                       (Snames.Name_Interfaces,
                        Data.Decl.Attributes,
                        In_Tree);

      List    : String_List_Id;
      Element : String_Element;
      Name    : File_Name_Type;

      Source   : Source_Id;
      Src_Data : Source_Data;

      Project_2 : Project_Id;
      Data_2     : Project_Data;

   begin
      if not Interfaces.Default then

         --  Set In_Interfaces to False for all sources. It will be set to True
         --  later for the sources in the Interfaces list.

         Project_2 := Project;
         Data_2    := Data;
         loop
            Source := Data_2.First_Source;
            while Source /= No_Source loop
               Src_Data := In_Tree.Sources.Table (Source);
               Src_Data.In_Interfaces := False;
               In_Tree.Sources.Table (Source) := Src_Data;
               Source := Src_Data.Next_In_Project;
            end loop;

            Project_2 := Data_2.Extends;

            exit when Project_2 = No_Project;

            Data_2 := In_Tree.Projects.Table (Project_2);
         end loop;

         List := Interfaces.Values;
         while List /= Nil_String loop
            Element := In_Tree.String_Elements.Table (List);
            Get_Name_String (Element.Value);
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            Name := Name_Find;

            Project_2 := Project;
            Data_2 := Data;
            Big_Loop :
            loop
               Source := Data_2.First_Source;
               while Source /= No_Source loop
                  Src_Data := In_Tree.Sources.Table (Source);
                  if Src_Data.File = Name then
                     if not Src_Data.Locally_Removed then
                        In_Tree.Sources.Table (Source).In_Interfaces := True;
                        In_Tree.Sources.Table
                          (Source).Declared_In_Interfaces := True;

                        if Src_Data.Other_Part /= No_Source then
                           In_Tree.Sources.Table
                             (Src_Data.Other_Part).In_Interfaces := True;
                           In_Tree.Sources.Table
                             (Src_Data.Other_Part).Declared_In_Interfaces :=
                             True;
                        end if;

                        if Current_Verbosity = High then
                           Write_Str ("   interface: ");
                           Write_Line (Get_Name_String (Src_Data.Path.Name));
                        end if;
                     end if;

                     exit Big_Loop;
                  end if;

                  Source := Src_Data.Next_In_Project;
               end loop;

               Project_2 := Data_2.Extends;

               exit Big_Loop when Project_2 = No_Project;

               Data_2 := In_Tree.Projects.Table (Project_2);
            end loop Big_Loop;

            if Source = No_Source then
               Error_Msg_File_1 := File_Name_Type (Element.Value);
               Error_Msg_Name_1 := Data.Name;

               Error_Msg
                 (Project,
                  In_Tree,
                  "{ cannot be an interface of project %% " &
                  "as it is not one of its sources",
                  Element.Location);
            end if;

            List := Element.Next;
         end loop;

         Data.Interfaces_Defined := True;

      elsif Data.Extends /= No_Project then
         Data.Interfaces_Defined :=
           In_Tree.Projects.Table (Data.Extends).Interfaces_Defined;

         if Data.Interfaces_Defined then
            Source := Data.First_Source;
            while Source /= No_Source loop
               Src_Data := In_Tree.Sources.Table (Source);

               if not Src_Data.Declared_In_Interfaces then
                  Src_Data.In_Interfaces := False;
                  In_Tree.Sources.Table (Source) := Src_Data;
               end if;

               Source := Src_Data.Next_In_Project;
            end loop;
         end if;
      end if;
   end Check_Interfaces;

   --------------------------
   -- Check_Naming_Schemes --
   --------------------------

   procedure Check_Naming_Schemes
     (Data    : in out Project_Data;
      Project : Project_Id;
      In_Tree : Project_Tree_Ref)
   is
      Naming_Id : constant Package_Id :=
                    Util.Value_Of (Name_Naming, Data.Decl.Packages, In_Tree);
      Naming    : Package_Element;

      procedure Check_Unit_Names (List : Array_Element_Id);
      --  Check that a list of unit names contains only valid names

      procedure Get_Exceptions (Kind : Source_Kind);

      procedure Get_Unit_Exceptions (Kind : Source_Kind);

      ----------------------
      -- Check_Unit_Names --
      ----------------------

      procedure Check_Unit_Names (List : Array_Element_Id) is
         Current   : Array_Element_Id;
         Element   : Array_Element;
         Unit_Name : Name_Id;

      begin
         --  Loop through elements of the string list

         Current := List;
         while Current /= No_Array_Element loop
            Element := In_Tree.Array_Elements.Table (Current);

            --  Put file name in canonical case

            if not Osint.File_Names_Case_Sensitive then
               Get_Name_String (Element.Value.Value);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Element.Value.Value := Name_Find;
            end if;

            --  Check that it contains a valid unit name

            Get_Name_String (Element.Index);
            Check_Ada_Name (Name_Buffer (1 .. Name_Len), Unit_Name);

            if Unit_Name = No_Name then
               Err_Vars.Error_Msg_Name_1 := Element.Index;
               Error_Msg
                 (Project, In_Tree,
                  "%% is not a valid unit name.",
                  Element.Value.Location);

            else
               if Current_Verbosity = High then
                  Write_Str ("    Unit (""");
                  Write_Str (Get_Name_String (Unit_Name));
                  Write_Line (""")");
               end if;

               Element.Index := Unit_Name;
               In_Tree.Array_Elements.Table (Current) := Element;
            end if;

            Current := Element.Next;
         end loop;
      end Check_Unit_Names;

      --------------------
      -- Get_Exceptions --
      --------------------

      procedure Get_Exceptions (Kind : Source_Kind) is
         Exceptions     : Array_Element_Id;
         Exception_List : Variable_Value;
         Element_Id     : String_List_Id;
         Element        : String_Element;
         File_Name      : File_Name_Type;
         Lang_Id        : Language_Index;
         Lang           : Name_Id;
         Lang_Kind      : Language_Kind;
         Source         : Source_Id;

      begin
         if Kind = Impl then
            Exceptions :=
              Value_Of
                (Name_Implementation_Exceptions,
                 In_Arrays => Naming.Decl.Arrays,
                 In_Tree   => In_Tree);

         else
            Exceptions :=
              Value_Of
                (Name_Specification_Exceptions,
                 In_Arrays => Naming.Decl.Arrays,
                 In_Tree   => In_Tree);
         end if;

         Lang_Id := Data.First_Language_Processing;
         while Lang_Id /= No_Language_Index loop
            if In_Tree.Languages_Data.Table (Lang_Id).Config.Kind =
                                                               File_Based
            then
               Lang := In_Tree.Languages_Data.Table (Lang_Id).Name;
               Lang_Kind :=
                 In_Tree.Languages_Data.Table (Lang_Id).Config.Kind;

               Exception_List := Value_Of
                 (Index    => Lang,
                  In_Array => Exceptions,
                  In_Tree  => In_Tree);

               if Exception_List /= Nil_Variable_Value then
                  Element_Id := Exception_List.Values;
                  while Element_Id /= Nil_String loop
                     Element := In_Tree.String_Elements.Table (Element_Id);

                     if Osint.File_Names_Case_Sensitive then
                        File_Name := File_Name_Type (Element.Value);
                     else
                        Get_Name_String (Element.Value);
                        Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                        File_Name := Name_Find;
                     end if;

                     Source := Data.First_Source;
                     while Source /= No_Source
                       and then
                       In_Tree.Sources.Table (Source).File /= File_Name
                     loop
                        Source :=
                          In_Tree.Sources.Table (Source).Next_In_Project;
                     end loop;

                     if Source = No_Source then
                        Add_Source
                          (Id           => Source,
                           Data         => Data,
                           In_Tree      => In_Tree,
                           Project      => Project,
                           Lang         => Lang,
                           Lang_Id      => Lang_Id,
                           Kind         => Kind,
                           File_Name    => File_Name,
                           Display_File => File_Name_Type (Element.Value),
                           Naming_Exception => True,
                           Lang_Kind    => Lang_Kind);

                     else
                        --  Check if the file name is already recorded for
                        --  another language or another kind.

                        if
                          In_Tree.Sources.Table (Source).Language /= Lang_Id
                        then
                           Error_Msg
                             (Project,
                              In_Tree,
                              "the same file cannot be a source " &
                              "of two languages",
                              Element.Location);

                        elsif In_Tree.Sources.Table (Source).Kind /= Kind then
                           Error_Msg
                             (Project,
                              In_Tree,
                              "the same file cannot be a source " &
                              "and a template",
                              Element.Location);
                        end if;

                        --  If the file is already recorded for the same
                        --  language and the same kind, it means that the file
                        --  name appears several times in the *_Exceptions
                        --  attribute; so there is nothing to do.

                     end if;

                     Element_Id := Element.Next;
                  end loop;
               end if;
            end if;

            Lang_Id := In_Tree.Languages_Data.Table (Lang_Id).Next;
         end loop;
      end Get_Exceptions;

      -------------------------
      -- Get_Unit_Exceptions --
      -------------------------

      procedure Get_Unit_Exceptions (Kind : Source_Kind) is
         Exceptions : Array_Element_Id;
         Element    : Array_Element;
         Unit       : Name_Id;
         Index      : Int;
         File_Name  : File_Name_Type;
         Lang_Id    : constant Language_Index :=
                        Data.Unit_Based_Language_Index;
         Lang       : constant Name_Id :=
                        Data.Unit_Based_Language_Name;

         Source            : Source_Id;
         Source_To_Replace : Source_Id := No_Source;

         Other_Project : Project_Id;
         Other_Part    : Source_Id := No_Source;

      begin
         if Lang_Id = No_Language_Index or else Lang = No_Name then
            return;
         end if;

         if Kind = Impl then
            Exceptions := Value_Of
              (Name_Body,
               In_Arrays => Naming.Decl.Arrays,
               In_Tree   => In_Tree);

            if Exceptions = No_Array_Element then
               Exceptions :=
                 Value_Of
                   (Name_Implementation,
                    In_Arrays => Naming.Decl.Arrays,
                    In_Tree   => In_Tree);
            end if;

         else
            Exceptions :=
              Value_Of
                (Name_Spec,
                 In_Arrays => Naming.Decl.Arrays,
                 In_Tree   => In_Tree);

            if Exceptions = No_Array_Element then
               Exceptions := Value_Of
                 (Name_Specification,
                  In_Arrays => Naming.Decl.Arrays,
                  In_Tree   => In_Tree);
            end if;

         end if;

         while Exceptions /= No_Array_Element loop
            Element := In_Tree.Array_Elements.Table (Exceptions);

            if Osint.File_Names_Case_Sensitive then
               File_Name := File_Name_Type (Element.Value.Value);
            else
               Get_Name_String (Element.Value.Value);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               File_Name := Name_Find;
            end if;

            Get_Name_String (Element.Index);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Unit := Name_Find;

            Index := Element.Value.Index;

            --  For Ada, check if it is a valid unit name

            if Lang = Name_Ada then
               Get_Name_String (Element.Index);
               Check_Ada_Name (Name_Buffer (1 .. Name_Len), Unit);

               if Unit = No_Name then
                  Err_Vars.Error_Msg_Name_1 := Element.Index;
                  Error_Msg
                    (Project, In_Tree,
                     "%% is not a valid unit name.",
                     Element.Value.Location);
               end if;
            end if;

            if Unit /= No_Name then

               --  Check if the source already exists

               Source := In_Tree.First_Source;
               Source_To_Replace := No_Source;

               while Source /= No_Source and then
                 (In_Tree.Sources.Table (Source).Unit /= Unit or else
                  In_Tree.Sources.Table (Source).Index /= Index)
               loop
                  Source := In_Tree.Sources.Table (Source).Next_In_Sources;
               end loop;

               if Source /= No_Source then
                  if In_Tree.Sources.Table (Source).Kind /= Kind then
                     Other_Part := Source;

                     loop
                        Source :=
                          In_Tree.Sources.Table (Source).Next_In_Sources;

                        exit when Source = No_Source or else
                          (In_Tree.Sources.Table (Source).Unit = Unit
                           and then
                           In_Tree.Sources.Table (Source).Index = Index);
                     end loop;
                  end if;

                  if Source /= No_Source then
                     Other_Project := In_Tree.Sources.Table (Source).Project;

                     if Is_Extending (Project, Other_Project, In_Tree) then
                        Other_Part :=
                          In_Tree.Sources.Table (Source).Other_Part;

                        --  Record the source to be removed

                        Source_To_Replace := Source;
                        Source := No_Source;

                     else
                        Error_Msg_Name_1 := Unit;
                        Error_Msg_Name_2 :=
                          In_Tree.Projects.Table (Other_Project).Name;
                        Error_Msg
                          (Project,
                           In_Tree,
                           "%% is already a source of project %%",
                           Element.Value.Location);
                     end if;
                  end if;
               end if;

               if Source = No_Source then
                  Add_Source
                    (Id           => Source,
                     Data         => Data,
                     In_Tree      => In_Tree,
                     Project      => Project,
                     Lang         => Lang,
                     Lang_Id      => Lang_Id,
                     Kind         => Kind,
                     File_Name    => File_Name,
                     Display_File => File_Name_Type (Element.Value.Value),
                     Lang_Kind    => Unit_Based,
                     Other_Part   => Other_Part,
                     Unit         => Unit,
                     Index        => Index,
                     Naming_Exception => True,
                     Source_To_Replace => Source_To_Replace);
               end if;
            end if;

            Exceptions := Element.Next;
         end loop;

      end Get_Unit_Exceptions;

   --  Start of processing for Check_Naming_Schemes

   begin
      if Get_Mode = Ada_Only then

         --  If there is a package Naming, we will put in Data.Naming what is
         --  in this package Naming.

         if Naming_Id /= No_Package then
            Naming := In_Tree.Packages.Table (Naming_Id);

            if Current_Verbosity = High then
               Write_Line ("Checking ""Naming"" for Ada.");
            end if;

            declare
               Bodies : constant Array_Element_Id :=
                          Util.Value_Of
                            (Name_Body, Naming.Decl.Arrays, In_Tree);

               Specs  : constant Array_Element_Id :=
                          Util.Value_Of
                            (Name_Spec, Naming.Decl.Arrays, In_Tree);

            begin
               if Bodies /= No_Array_Element then

                  --  We have elements in the array Body_Part

                  if Current_Verbosity = High then
                     Write_Line ("Found Bodies.");
                  end if;

                  Data.Naming.Bodies := Bodies;
                  Check_Unit_Names (Bodies);

               else
                  if Current_Verbosity = High then
                     Write_Line ("No Bodies.");
                  end if;
               end if;

               if Specs /= No_Array_Element then

                  --  We have elements in the array Specs

                  if Current_Verbosity = High then
                     Write_Line ("Found Specs.");
                  end if;

                  Data.Naming.Specs := Specs;
                  Check_Unit_Names (Specs);

               else
                  if Current_Verbosity = High then
                     Write_Line ("No Specs.");
                  end if;
               end if;
            end;

            --  We are now checking if variables Dot_Replacement, Casing,
            --  Spec_Suffix, Body_Suffix and/or Separate_Suffix exist.

            --  For each variable, if it does not exist, we do nothing,
            --  because we already have the default.

            --  Check Dot_Replacement

            declare
               Dot_Replacement : constant Variable_Value :=
                                   Util.Value_Of
                                     (Name_Dot_Replacement,
                                      Naming.Decl.Attributes, In_Tree);

            begin
               pragma Assert (Dot_Replacement.Kind = Single,
                              "Dot_Replacement is not a single string");

               if not Dot_Replacement.Default then
                  Get_Name_String (Dot_Replacement.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Project, In_Tree,
                        "Dot_Replacement cannot be empty",
                        Dot_Replacement.Location);

                  else
                     if Osint.File_Names_Case_Sensitive then
                        Data.Naming.Dot_Replacement :=
                          File_Name_Type (Dot_Replacement.Value);
                     else
                        Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                        Data.Naming.Dot_Replacement := Name_Find;
                     end if;
                     Data.Naming.Dot_Repl_Loc := Dot_Replacement.Location;
                  end if;
               end if;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Dot_Replacement = """);
               Write_Str  (Get_Name_String (Data.Naming.Dot_Replacement));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Check Casing

            declare
               Casing_String : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Casing,
                                    Naming.Decl.Attributes,
                                    In_Tree);

            begin
               pragma Assert (Casing_String.Kind = Single,
                              "Casing is not a single string");

               if not Casing_String.Default then
                  declare
                     Casing_Image : constant String :=
                                      Get_Name_String (Casing_String.Value);
                  begin
                     declare
                        Casing_Value : constant Casing_Type :=
                                         Value (Casing_Image);
                     begin
                        Data.Naming.Casing := Casing_Value;
                     end;

                  exception
                     when Constraint_Error =>
                        if Casing_Image'Length = 0 then
                           Error_Msg
                             (Project, In_Tree,
                              "Casing cannot be an empty string",
                              Casing_String.Location);

                        else
                           Name_Len := Casing_Image'Length;
                           Name_Buffer (1 .. Name_Len) := Casing_Image;
                           Err_Vars.Error_Msg_Name_1 := Name_Find;
                           Error_Msg
                             (Project, In_Tree,
                              "%% is not a correct Casing",
                              Casing_String.Location);
                        end if;
                  end;
               end if;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Casing = ");
               Write_Str  (Image (Data.Naming.Casing));
               Write_Char ('.');
               Write_Eol;
            end if;

            --  Check Spec_Suffix

            declare
               Ada_Spec_Suffix : constant Variable_Value :=
                                   Prj.Util.Value_Of
                                     (Index     => Name_Ada,
                                      Src_Index => 0,
                                      In_Array  => Data.Naming.Spec_Suffix,
                                      In_Tree   => In_Tree);

            begin
               if Ada_Spec_Suffix.Kind = Single
                 and then Get_Name_String (Ada_Spec_Suffix.Value) /= ""
               then
                  Get_Name_String (Ada_Spec_Suffix.Value);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                  Set_Spec_Suffix (In_Tree, "ada", Data.Naming, Name_Find);
                  Data.Naming.Ada_Spec_Suffix_Loc := Ada_Spec_Suffix.Location;

               else
                  Set_Spec_Suffix
                    (In_Tree,
                     "ada",
                     Data.Naming,
                     Default_Ada_Spec_Suffix);
               end if;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Spec_Suffix = """);
               Write_Str  (Spec_Suffix_Of (In_Tree, "ada", Data.Naming));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Check Body_Suffix

            declare
               Ada_Body_Suffix : constant Variable_Value :=
                                   Prj.Util.Value_Of
                                     (Index     => Name_Ada,
                                      Src_Index => 0,
                                      In_Array  => Data.Naming.Body_Suffix,
                                      In_Tree   => In_Tree);

            begin
               if Ada_Body_Suffix.Kind = Single
                 and then Get_Name_String (Ada_Body_Suffix.Value) /= ""
               then
                  Get_Name_String (Ada_Body_Suffix.Value);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                  Set_Body_Suffix (In_Tree, "ada", Data.Naming, Name_Find);
                  Data.Naming.Ada_Body_Suffix_Loc := Ada_Body_Suffix.Location;

               else
                  Set_Body_Suffix
                    (In_Tree,
                     "ada",
                     Data.Naming,
                     Default_Ada_Body_Suffix);
               end if;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Body_Suffix = """);
               Write_Str  (Body_Suffix_Of (In_Tree, "ada", Data.Naming));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Check Separate_Suffix

            declare
               Ada_Sep_Suffix : constant Variable_Value :=
                                  Prj.Util.Value_Of
                                    (Variable_Name => Name_Separate_Suffix,
                                     In_Variables  => Naming.Decl.Attributes,
                                     In_Tree       => In_Tree);

            begin
               if Ada_Sep_Suffix.Default then
                  Data.Naming.Separate_Suffix :=
                    Body_Suffix_Id_Of (In_Tree, Name_Ada, Data.Naming);

               else
                  Get_Name_String (Ada_Sep_Suffix.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Project, In_Tree,
                        "Separate_Suffix cannot be empty",
                        Ada_Sep_Suffix.Location);

                  else
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Data.Naming.Separate_Suffix := Name_Find;
                     Data.Naming.Sep_Suffix_Loc  := Ada_Sep_Suffix.Location;
                  end if;
               end if;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Separate_Suffix = """);
               Write_Str  (Get_Name_String (Data.Naming.Separate_Suffix));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Check if Data.Naming is valid

            Check_Ada_Naming_Scheme_Validity (Project, In_Tree, Data.Naming);
         end if;

      elsif not In_Configuration then

         --  Look into package Naming, if there is one

         if Naming_Id /= No_Package then
            Naming := In_Tree.Packages.Table (Naming_Id);

            if Current_Verbosity = High then
               Write_Line ("Checking package Naming.");
            end if;

            --  We are now checking if attribute Dot_Replacement, Casing,
            --  and/or Separate_Suffix exist.

            --  For each attribute, if it does not exist, we do nothing,
            --  because we already have the default.
            --  Otherwise, for all unit-based languages, we put the declared
            --  value in the language config.

            declare
               Dot_Repl        : constant  Variable_Value :=
                                   Util.Value_Of
                                     (Name_Dot_Replacement,
                                      Naming.Decl.Attributes, In_Tree);
               Dot_Replacement : File_Name_Type := No_File;

               Casing_String : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Casing,
                                    Naming.Decl.Attributes,
                                    In_Tree);
               Casing          : Casing_Type;
               Casing_Defined  : Boolean := False;

               Sep_Suffix : constant Variable_Value :=
                              Prj.Util.Value_Of
                                (Variable_Name => Name_Separate_Suffix,
                                 In_Variables  => Naming.Decl.Attributes,
                                 In_Tree       => In_Tree);
               Separate_Suffix : File_Name_Type := No_File;

               Lang_Id : Language_Index;
            begin
               --  Check attribute Dot_Replacement

               if not Dot_Repl.Default then
                  Get_Name_String (Dot_Repl.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Project, In_Tree,
                        "Dot_Replacement cannot be empty",
                        Dot_Repl.Location);

                  else
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Dot_Replacement := Name_Find;

                     if Current_Verbosity = High then
                        Write_Str  ("  Dot_Replacement = """);
                        Write_Str  (Get_Name_String (Dot_Replacement));
                        Write_Char ('"');
                        Write_Eol;
                     end if;
                  end if;
               end if;

               --  Check attribute Casing

               if not Casing_String.Default then
                  declare
                     Casing_Image : constant String :=
                                      Get_Name_String (Casing_String.Value);
                  begin
                     declare
                        Casing_Value : constant Casing_Type :=
                                         Value (Casing_Image);
                     begin
                        Casing := Casing_Value;
                        Casing_Defined := True;

                        if Current_Verbosity = High then
                           Write_Str  ("  Casing = ");
                           Write_Str  (Image (Casing));
                           Write_Char ('.');
                           Write_Eol;
                        end if;
                     end;

                  exception
                     when Constraint_Error =>
                        if Casing_Image'Length = 0 then
                           Error_Msg
                             (Project, In_Tree,
                              "Casing cannot be an empty string",
                              Casing_String.Location);

                        else
                           Name_Len := Casing_Image'Length;
                           Name_Buffer (1 .. Name_Len) := Casing_Image;
                           Err_Vars.Error_Msg_Name_1 := Name_Find;
                           Error_Msg
                             (Project, In_Tree,
                              "%% is not a correct Casing",
                              Casing_String.Location);
                        end if;
                  end;
               end if;

               if not Sep_Suffix.Default then
                  Get_Name_String (Sep_Suffix.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Project, In_Tree,
                        "Separate_Suffix cannot be empty",
                        Sep_Suffix.Location);

                  else
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Separate_Suffix := Name_Find;

                     if Current_Verbosity = High then
                        Write_Str ("  Separate_Suffix = """);
                        Write_Str (Get_Name_String (Separate_Suffix));
                        Write_Char ('"');
                        Write_Eol;
                     end if;
                  end if;
               end if;

               --  For all unit based languages, if any, set the specified
               --  value of Dot_Replacement, Casing and/or Separate_Suffix.

               if Dot_Replacement /= No_File
                 or else Casing_Defined
                 or else Separate_Suffix /= No_File
               then
                  Lang_Id := Data.First_Language_Processing;
                  while Lang_Id /= No_Language_Index loop
                     if In_Tree.Languages_Data.Table
                       (Lang_Id).Config.Kind = Unit_Based
                     then
                        if Dot_Replacement /= No_File then
                           In_Tree.Languages_Data.Table
                             (Lang_Id).Config.Naming_Data.Dot_Replacement :=
                             Dot_Replacement;
                        end if;

                        if Casing_Defined then
                           In_Tree.Languages_Data.Table
                             (Lang_Id).Config.Naming_Data.Casing := Casing;
                        end if;

                        if Separate_Suffix /= No_File then
                           In_Tree.Languages_Data.Table
                             (Lang_Id).Config.Naming_Data.Separate_Suffix :=
                               Separate_Suffix;
                        end if;
                     end if;

                     Lang_Id :=
                       In_Tree.Languages_Data.Table (Lang_Id).Next;
                  end loop;
               end if;
            end;

            --  Next, get the spec and body suffixes

            declare
               Suffix  : Variable_Value;
               Lang_Id : Language_Index;
               Lang    : Name_Id;

            begin
               Lang_Id := Data.First_Language_Processing;
               while Lang_Id /= No_Language_Index loop
                  Lang := In_Tree.Languages_Data.Table (Lang_Id).Name;

                  --  Spec_Suffix

                  Suffix := Value_Of
                    (Name                    => Lang,
                     Attribute_Or_Array_Name => Name_Spec_Suffix,
                     In_Package              => Naming_Id,
                     In_Tree                 => In_Tree);

                  if Suffix = Nil_Variable_Value then
                     Suffix := Value_Of
                       (Name                    => Lang,
                        Attribute_Or_Array_Name => Name_Specification_Suffix,
                        In_Package              => Naming_Id,
                        In_Tree                 => In_Tree);
                  end if;

                  if Suffix /= Nil_Variable_Value then
                     In_Tree.Languages_Data.Table (Lang_Id).
                       Config.Naming_Data.Spec_Suffix :=
                         File_Name_Type (Suffix.Value);
                  end if;

                  --  Body_Suffix

                  Suffix := Value_Of
                    (Name                    => Lang,
                     Attribute_Or_Array_Name => Name_Body_Suffix,
                     In_Package              => Naming_Id,
                     In_Tree                 => In_Tree);

                  if Suffix = Nil_Variable_Value then
                     Suffix := Value_Of
                       (Name                    => Lang,
                        Attribute_Or_Array_Name => Name_Implementation_Suffix,
                        In_Package              => Naming_Id,
                        In_Tree                 => In_Tree);
                  end if;

                  if Suffix /= Nil_Variable_Value then
                     In_Tree.Languages_Data.Table (Lang_Id).
                       Config.Naming_Data.Body_Suffix :=
                         File_Name_Type (Suffix.Value);
                  end if;

                  Lang_Id := In_Tree.Languages_Data.Table (Lang_Id).Next;
               end loop;
            end;

            --  Get the exceptions for file based languages

            Get_Exceptions (Spec);
            Get_Exceptions (Impl);

            --  Get the exceptions for unit based languages

            Get_Unit_Exceptions (Spec);
            Get_Unit_Exceptions (Impl);

         end if;
      end if;
   end Check_Naming_Schemes;

   ------------------------------
   -- Check_Library_Attributes --
   ------------------------------

   procedure Check_Library_Attributes
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Current_Dir : String;
      Data    : in out Project_Data)
   is
      Attributes   : constant Prj.Variable_Id := Data.Decl.Attributes;

      Lib_Dir      : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Dir, Attributes, In_Tree);

      Lib_Name     : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Name, Attributes, In_Tree);

      Lib_Version  : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Version, Attributes, In_Tree);

      Lib_ALI_Dir  : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Ali_Dir, Attributes, In_Tree);

      The_Lib_Kind : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Kind, Attributes, In_Tree);

      Imported_Project_List : Project_List := Empty_Project_List;

      Continuation : String_Access := No_Continuation_String'Access;

      Support_For_Libraries : Library_Support;

      Library_Directory_Present : Boolean;

      procedure Check_Library (Proj : Project_Id; Extends : Boolean);
      --  Check if an imported or extended project if also a library project

      -------------------
      -- Check_Library --
      -------------------

      procedure Check_Library (Proj : Project_Id; Extends : Boolean) is
         Proj_Data : Project_Data;
         Src_Id    : Source_Id;
         Src       : Source_Data;

      begin
         if Proj /= No_Project then
            Proj_Data := In_Tree.Projects.Table (Proj);

            if not Proj_Data.Library then

               --  The only not library projects that are OK are those that
               --  have no sources. However, header files from non-Ada
               --  languages are OK, as there is nothing to compile.

               Src_Id := Proj_Data.First_Source;
               while Src_Id /= No_Source loop
                  Src := In_Tree.Sources.Table (Src_Id);

                  exit when Src.Lang_Kind /= File_Based
                    or else Src.Kind /= Spec;

                  Src_Id := Src.Next_In_Project;
               end loop;

               if Src_Id /= No_Source then
                  Error_Msg_Name_1 := Data.Name;
                  Error_Msg_Name_2 := Proj_Data.Name;

                  if Extends then
                     if Data.Library_Kind /= Static then
                        Error_Msg
                          (Project, In_Tree,
                           Continuation.all &
                           "shared library project %% cannot extend " &
                           "project %% that is not a library project",
                           Data.Location);
                        Continuation := Continuation_String'Access;
                     end if;

                  elsif Data.Library_Kind /= Static then
                     Error_Msg
                       (Project, In_Tree,
                        Continuation.all &
                        "shared library project %% cannot import project %% " &
                        "that is not a shared library project",
                        Data.Location);
                     Continuation := Continuation_String'Access;
                  end if;
               end if;

            elsif Data.Library_Kind /= Static and then
                  Proj_Data.Library_Kind = Static
            then
               Error_Msg_Name_1 := Data.Name;
               Error_Msg_Name_2 := Proj_Data.Name;

               if Extends then
                  Error_Msg
                    (Project, In_Tree,
                     Continuation.all &
                     "shared library project %% cannot extend static " &
                     "library project %%",
                     Data.Location);

               else
                  Error_Msg
                    (Project, In_Tree,
                     Continuation.all &
                     "shared library project %% cannot import static " &
                     "library project %%",
                     Data.Location);
               end if;

               Continuation := Continuation_String'Access;
            end if;
         end if;
      end Check_Library;

   --  Start of processing for Check_Library_Attributes

   begin
      Library_Directory_Present := Lib_Dir.Value /= Empty_String;

      --  Special case of extending project

      if Data.Extends /= No_Project then
         declare
            Extended_Data : constant Project_Data :=
                              In_Tree.Projects.Table (Data.Extends);

         begin
            --  If the project extended is a library project, we inherit the
            --  library name, if it is not redefined; we check that the library
            --  directory is specified.

            if Extended_Data.Library then
               if Data.Qualifier = Standard then
                  Error_Msg
                    (Project, In_Tree,
                     "a standard project cannot extend a library project",
                     Data.Location);

               else
                  if Lib_Name.Default then
                     Data.Library_Name := Extended_Data.Library_Name;
                  end if;

                  if Lib_Dir.Default then
                     if not Data.Virtual then
                        Error_Msg
                          (Project, In_Tree,
                           "a project extending a library project must " &
                           "specify an attribute Library_Dir",
                           Data.Location);

                     else
                        --  For a virtual project extending a library project,
                        --  inherit library directory.

                        Data.Library_Dir := Extended_Data.Library_Dir;
                        Library_Directory_Present := True;
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;

      pragma Assert (Lib_Name.Kind = Single);

      if Lib_Name.Value = Empty_String then
         if Current_Verbosity = High
           and then Data.Library_Name = No_Name
         then
            Write_Line ("No library name");
         end if;

      else
         --  There is no restriction on the syntax of library names

         Data.Library_Name := Lib_Name.Value;
      end if;

      if Data.Library_Name /= No_Name then
         if Current_Verbosity = High then
            Write_Str ("Library name = """);
            Write_Str (Get_Name_String (Data.Library_Name));
            Write_Line ("""");
         end if;

         pragma Assert (Lib_Dir.Kind = Single);

         if not Library_Directory_Present then
            if Current_Verbosity = High then
               Write_Line ("No library directory");
            end if;

         else
            --  Find path name (unless inherited), check that it is a directory

            if Data.Library_Dir = No_Path_Information then
               Locate_Directory
                 (Project,
                  In_Tree,
                  File_Name_Type (Lib_Dir.Value),
                  Data.Directory.Display_Name,
                  Data.Library_Dir.Name,
                  Data.Library_Dir.Display_Name,
                  Create      => "library",
                  Current_Dir => Current_Dir,
                  Location    => Lib_Dir.Location);
            end if;

            if Data.Library_Dir = No_Path_Information then

               --  Get the absolute name of the library directory that
               --  does not exist, to report an error.

               declare
                  Dir_Name : constant String :=
                               Get_Name_String (Lib_Dir.Value);

               begin
                  if Is_Absolute_Path (Dir_Name) then
                     Err_Vars.Error_Msg_File_1 :=
                       File_Name_Type (Lib_Dir.Value);

                  else
                     Get_Name_String (Data.Directory.Display_Name);

                     if Name_Buffer (Name_Len) /= Directory_Separator then
                        Name_Len := Name_Len + 1;
                        Name_Buffer (Name_Len) := Directory_Separator;
                     end if;

                     Name_Buffer
                       (Name_Len + 1 .. Name_Len + Dir_Name'Length) :=
                       Dir_Name;
                     Name_Len := Name_Len + Dir_Name'Length;
                     Err_Vars.Error_Msg_File_1 := Name_Find;
                  end if;

                  --  Report the error

                  Error_Msg
                    (Project, In_Tree,
                     "library directory { does not exist",
                     Lib_Dir.Location);
               end;

               --  The library directory cannot be the same as the Object
               --  directory.

            elsif Data.Library_Dir.Name = Data.Object_Directory.Name then
               Error_Msg
                 (Project, In_Tree,
                  "library directory cannot be the same " &
                  "as object directory",
                  Lib_Dir.Location);
               Data.Library_Dir := No_Path_Information;

            else
               declare
                  OK       : Boolean := True;
                  Dirs_Id  : String_List_Id;
                  Dir_Elem : String_Element;

               begin
                  --  The library directory cannot be the same as a source
                  --  directory of the current project.

                  Dirs_Id := Data.Source_Dirs;
                  while Dirs_Id /= Nil_String loop
                     Dir_Elem := In_Tree.String_Elements.Table (Dirs_Id);
                     Dirs_Id  := Dir_Elem.Next;

                     if
                       Data.Library_Dir.Name = Path_Name_Type (Dir_Elem.Value)
                     then
                        Err_Vars.Error_Msg_File_1 :=
                          File_Name_Type (Dir_Elem.Value);
                        Error_Msg
                          (Project, In_Tree,
                           "library directory cannot be the same " &
                           "as source directory {",
                           Lib_Dir.Location);
                        OK := False;
                        exit;
                     end if;
                  end loop;

                  if OK then

                     --  The library directory cannot be the same as a source
                     --  directory of another project either.

                     Project_Loop :
                     for Pid in 1 .. Project_Table.Last (In_Tree.Projects) loop
                        if Pid /= Project then
                           Dirs_Id := In_Tree.Projects.Table (Pid).Source_Dirs;

                           Dir_Loop : while Dirs_Id /= Nil_String loop
                              Dir_Elem :=
                                In_Tree.String_Elements.Table (Dirs_Id);
                              Dirs_Id  := Dir_Elem.Next;

                              if Data.Library_Dir.Name =
                                Path_Name_Type (Dir_Elem.Value)
                              then
                                 Err_Vars.Error_Msg_File_1 :=
                                   File_Name_Type (Dir_Elem.Value);
                                 Err_Vars.Error_Msg_Name_1 :=
                                   In_Tree.Projects.Table (Pid).Name;

                                 Error_Msg
                                   (Project, In_Tree,
                                    "library directory cannot be the same " &
                                    "as source directory { of project %%",
                                    Lib_Dir.Location);
                                 OK := False;
                                 exit Project_Loop;
                              end if;
                           end loop Dir_Loop;
                        end if;
                     end loop Project_Loop;
                  end if;

                  if not OK then
                     Data.Library_Dir := No_Path_Information;

                  elsif Current_Verbosity = High then

                     --  Display the Library directory in high verbosity

                     Write_Str ("Library directory =""");
                     Write_Str
                       (Get_Name_String (Data.Library_Dir.Display_Name));
                     Write_Line ("""");
                  end if;
               end;
            end if;
         end if;

      end if;

      Data.Library :=
        Data.Library_Dir /= No_Path_Information
        and then
      Data.Library_Name /= No_Name;

      if Data.Extends = No_Project then
         case Data.Qualifier is
            when Standard =>
               if Data.Library then
                  Error_Msg
                    (Project, In_Tree,
                     "a standard project cannot be a library project",
                     Lib_Name.Location);
               end if;

            when Library =>
               if not Data.Library then
                  Error_Msg
                    (Project, In_Tree,
                     "not a library project",
                     Data.Location);
               end if;

            when others =>
               null;

         end case;
      end if;

      if Data.Library then
         if Get_Mode = Multi_Language then
            Support_For_Libraries := Data.Config.Lib_Support;

         else
            Support_For_Libraries := MLib.Tgt.Support_For_Libraries;
         end if;

         if Support_For_Libraries = Prj.None then
            Error_Msg
              (Project, In_Tree,
               "?libraries are not supported on this platform",
               Lib_Name.Location);
            Data.Library := False;

         else
            if Lib_ALI_Dir.Value = Empty_String then
               if Current_Verbosity = High then
                  Write_Line ("No library ALI directory specified");
               end if;
               Data.Library_ALI_Dir := Data.Library_Dir;

            else
               --  Find path name, check that it is a directory

               Locate_Directory
                 (Project,
                  In_Tree,
                  File_Name_Type (Lib_ALI_Dir.Value),
                  Data.Directory.Display_Name,
                  Data.Library_ALI_Dir.Name,
                  Data.Library_ALI_Dir.Display_Name,
                  Create   => "library ALI",
                  Current_Dir => Current_Dir,
                  Location => Lib_ALI_Dir.Location);

               if Data.Library_ALI_Dir = No_Path_Information then

                  --  Get the absolute name of the library ALI directory that
                  --  does not exist, to report an error.

                  declare
                     Dir_Name : constant String :=
                                  Get_Name_String (Lib_ALI_Dir.Value);

                  begin
                     if Is_Absolute_Path (Dir_Name) then
                        Err_Vars.Error_Msg_File_1 :=
                          File_Name_Type (Lib_Dir.Value);

                     else
                        Get_Name_String (Data.Directory.Display_Name);

                        if Name_Buffer (Name_Len) /= Directory_Separator then
                           Name_Len := Name_Len + 1;
                           Name_Buffer (Name_Len) := Directory_Separator;
                        end if;

                        Name_Buffer
                          (Name_Len + 1 .. Name_Len + Dir_Name'Length) :=
                          Dir_Name;
                        Name_Len := Name_Len + Dir_Name'Length;
                        Err_Vars.Error_Msg_File_1 := Name_Find;
                     end if;

                     --  Report the error

                     Error_Msg
                       (Project, In_Tree,
                        "library 'A'L'I directory { does not exist",
                        Lib_ALI_Dir.Location);
                  end;
               end if;

               if Data.Library_ALI_Dir /= Data.Library_Dir then

                  --  The library ALI directory cannot be the same as the
                  --  Object directory.

                  if Data.Library_ALI_Dir = Data.Object_Directory then
                     Error_Msg
                       (Project, In_Tree,
                        "library 'A'L'I directory cannot be the same " &
                        "as object directory",
                        Lib_ALI_Dir.Location);
                     Data.Library_ALI_Dir := No_Path_Information;

                  else
                     declare
                        OK       : Boolean := True;
                        Dirs_Id  : String_List_Id;
                        Dir_Elem : String_Element;

                     begin
                        --  The library ALI directory cannot be the same as
                        --  a source directory of the current project.

                        Dirs_Id := Data.Source_Dirs;
                        while Dirs_Id /= Nil_String loop
                           Dir_Elem := In_Tree.String_Elements.Table (Dirs_Id);
                           Dirs_Id  := Dir_Elem.Next;

                           if Data.Library_ALI_Dir.Name =
                             Path_Name_Type (Dir_Elem.Value)
                           then
                              Err_Vars.Error_Msg_File_1 :=
                                File_Name_Type (Dir_Elem.Value);
                              Error_Msg
                                (Project, In_Tree,
                                 "library 'A'L'I directory cannot be " &
                                 "the same as source directory {",
                                 Lib_ALI_Dir.Location);
                              OK := False;
                              exit;
                           end if;
                        end loop;

                        if OK then

                           --  The library ALI directory cannot be the same as
                           --  a source directory of another project either.

                           ALI_Project_Loop :
                           for
                             Pid in 1 .. Project_Table.Last (In_Tree.Projects)
                           loop
                              if Pid /= Project then
                                 Dirs_Id :=
                                   In_Tree.Projects.Table (Pid).Source_Dirs;

                                 ALI_Dir_Loop :
                                 while Dirs_Id /= Nil_String loop
                                    Dir_Elem :=
                                      In_Tree.String_Elements.Table (Dirs_Id);
                                    Dirs_Id  := Dir_Elem.Next;

                                    if Data.Library_ALI_Dir.Name =
                                        Path_Name_Type (Dir_Elem.Value)
                                    then
                                       Err_Vars.Error_Msg_File_1 :=
                                         File_Name_Type (Dir_Elem.Value);
                                       Err_Vars.Error_Msg_Name_1 :=
                                         In_Tree.Projects.Table (Pid).Name;

                                       Error_Msg
                                         (Project, In_Tree,
                                          "library 'A'L'I directory cannot " &
                                          "be the same as source directory " &
                                          "{ of project %%",
                                          Lib_ALI_Dir.Location);
                                       OK := False;
                                       exit ALI_Project_Loop;
                                    end if;
                                 end loop ALI_Dir_Loop;
                              end if;
                           end loop ALI_Project_Loop;
                        end if;

                        if not OK then
                           Data.Library_ALI_Dir := No_Path_Information;

                        elsif Current_Verbosity = High then

                           --  Display the Library ALI directory in high
                           --  verbosity.

                           Write_Str ("Library ALI directory =""");
                           Write_Str
                             (Get_Name_String
                                (Data.Library_ALI_Dir.Display_Name));
                           Write_Line ("""");
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
               Data.Lib_Internal_Name := Lib_Version.Value;
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
                     Data.Library_Kind := Static;

                  elsif Kind_Name = "dynamic" then
                     Data.Library_Kind := Dynamic;

                  elsif Kind_Name = "relocatable" then
                     Data.Library_Kind := Relocatable;

                  else
                     Error_Msg
                       (Project, In_Tree,
                        "illegal value for Library_Kind",
                        The_Lib_Kind.Location);
                     OK := False;
                  end if;

                  if Current_Verbosity = High and then OK then
                     Write_Str ("Library kind = ");
                     Write_Line (Kind_Name);
                  end if;

                  if Data.Library_Kind /= Static and then
                    Support_For_Libraries = Prj.Static_Only
                  then
                     Error_Msg
                       (Project, In_Tree,
                        "only static libraries are supported " &
                        "on this platform",
                        The_Lib_Kind.Location);
                     Data.Library := False;
                  end if;
               end;
            end if;

            if Data.Library then
               if Current_Verbosity = High then
                  Write_Line ("This is a library project file");
               end if;

               if Get_Mode = Multi_Language then
                  Check_Library (Data.Extends, Extends => True);

                  Imported_Project_List := Data.Imported_Projects;
                  while Imported_Project_List /= Empty_Project_List loop
                     Check_Library
                       (In_Tree.Project_Lists.Table
                          (Imported_Project_List).Project,
                        Extends => False);
                     Imported_Project_List :=
                       In_Tree.Project_Lists.Table
                         (Imported_Project_List).Next;
                  end loop;
               end if;
            end if;

         end if;
      end if;

      --  Check if Linker'Switches or Linker'Default_Switches are declared.
      --  Warn if they are declared, as it is a common error to think that
      --  library are "linked" with Linker switches.

      if Data.Library then
         declare
            Linker_Package_Id : constant Package_Id :=
                                  Util.Value_Of
                                    (Name_Linker, Data.Decl.Packages, In_Tree);
            Linker_Package    : Package_Element;
            Switches          : Array_Element_Id := No_Array_Element;

         begin
            if Linker_Package_Id /= No_Package then
               Linker_Package := In_Tree.Packages.Table (Linker_Package_Id);

               Switches :=
                 Value_Of
                   (Name      => Name_Switches,
                    In_Arrays => Linker_Package.Decl.Arrays,
                    In_Tree   => In_Tree);

               if Switches = No_Array_Element then
                  Switches :=
                    Value_Of
                      (Name      => Name_Default_Switches,
                       In_Arrays => Linker_Package.Decl.Arrays,
                       In_Tree   => In_Tree);
               end if;

               if Switches /= No_Array_Element then
                  Error_Msg
                    (Project, In_Tree,
                     "?Linker switches not taken into account in library " &
                     "projects",
                     No_Location);
               end if;
            end if;
         end;
      end if;

      if Data.Extends /= No_Project then
         In_Tree.Projects.Table (Data.Extends).Library := False;
      end if;
   end Check_Library_Attributes;

   --------------------------
   -- Check_Package_Naming --
   --------------------------

   procedure Check_Package_Naming
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Naming_Id : constant Package_Id :=
                    Util.Value_Of (Name_Naming, Data.Decl.Packages, In_Tree);

      Naming    : Package_Element;

   begin
      --  If there is a package Naming, we will put in Data.Naming
      --  what is in this package Naming.

      if Naming_Id /= No_Package then
         Naming := In_Tree.Packages.Table (Naming_Id);

         if Current_Verbosity = High then
            Write_Line ("Checking ""Naming"".");
         end if;

         --  Check Spec_Suffix

         declare
            Spec_Suffixs : Array_Element_Id :=
                             Util.Value_Of
                               (Name_Spec_Suffix,
                                Naming.Decl.Arrays,
                                In_Tree);

            Suffix  : Array_Element_Id;
            Element : Array_Element;
            Suffix2 : Array_Element_Id;

         begin
            --  If some suffixes have been specified, we make sure that
            --  for each language for which a default suffix has been
            --  specified, there is a suffix specified, either the one
            --  in the project file or if there were none, the default.

            if Spec_Suffixs /= No_Array_Element then
               Suffix := Data.Naming.Spec_Suffix;

               while Suffix /= No_Array_Element loop
                  Element :=
                    In_Tree.Array_Elements.Table (Suffix);
                  Suffix2 := Spec_Suffixs;

                  while Suffix2 /= No_Array_Element loop
                     exit when In_Tree.Array_Elements.Table
                                (Suffix2).Index = Element.Index;
                     Suffix2 := In_Tree.Array_Elements.Table
                                 (Suffix2).Next;
                  end loop;

                  --  There is a registered default suffix, but no
                  --  suffix specified in the project file.
                  --  Add the default to the array.

                  if Suffix2 = No_Array_Element then
                     Array_Element_Table.Increment_Last
                       (In_Tree.Array_Elements);
                     In_Tree.Array_Elements.Table
                       (Array_Element_Table.Last
                          (In_Tree.Array_Elements)) :=
                       (Index                => Element.Index,
                        Src_Index            => Element.Src_Index,
                        Index_Case_Sensitive => False,
                        Value                => Element.Value,
                        Next                 => Spec_Suffixs);
                     Spec_Suffixs := Array_Element_Table.Last
                                       (In_Tree.Array_Elements);
                  end if;

                  Suffix := Element.Next;
               end loop;

               --  Put the resulting array as the specification suffixes

               Data.Naming.Spec_Suffix := Spec_Suffixs;
            end if;
         end;

         declare
            Current : Array_Element_Id;
            Element : Array_Element;

         begin
            Current := Data.Naming.Spec_Suffix;
            while Current /= No_Array_Element loop
               Element := In_Tree.Array_Elements.Table (Current);
               Get_Name_String (Element.Value.Value);

               if Name_Len = 0 then
                  Error_Msg
                    (Project, In_Tree,
                     "Spec_Suffix cannot be empty",
                     Element.Value.Location);
               end if;

               In_Tree.Array_Elements.Table (Current) := Element;
               Current := Element.Next;
            end loop;
         end;

         --  Check Body_Suffix

         declare
            Impl_Suffixs : Array_Element_Id :=
                             Util.Value_Of
                               (Name_Body_Suffix,
                                Naming.Decl.Arrays,
                                In_Tree);

            Suffix  : Array_Element_Id;
            Element : Array_Element;
            Suffix2 : Array_Element_Id;

         begin
            --  If some suffixes have been specified, we make sure that
            --  for each language for which a default suffix has been
            --  specified, there is a suffix specified, either the one
            --  in the project file or if there were none, the default.

            if Impl_Suffixs /= No_Array_Element then
               Suffix := Data.Naming.Body_Suffix;
               while Suffix /= No_Array_Element loop
                  Element :=
                    In_Tree.Array_Elements.Table (Suffix);

                  Suffix2 := Impl_Suffixs;
                  while Suffix2 /= No_Array_Element loop
                     exit when In_Tree.Array_Elements.Table
                                (Suffix2).Index = Element.Index;
                     Suffix2 := In_Tree.Array_Elements.Table
                                  (Suffix2).Next;
                  end loop;

                  --  There is a registered default suffix, but no suffix was
                  --  specified in the project file. Add default to the array.

                  if Suffix2 = No_Array_Element then
                     Array_Element_Table.Increment_Last
                       (In_Tree.Array_Elements);
                     In_Tree.Array_Elements.Table
                       (Array_Element_Table.Last
                          (In_Tree.Array_Elements)) :=
                       (Index                => Element.Index,
                        Src_Index            => Element.Src_Index,
                        Index_Case_Sensitive => False,
                        Value                => Element.Value,
                        Next                 => Impl_Suffixs);
                     Impl_Suffixs := Array_Element_Table.Last
                                       (In_Tree.Array_Elements);
                  end if;

                  Suffix := Element.Next;
               end loop;

               --  Put the resulting array as the implementation suffixes

               Data.Naming.Body_Suffix := Impl_Suffixs;
            end if;
         end;

         declare
            Current : Array_Element_Id;
            Element : Array_Element;

         begin
            Current := Data.Naming.Body_Suffix;
            while Current /= No_Array_Element loop
               Element := In_Tree.Array_Elements.Table (Current);
               Get_Name_String (Element.Value.Value);

               if Name_Len = 0 then
                  Error_Msg
                    (Project, In_Tree,
                     "Body_Suffix cannot be empty",
                     Element.Value.Location);
               end if;

               In_Tree.Array_Elements.Table (Current) := Element;
               Current := Element.Next;
            end loop;
         end;

         --  Get the exceptions, if any

         Data.Naming.Specification_Exceptions :=
           Util.Value_Of
             (Name_Specification_Exceptions,
              In_Arrays => Naming.Decl.Arrays,
              In_Tree   => In_Tree);

         Data.Naming.Implementation_Exceptions :=
           Util.Value_Of
             (Name_Implementation_Exceptions,
              In_Arrays => Naming.Decl.Arrays,
              In_Tree   => In_Tree);
      end if;
   end Check_Package_Naming;

   ---------------------------------
   -- Check_Programming_Languages --
   ---------------------------------

   procedure Check_Programming_Languages
     (In_Tree : Project_Tree_Ref;
      Project : Project_Id;
      Data    : in out Project_Data)
   is
      Languages   : Variable_Value := Nil_Variable_Value;
      Def_Lang    : Variable_Value := Nil_Variable_Value;
      Def_Lang_Id : Name_Id;

   begin
      Data.First_Language_Processing := No_Language_Index;
      Languages :=
        Prj.Util.Value_Of (Name_Languages, Data.Decl.Attributes, In_Tree);
      Def_Lang :=
        Prj.Util.Value_Of
          (Name_Default_Language, Data.Decl.Attributes, In_Tree);
      Data.Ada_Sources_Present   := Data.Source_Dirs /= Nil_String;
      Data.Other_Sources_Present := Data.Source_Dirs /= Nil_String;

      if Data.Source_Dirs /= Nil_String then

         --  Check if languages are specified in this project

         if Languages.Default then

            --  Attribute Languages is not specified. So, it defaults to
            --  a project of the default language only.

            Name_List_Table.Increment_Last (In_Tree.Name_Lists);
            Data.Languages := Name_List_Table.Last (In_Tree.Name_Lists);

            --  In Ada_Only mode, the default language is Ada

            if Get_Mode = Ada_Only then
               In_Tree.Name_Lists.Table (Data.Languages) :=
                 (Name => Name_Ada, Next => No_Name_List);

               --  Attribute Languages is not specified. So, it defaults to
               --  a project of language Ada only. No sources of languages
               --  other than Ada

               Data.Other_Sources_Present := False;

            else
               --  Fail if there is no default language defined

               if Def_Lang.Default then
                  if not Default_Language_Is_Ada then
                     Error_Msg
                       (Project,
                        In_Tree,
                        "no languages defined for this project",
                        Data.Location);
                     Def_Lang_Id := No_Name;
                  else
                     Def_Lang_Id := Name_Ada;
                  end if;

               else
                  Get_Name_String (Def_Lang.Value);
                  To_Lower (Name_Buffer (1 .. Name_Len));
                  Def_Lang_Id := Name_Find;
               end if;

               if Def_Lang_Id /=  No_Name then
                  In_Tree.Name_Lists.Table (Data.Languages) :=
                    (Name => Def_Lang_Id, Next => No_Name_List);

                  Language_Data_Table.Increment_Last (In_Tree.Languages_Data);

                  Data.First_Language_Processing :=
                    Language_Data_Table.Last (In_Tree.Languages_Data);
                  In_Tree.Languages_Data.Table
                    (Data.First_Language_Processing) := No_Language_Data;
                  In_Tree.Languages_Data.Table
                    (Data.First_Language_Processing).Name := Def_Lang_Id;
                  Get_Name_String (Def_Lang_Id);
                  Name_Buffer (1) := GNAT.Case_Util.To_Upper (Name_Buffer (1));
                  In_Tree.Languages_Data.Table
                    (Data.First_Language_Processing).Display_Name := Name_Find;

                  if Def_Lang_Id = Name_Ada then
                     In_Tree.Languages_Data.Table
                       (Data.First_Language_Processing).Config.Kind
                       := Unit_Based;
                     In_Tree.Languages_Data.Table
                       (Data.First_Language_Processing).Config.Dependency_Kind
                       := ALI_File;
                     Data.Unit_Based_Language_Name := Name_Ada;
                     Data.Unit_Based_Language_Index :=
                       Data.First_Language_Processing;
                  else
                     In_Tree.Languages_Data.Table
                       (Data.First_Language_Processing).Config.Kind
                       := File_Based;
                  end if;
               end if;
            end if;

         else
            declare
               Current           : String_List_Id := Languages.Values;
               Element           : String_Element;
               Lang_Name         : Name_Id;
               Index             : Language_Index;
               Lang_Data         : Language_Data;
               NL_Id             : Name_List_Index := No_Name_List;

            begin
               --  Assume there are no language declared

               Data.Ada_Sources_Present := False;
               Data.Other_Sources_Present := False;

               --  If there are no languages declared, there are no sources

               if Current = Nil_String then
                  Data.Source_Dirs := Nil_String;

                  if Data.Qualifier = Standard then
                     Error_Msg
                       (Project,
                        In_Tree,
                        "a standard project cannot have no language declared",
                        Languages.Location);
                  end if;

               else
                  --  Look through all the languages specified in attribute
                  --  Languages.

                  while Current /= Nil_String loop
                     Element :=
                       In_Tree.String_Elements.Table (Current);
                     Get_Name_String (Element.Value);
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Lang_Name := Name_Find;

                     NL_Id := Data.Languages;
                     while NL_Id /= No_Name_List loop
                        exit when
                          Lang_Name = In_Tree.Name_Lists.Table (NL_Id).Name;
                        NL_Id := In_Tree.Name_Lists.Table (NL_Id).Next;
                     end loop;

                     if NL_Id = No_Name_List then
                        Name_List_Table.Increment_Last (In_Tree.Name_Lists);

                        if Data.Languages = No_Name_List then
                           Data.Languages :=
                             Name_List_Table.Last (In_Tree.Name_Lists);

                        else
                           NL_Id := Data.Languages;
                           while In_Tree.Name_Lists.Table (NL_Id).Next /=
                                   No_Name_List
                           loop
                              NL_Id := In_Tree.Name_Lists.Table (NL_Id).Next;
                           end loop;

                           In_Tree.Name_Lists.Table (NL_Id).Next :=
                             Name_List_Table.Last (In_Tree.Name_Lists);
                        end if;

                        NL_Id := Name_List_Table.Last (In_Tree.Name_Lists);
                        In_Tree.Name_Lists.Table (NL_Id) :=
                          (Lang_Name, No_Name_List);

                        if Get_Mode = Ada_Only then
                           --  Check for language Ada

                           if Lang_Name = Name_Ada then
                              Data.Ada_Sources_Present := True;

                           else
                              Data.Other_Sources_Present := True;
                           end if;

                        else
                           Language_Data_Table.Increment_Last
                                                 (In_Tree.Languages_Data);
                           Index :=
                             Language_Data_Table.Last (In_Tree.Languages_Data);
                           Lang_Data.Name := Lang_Name;
                           Lang_Data.Display_Name := Element.Value;
                           Lang_Data.Next := Data.First_Language_Processing;

                           if Lang_Name = Name_Ada then
                              Lang_Data.Config.Kind := Unit_Based;
                              Lang_Data.Config.Dependency_Kind := ALI_File;
                              Data.Unit_Based_Language_Name := Name_Ada;
                              Data.Unit_Based_Language_Index := Index;

                           else
                              Lang_Data.Config.Kind := File_Based;
                              Lang_Data.Config.Dependency_Kind := None;
                           end if;

                           In_Tree.Languages_Data.Table (Index) := Lang_Data;
                           Data.First_Language_Processing := Index;
                        end if;
                     end if;

                     Current := Element.Next;
                  end loop;
               end if;
            end;
         end if;
      end if;
   end Check_Programming_Languages;

   -------------------
   -- Check_Project --
   -------------------

   function Check_Project
     (P            : Project_Id;
      Root_Project : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Extending    : Boolean) return Boolean
   is
   begin
      if P = Root_Project then
         return True;

      elsif Extending then
         declare
            Data : Project_Data := In_Tree.Projects.Table (Root_Project);

         begin
            while Data.Extends /= No_Project loop
               if P = Data.Extends then
                  return True;
               end if;

               Data := In_Tree.Projects.Table (Data.Extends);
            end loop;
         end;
      end if;

      return False;
   end Check_Project;

   -------------------------------
   -- Check_Stand_Alone_Library --
   -------------------------------

   procedure Check_Stand_Alone_Library
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Data        : in out Project_Data;
      Current_Dir : String;
      Extending   : Boolean)
   is
      Lib_Interfaces      : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Interface,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Auto_Init       : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Auto_Init,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Src_Dir         : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Src_Dir,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Symbol_File     : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Symbol_File,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Symbol_Policy   : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Symbol_Policy,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Ref_Symbol_File : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Reference_Symbol_File,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Auto_Init_Supported : Boolean;
      OK                  : Boolean := True;
      Source              : Source_Id;
      Next_Proj           : Project_Id;

   begin
      if Get_Mode = Multi_Language then
         Auto_Init_Supported := Data.Config.Auto_Init_Supported;
      else
         Auto_Init_Supported :=
           MLib.Tgt.Standalone_Library_Auto_Init_Is_Supported;
      end if;

      pragma Assert (Lib_Interfaces.Kind = List);

      --  It is a stand-alone library project file if attribute
      --  Library_Interface is defined.

      if not Lib_Interfaces.Default then
         SAL_Library : declare
            Interfaces     : String_List_Id := Lib_Interfaces.Values;
            Interface_ALIs : String_List_Id := Nil_String;
            Unit           : Name_Id;
            The_Unit_Id    : Unit_Index;
            The_Unit_Data  : Unit_Data;

            procedure Add_ALI_For (Source : File_Name_Type);
            --  Add an ALI file name to the list of Interface ALIs

            -----------------
            -- Add_ALI_For --
            -----------------

            procedure Add_ALI_For (Source : File_Name_Type) is
            begin
               Get_Name_String (Source);

               declare
                  ALI         : constant String :=
                                  ALI_File_Name (Name_Buffer (1 .. Name_Len));
                  ALI_Name_Id : Name_Id;

               begin
                  Name_Len := ALI'Length;
                  Name_Buffer (1 .. Name_Len) := ALI;
                  ALI_Name_Id := Name_Find;

                  String_Element_Table.Increment_Last
                    (In_Tree.String_Elements);
                  In_Tree.String_Elements.Table
                    (String_Element_Table.Last
                      (In_Tree.String_Elements)) :=
                    (Value         => ALI_Name_Id,
                     Index         => 0,
                     Display_Value => ALI_Name_Id,
                     Location      =>
                       In_Tree.String_Elements.Table
                         (Interfaces).Location,
                     Flag          => False,
                     Next          => Interface_ALIs);
                  Interface_ALIs := String_Element_Table.Last
                                      (In_Tree.String_Elements);
               end;
            end Add_ALI_For;

         --  Start of processing for SAL_Library

         begin
            Data.Standalone_Library := True;

            --  Library_Interface cannot be an empty list

            if Interfaces = Nil_String then
               Error_Msg
                 (Project, In_Tree,
                  "Library_Interface cannot be an empty list",
                  Lib_Interfaces.Location);
            end if;

            --  Process each unit name specified in the attribute
            --  Library_Interface.

            while Interfaces /= Nil_String loop
               Get_Name_String
                 (In_Tree.String_Elements.Table (Interfaces).Value);
               To_Lower (Name_Buffer (1 .. Name_Len));

               if Name_Len = 0 then
                  Error_Msg
                    (Project, In_Tree,
                     "an interface cannot be an empty string",
                     In_Tree.String_Elements.Table (Interfaces).Location);

               else
                  Unit := Name_Find;
                  Error_Msg_Name_1 := Unit;

                  if Get_Mode = Ada_Only then
                     The_Unit_Id :=
                       Units_Htable.Get (In_Tree.Units_HT, Unit);

                     if The_Unit_Id = No_Unit_Index then
                        Error_Msg
                          (Project, In_Tree,
                           "unknown unit %%",
                           In_Tree.String_Elements.Table
                             (Interfaces).Location);

                     else
                        --  Check that the unit is part of the project

                        The_Unit_Data :=
                          In_Tree.Units.Table (The_Unit_Id);

                        if The_Unit_Data.File_Names (Body_Part).Name /= No_File
                          and then The_Unit_Data.File_Names
                                     (Body_Part).Path.Name /= Slash
                        then
                           if Check_Project
                             (The_Unit_Data.File_Names (Body_Part).Project,
                              Project, In_Tree, Extending)
                           then
                              --  There is a body for this unit.
                              --  If there is no spec, we need to check
                              --  that it is not a subunit.

                              if The_Unit_Data.File_Names
                                (Specification).Name = No_File
                              then
                                 declare
                                    Src_Ind : Source_File_Index;

                                 begin
                                    Src_Ind := Sinput.P.Load_Project_File
                                      (Get_Name_String
                                         (The_Unit_Data.File_Names
                                            (Body_Part).Path.Name));

                                    if Sinput.P.Source_File_Is_Subunit
                                      (Src_Ind)
                                    then
                                       Error_Msg
                                         (Project, In_Tree,
                                          "%% is a subunit; " &
                                          "it cannot be an interface",
                                          In_Tree.
                                            String_Elements.Table
                                              (Interfaces).Location);
                                    end if;
                                 end;
                              end if;

                              --  The unit is not a subunit, so we add
                              --  to the Interface ALIs the ALI file
                              --  corresponding to the body.

                              Add_ALI_For
                                (The_Unit_Data.File_Names (Body_Part).Name);

                           else
                              Error_Msg
                                (Project, In_Tree,
                                 "%% is not an unit of this project",
                                 In_Tree.String_Elements.Table
                                   (Interfaces).Location);
                           end if;

                        elsif The_Unit_Data.File_Names
                          (Specification).Name /= No_File
                          and then The_Unit_Data.File_Names
                            (Specification).Path.Name /= Slash
                          and then Check_Project
                            (The_Unit_Data.File_Names
                                 (Specification).Project,
                             Project, In_Tree, Extending)

                        then
                           --  The unit is part of the project, it has
                           --  a spec, but no body. We add to the Interface
                           --  ALIs the ALI file corresponding to the spec.

                           Add_ALI_For
                             (The_Unit_Data.File_Names (Specification).Name);

                        else
                           Error_Msg
                             (Project, In_Tree,
                              "%% is not an unit of this project",
                              In_Tree.String_Elements.Table
                                (Interfaces).Location);
                        end if;
                     end if;

                  else
                     --  Multi_Language mode

                     Next_Proj := Data.Extends;
                     Source := Data.First_Source;

                     loop
                        while Source /= No_Source and then
                              In_Tree.Sources.Table (Source).Unit /= Unit
                        loop
                           Source :=
                             In_Tree.Sources.Table (Source).Next_In_Project;
                        end loop;

                        exit when Source /= No_Source or else
                                  Next_Proj = No_Project;

                        Source :=
                          In_Tree.Projects.Table (Next_Proj).First_Source;
                        Next_Proj :=
                          In_Tree.Projects.Table (Next_Proj).Extends;
                     end loop;

                     if Source /= No_Source then
                        if In_Tree.Sources.Table (Source).Kind = Sep then
                           Source := No_Source;

                        elsif In_Tree.Sources.Table (Source).Kind = Spec
                          and then
                          In_Tree.Sources.Table (Source).Other_Part /=
                          No_Source
                        then
                           Source := In_Tree.Sources.Table (Source).Other_Part;
                        end if;
                     end if;

                     if Source /= No_Source then
                        if In_Tree.Sources.Table (Source).Project /= Project
                          and then
                            not Is_Extending
                              (Project,
                               In_Tree.Sources.Table (Source).Project,
                               In_Tree)
                        then
                           Source := No_Source;
                        end if;
                     end if;

                     if Source = No_Source then
                           Error_Msg
                             (Project, In_Tree,
                              "%% is not an unit of this project",
                              In_Tree.String_Elements.Table
                                (Interfaces).Location);

                     else
                        if In_Tree.Sources.Table (Source).Kind = Spec and then
                          In_Tree.Sources.Table (Source).Other_Part /=
                            No_Source
                        then
                           Source := In_Tree.Sources.Table (Source).Other_Part;
                        end if;

                        String_Element_Table.Increment_Last
                          (In_Tree.String_Elements);
                        In_Tree.String_Elements.Table
                          (String_Element_Table.Last
                             (In_Tree.String_Elements)) :=
                          (Value         =>
                             Name_Id (In_Tree.Sources.Table (Source).Dep_Name),
                           Index         => 0,
                           Display_Value =>
                             Name_Id (In_Tree.Sources.Table (Source).Dep_Name),
                           Location      =>
                             In_Tree.String_Elements.Table
                               (Interfaces).Location,
                           Flag          => False,
                           Next          => Interface_ALIs);
                        Interface_ALIs := String_Element_Table.Last
                          (In_Tree.String_Elements);
                     end if;

                  end if;

               end if;

               Interfaces :=
                 In_Tree.String_Elements.Table (Interfaces).Next;
            end loop;

            --  Put the list of Interface ALIs in the project data

            Data.Lib_Interface_ALIs := Interface_ALIs;

            --  Check value of attribute Library_Auto_Init and set
            --  Lib_Auto_Init accordingly.

            if Lib_Auto_Init.Default then

               --  If no attribute Library_Auto_Init is declared, then set auto
               --  init only if it is supported.

               Data.Lib_Auto_Init := Auto_Init_Supported;

            else
               Get_Name_String (Lib_Auto_Init.Value);
               To_Lower (Name_Buffer (1 .. Name_Len));

               if Name_Buffer (1 .. Name_Len) = "false" then
                  Data.Lib_Auto_Init := False;

               elsif Name_Buffer (1 .. Name_Len) = "true" then
                  if Auto_Init_Supported then
                     Data.Lib_Auto_Init := True;

                  else
                     --  Library_Auto_Init cannot be "true" if auto init is not
                     --  supported

                     Error_Msg
                       (Project, In_Tree,
                        "library auto init not supported " &
                        "on this platform",
                        Lib_Auto_Init.Location);
                  end if;

               else
                  Error_Msg
                    (Project, In_Tree,
                     "invalid value for attribute Library_Auto_Init",
                     Lib_Auto_Init.Location);
               end if;
            end if;
         end SAL_Library;

         --  If attribute Library_Src_Dir is defined and not the empty string,
         --  check if the directory exist and is not the object directory or
         --  one of the source directories. This is the directory where copies
         --  of the interface sources will be copied. Note that this directory
         --  may be the library directory.

         if Lib_Src_Dir.Value /= Empty_String then
            declare
               Dir_Id : constant File_Name_Type :=
                          File_Name_Type (Lib_Src_Dir.Value);

            begin
               Locate_Directory
                 (Project,
                  In_Tree,
                  Dir_Id,
                  Data.Directory.Display_Name,
                  Data.Library_Src_Dir.Name,
                  Data.Library_Src_Dir.Display_Name,
                  Create => "library source copy",
                  Current_Dir => Current_Dir,
                  Location => Lib_Src_Dir.Location);

               --  If directory does not exist, report an error

               if Data.Library_Src_Dir = No_Path_Information then

                  --  Get the absolute name of the library directory that does
                  --  not exist, to report an error.

                  declare
                     Dir_Name : constant String :=
                                  Get_Name_String (Dir_Id);

                  begin
                     if Is_Absolute_Path (Dir_Name) then
                        Err_Vars.Error_Msg_File_1 := Dir_Id;

                     else
                        Get_Name_String (Data.Directory.Name);

                        if Name_Buffer (Name_Len) /=
                          Directory_Separator
                        then
                           Name_Len := Name_Len + 1;
                           Name_Buffer (Name_Len) :=
                             Directory_Separator;
                        end if;

                        Name_Buffer
                          (Name_Len + 1 ..
                             Name_Len + Dir_Name'Length) :=
                            Dir_Name;
                        Name_Len := Name_Len + Dir_Name'Length;
                        Err_Vars.Error_Msg_Name_1 := Name_Find;
                     end if;

                     --  Report the error

                     Error_Msg_File_1 := Dir_Id;
                     Error_Msg
                       (Project, In_Tree,
                        "Directory { does not exist",
                        Lib_Src_Dir.Location);
                  end;

                  --  Report error if it is the same as the object directory

               elsif Data.Library_Src_Dir = Data.Object_Directory then
                  Error_Msg
                    (Project, In_Tree,
                     "directory to copy interfaces cannot be " &
                     "the object directory",
                     Lib_Src_Dir.Location);
                  Data.Library_Src_Dir := No_Path_Information;

               else
                  declare
                     Src_Dirs : String_List_Id;
                     Src_Dir  : String_Element;

                  begin
                     --  Interface copy directory cannot be one of the source
                     --  directory of the current project.

                     Src_Dirs := Data.Source_Dirs;
                     while Src_Dirs /= Nil_String loop
                        Src_Dir := In_Tree.String_Elements.Table (Src_Dirs);

                        --  Report error if it is one of the source directories

                        if Data.Library_Src_Dir.Name =
                          Path_Name_Type (Src_Dir.Value)
                        then
                           Error_Msg
                             (Project, In_Tree,
                              "directory to copy interfaces cannot " &
                              "be one of the source directories",
                              Lib_Src_Dir.Location);
                           Data.Library_Src_Dir := No_Path_Information;
                           exit;
                        end if;

                        Src_Dirs := Src_Dir.Next;
                     end loop;

                     if Data.Library_Src_Dir /= No_Path_Information then

                        --  It cannot be a source directory of any other
                        --  project either.

                        Project_Loop : for Pid in 1 ..
                          Project_Table.Last (In_Tree.Projects)
                        loop
                           Src_Dirs :=
                             In_Tree.Projects.Table (Pid).Source_Dirs;
                           Dir_Loop : while Src_Dirs /= Nil_String loop
                              Src_Dir :=
                                In_Tree.String_Elements.Table (Src_Dirs);

                              --  Report error if it is one of the source
                              --  directories

                              if Data.Library_Src_Dir.Name =
                                Path_Name_Type (Src_Dir.Value)
                              then
                                 Error_Msg_File_1 :=
                                   File_Name_Type (Src_Dir.Value);
                                 Error_Msg_Name_1 :=
                                   In_Tree.Projects.Table (Pid).Name;
                                 Error_Msg
                                   (Project, In_Tree,
                                    "directory to copy interfaces cannot " &
                                    "be the same as source directory { of " &
                                    "project %%",
                                    Lib_Src_Dir.Location);
                                 Data.Library_Src_Dir := No_Path_Information;
                                 exit Project_Loop;
                              end if;

                              Src_Dirs := Src_Dir.Next;
                           end loop Dir_Loop;
                        end loop Project_Loop;
                     end if;
                  end;

                  --  In high verbosity, if there is a valid Library_Src_Dir,
                  --  display its path name.

                  if Data.Library_Src_Dir /= No_Path_Information
                    and then Current_Verbosity = High
                  then
                     Write_Str ("Directory to copy interfaces =""");
                     Write_Str (Get_Name_String (Data.Library_Src_Dir.Name));
                     Write_Line ("""");
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
                  Data.Symbol_Data.Symbol_Policy := Autonomous;

               elsif Value = "compliant" then
                  Data.Symbol_Data.Symbol_Policy := Compliant;

               elsif Value = "controlled" then
                  Data.Symbol_Data.Symbol_Policy := Controlled;

               elsif Value = "restricted" then
                  Data.Symbol_Data.Symbol_Policy := Restricted;

               elsif Value = "direct" then
                  Data.Symbol_Data.Symbol_Policy := Direct;

               else
                  Error_Msg
                    (Project, In_Tree,
                     "illegal value for Library_Symbol_Policy",
                     Lib_Symbol_Policy.Location);
               end if;
            end;
         end if;

         --  If attribute Library_Symbol_File is not specified, symbol policy
         --  cannot be Restricted.

         if Lib_Symbol_File.Default then
            if Data.Symbol_Data.Symbol_Policy = Restricted then
               Error_Msg
                 (Project, In_Tree,
                  "Library_Symbol_File needs to be defined when " &
                  "symbol policy is Restricted",
                  Lib_Symbol_Policy.Location);
            end if;

         else
            --  Library_Symbol_File is defined

            Data.Symbol_Data.Symbol_File :=
              Path_Name_Type (Lib_Symbol_File.Value);

            Get_Name_String (Lib_Symbol_File.Value);

            if Name_Len = 0 then
               Error_Msg
                 (Project, In_Tree,
                  "symbol file name cannot be an empty string",
                  Lib_Symbol_File.Location);

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
                    (Project, In_Tree,
                     "symbol file name { is illegal. " &
                     "Name cannot include directory info.",
                     Lib_Symbol_File.Location);
               end if;
            end if;
         end if;

         --  If attribute Library_Reference_Symbol_File is not defined,
         --  symbol policy cannot be Compliant or Controlled.

         if Lib_Ref_Symbol_File.Default then
            if Data.Symbol_Data.Symbol_Policy = Compliant
              or else Data.Symbol_Data.Symbol_Policy = Controlled
            then
               Error_Msg
                 (Project, In_Tree,
                  "a reference symbol file need to be defined",
                  Lib_Symbol_Policy.Location);
            end if;

         else
            --  Library_Reference_Symbol_File is defined, check file exists

            Data.Symbol_Data.Reference :=
              Path_Name_Type (Lib_Ref_Symbol_File.Value);

            Get_Name_String (Lib_Ref_Symbol_File.Value);

            if Name_Len = 0 then
               Error_Msg
                 (Project, In_Tree,
                  "reference symbol file name cannot be an empty string",
                  Lib_Symbol_File.Location);

            else
               if not Is_Absolute_Path (Name_Buffer (1 .. Name_Len)) then
                  Name_Len := 0;
                  Add_Str_To_Name_Buffer
                    (Get_Name_String (Data.Directory.Name));
                  Add_Char_To_Name_Buffer (Directory_Separator);
                  Add_Str_To_Name_Buffer
                    (Get_Name_String (Lib_Ref_Symbol_File.Value));
                  Data.Symbol_Data.Reference := Name_Find;
               end if;

               if not Is_Regular_File
                 (Get_Name_String (Data.Symbol_Data.Reference))
               then
                  Error_Msg_File_1 :=
                    File_Name_Type (Lib_Ref_Symbol_File.Value);

                  --  For controlled and direct symbol policies, it is an error
                  --  if the reference symbol file does not exist. For other
                  --  symbol policies, this is just a warning

                  Error_Msg_Warn :=
                    Data.Symbol_Data.Symbol_Policy /= Controlled
                    and then Data.Symbol_Data.Symbol_Policy /= Direct;

                  Error_Msg
                    (Project, In_Tree,
                     "<library reference symbol file { does not exist",
                     Lib_Ref_Symbol_File.Location);

                  --  In addition in the non-controlled case, if symbol policy
                  --  is Compliant, it is changed to Autonomous, because there
                  --  is no reference to check against, and we don't want to
                  --  fail in this case.

                  if Data.Symbol_Data.Symbol_Policy /= Controlled then
                     if Data.Symbol_Data.Symbol_Policy = Compliant then
                        Data.Symbol_Data.Symbol_Policy := Autonomous;
                     end if;
                  end if;
               end if;

               --  If both the reference symbol file and the symbol file are
               --  defined, then check that they are not the same file.

               if Data.Symbol_Data.Symbol_File /= No_Path then
                  Get_Name_String (Data.Symbol_Data.Symbol_File);

                  if Name_Len > 0 then
                     declare
                        Symb_Path : constant String :=
                                      Normalize_Pathname
                                        (Get_Name_String
                                           (Data.Object_Directory.Name) &
                                         Directory_Separator &
                                         Name_Buffer (1 .. Name_Len),
                                         Directory     => Current_Dir,
                                         Resolve_Links =>
                                           Opt.Follow_Links_For_Files);
                        Ref_Path  : constant String :=
                                      Normalize_Pathname
                                        (Get_Name_String
                                           (Data.Symbol_Data.Reference),
                                         Directory     => Current_Dir,
                                         Resolve_Links =>
                                           Opt.Follow_Links_For_Files);
                     begin
                        if Symb_Path = Ref_Path then
                           Error_Msg
                             (Project, In_Tree,
                              "library reference symbol file and library" &
                              " symbol file cannot be the same file",
                              Lib_Ref_Symbol_File.Location);
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
                  or else Dir (Dir'Last - 1) = '/')
      then
         return Dir'Last - 1;
      else
         return Dir'Last;
      end if;
   end Compute_Directory_Last;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg
     (Project       : Project_Id;
      In_Tree       : Project_Tree_Ref;
      Msg           : String;
      Flag_Location : Source_Ptr)
   is
      Real_Location : Source_Ptr := Flag_Location;
      Error_Buffer  : String (1 .. 5_000);
      Error_Last    : Natural := 0;
      Name_Number   : Natural := 0;
      File_Number   : Natural := 0;
      First         : Positive := Msg'First;
      Index         : Positive;

      procedure Add (C : Character);
      --  Add a character to the buffer

      procedure Add (S : String);
      --  Add a string to the buffer

      procedure Add_Name;
      --  Add a name to the buffer

      procedure Add_File;
      --  Add a file name to the buffer

      ---------
      -- Add --
      ---------

      procedure Add (C : Character) is
      begin
         Error_Last := Error_Last + 1;
         Error_Buffer (Error_Last) := C;
      end Add;

      procedure Add (S : String) is
      begin
         Error_Buffer (Error_Last + 1 .. Error_Last + S'Length) := S;
         Error_Last := Error_Last + S'Length;
      end Add;

      --------------
      -- Add_File --
      --------------

      procedure Add_File is
         File : File_Name_Type;

      begin
         Add ('"');
         File_Number := File_Number + 1;

         case File_Number is
            when 1 =>
               File := Err_Vars.Error_Msg_File_1;
            when 2 =>
               File := Err_Vars.Error_Msg_File_2;
            when 3 =>
               File := Err_Vars.Error_Msg_File_3;
            when others =>
               null;
         end case;

         Get_Name_String (File);
         Add (Name_Buffer (1 .. Name_Len));
         Add ('"');
      end Add_File;

      --------------
      -- Add_Name --
      --------------

      procedure Add_Name is
         Name : Name_Id;

      begin
         Add ('"');
         Name_Number := Name_Number + 1;

         case Name_Number is
            when 1 =>
               Name := Err_Vars.Error_Msg_Name_1;
            when 2 =>
               Name := Err_Vars.Error_Msg_Name_2;
            when 3 =>
               Name := Err_Vars.Error_Msg_Name_3;
            when others =>
               null;
         end case;

         Get_Name_String (Name);
         Add (Name_Buffer (1 .. Name_Len));
         Add ('"');
      end Add_Name;

   --  Start of processing for Error_Msg

   begin
      --  If location of error is unknown, use the location of the project

      if Real_Location = No_Location then
         Real_Location := In_Tree.Projects.Table (Project).Location;
      end if;

      if Error_Report = null then
         Prj.Err.Error_Msg (Msg, Real_Location);
         return;
      end if;

      --  Ignore continuation character

      if Msg (First) = '\' then
         First := First + 1;
      end if;

      --  Warning character is always the first one in this package
      --  this is an undocumented kludge???

      if Msg (First) = '?' then
         First := First + 1;
         Add ("Warning: ");

      elsif Msg (First) = '<' then
         First := First + 1;

         if Err_Vars.Error_Msg_Warn then
            Add ("Warning: ");
         end if;
      end if;

      Index := First;
      while Index <= Msg'Last loop
         if Msg (Index) = '{' then
            Add_File;

         elsif Msg (Index) = '%' then
            if Index < Msg'Last and then Msg (Index + 1) = '%' then
               Index := Index + 1;
            end if;

            Add_Name;
         else
            Add (Msg (Index));
         end if;
         Index := Index + 1;

      end loop;

      Error_Report (Error_Buffer (1 .. Error_Last), Project, In_Tree);
   end Error_Msg;

   ----------------------
   -- Find_Ada_Sources --
   ----------------------

   procedure Find_Ada_Sources
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Data         : in out Project_Data;
      Current_Dir  : String)
   is
      Source_Dir      : String_List_Id := Data.Source_Dirs;
      Element         : String_Element;
      Dir             : Dir_Type;
      Current_Source  : String_List_Id := Nil_String;
      Source_Recorded : Boolean := False;

   begin
      if Current_Verbosity = High then
         Write_Line ("Looking for sources:");
      end if;

      --  For each subdirectory

      while Source_Dir /= Nil_String loop
         begin
            Source_Recorded := False;
            Element := In_Tree.String_Elements.Table (Source_Dir);
            if Element.Value /= No_Name then
               Get_Name_String (Element.Display_Value);

               declare
                  Source_Directory : constant String :=
                    Name_Buffer (1 .. Name_Len) & Directory_Separator;
                  Dir_Last  : constant Natural :=
                     Compute_Directory_Last (Source_Directory);

               begin
                  if Current_Verbosity = High then
                     Write_Str ("Source_Dir = ");
                     Write_Line (Source_Directory);
                  end if;

                  --  We look at every entry in the source directory

                  Open (Dir,
                        Source_Directory (Source_Directory'First .. Dir_Last));

                  loop
                     Read (Dir, Name_Buffer, Name_Len);

                     if Current_Verbosity = High then
                        Write_Str  ("   Checking ");
                        Write_Line (Name_Buffer (1 .. Name_Len));
                     end if;

                     exit when Name_Len = 0;

                     declare
                        File_Name : constant File_Name_Type := Name_Find;

                        --  ??? We could probably optimize the following call:
                        --  we need to resolve links only once for the
                        --  directory itself, and then do a single call to
                        --  readlink() for each file. Unfortunately that would
                        --  require a change in Normalize_Pathname so that it
                        --  has the option of not resolving links for its
                        --  Directory parameter, only for Name.

                        Path : constant String :=
                                 Normalize_Pathname
                                   (Name      => Name_Buffer (1 .. Name_Len),
                                    Directory =>
                                      Source_Directory
                                        (Source_Directory'First .. Dir_Last),
                                    Resolve_Links =>
                                      Opt.Follow_Links_For_Files,
                                    Case_Sensitive => True);

                        Path_Name : Path_Name_Type;

                     begin
                        Name_Len := Path'Length;
                        Name_Buffer (1 .. Name_Len) := Path;
                        Path_Name := Name_Find;

                        --  We attempt to register it as a source. However,
                        --  there is no error if the file does not contain a
                        --  valid source. But there is an error if we have a
                        --  duplicate unit name.

                        Record_Ada_Source
                          (File_Name       => File_Name,
                           Path_Name       => Path_Name,
                           Project         => Project,
                           In_Tree         => In_Tree,
                           Data            => Data,
                           Location        => No_Location,
                           Current_Source  => Current_Source,
                           Source_Recorded => Source_Recorded,
                           Current_Dir     => Current_Dir);
                     end;
                  end loop;

                  Close (Dir);
               end;
            end if;

         exception
            when Directory_Error =>
               null;
         end;

         if Source_Recorded then
            In_Tree.String_Elements.Table (Source_Dir).Flag :=
              True;
         end if;

         Source_Dir := Element.Next;
      end loop;

      if Current_Verbosity = High then
         Write_Line ("end Looking for sources.");
      end if;

   end Find_Ada_Sources;

   --------------------------------
   -- Free_Ada_Naming_Exceptions --
   --------------------------------

   procedure Free_Ada_Naming_Exceptions is
   begin
      Ada_Naming_Exception_Table.Set_Last (0);
      Ada_Naming_Exceptions.Reset;
      Reverse_Ada_Naming_Exceptions.Reset;
   end Free_Ada_Naming_Exceptions;

   ---------------------
   -- Get_Directories --
   ---------------------

   procedure Get_Directories
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Current_Dir : String;
      Data        : in out Project_Data)
   is
      Object_Dir  : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Object_Dir, Data.Decl.Attributes, In_Tree);

      Exec_Dir : constant Variable_Value :=
                   Util.Value_Of
                     (Name_Exec_Dir, Data.Decl.Attributes, In_Tree);

      Source_Dirs : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Source_Dirs, Data.Decl.Attributes, In_Tree);

      Excluded_Source_Dirs : constant Variable_Value :=
                              Util.Value_Of
                                (Name_Excluded_Source_Dirs,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Source_Files : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Source_Files, Data.Decl.Attributes, In_Tree);

      Last_Source_Dir : String_List_Id  := Nil_String;

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
                            Directory     => Current_Dir,
                            Resolve_Links => Opt.Follow_Links_For_Dirs) &
                         Directory_Separator;

            The_Path_Last : constant Natural :=
                              Compute_Directory_Last (The_Path);

         begin
            Name_Len := The_Path_Last - The_Path'First + 1;
            Name_Buffer (1 .. Name_Len) :=
              The_Path (The_Path'First .. The_Path_Last);
            Non_Canonical_Path := Name_Find;

            if Osint.File_Names_Case_Sensitive then
               Canonical_Path := Non_Canonical_Path;
            else
               Get_Name_String (Non_Canonical_Path);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Canonical_Path := Name_Find;
            end if;

            --  To avoid processing the same directory several times, check
            --  if the directory is already in Recursive_Dirs. If it is, then
            --  there is nothing to do, just return. If it is not, put it there
            --  and continue recursive processing.

            if not Removed then
               if Recursive_Dirs.Get (Canonical_Path) then
                  return;
               else
                  Recursive_Dirs.Set (Canonical_Path, True);
               end if;
            end if;

            --  Check if directory is already in list

            List := Data.Source_Dirs;
            Prev := Nil_String;
            while List /= Nil_String loop
               Element := In_Tree.String_Elements.Table (List);

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

               String_Element_Table.Increment_Last
                 (In_Tree.String_Elements);
               Element :=
                 (Value         => Canonical_Path,
                  Display_Value => Non_Canonical_Path,
                  Location      => No_Location,
                  Flag          => False,
                  Next          => Nil_String,
                  Index         => 0);

               --  Case of first source directory

               if Last_Source_Dir = Nil_String then
                  Data.Source_Dirs := String_Element_Table.Last
                                        (In_Tree.String_Elements);

                  --  Here we already have source directories

               else
                  --  Link the previous last to the new one

                  In_Tree.String_Elements.Table
                    (Last_Source_Dir).Next :=
                      String_Element_Table.Last
                        (In_Tree.String_Elements);
               end if;

               --  And register this source directory as the new last

               Last_Source_Dir  := String_Element_Table.Last
                 (In_Tree.String_Elements);
               In_Tree.String_Elements.Table (Last_Source_Dir) :=
                 Element;

            elsif Removed and Found then
               if Prev = Nil_String then
                  Data.Source_Dirs :=
                    In_Tree.String_Elements.Table (List).Next;
               else
                  In_Tree.String_Elements.Table (Prev).Next :=
                    In_Tree.String_Elements.Table (List).Next;
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
               Data.Known_Order_Of_Source_Dirs := False;
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
                                 Get_Name_String (Data.Directory.Display_Name),
                               Resolve_Links  => False,
                               Case_Sensitive => True);

            begin
               if Root_Dir'Length = 0 then
                  Err_Vars.Error_Msg_File_1 := Base_Dir;

                  if Location = No_Location then
                     Error_Msg
                       (Project, In_Tree,
                        "{ is not a valid directory.",
                        Data.Location);
                  else
                     Error_Msg
                       (Project, In_Tree,
                        "{ is not a valid directory.",
                        Location);
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
               Path_Name         : Path_Name_Type;
               Display_Path_Name : Path_Name_Type;
               List              : String_List_Id;
               Prev              : String_List_Id;

            begin
               Locate_Directory
                 (Project     => Project,
                  In_Tree     => In_Tree,
                  Name        => From,
                  Parent      => Data.Directory.Display_Name,
                  Dir         => Path_Name,
                  Display     => Display_Path_Name,
                  Current_Dir => Current_Dir);

               if Path_Name = No_Path then
                  Err_Vars.Error_Msg_File_1 := From;

                  if Location = No_Location then
                     Error_Msg
                       (Project, In_Tree,
                        "{ is not a valid directory",
                        Data.Location);
                  else
                     Error_Msg
                       (Project, In_Tree,
                        "{ is not a valid directory",
                        Location);
                  end if;

               else
                  declare
                     Path              : constant String :=
                                           Get_Name_String (Path_Name) &
                                           Directory_Separator;
                     Last_Path         : constant Natural :=
                                           Compute_Directory_Last (Path);
                     Path_Id           : Name_Id;
                     Display_Path      : constant String :=
                                           Get_Name_String
                                             (Display_Path_Name) &
                                           Directory_Separator;
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
                          (In_Tree.String_Elements);
                        Element :=
                          (Value         => Path_Id,
                           Index         => 0,
                           Display_Value => Display_Path_Id,
                           Location      => No_Location,
                           Flag          => False,
                           Next          => Nil_String);

                        if Last_Source_Dir = Nil_String then

                           --  This is the first source directory

                           Data.Source_Dirs := String_Element_Table.Last
                             (In_Tree.String_Elements);

                        else
                           --  We already have source directories, link the
                           --  previous last to the new one.

                           In_Tree.String_Elements.Table
                             (Last_Source_Dir).Next :=
                             String_Element_Table.Last
                               (In_Tree.String_Elements);
                        end if;

                        --  And register this source directory as the new last

                        Last_Source_Dir := String_Element_Table.Last
                          (In_Tree.String_Elements);
                        In_Tree.String_Elements.Table
                          (Last_Source_Dir) := Element;

                     else
                        --  Remove source dir, if present

                        List := Data.Source_Dirs;
                        Prev := Nil_String;

                        --  Look for source dir in current list

                        while List /= Nil_String loop
                           Element := In_Tree.String_Elements.Table (List);
                           exit when Element.Value = Path_Id;
                           Prev := List;
                           List := Element.Next;
                        end loop;

                        if List /= Nil_String then
                           --  Source dir was found, remove it from the list

                           if Prev = Nil_String then
                              Data.Source_Dirs :=
                                In_Tree.String_Elements.Table (List).Next;

                           else
                              In_Tree.String_Elements.Table (Prev).Next :=
                                In_Tree.String_Elements.Table (List).Next;
                           end if;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;
      end Find_Source_Dirs;

   --  Start of processing for Get_Directories

   begin
      if Current_Verbosity = High then
         Write_Line ("Starting to look for directories");
      end if;

      --  Check the object directory

      pragma Assert (Object_Dir.Kind = Single,
                     "Object_Dir is not a single string");

      --  We set the object directory to its default

      Data.Object_Directory := Data.Directory;

      if Object_Dir.Value /= Empty_String then
         Get_Name_String (Object_Dir.Value);

         if Name_Len = 0 then
            Error_Msg
              (Project, In_Tree,
               "Object_Dir cannot be empty",
               Object_Dir.Location);

         else
            --  We check that the specified object directory does exist

            Locate_Directory
              (Project,
               In_Tree,
               File_Name_Type (Object_Dir.Value),
               Data.Directory.Display_Name,
               Data.Object_Directory.Name,
               Data.Object_Directory.Display_Name,
               Create   => "object",
               Location => Object_Dir.Location,
               Current_Dir => Current_Dir);

            if Data.Object_Directory = No_Path_Information then

               --  The object directory does not exist, report an error if the
               --  project is not externally built.

               if not Data.Externally_Built then
                  Err_Vars.Error_Msg_File_1 :=
                    File_Name_Type (Object_Dir.Value);
                  Error_Msg
                    (Project, In_Tree,
                     "the object directory { cannot be found",
                     Data.Location);
               end if;

               --  Do not keep a nil Object_Directory. Set it to the specified
               --  (relative or absolute) path. This is for the benefit of
               --  tools that recover from errors; for example, these tools
               --  could create the non existent directory.

               Data.Object_Directory.Display_Name :=
                 Path_Name_Type (Object_Dir.Value);

               if Osint.File_Names_Case_Sensitive then
                  Data.Object_Directory.Name :=
                    Path_Name_Type (Object_Dir.Value);
               else
                  Get_Name_String (Object_Dir.Value);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                  Data.Object_Directory.Name := Name_Find;
               end if;
            end if;
         end if;

      elsif Subdirs /= null then
         Name_Len := 1;
         Name_Buffer (1) := '.';
         Locate_Directory
           (Project,
            In_Tree,
            Name_Find,
            Data.Directory.Display_Name,
            Data.Object_Directory.Name,
            Data.Object_Directory.Display_Name,
            Create      => "object",
            Location    => Object_Dir.Location,
            Current_Dir => Current_Dir);
      end if;

      if Current_Verbosity = High then
         if Data.Object_Directory = No_Path_Information then
            Write_Line ("No object directory");
         else
            Write_Str ("Object directory: """);
            Write_Str (Get_Name_String (Data.Object_Directory.Display_Name));
            Write_Line ("""");
         end if;
      end if;

      --  Check the exec directory

      pragma Assert (Exec_Dir.Kind = Single,
                     "Exec_Dir is not a single string");

      --  We set the object directory to its default

      Data.Exec_Directory   := Data.Object_Directory;

      if Exec_Dir.Value /= Empty_String then
         Get_Name_String (Exec_Dir.Value);

         if Name_Len = 0 then
            Error_Msg
              (Project, In_Tree,
               "Exec_Dir cannot be empty",
               Exec_Dir.Location);

         else
            --  We check that the specified exec directory does exist

            Locate_Directory
              (Project,
               In_Tree,
               File_Name_Type (Exec_Dir.Value),
               Data.Directory.Display_Name,
               Data.Exec_Directory.Name,
               Data.Exec_Directory.Display_Name,
               Create   => "exec",
               Location => Exec_Dir.Location,
               Current_Dir => Current_Dir);

            if Data.Exec_Directory = No_Path_Information then
               Err_Vars.Error_Msg_File_1 := File_Name_Type (Exec_Dir.Value);
               Error_Msg
                 (Project, In_Tree,
                  "the exec directory { cannot be found",
                  Data.Location);
            end if;
         end if;
      end if;

      if Current_Verbosity = High then
         if Data.Exec_Directory = No_Path_Information then
            Write_Line ("No exec directory");
         else
            Write_Str ("Exec directory: """);
            Write_Str (Get_Name_String (Data.Exec_Directory.Display_Name));
            Write_Line ("""");
         end if;
      end if;

      --  Look for the source directories

      if Current_Verbosity = High then
         Write_Line ("Starting to look for source directories");
      end if;

      pragma Assert (Source_Dirs.Kind = List, "Source_Dirs is not a list");

      if (not Source_Files.Default) and then
        Source_Files.Values = Nil_String
      then
         Data.Source_Dirs := Nil_String;

         if Data.Qualifier = Standard then
            Error_Msg
              (Project,
               In_Tree,
               "a standard project cannot have no sources",
               Source_Files.Location);
         end if;

         if Data.Extends = No_Project
           and then Data.Object_Directory = Data.Directory
         then
            Data.Object_Directory := No_Path_Information;
         end if;

      elsif Source_Dirs.Default then

         --  No Source_Dirs specified: the single source directory is the one
         --  containing the project file

         String_Element_Table.Increment_Last
           (In_Tree.String_Elements);
         Data.Source_Dirs := String_Element_Table.Last
           (In_Tree.String_Elements);
         In_Tree.String_Elements.Table (Data.Source_Dirs) :=
           (Value         => Name_Id (Data.Directory.Name),
            Display_Value => Name_Id (Data.Directory.Display_Name),
            Location      => No_Location,
            Flag          => False,
            Next          => Nil_String,
            Index         => 0);

         if Current_Verbosity = High then
            Write_Line ("Single source directory:");
            Write_Str ("    """);
            Write_Str (Get_Name_String (Data.Directory.Display_Name));
            Write_Line ("""");
         end if;

      elsif Source_Dirs.Values = Nil_String then
         if Data.Qualifier = Standard then
            Error_Msg
              (Project,
               In_Tree,
               "a standard project cannot have no source directories",
               Source_Dirs.Location);
         end if;

         --  If Source_Dirs is an empty string list, this means that this
         --  project contains no source. For projects that don't extend other
         --  projects, this also means that there is no need for an object
         --  directory, if not specified.

         if Data.Extends = No_Project
           and then  Data.Object_Directory = Data.Directory
         then
            Data.Object_Directory := No_Path_Information;
         end if;

         Data.Source_Dirs := Nil_String;

      else
         declare
            Source_Dir : String_List_Id;
            Element    : String_Element;

         begin
            --  Process the source directories for each element of the list

            Source_Dir := Source_Dirs.Values;
            while Source_Dir /= Nil_String loop
               Element := In_Tree.String_Elements.Table (Source_Dir);
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
               Element := In_Tree.String_Elements.Table (Source_Dir);
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
         Current : String_List_Id := Data.Source_Dirs;
         Element : String_Element;

      begin
         while Current /= Nil_String loop
            Element := In_Tree.String_Elements.Table (Current);
            if Element.Value /= No_Name then
               if not Osint.File_Names_Case_Sensitive then
                  Get_Name_String (Element.Value);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                  Element.Value := Name_Find;
               end if;

               In_Tree.String_Elements.Table (Current) := Element;
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
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Mains : constant Variable_Value :=
                Prj.Util.Value_Of (Name_Main, Data.Decl.Attributes, In_Tree);

   begin
      Data.Mains := Mains.Values;

      --  If no Mains were specified, and if we are an extending project,
      --  inherit the Mains from the project we are extending.

      if Mains.Default then
         if not Data.Library and then Data.Extends /= No_Project then
            Data.Mains :=
              In_Tree.Projects.Table (Data.Extends).Mains;
         end if;

      --  In a library project file, Main cannot be specified

      elsif Data.Library then
         Error_Msg
           (Project, In_Tree,
            "a library project file cannot have Main specified",
            Mains.Location);
      end if;
   end Get_Mains;

   ---------------------------
   -- Get_Sources_From_File --
   ---------------------------

   procedure Get_Sources_From_File
     (Path     : String;
      Location : Source_Ptr;
      Project  : Project_Id;
      In_Tree  : Project_Tree_Ref)
   is
      File        : Prj.Util.Text_File;
      Line        : String (1 .. 250);
      Last        : Natural;
      Source_Name : File_Name_Type;
      Name_Loc    : Name_Location;

   begin
      if Get_Mode = Ada_Only then
         Source_Names.Reset;
      end if;

      if Current_Verbosity = High then
         Write_Str  ("Opening """);
         Write_Str  (Path);
         Write_Line (""".");
      end if;

      --  Open the file

      Prj.Util.Open (File, Path);

      if not Prj.Util.Is_Valid (File) then
         Error_Msg (Project, In_Tree, "file does not exist", Location);

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
                       (Project,
                        In_Tree,
                        "file name cannot include directory information ({)",
                        Location);
                     exit;
                  end if;
               end loop;

               Name_Loc := Source_Names.Get (Source_Name);

               if Name_Loc = No_Name_Location then
                  Name_Loc :=
                    (Name     => Source_Name,
                     Location => Location,
                     Source   => No_Source,
                     Except   => False,
                     Found    => False);
               end if;

               Source_Names.Set (Source_Name, Name_Loc);
            end if;
         end loop;

         Prj.Util.Close (File);

      end if;
   end Get_Sources_From_File;

   --------------
   -- Get_Unit --
   --------------

   procedure Get_Unit
     (In_Tree             : Project_Tree_Ref;
      Canonical_File_Name : File_Name_Type;
      Naming              : Naming_Data;
      Exception_Id        : out Ada_Naming_Exception_Id;
      Unit_Name           : out Name_Id;
      Unit_Kind           : out Spec_Or_Body;
      Needs_Pragma        : out Boolean)
   is
      Info_Id  : Ada_Naming_Exception_Id :=
                   Ada_Naming_Exceptions.Get (Canonical_File_Name);
      VMS_Name : File_Name_Type;

   begin
      if Info_Id = No_Ada_Naming_Exception then
         if Hostparm.OpenVMS then
            VMS_Name := Canonical_File_Name;
            Get_Name_String (VMS_Name);

            if Name_Buffer (Name_Len) = '.' then
               Name_Len := Name_Len - 1;
               VMS_Name := Name_Find;
            end if;

            Info_Id := Ada_Naming_Exceptions.Get (VMS_Name);
         end if;

      end if;

      if Info_Id /= No_Ada_Naming_Exception then
         Exception_Id := Info_Id;
         Unit_Name := No_Name;
         Unit_Kind := Specification;
         Needs_Pragma := True;
         return;
      end if;

      Needs_Pragma := False;
      Exception_Id := No_Ada_Naming_Exception;

      Get_Name_String (Canonical_File_Name);

      --  How about some comments and a name for this declare block ???
      --  In fact the whole code below needs more comments ???

      declare
         File          : String := Name_Buffer (1 .. Name_Len);
         First         : constant Positive := File'First;
         Last          : Natural           := File'Last;
         Standard_GNAT : Boolean;
         Spec          : constant File_Name_Type :=
                           Spec_Suffix_Id_Of (In_Tree, Name_Ada, Naming);
         Body_Suff     : constant File_Name_Type :=
                           Body_Suffix_Id_Of (In_Tree, Name_Ada, Naming);

      begin
         Standard_GNAT := Spec = Default_Ada_Spec_Suffix
            and then Body_Suff = Default_Ada_Body_Suffix;

         declare
            Spec_Suffix : constant String := Get_Name_String (Spec);
            Body_Suffix : constant String := Get_Name_String (Body_Suff);
            Sep_Suffix  : constant String :=
                            Get_Name_String (Naming.Separate_Suffix);

            May_Be_Spec : Boolean;
            May_Be_Body : Boolean;
            May_Be_Sep  : Boolean;

         begin
            May_Be_Spec :=
              File'Length > Spec_Suffix'Length
              and then
              File (Last - Spec_Suffix'Length + 1 .. Last) = Spec_Suffix;

            May_Be_Body :=
              File'Length > Body_Suffix'Length
              and then
              File (Last - Body_Suffix'Length + 1 .. Last) = Body_Suffix;

            May_Be_Sep :=
              File'Length > Sep_Suffix'Length
              and then
              File (Last - Sep_Suffix'Length + 1 .. Last) = Sep_Suffix;

            --  If two May_Be_ booleans are True, always choose the longer one

            if May_Be_Spec then
               if May_Be_Body and then
                 Spec_Suffix'Length < Body_Suffix'Length
               then
                  Unit_Kind := Body_Part;

                  if May_Be_Sep and then
                    Body_Suffix'Length < Sep_Suffix'Length
                  then
                     Last := Last - Sep_Suffix'Length;
                     May_Be_Body := False;

                  else
                     Last := Last - Body_Suffix'Length;
                     May_Be_Sep := False;
                  end if;

               elsif May_Be_Sep and then
                     Spec_Suffix'Length < Sep_Suffix'Length
               then
                  Unit_Kind := Body_Part;
                  Last := Last - Sep_Suffix'Length;

               else
                  Unit_Kind := Specification;
                  Last := Last - Spec_Suffix'Length;
               end if;

            elsif May_Be_Body then
               Unit_Kind := Body_Part;

               if May_Be_Sep and then
                  Body_Suffix'Length < Sep_Suffix'Length
               then
                  Last := Last - Sep_Suffix'Length;
                  May_Be_Body := False;
               else
                  Last := Last - Body_Suffix'Length;
                  May_Be_Sep := False;
               end if;

            elsif May_Be_Sep then
               Unit_Kind := Body_Part;
               Last := Last - Sep_Suffix'Length;

            else
               Last := 0;
            end if;

            if Last = 0 then

               --  This is not a source file

               Unit_Name := No_Name;
               Unit_Kind := Specification;

               if Current_Verbosity = High then
                  Write_Line ("   Not a valid file name.");
               end if;

               return;

            elsif Current_Verbosity = High then
               case Unit_Kind is
               when Specification =>
                  Write_Str  ("   Specification: ");
                  Write_Line (File (First .. Last + Spec_Suffix'Length));

               when Body_Part =>
                  if May_Be_Body then
                     Write_Str  ("   Body: ");
                     Write_Line (File (First .. Last + Body_Suffix'Length));

                  else
                     Write_Str  ("   Separate: ");
                     Write_Line (File (First .. Last + Sep_Suffix'Length));
                  end if;
               end case;
            end if;
         end;

         Get_Name_String (Naming.Dot_Replacement);
         Standard_GNAT :=
           Standard_GNAT and then Name_Buffer (1 .. Name_Len) = "-";

         if Name_Buffer (1 .. Name_Len) /= "." then

            --  If Dot_Replacement is not a single dot, then there should not
            --  be any dot in the name.

            for Index in First .. Last loop
               if File (Index) = '.' then
                  if Current_Verbosity = High then
                     Write_Line
                       ("   Not a valid file name (some dot not replaced).");
                  end if;

                  Unit_Name := No_Name;
                  return;

               end if;
            end loop;

            --  Replace the substring Dot_Replacement with dots

            declare
               Index : Positive := First;

            begin
               while Index <= Last - Name_Len + 1 loop

                  if File (Index .. Index + Name_Len - 1) =
                    Name_Buffer (1 .. Name_Len)
                  then
                     File (Index) := '.';

                     if Name_Len > 1 and then Index < Last then
                        File (Index + 1 .. Last - Name_Len + 1) :=
                          File (Index + Name_Len .. Last);
                     end if;

                     Last := Last - Name_Len + 1;
                  end if;

                  Index := Index + 1;
               end loop;
            end;
         end if;

         --  Check if the casing is right

         declare
            Src      : String := File (First .. Last);
            Src_Last : Positive := Last;

         begin
            case Naming.Casing is
               when All_Lower_Case =>
                  Fixed.Translate
                    (Source  => Src,
                     Mapping => Lower_Case_Map);

               when All_Upper_Case =>
                  Fixed.Translate
                    (Source  => Src,
                     Mapping => Upper_Case_Map);

               when Mixed_Case | Unknown =>
                  null;
            end case;

            if Src /= File (First .. Last) then
               if Current_Verbosity = High then
                  Write_Line ("   Not a valid file name (casing).");
               end if;

               Unit_Name := No_Name;
               return;
            end if;

            --  We put the name in lower case

            Fixed.Translate
              (Source  => Src,
               Mapping => Lower_Case_Map);

            --  In the standard GNAT naming scheme, check for special cases:
            --  children or separates of A, G, I or S, and run time sources.

            if Standard_GNAT and then Src'Length >= 3 then
               declare
                  S1 : constant Character := Src (Src'First);
                  S2 : constant Character := Src (Src'First + 1);
                  S3 : constant Character := Src (Src'First + 2);

               begin
                  if S1 = 'a' or else
                     S1 = 'g' or else
                     S1 = 'i' or else
                     S1 = 's'
                  then
                     --  Children or separates of packages A, G, I or S. These
                     --  names are x__ ... or x~... (where x is a, g, i, or s).
                     --  Both versions (x__... and x~...) are allowed in all
                     --  platforms, because it is not possible to know the
                     --  platform before processing of the project files.

                     if S2 = '_' and then S3 = '_' then
                        Src (Src'First + 1) := '.';
                        Src_Last := Src_Last - 1;
                        Src (Src'First + 2 .. Src_Last) :=
                          Src (Src'First + 3 .. Src_Last + 1);

                     elsif S2 = '~' then
                        Src (Src'First + 1) := '.';

                     --  If it is potentially a run time source, disable
                     --  filling of the mapping file to avoid warnings.

                     elsif S2 = '.' then
                        Set_Mapping_File_Initial_State_To_Empty;
                     end if;
                  end if;
               end;
            end if;

            if Current_Verbosity = High then
               Write_Str  ("      ");
               Write_Line (Src (Src'First .. Src_Last));
            end if;

            --  Now, we check if this name is a valid unit name

            Check_Ada_Name
              (Name => Src (Src'First .. Src_Last), Unit => Unit_Name);
         end;

      end;
   end Get_Unit;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Unit_Info) return Header_Num is
   begin
      return Header_Num (Unit.Unit mod 2048);
   end Hash;

   -----------------------
   -- Is_Illegal_Suffix --
   -----------------------

   function Is_Illegal_Suffix
     (Suffix                          : String;
      Dot_Replacement_Is_A_Single_Dot : Boolean) return Boolean
   is
   begin
      if Suffix'Length = 0 or else Index (Suffix, ".") = 0 then
         return True;
      end if;

      --  If dot replacement is a single dot, and first character of suffix is
      --  also a dot

      if Dot_Replacement_Is_A_Single_Dot
        and then Suffix (Suffix'First) = '.'
      then
         for Index in Suffix'First + 1 .. Suffix'Last loop

            --  If there is another dot

            if Suffix (Index) = '.' then

               --  It is illegal to have a letter following the initial dot

               return Is_Letter (Suffix (Suffix'First + 1));
            end if;
         end loop;
      end if;

      --  Everything is OK

      return False;
   end Is_Illegal_Suffix;

   ----------------------
   -- Locate_Directory --
   ----------------------

   procedure Locate_Directory
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Name        : File_Name_Type;
      Parent      : Path_Name_Type;
      Dir         : out Path_Name_Type;
      Display     : out Path_Name_Type;
      Create      : String := "";
      Current_Dir : String;
      Location    : Source_Ptr := No_Location)
   is
      The_Parent      : constant String :=
                          Get_Name_String (Parent) & Directory_Separator;

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

      Dir     := No_Path;
      Display := No_Path;

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
         Full_Path_Name : constant String := Get_Name_String (Full_Name);

      begin
         if (Setup_Projects or else Subdirs /= null)
           and then Create'Length > 0
           and then not Is_Directory (Full_Path_Name)
         then
            begin
               Create_Path (Full_Path_Name);

               if not Quiet_Output then
                  Write_Str (Create);
                  Write_Str (" directory """);
                  Write_Str (Full_Path_Name);
                  Write_Line (""" created");
               end if;

            exception
               when Use_Error =>
                  Error_Msg
                    (Project, In_Tree,
                     "could not create " & Create &
                     " directory " & Full_Path_Name,
                     Location);
            end;
         end if;

         if Is_Directory (Full_Path_Name) then
            declare
               Normed : constant String :=
                          Normalize_Pathname
                            (Full_Path_Name,
                             Directory      => Current_Dir,
                             Resolve_Links  => False,
                             Case_Sensitive => True);

               Canonical_Path : constant String :=
                                  Normalize_Pathname
                                    (Normed,
                                     Directory      => Current_Dir,
                                     Resolve_Links  =>
                                        Opt.Follow_Links_For_Dirs,
                                     Case_Sensitive => False);

            begin
               Name_Len := Normed'Length;
               Name_Buffer (1 .. Name_Len) := Normed;
               Display := Name_Find;

               Name_Len := Canonical_Path'Length;
               Name_Buffer (1 .. Name_Len) := Canonical_Path;
               Dir := Name_Find;
            end;
         end if;
      end;
   end Locate_Directory;

   ---------------------------
   -- Find_Excluded_Sources --
   ---------------------------

   procedure Find_Excluded_Sources
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : Project_Data)
   is
      Excluded_Sources : Variable_Value;

      Excluded_Source_List_File : Variable_Value;

      Current          : String_List_Id;

      Element : String_Element;

      Location : Source_Ptr;

      Name : File_Name_Type;

      File : Prj.Util.Text_File;
      Line : String (1 .. 300);
      Last : Natural;

      Locally_Removed : Boolean := False;
   begin
      Excluded_Source_List_File :=
        Util.Value_Of
          (Name_Excluded_Source_List_File, Data.Decl.Attributes, In_Tree);

      Excluded_Sources :=
        Util.Value_Of
          (Name_Excluded_Source_Files, Data.Decl.Attributes, In_Tree);

      --  If Excluded_Source_Files is not declared, check
      --  Locally_Removed_Files.

      if Excluded_Sources.Default then
         Locally_Removed := True;
         Excluded_Sources :=
           Util.Value_Of
             (Name_Locally_Removed_Files, Data.Decl.Attributes, In_Tree);
      end if;

      Excluded_Sources_Htable.Reset;

      --  If there are excluded sources, put them in the table

      if not Excluded_Sources.Default then
         if not Excluded_Source_List_File.Default then
            if Locally_Removed then
               Error_Msg
                 (Project, In_Tree,
                  "?both attributes Locally_Removed_Files and " &
                  "Excluded_Source_List_File are present",
                  Excluded_Source_List_File.Location);
            else
               Error_Msg
                 (Project, In_Tree,
                  "?both attributes Excluded_Source_Files and " &
                  "Excluded_Source_List_File are present",
                  Excluded_Source_List_File.Location);
            end if;
         end if;

         Current := Excluded_Sources.Values;
         while Current /= Nil_String loop
            Element := In_Tree.String_Elements.Table (Current);

            if Osint.File_Names_Case_Sensitive then
               Name := File_Name_Type (Element.Value);
            else
               Get_Name_String (Element.Value);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Name := Name_Find;
            end if;

            --  If the element has no location, then use the location
            --  of Excluded_Sources to report possible errors.

            if Element.Location = No_Location then
               Location := Excluded_Sources.Location;
            else
               Location := Element.Location;
            end if;

            Excluded_Sources_Htable.Set (Name, (Name, False, Location));
            Current := Element.Next;
         end loop;

      elsif not Excluded_Source_List_File.Default then
         Location := Excluded_Source_List_File.Location;

         declare
            Source_File_Path_Name : constant String :=
                                      Path_Name_Of
                                        (File_Name_Type
                                           (Excluded_Source_List_File.Value),
                                         Data.Directory.Name);

         begin
            if Source_File_Path_Name'Length = 0 then
               Err_Vars.Error_Msg_File_1 :=
                 File_Name_Type (Excluded_Source_List_File.Value);
               Error_Msg
                 (Project, In_Tree,
                  "file with excluded sources { does not exist",
                  Excluded_Source_List_File.Location);

            else
               --  Open the file

               Prj.Util.Open (File, Source_File_Path_Name);

               if not Prj.Util.Is_Valid (File) then
                  Error_Msg
                    (Project, In_Tree, "file does not exist", Location);
               else
                  --  Read the lines one by one

                  while not Prj.Util.End_Of_File (File) loop
                     Prj.Util.Get_Line (File, Line, Last);

                     --  A non empty, non comment line should contain a file
                     --  name

                     if Last /= 0
                       and then (Last = 1 or else Line (1 .. 2) /= "--")
                     then
                        Name_Len := Last;
                        Name_Buffer (1 .. Name_Len) := Line (1 .. Last);
                        Canonical_Case_File_Name
                          (Name_Buffer (1 .. Name_Len));
                        Name := Name_Find;

                        --  Check that there is no directory information

                        for J in 1 .. Last loop
                           if Line (J) = '/'
                             or else Line (J) = Directory_Separator
                           then
                              Error_Msg_File_1 := Name;
                              Error_Msg
                                (Project,
                                 In_Tree,
                                 "file name cannot include " &
                                 "directory information ({)",
                                 Location);
                              exit;
                           end if;
                        end loop;

                        Excluded_Sources_Htable.Set
                          (Name, (Name, False, Location));
                     end if;
                  end loop;

                  Prj.Util.Close (File);
               end if;
            end if;
         end;
      end if;
   end Find_Excluded_Sources;

   ---------------------------
   -- Find_Explicit_Sources --
   ---------------------------

   procedure Find_Explicit_Sources
     (Current_Dir : String;
      Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Data        : in out Project_Data)
   is
      Sources          : constant Variable_Value :=
                           Util.Value_Of
                             (Name_Source_Files,
                              Data.Decl.Attributes,
                              In_Tree);
      Source_List_File : constant Variable_Value :=
                           Util.Value_Of
                             (Name_Source_List_File,
                              Data.Decl.Attributes,
                              In_Tree);
      Name_Loc         : Name_Location;

   begin
      pragma Assert (Sources.Kind = List, "Source_Files is not a list");
      pragma Assert
        (Source_List_File.Kind = Single,
         "Source_List_File is not a single string");

      --  If the user has specified a Sources attribute

      if not Sources.Default then
         if not Source_List_File.Default then
            Error_Msg
              (Project, In_Tree,
               "?both attributes source_files and " &
               "source_list_file are present",
               Source_List_File.Location);
         end if;

         --  Sources is a list of file names

         declare
            Current  : String_List_Id := Sources.Values;
            Element  : String_Element;
            Location : Source_Ptr;
            Name     : File_Name_Type;

         begin
            if Get_Mode = Ada_Only then
               Data.Ada_Sources_Present := Current /= Nil_String;
            end if;

            if Get_Mode = Multi_Language then
               if Current = Nil_String then
                  Data.First_Language_Processing := No_Language_Index;

                  --  This project contains no source. For projects that
                  --  don't extend other projects, this also means that
                  --  there is no need for an object directory, if not
                  --  specified.

                  if Data.Extends = No_Project
                    and then Data.Object_Directory = Data.Directory
                  then
                     Data.Object_Directory := No_Path_Information;
                  end if;
               end if;
            end if;

            while Current /= Nil_String loop
               Element := In_Tree.String_Elements.Table (Current);
               Get_Name_String (Element.Value);

               if Osint.File_Names_Case_Sensitive then
                  Name := File_Name_Type (Element.Value);
               else
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                  Name := Name_Find;
               end if;

               --  If the element has no location, then use the
               --  location of Sources to report possible errors.

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
                       (Project,
                        In_Tree,
                        "file name cannot include directory " &
                        "information ({)",
                        Location);
                     exit;
                  end if;
               end loop;

               --  In Multi_Language mode, check whether the file is
               --  already there: the same file name may be in the list; if
               --  the source is missing, the error will be on the first
               --  mention of the source file name.

               case Get_Mode is
                  when Ada_Only =>
                     Name_Loc := No_Name_Location;
                  when Multi_Language =>
                     Name_Loc := Source_Names.Get (Name);
               end case;

               if Name_Loc = No_Name_Location then
                  Name_Loc :=
                    (Name     => Name,
                     Location => Location,
                     Source   => No_Source,
                     Except   => False,
                     Found    => False);
                  Source_Names.Set (Name, Name_Loc);
               end if;

               Current := Element.Next;
            end loop;

            if Get_Mode = Ada_Only then
               Get_Path_Names_And_Record_Ada_Sources
                 (Project, In_Tree, Data, Current_Dir);
            end if;
         end;

         --  If we have no Source_Files attribute, check the Source_List_File
         --  attribute

      elsif not Source_List_File.Default then

         --  Source_List_File is the name of the file
         --  that contains the source file names

         declare
            Source_File_Path_Name : constant String :=
              Path_Name_Of
                (File_Name_Type (Source_List_File.Value), Data.Directory.Name);

         begin
            if Source_File_Path_Name'Length = 0 then
               Err_Vars.Error_Msg_File_1 :=
                 File_Name_Type (Source_List_File.Value);
               Error_Msg
                 (Project, In_Tree,
                  "file with sources { does not exist",
                  Source_List_File.Location);

            else
               Get_Sources_From_File
                 (Source_File_Path_Name, Source_List_File.Location,
                  Project, In_Tree);

               if Get_Mode = Ada_Only then
                  --  Look in the source directories to find those sources

                  Get_Path_Names_And_Record_Ada_Sources
                    (Project, In_Tree, Data, Current_Dir);
               end if;
            end if;
         end;

      else
         --  Neither Source_Files nor Source_List_File has been
         --  specified. Find all the files that satisfy the naming
         --  scheme in all the source directories.

         if Get_Mode = Ada_Only then
            Find_Ada_Sources (Project, In_Tree, Data, Current_Dir);
         end if;
      end if;

      if Get_Mode = Multi_Language then
         Search_Directories
           (Project, In_Tree, Data,
            For_All_Sources =>
              Sources.Default and then Source_List_File.Default);

         --  Check if all exceptions have been found.
         --  For Ada, it is an error if an exception is not found.
         --  For other language, the source is simply removed.

         declare
            Source   : Source_Id;
            Src_Data : Source_Data;

         begin
            Source := Data.First_Source;
            while Source /= No_Source loop
               Src_Data := In_Tree.Sources.Table (Source);

               if Src_Data.Naming_Exception
                 and then Src_Data.Path = No_Path_Information
               then
                  if Src_Data.Unit /= No_Name then
                     Error_Msg_Name_1 := Name_Id (Src_Data.Display_File);
                     Error_Msg_Name_2 := Name_Id (Src_Data.Unit);
                     Error_Msg
                       (Project, In_Tree,
                        "source file %% for unit %% not found",
                        No_Location);
                  end if;

                  Remove_Source (Source, No_Source, Project, Data, In_Tree);
               end if;

               Source := Src_Data.Next_In_Project;
            end loop;
         end;

         --  Check that all sources in Source_Files or the file
         --  Source_List_File has been found.

         declare
            Name_Loc : Name_Location;

         begin
            Name_Loc := Source_Names.Get_First;
            while Name_Loc /= No_Name_Location loop
               if (not Name_Loc.Except) and then (not Name_Loc.Found) then
                  Error_Msg_Name_1 := Name_Id (Name_Loc.Name);
                  Error_Msg
                    (Project,
                     In_Tree,
                     "file %% not found",
                     Name_Loc.Location);
               end if;

               Name_Loc := Source_Names.Get_Next;
            end loop;
         end;
      end if;

      if Get_Mode = Ada_Only
        and then Data.Extends = No_Project
      then
         --  We should have found at least one source, if not report an error

         if Data.Ada_Sources = Nil_String then
            Report_No_Sources
              (Project, "Ada", In_Tree, Source_List_File.Location);
         end if;
      end if;

   end Find_Explicit_Sources;

   -------------------------------------------
   -- Get_Path_Names_And_Record_Ada_Sources --
   -------------------------------------------

   procedure Get_Path_Names_And_Record_Ada_Sources
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Data        : in out Project_Data;
      Current_Dir : String)
   is
      Source_Dir      : String_List_Id;
      Element         : String_Element;
      Path            : Path_Name_Type;
      Dir             : Dir_Type;
      Name            : File_Name_Type;
      Canonical_Name  : File_Name_Type;
      Name_Str        : String (1 .. 1_024);
      Last            : Natural := 0;
      NL              : Name_Location;
      Current_Source  : String_List_Id := Nil_String;
      First_Error     : Boolean := True;
      Source_Recorded : Boolean := False;

   begin
      --  We look in all source directories for the file names in the hash
      --  table Source_Names.

      Source_Dir := Data.Source_Dirs;
      while Source_Dir /= Nil_String loop
         Source_Recorded := False;
         Element := In_Tree.String_Elements.Table (Source_Dir);

         declare
            Dir_Path : constant String :=
              Get_Name_String (Element.Display_Value);
         begin
            if Current_Verbosity = High then
               Write_Str ("checking directory """);
               Write_Str (Dir_Path);
               Write_Line ("""");
            end if;

            Open (Dir, Dir_Path);

            loop
               Read (Dir, Name_Str, Last);
               exit when Last = 0;

               Name_Len := Last;
               Name_Buffer (1 .. Name_Len) := Name_Str (1 .. Last);
               Name := Name_Find;

               if Osint.File_Names_Case_Sensitive then
                  Canonical_Name := Name;
               else
                  Canonical_Case_File_Name (Name_Str (1 .. Last));
                  Name_Buffer (1 .. Name_Len) := Name_Str (1 .. Last);
                  Canonical_Name := Name_Find;
               end if;

               NL := Source_Names.Get (Canonical_Name);

               if NL /= No_Name_Location and then not NL.Found then
                  NL.Found := True;
                  Source_Names.Set (Canonical_Name, NL);
                  Name_Len := Dir_Path'Length;
                  Name_Buffer (1 .. Name_Len) := Dir_Path;

                  if Name_Buffer (Name_Len) /= Directory_Separator then
                     Add_Char_To_Name_Buffer (Directory_Separator);
                  end if;

                  Add_Str_To_Name_Buffer (Name_Str (1 .. Last));
                  Path := Name_Find;

                  if Current_Verbosity = High then
                     Write_Str  ("  found ");
                     Write_Line (Get_Name_String (Name));
                  end if;

                  --  Register the source if it is an Ada compilation unit

                  Record_Ada_Source
                    (File_Name       => Name,
                     Path_Name       => Path,
                     Project         => Project,
                     In_Tree         => In_Tree,
                     Data            => Data,
                     Location        => NL.Location,
                     Current_Source  => Current_Source,
                     Source_Recorded => Source_Recorded,
                     Current_Dir     => Current_Dir);
               end if;
            end loop;

            Close (Dir);
         end;

         if Source_Recorded then
            In_Tree.String_Elements.Table (Source_Dir).Flag :=
              True;
         end if;

         Source_Dir := Element.Next;
      end loop;

      --  It is an error if a source file name in a source list or
      --  in a source list file is not found.

      NL := Source_Names.Get_First;
      while NL /= No_Name_Location loop
         if not NL.Found then
            Err_Vars.Error_Msg_File_1 := NL.Name;

            if First_Error then
               Error_Msg
                 (Project, In_Tree,
                  "source file { cannot be found",
                  NL.Location);
               First_Error := False;

            else
               Error_Msg
                 (Project, In_Tree,
                  "\source file { cannot be found",
                  NL.Location);
            end if;
         end if;

         NL := Source_Names.Get_Next;
      end loop;
   end Get_Path_Names_And_Record_Ada_Sources;

   --------------------------
   -- Check_Naming_Schemes --
   --------------------------

   procedure Check_Naming_Schemes
     (In_Tree               : Project_Tree_Ref;
      Data                  : in out Project_Data;
      Filename              : String;
      File_Name             : File_Name_Type;
      Alternate_Languages   : out Alternate_Language_Id;
      Language              : out Language_Index;
      Language_Name         : out Name_Id;
      Display_Language_Name : out Name_Id;
      Unit                  : out Name_Id;
      Lang_Kind             : out Language_Kind;
      Kind                  : out Source_Kind)
   is
      Last           : Positive := Filename'Last;
      Config         : Language_Config;
      Lang           : Name_List_Index := Data.Languages;
      Header_File    : Boolean := False;
      First_Language : Language_Index;
      OK             : Boolean;

      Last_Spec : Natural;
      Last_Body : Natural;
      Last_Sep  : Natural;

   begin
      Unit := No_Name;
      Alternate_Languages := No_Alternate_Language;

      while Lang /= No_Name_List loop
         Language_Name := In_Tree.Name_Lists.Table (Lang).Name;
         Language      := Data.First_Language_Processing;

         if Current_Verbosity = High then
            Write_Line
              ("     Testing language "
               & Get_Name_String (Language_Name)
               & " Header_File=" & Header_File'Img);
         end if;

         while Language /= No_Language_Index loop
            if In_Tree.Languages_Data.Table (Language).Name =
              Language_Name
            then
               Display_Language_Name :=
                 In_Tree.Languages_Data.Table (Language).Display_Name;
               Config := In_Tree.Languages_Data.Table (Language).Config;
               Lang_Kind := Config.Kind;

               if Config.Kind = File_Based then

                  --  For file based languages, there is no Unit. Just
                  --  check if the file name has the implementation or,
                  --  if it is specified, the template suffix of the
                  --  language.

                  Unit := No_Name;

                  if not Header_File
                    and then Config.Naming_Data.Body_Suffix /= No_File
                  then
                     declare
                        Impl_Suffix : constant String :=
                          Get_Name_String (Config.Naming_Data.Body_Suffix);

                     begin
                        if Filename'Length > Impl_Suffix'Length
                          and then
                            Filename
                              (Last - Impl_Suffix'Length + 1 .. Last) =
                              Impl_Suffix
                        then
                           Kind := Impl;

                           if Current_Verbosity = High then
                              Write_Str ("     source of language ");
                              Write_Line
                                (Get_Name_String (Display_Language_Name));
                           end if;

                           return;
                        end if;
                     end;
                  end if;

                  if Config.Naming_Data.Spec_Suffix /= No_File then
                     declare
                        Spec_Suffix : constant String :=
                          Get_Name_String
                            (Config.Naming_Data.Spec_Suffix);

                     begin
                        if Filename'Length > Spec_Suffix'Length
                          and then
                            Filename
                              (Last - Spec_Suffix'Length + 1 .. Last) =
                              Spec_Suffix
                        then
                           Kind := Spec;

                           if Current_Verbosity = High then
                              Write_Str ("     header file of language ");
                              Write_Line
                                (Get_Name_String (Display_Language_Name));
                           end if;

                           if Header_File then
                              Alternate_Language_Table.Increment_Last
                                (In_Tree.Alt_Langs);
                              In_Tree.Alt_Langs.Table
                                (Alternate_Language_Table.Last
                                   (In_Tree.Alt_Langs)) :=
                                (Language => Language,
                                 Next     => Alternate_Languages);
                              Alternate_Languages :=
                                Alternate_Language_Table.Last
                                  (In_Tree.Alt_Langs);
                           else
                              Header_File    := True;
                              First_Language := Language;
                           end if;
                        end if;
                     end;
                  end if;

               elsif not Header_File then
                  --  Unit based language

                  OK := Config.Naming_Data.Dot_Replacement /= No_File;

                  if OK then

                     --  Check casing
                     --  ??? Are we doing this once per file in the project ?
                     --  It should be done only once per project.

                     case Config.Naming_Data.Casing is
                        when All_Lower_Case =>
                           for J in Filename'Range loop
                              if Is_Letter (Filename (J)) then
                                 if not Is_Lower (Filename (J)) then
                                    OK := False;
                                    exit;
                                 end if;
                              end if;
                           end loop;

                        when All_Upper_Case =>
                           for J in Filename'Range loop
                              if Is_Letter (Filename (J)) then
                                 if not Is_Upper (Filename (J)) then
                                    OK := False;
                                    exit;
                                 end if;
                              end if;
                           end loop;

                        when Mixed_Case =>
                           null;

                        when others =>
                           OK := False;
                     end case;
                  end if;

                  if OK then
                     Last_Spec := Natural'Last;
                     Last_Body := Natural'Last;
                     Last_Sep  := Natural'Last;

                     if Config.Naming_Data.Separate_Suffix /= No_File
                       and then
                         Config.Naming_Data.Separate_Suffix /=
                           Config.Naming_Data.Body_Suffix
                     then
                        declare
                           Suffix : constant String :=
                             Get_Name_String
                               (Config.Naming_Data.Separate_Suffix);
                        begin
                           if Filename'Length > Suffix'Length
                             and then
                               Filename
                                 (Last - Suffix'Length + 1 .. Last) =
                                 Suffix
                           then
                              Last_Sep := Last - Suffix'Length;
                           end if;
                        end;
                     end if;

                     if Config.Naming_Data.Body_Suffix /= No_File then
                        declare
                           Suffix : constant String :=
                             Get_Name_String
                               (Config.Naming_Data.Body_Suffix);
                        begin
                           if Filename'Length > Suffix'Length
                             and then
                               Filename
                                 (Last - Suffix'Length + 1 .. Last) =
                                 Suffix
                           then
                              Last_Body := Last - Suffix'Length;
                           end if;
                        end;
                     end if;

                     if Config.Naming_Data.Spec_Suffix /= No_File then
                        declare
                           Suffix : constant String :=
                             Get_Name_String
                               (Config.Naming_Data.Spec_Suffix);
                        begin
                           if Filename'Length > Suffix'Length
                             and then
                               Filename
                                 (Last - Suffix'Length + 1 .. Last) =
                                 Suffix
                           then
                              Last_Spec := Last - Suffix'Length;
                           end if;
                        end;
                     end if;

                     declare
                        Last_Min : constant Natural :=
                                     Natural'Min (Natural'Min (Last_Spec,
                                                               Last_Body),
                                                               Last_Sep);

                     begin
                        OK := Last_Min < Last;

                        if OK then
                           Last := Last_Min;

                           if Last_Min = Last_Spec then
                              Kind := Spec;

                           elsif Last_Min = Last_Body then
                              Kind := Impl;

                           else
                              Kind := Sep;
                           end if;
                        end if;
                     end;
                  end if;

                  if OK then

                     --  Replace dot replacements with dots

                     Name_Len := 0;

                     declare
                        J : Positive := Filename'First;

                        Dot_Replacement : constant String :=
                          Get_Name_String
                            (Config.Naming_Data.
                                 Dot_Replacement);

                        Max : constant Positive :=
                          Last - Dot_Replacement'Length + 1;

                     begin
                        loop
                           Name_Len := Name_Len + 1;

                           if J <= Max and then
                             Filename
                               (J .. J + Dot_Replacement'Length - 1) =
                               Dot_Replacement
                           then
                              Name_Buffer (Name_Len) := '.';
                              J := J + Dot_Replacement'Length;

                           else
                              if Filename (J) = '.' then
                                 OK := False;
                                 exit;
                              end if;

                              Name_Buffer (Name_Len) :=
                                GNAT.Case_Util.To_Lower (Filename (J));
                              J := J + 1;
                           end if;

                           exit when J > Last;
                        end loop;
                     end;
                  end if;

                  if OK then

                     --  The name buffer should contain the name of the
                     --  the unit, if it is one.

                     --  Check that this is a valid unit name

                     Check_Ada_Name (Name_Buffer (1 .. Name_Len), Unit);

                     if Unit /= No_Name then

                        if Current_Verbosity = High then
                           if Kind = Spec then
                              Write_Str ("     spec of ");
                           else
                              Write_Str ("     body of ");
                           end if;

                           Write_Str (Get_Name_String (Unit));
                           Write_Str (" (language ");
                           Write_Str
                             (Get_Name_String (Display_Language_Name));
                           Write_Line (")");
                        end if;

                        --  Comments required, declare block should
                        --  be named ???

                        declare
                           Unit_Except : constant Unit_Exception :=
                             Unit_Exceptions.Get (Unit);

                           procedure Masked_Unit (Spec : Boolean);
                           --  Indicate that there is an exception for
                           --  the same unit, so the file is not a
                           --  source for the unit.

                           -----------------
                           -- Masked_Unit --
                           -----------------

                           procedure Masked_Unit (Spec : Boolean) is
                           begin
                              if Current_Verbosity = High then
                                 Write_Str ("   """);
                                 Write_Str (Filename);
                                 Write_Str (""" contains the ");

                                 if Spec then
                                    Write_Str ("spec");
                                 else
                                    Write_Str ("body");
                                 end if;

                                 Write_Str
                                   (" of a unit that is found in """);

                                 if Spec then
                                    Write_Str
                                      (Get_Name_String
                                         (Unit_Except.Spec));
                                 else
                                    Write_Str
                                      (Get_Name_String
                                         (Unit_Except.Impl));
                                 end if;

                                 Write_Line (""" (ignored)");
                              end if;

                              Language := No_Language_Index;
                           end Masked_Unit;

                        begin
                           if Kind = Spec then
                              if Unit_Except.Spec /= No_File
                                and then Unit_Except.Spec /= File_Name
                              then
                                 Masked_Unit (Spec => True);
                              end if;

                           else
                              if Unit_Except.Impl /= No_File
                                and then Unit_Except.Impl /= File_Name
                              then
                                 Masked_Unit (Spec => False);
                              end if;
                           end if;
                        end;

                        return;
                     end if;
                  end if;
               end if;
            end if;

            Language := In_Tree.Languages_Data.Table (Language).Next;
         end loop;

         Lang := In_Tree.Name_Lists.Table (Lang).Next;
      end loop;

      --  Comment needed here ???

      if Header_File then
         Language := First_Language;

      else
         Language := No_Language_Index;

         if Current_Verbosity = High then
            Write_Line ("     not a source of any language");
         end if;
      end if;
   end Check_Naming_Schemes;

   ----------------
   -- Check_File --
   ----------------

   procedure Check_File
     (Project           : Project_Id;
      In_Tree           : Project_Tree_Ref;
      Data              : in out Project_Data;
      Name              : String;
      File_Name         : File_Name_Type;
      Display_File_Name : File_Name_Type;
      Source_Directory  : String;
      For_All_Sources   : Boolean)
   is
      Display_Path    : constant String :=
        Normalize_Pathname
          (Name           => Name,
           Directory      => Source_Directory,
           Resolve_Links  => Opt.Follow_Links_For_Files,
           Case_Sensitive => True);

      Name_Loc          : Name_Location := Source_Names.Get (File_Name);
      Path_Id           : Path_Name_Type;
      Display_Path_Id   : Path_Name_Type;
      Check_Name        : Boolean := False;
      Alternate_Languages : Alternate_Language_Id := No_Alternate_Language;
      Language          : Language_Index;
      Source            : Source_Id;
      Other_Part        : Source_Id;
      Add_Src           : Boolean;
      Src_Ind           : Source_File_Index;
      Src_Data          : Source_Data;
      Unit              : Name_Id;
      Source_To_Replace : Source_Id := No_Source;
      Language_Name         : Name_Id;
      Display_Language_Name : Name_Id;
      Lang_Kind             : Language_Kind;
      Kind                  : Source_Kind := Spec;

   begin
      Name_Len := Display_Path'Length;
      Name_Buffer (1 .. Name_Len) := Display_Path;
      Display_Path_Id := Name_Find;

      if Osint.File_Names_Case_Sensitive then
         Path_Id := Display_Path_Id;
      else
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
         Path_Id := Name_Find;
      end if;

      if Name_Loc = No_Name_Location then
         Check_Name := For_All_Sources;

      else
         if Name_Loc.Found then

            --  Check if it is OK to have the same file name in several
            --  source directories.

            if not Data.Known_Order_Of_Source_Dirs then
               Error_Msg_File_1 := File_Name;
               Error_Msg
                 (Project, In_Tree,
                  "{ is found in several source directories",
                  Name_Loc.Location);
            end if;

         else
            Name_Loc.Found := True;

            Source_Names.Set (File_Name, Name_Loc);

            if Name_Loc.Source = No_Source then
               Check_Name := True;

            else
               In_Tree.Sources.Table (Name_Loc.Source).Path :=
                 (Path_Id, Display_Path_Id);

               Source_Paths_Htable.Set
                 (In_Tree.Source_Paths_HT,
                  Path_Id,
                  Name_Loc.Source);

               --  Check if this is a subunit

               if In_Tree.Sources.Table (Name_Loc.Source).Unit /= No_Name
                 and then
                   In_Tree.Sources.Table (Name_Loc.Source).Kind = Impl
               then
                  Src_Ind := Sinput.P.Load_Project_File
                    (Get_Name_String (Path_Id));

                  if Sinput.P.Source_File_Is_Subunit (Src_Ind) then
                     In_Tree.Sources.Table (Name_Loc.Source).Kind := Sep;
                  end if;
               end if;
            end if;
         end if;
      end if;

      if Check_Name then
         Other_Part := No_Source;

         Check_Naming_Schemes
           (In_Tree               => In_Tree,
            Data                  => Data,
            Filename              => Get_Name_String (File_Name),
            File_Name             => File_Name,
            Alternate_Languages   => Alternate_Languages,
            Language              => Language,
            Language_Name         => Language_Name,
            Display_Language_Name => Display_Language_Name,
            Unit                  => Unit,
            Lang_Kind             => Lang_Kind,
            Kind                  => Kind);

         if Language = No_Language_Index then

            --  A file name in a list must be a source of a language

            if Name_Loc.Found then
               Error_Msg_File_1 := File_Name;
               Error_Msg
                 (Project,
                  In_Tree,
                  "language unknown for {",
                  Name_Loc.Location);
            end if;

         else
            --  Check if the same file name or unit is used in the prj tree

            Source := In_Tree.First_Source;
            Add_Src := True;
            while Source /= No_Source loop
               Src_Data := In_Tree.Sources.Table (Source);

               if Unit /= No_Name
                 and then Src_Data.Unit = Unit
                 and then
                   ((Src_Data.Kind = Spec and then Kind = Impl)
                      or else
                    (Src_Data.Kind = Impl and then Kind = Spec))
               then
                  Other_Part := Source;

               elsif (Unit /= No_Name
                       and then Src_Data.Unit = Unit
                       and then
                         (Src_Data.Kind = Kind
                            or else
                         (Src_Data.Kind = Sep and then Kind = Impl)
                            or else
                         (Src_Data.Kind = Impl and then Kind = Sep)))
                 or else (Unit = No_Name and then Src_Data.File = File_Name)
               then
                  --  Duplication of file/unit in same project is only
                  --  allowed if order of source directories is known.

                  if Project = Src_Data.Project then
                     if Data.Known_Order_Of_Source_Dirs then
                        Add_Src := False;

                     elsif Unit /= No_Name then
                        Error_Msg_Name_1 := Unit;
                        Error_Msg
                          (Project, In_Tree, "duplicate unit %%", No_Location);
                        Add_Src := False;

                     else
                        Error_Msg_File_1 := File_Name;
                        Error_Msg
                          (Project, In_Tree, "duplicate source file name {",
                           No_Location);
                        Add_Src := False;
                     end if;

                     --  Do not allow the same unit name in different
                     --  projects, except if one is extending the other.

                     --  For a file based language, the same file name
                     --  replaces a file in a project being extended, but
                     --  it is allowed to have the same file name in
                     --  unrelated projects.

                  elsif Is_Extending
                    (Project, Src_Data.Project, In_Tree)
                  then
                     Source_To_Replace := Source;

                  elsif Unit /= No_Name
                    and then not Src_Data.Locally_Removed
                  then
                     Error_Msg_Name_1 := Unit;
                     Error_Msg
                       (Project, In_Tree,
                        "unit %% cannot belong to several projects",
                        No_Location);

                     Error_Msg_Name_1 := In_Tree.Projects.Table (Project).Name;
                     Error_Msg_Name_2 := Name_Id (Display_Path_Id);
                     Error_Msg
                       (Project, In_Tree, "\  project %%, %%", No_Location);

                     Error_Msg_Name_1 :=
                       In_Tree.Projects.Table (Src_Data.Project).Name;
                     Error_Msg_Name_2 := Name_Id (Src_Data.Path.Display_Name);
                     Error_Msg
                       (Project, In_Tree, "\  project %%, %%", No_Location);

                     Add_Src := False;
                  end if;
               end if;

               Source := Src_Data.Next_In_Sources;
            end loop;

            if Add_Src then
               Add_Source
                 (Id                  => Source,
                  Data                => Data,
                  In_Tree             => In_Tree,
                  Project             => Project,
                  Lang                => Language_Name,
                  Lang_Id             => Language,
                  Lang_Kind           => Lang_Kind,
                  Kind                => Kind,
                  Alternate_Languages => Alternate_Languages,
                  File_Name           => File_Name,
                  Display_File        => Display_File_Name,
                  Other_Part          => Other_Part,
                  Unit                => Unit,
                  Path                => Path_Id,
                  Display_Path        => Display_Path_Id,
                  Source_To_Replace   => Source_To_Replace);
            end if;
         end if;
      end if;
   end Check_File;

   ------------------------
   -- Search_Directories --
   ------------------------

   procedure Search_Directories
     (Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Data            : in out Project_Data;
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

      Source_Dir := Data.Source_Dirs;
      while Source_Dir /= Nil_String loop
         begin
            Element := In_Tree.String_Elements.Table (Source_Dir);
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
                     Write_Str ("Source_Dir = ");
                     Write_Line (Source_Directory);
                  end if;

                  --  We look to every entry in the source directory

                  Open (Dir, Source_Directory);

                  loop
                     Read (Dir, Name, Last);

                     exit when Last = 0;

                     --  ??? Duplicate system call here, we just did a
                     --  a similar one. Maybe Ada.Directories would be more
                     --  appropriate here

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
                           FF : File_Found :=
                                  Excluded_Sources_Htable.Get (File_Name);

                        begin
                           if FF /= No_File_Found then
                              if not FF.Found then
                                 FF.Found := True;
                                 Excluded_Sources_Htable.Set
                                   (File_Name, FF);

                                 if Current_Verbosity = High then
                                    Write_Str ("     excluded source """);
                                    Write_Str (Get_Name_String (File_Name));
                                    Write_Line ("""");
                                 end if;
                              end if;

                           else
                              Check_File
                                (Project           => Project,
                                 In_Tree           => In_Tree,
                                 Data              => Data,
                                 Name              => Name (1 .. Last),
                                 File_Name         => File_Name,
                                 Display_File_Name => Display_File_Name,
                                 Source_Directory  => Source_Directory
                                   (Source_Directory'First .. Dir_Last),
                                 For_All_Sources   => For_All_Sources);
                           end if;
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

   ----------------------
   -- Look_For_Sources --
   ----------------------

   procedure Look_For_Sources
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Data        : in out Project_Data;
      Current_Dir : String)
   is
      procedure Remove_Locally_Removed_Files_From_Units;
      --  Mark all locally removed sources as such in the Units table

      procedure Process_Sources_In_Multi_Language_Mode;
      --  Find all source files when in multi language mode

      ---------------------------------------------
      -- Remove_Locally_Removed_Files_From_Units --
      ---------------------------------------------

      procedure Remove_Locally_Removed_Files_From_Units is
         Excluded : File_Found;
         OK       : Boolean;
         Unit     : Unit_Data;
         Extended : Project_Id;

      begin
         Excluded := Excluded_Sources_Htable.Get_First;
         while Excluded /= No_File_Found loop
            OK := False;

            For_Each_Unit :
            for Index in Unit_Table.First ..
              Unit_Table.Last (In_Tree.Units)
            loop
               Unit := In_Tree.Units.Table (Index);

               for Kind in Spec_Or_Body'Range loop
                  if Unit.File_Names (Kind).Name = Excluded.File then
                     OK := True;

                     --  Check that this is from the current project or
                     --  that the current project extends.

                     Extended := Unit.File_Names (Kind).Project;

                     if Extended = Project
                       or else Project_Extends (Project, Extended, In_Tree)
                     then
                        Unit.File_Names (Kind).Path.Name := Slash;
                        Unit.File_Names (Kind).Needs_Pragma := False;
                        In_Tree.Units.Table (Index) := Unit;
                        Add_Forbidden_File_Name
                          (Unit.File_Names (Kind).Name);
                     else
                        Error_Msg
                          (Project, In_Tree,
                           "cannot remove a source from " &
                           "another project",
                           Excluded.Location);
                     end if;
                     exit For_Each_Unit;
                  end if;
               end loop;
            end loop For_Each_Unit;

            if not OK then
               Err_Vars.Error_Msg_File_1 := Excluded.File;
               Error_Msg
                 (Project, In_Tree, "unknown file {", Excluded.Location);
            end if;

            Excluded := Excluded_Sources_Htable.Get_Next;
         end loop;
      end Remove_Locally_Removed_Files_From_Units;

      --------------------------------------------
      -- Process_Sources_In_Multi_Language_Mode --
      --------------------------------------------

      procedure Process_Sources_In_Multi_Language_Mode is
         Source   : Source_Id;
         Src_Data : Source_Data;
         Name_Loc : Name_Location;
         OK       : Boolean;
         FF       : File_Found;

      begin
         --  First, put all naming exceptions if any, in the Source_Names table

         Unit_Exceptions.Reset;

         Source := Data.First_Source;
         while Source /= No_Source loop
            Src_Data := In_Tree.Sources.Table (Source);

            --  A file that is excluded cannot also be an exception file name

            if Excluded_Sources_Htable.Get (Src_Data.File) /=
              No_File_Found
            then
               Error_Msg_File_1 := Src_Data.File;
               Error_Msg
                 (Project, In_Tree,
                  "{ cannot be both excluded and an exception file name",
                  No_Location);
            end if;

            Name_Loc := (Name     => Src_Data.File,
                         Location => No_Location,
                         Source   => Source,
                         Except   => Src_Data.Unit /= No_Name,
                         Found    => False);

            if Current_Verbosity = High then
               Write_Str ("Putting source #");
               Write_Str (Source'Img);
               Write_Str (", file ");
               Write_Str (Get_Name_String (Src_Data.File));
               Write_Line (" in Source_Names");
            end if;

            Source_Names.Set (K => Src_Data.File, E => Name_Loc);

            --  If this is an Ada exception, record it in table Unit_Exceptions

            if Src_Data.Unit /= No_Name then
               declare
                  Unit_Except : Unit_Exception :=
                                  Unit_Exceptions.Get (Src_Data.Unit);

               begin
                  Unit_Except.Name := Src_Data.Unit;

                  if Src_Data.Kind = Spec then
                     Unit_Except.Spec := Src_Data.File;
                  else
                     Unit_Except.Impl := Src_Data.File;
                  end if;

                  Unit_Exceptions.Set (Src_Data.Unit, Unit_Except);
               end;
            end if;

            Source := Src_Data.Next_In_Project;
         end loop;

         Find_Explicit_Sources
           (Current_Dir, Project, In_Tree, Data);

         --  Mark as such the sources that are declared as excluded

         FF := Excluded_Sources_Htable.Get_First;
         while FF /= No_File_Found loop
            OK     := False;
            Source := In_Tree.First_Source;

            while Source /= No_Source loop
               Src_Data := In_Tree.Sources.Table (Source);

               if Src_Data.File = FF.File then

                  --  Check that this is from this project or a project that
                  --  the current project extends.

                  if Src_Data.Project = Project or else
                    Is_Extending (Project, Src_Data.Project, In_Tree)
                  then
                     Src_Data.Locally_Removed := True;
                     Src_Data.In_Interfaces := False;
                     In_Tree.Sources.Table (Source) := Src_Data;
                     Add_Forbidden_File_Name (FF.File);
                     OK := True;
                     exit;
                  end if;
               end if;

               Source := Src_Data.Next_In_Sources;
            end loop;

            if not FF.Found and not OK then
               Err_Vars.Error_Msg_File_1 := FF.File;
               Error_Msg (Project, In_Tree, "unknown file {", FF.Location);
            end if;

            FF := Excluded_Sources_Htable.Get_Next;
         end loop;

         --  Check that two sources of this project do not have the same object
         --  file name.

         Check_Object_File_Names : declare
            Src_Id      : Source_Id;
            Src_Data    : Source_Data;
            Source_Name : File_Name_Type;

            procedure Check_Object;
            --  Check if object file name of the current source is already in
            --  hash table Object_File_Names. If it is, report an error. If it
            --  is not, put it there with the file name of the current source.

            ------------------
            -- Check_Object --
            ------------------

            procedure Check_Object is
            begin
               Source_Name := Object_File_Names.Get (Src_Data.Object);

               if Source_Name /= No_File then
                  Error_Msg_File_1 := Src_Data.File;
                  Error_Msg_File_2 := Source_Name;
                  Error_Msg
                    (Project,
                     In_Tree,
                     "{ and { have the same object file name",
                     No_Location);

               else
                  Object_File_Names.Set (Src_Data.Object, Src_Data.File);
               end if;
            end Check_Object;

         --  Start of processing for Check_Object_File_Names

         begin
            Object_File_Names.Reset;
            Src_Id := In_Tree.First_Source;
            while Src_Id /= No_Source loop
               Src_Data := In_Tree.Sources.Table (Src_Id);

               if Src_Data.Compiled and then Src_Data.Object_Exists
                 and then Project_Extends (Project, Src_Data.Project, In_Tree)
               then
                  if Src_Data.Unit = No_Name then
                     if Src_Data.Kind = Impl then
                        Check_Object;
                     end if;

                  else
                     case Src_Data.Kind is
                        when Spec =>
                           if Src_Data.Other_Part = No_Source then
                              Check_Object;
                           end if;

                        when Sep =>
                           null;

                        when Impl =>
                           if Src_Data.Other_Part /= No_Source then
                              Check_Object;

                           else
                              --  Check if it is a subunit

                              declare
                                 Src_Ind : constant Source_File_Index :=
                                             Sinput.P.Load_Project_File
                                               (Get_Name_String
                                                  (Src_Data.Path.Name));

                              begin
                                 if Sinput.P.Source_File_Is_Subunit
                                     (Src_Ind)
                                 then
                                    In_Tree.Sources.Table (Src_Id).Kind := Sep;
                                 else
                                    Check_Object;
                                 end if;
                              end;
                           end if;
                     end case;
                  end if;
               end if;

               Src_Id := Src_Data.Next_In_Sources;
            end loop;
         end Check_Object_File_Names;
      end Process_Sources_In_Multi_Language_Mode;

   --  Start of processing for Look_For_Sources

   begin
      Source_Names.Reset;
      Find_Excluded_Sources (Project, In_Tree, Data);

      case Get_Mode is
         when Ada_Only =>
            if Is_A_Language (In_Tree, Data, Name_Ada) then
               Find_Explicit_Sources (Current_Dir, Project, In_Tree, Data);
               Remove_Locally_Removed_Files_From_Units;
            end if;

         when Multi_Language =>
            if Data.First_Language_Processing /= No_Language_Index then
               Process_Sources_In_Multi_Language_Mode;
            end if;
      end case;
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
         Canonical_Case_File_Name (Result.all);
         return Result.all;
      end if;
   end Path_Name_Of;

   -------------------------------
   -- Prepare_Ada_Naming_Exceptions --
   -------------------------------

   procedure Prepare_Ada_Naming_Exceptions
     (List    : Array_Element_Id;
      In_Tree : Project_Tree_Ref;
      Kind    : Spec_Or_Body)
   is
      Current : Array_Element_Id;
      Element : Array_Element;
      Unit    : Unit_Info;

   begin
      --  Traverse the list

      Current := List;
      while Current /= No_Array_Element loop
         Element := In_Tree.Array_Elements.Table (Current);

         if Element.Index /= No_Name then
            Unit :=
              (Kind => Kind,
               Unit => Element.Index,
               Next => No_Ada_Naming_Exception);
            Reverse_Ada_Naming_Exceptions.Set
              (Unit, (Element.Value.Value, Element.Value.Index));
            Unit.Next :=
              Ada_Naming_Exceptions.Get (File_Name_Type (Element.Value.Value));
            Ada_Naming_Exception_Table.Increment_Last;
            Ada_Naming_Exception_Table.Table
              (Ada_Naming_Exception_Table.Last) := Unit;
            Ada_Naming_Exceptions.Set
              (File_Name_Type (Element.Value.Value),
               Ada_Naming_Exception_Table.Last);
         end if;

         Current := Element.Next;
      end loop;
   end Prepare_Ada_Naming_Exceptions;

   ---------------------
   -- Project_Extends --
   ---------------------

   function Project_Extends
     (Extending : Project_Id;
      Extended  : Project_Id;
      In_Tree   : Project_Tree_Ref) return Boolean
   is
      Current : Project_Id := Extending;

   begin
      loop
         if Current = No_Project then
            return False;

         elsif Current = Extended then
            return True;
         end if;

         Current := In_Tree.Projects.Table (Current).Extends;
      end loop;
   end Project_Extends;

   -----------------------
   -- Record_Ada_Source --
   -----------------------

   procedure Record_Ada_Source
     (File_Name       : File_Name_Type;
      Path_Name       : Path_Name_Type;
      Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Data            : in out Project_Data;
      Location        : Source_Ptr;
      Current_Source  : in out String_List_Id;
      Source_Recorded : in out Boolean;
      Current_Dir     : String)
   is
      Canonical_File_Name : File_Name_Type;
      Canonical_Path_Name : Path_Name_Type;

      Exception_Id : Ada_Naming_Exception_Id;
      Unit_Name    : Name_Id;
      Unit_Kind    : Spec_Or_Body;
      Unit_Ind     : Int := 0;
      Info         : Unit_Info;
      Name_Index   : Name_And_Index;
      Needs_Pragma : Boolean;

      The_Location    : Source_Ptr              := Location;
      Previous_Source : constant String_List_Id := Current_Source;
      Except_Name     : Name_And_Index          := No_Name_And_Index;

      Unit_Prj : Unit_Project;

      File_Name_Recorded : Boolean := False;

   begin
      if Osint.File_Names_Case_Sensitive then
         Canonical_File_Name := File_Name;
         Canonical_Path_Name := Path_Name;
      else
         Get_Name_String (File_Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
         Canonical_File_Name := Name_Find;

         declare
            Canonical_Path : constant String :=
                               Normalize_Pathname
                                 (Get_Name_String (Path_Name),
                                  Directory      => Current_Dir,
                                  Resolve_Links  => Opt.Follow_Links_For_Files,
                                  Case_Sensitive => False);
         begin
            Name_Len := 0;
            Add_Str_To_Name_Buffer (Canonical_Path);
            Canonical_Path_Name := Name_Find;
         end;
      end if;

      --  Find out the unit name, the unit kind and if it needs
      --  a specific SFN pragma.

      Get_Unit
        (In_Tree             => In_Tree,
         Canonical_File_Name => Canonical_File_Name,
         Naming              => Data.Naming,
         Exception_Id        => Exception_Id,
         Unit_Name           => Unit_Name,
         Unit_Kind           => Unit_Kind,
         Needs_Pragma        => Needs_Pragma);

      if Exception_Id = No_Ada_Naming_Exception
        and then Unit_Name = No_Name
      then
         if Current_Verbosity = High then
            Write_Str  ("   """);
            Write_Str  (Get_Name_String (Canonical_File_Name));
            Write_Line (""" is not a valid source file name (ignored).");
         end if;

      else
         --  Check to see if the source has been hidden by an exception,
         --  but only if it is not an exception.

         if not Needs_Pragma then
            Except_Name :=
              Reverse_Ada_Naming_Exceptions.Get
                ((Unit_Kind, Unit_Name, No_Ada_Naming_Exception));

            if Except_Name /= No_Name_And_Index then
               if Current_Verbosity = High then
                  Write_Str  ("   """);
                  Write_Str  (Get_Name_String (Canonical_File_Name));
                  Write_Str  (""" contains a unit that is found in """);
                  Write_Str  (Get_Name_String (Except_Name.Name));
                  Write_Line (""" (ignored).");
               end if;

               --  The file is not included in the source of the project since
               --  it is hidden by the exception. So, nothing else to do.

               return;
            end if;
         end if;

         loop
            if Exception_Id /= No_Ada_Naming_Exception then
               Info := Ada_Naming_Exception_Table.Table (Exception_Id);
               Exception_Id := Info.Next;
               Info.Next := No_Ada_Naming_Exception;
               Name_Index := Reverse_Ada_Naming_Exceptions.Get (Info);

               Unit_Name := Info.Unit;
               Unit_Ind  := Name_Index.Index;
               Unit_Kind := Info.Kind;
            end if;

            --  Put the file name in the list of sources of the project

            String_Element_Table.Increment_Last (In_Tree.String_Elements);
            In_Tree.String_Elements.Table
              (String_Element_Table.Last (In_Tree.String_Elements)) :=
                (Value         => Name_Id (Canonical_File_Name),
                 Display_Value => Name_Id (File_Name),
                 Location      => No_Location,
                 Flag          => False,
                 Next          => Nil_String,
                 Index         => Unit_Ind);

            if Current_Source = Nil_String then
               Data.Ada_Sources :=
                 String_Element_Table.Last (In_Tree.String_Elements);
            else
               In_Tree.String_Elements.Table (Current_Source).Next :=
                 String_Element_Table.Last (In_Tree.String_Elements);
            end if;

            Current_Source :=
              String_Element_Table.Last (In_Tree.String_Elements);

            --  Put the unit in unit list

            declare
               The_Unit : Unit_Index :=
                            Units_Htable.Get (In_Tree.Units_HT, Unit_Name);

               The_Unit_Data : Unit_Data;

            begin
               if Current_Verbosity = High then
                  Write_Str  ("Putting ");
                  Write_Str  (Get_Name_String (Unit_Name));
                  Write_Line (" in the unit list.");
               end if;

               --  The unit is already in the list, but may be it is
               --  only the other unit kind (spec or body), or what is
               --  in the unit list is a unit of a project we are extending.

               if The_Unit /= No_Unit_Index then
                  The_Unit_Data := In_Tree.Units.Table (The_Unit);

                  if (The_Unit_Data.File_Names (Unit_Kind).Name =
                                                          Canonical_File_Name
                        and then
                        The_Unit_Data.File_Names
                          (Unit_Kind).Path.Name = Slash)
                    or else The_Unit_Data.File_Names (Unit_Kind).Name = No_File
                    or else Project_Extends
                      (Data.Extends,
                       The_Unit_Data.File_Names (Unit_Kind).Project,
                       In_Tree)
                  then
                     if
                       The_Unit_Data.File_Names (Unit_Kind).Path.Name = Slash
                     then
                        Remove_Forbidden_File_Name
                          (The_Unit_Data.File_Names (Unit_Kind).Name);
                     end if;

                     --  Record the file name in the hash table Files_Htable

                     Unit_Prj := (Unit => The_Unit, Project => Project);
                     Files_Htable.Set
                       (In_Tree.Files_HT,
                        Canonical_File_Name,
                        Unit_Prj);

                     The_Unit_Data.File_Names (Unit_Kind) :=
                       (Name         => Canonical_File_Name,
                        Index        => Unit_Ind,
                        Display_Name => File_Name,
                        Path         => (Canonical_Path_Name, Path_Name),
                        Project      => Project,
                        Needs_Pragma => Needs_Pragma);
                     In_Tree.Units.Table (The_Unit) := The_Unit_Data;
                     Source_Recorded := True;

                  elsif The_Unit_Data.File_Names (Unit_Kind).Project = Project
                    and then (Data.Known_Order_Of_Source_Dirs
                                or else
                                The_Unit_Data.File_Names
                                  (Unit_Kind).Path.Name = Canonical_Path_Name)
                  then
                     if Previous_Source = Nil_String then
                        Data.Ada_Sources := Nil_String;
                     else
                        In_Tree.String_Elements.Table (Previous_Source).Next :=
                          Nil_String;
                        String_Element_Table.Decrement_Last
                          (In_Tree.String_Elements);
                     end if;

                     Current_Source := Previous_Source;

                  else
                     --  It is an error to have two units with the same name
                     --  and the same kind (spec or body).

                     if The_Location = No_Location then
                        The_Location :=
                          In_Tree.Projects.Table (Project).Location;
                     end if;

                     Err_Vars.Error_Msg_Name_1 := Unit_Name;
                     Error_Msg
                       (Project, In_Tree, "duplicate unit %%", The_Location);

                     Err_Vars.Error_Msg_Name_1 :=
                       In_Tree.Projects.Table
                         (The_Unit_Data.File_Names (Unit_Kind).Project).Name;
                     Err_Vars.Error_Msg_File_1 :=
                       File_Name_Type
                         (The_Unit_Data.File_Names (Unit_Kind).Path.Name);
                     Error_Msg
                       (Project, In_Tree,
                        "\   project file %%, {", The_Location);

                     Err_Vars.Error_Msg_Name_1 :=
                       In_Tree.Projects.Table (Project).Name;
                     Err_Vars.Error_Msg_File_1 :=
                       File_Name_Type (Canonical_Path_Name);
                     Error_Msg
                       (Project, In_Tree,
                        "\   project file %%, {", The_Location);
                  end if;

               --  It is a new unit, create a new record

               else
                  --  First, check if there is no other unit with this file
                  --  name in another project. If it is, report error but note
                  --  we do that only for the first unit in the source file.

                  Unit_Prj :=
                    Files_Htable.Get (In_Tree.Files_HT, Canonical_File_Name);

                  if not File_Name_Recorded and then
                    Unit_Prj /= No_Unit_Project
                  then
                     Error_Msg_File_1 := File_Name;
                     Error_Msg_Name_1 :=
                       In_Tree.Projects.Table (Unit_Prj.Project).Name;
                     Error_Msg
                       (Project, In_Tree,
                        "{ is already a source of project %%",
                        Location);

                  else
                     Unit_Table.Increment_Last (In_Tree.Units);
                     The_Unit := Unit_Table.Last (In_Tree.Units);
                     Units_Htable.Set
                       (In_Tree.Units_HT, Unit_Name, The_Unit);
                     Unit_Prj := (Unit => The_Unit, Project => Project);
                     Files_Htable.Set
                       (In_Tree.Files_HT,
                        Canonical_File_Name,
                        Unit_Prj);
                     The_Unit_Data.Name := Unit_Name;
                     The_Unit_Data.File_Names (Unit_Kind) :=
                       (Name         => Canonical_File_Name,
                        Index        => Unit_Ind,
                        Display_Name => File_Name,
                        Path         => (Canonical_Path_Name, Path_Name),
                        Project      => Project,
                        Needs_Pragma => Needs_Pragma);
                     In_Tree.Units.Table (The_Unit) := The_Unit_Data;
                     Source_Recorded := True;
                  end if;
               end if;
            end;

            exit when Exception_Id = No_Ada_Naming_Exception;
            File_Name_Recorded := True;
         end loop;
      end if;
   end Record_Ada_Source;

   -------------------
   -- Remove_Source --
   -------------------

   procedure Remove_Source
     (Id          : Source_Id;
      Replaced_By : Source_Id;
      Project     : Project_Id;
      Data        : in out Project_Data;
      In_Tree     : Project_Tree_Ref)
   is
      Src_Data : constant Source_Data := In_Tree.Sources.Table (Id);
      Source   : Source_Id;

   begin
      if Current_Verbosity = High then
         Write_Str ("Removing source #");
         Write_Line (Id'Img);
      end if;

      if Replaced_By /= No_Source then
         In_Tree.Sources.Table (Id).Replaced_By := Replaced_By;
         In_Tree.Sources.Table (Replaced_By).Declared_In_Interfaces :=
           In_Tree.Sources.Table (Id).Declared_In_Interfaces;
      end if;

      --  Remove the source from the global source list

      Source := In_Tree.First_Source;

      if Source = Id then
         In_Tree.First_Source := Src_Data.Next_In_Sources;

      else
         while In_Tree.Sources.Table (Source).Next_In_Sources /= Id loop
            Source := In_Tree.Sources.Table (Source).Next_In_Sources;
         end loop;

         In_Tree.Sources.Table (Source).Next_In_Sources :=
           Src_Data.Next_In_Sources;
      end if;

      --  Remove the source from the project list

      if Src_Data.Project = Project then
         Source := Data.First_Source;

         if Source = Id then
            Data.First_Source := Src_Data.Next_In_Project;

            if Src_Data.Next_In_Project = No_Source then
               Data.Last_Source := No_Source;
            end if;

         else
            while In_Tree.Sources.Table (Source).Next_In_Project /= Id loop
               Source := In_Tree.Sources.Table (Source).Next_In_Project;
            end loop;

            In_Tree.Sources.Table (Source).Next_In_Project :=
              Src_Data.Next_In_Project;

            if Src_Data.Next_In_Project = No_Source then
               In_Tree.Projects.Table (Src_Data.Project).Last_Source := Source;
            end if;
         end if;

      else
         Source := In_Tree.Projects.Table (Src_Data.Project).First_Source;

         if Source = Id then
            In_Tree.Projects.Table (Src_Data.Project).First_Source :=
              Src_Data.Next_In_Project;

            if Src_Data.Next_In_Project = No_Source then
               In_Tree.Projects.Table (Src_Data.Project).Last_Source :=
                 No_Source;
            end if;

         else
            while In_Tree.Sources.Table (Source).Next_In_Project /= Id loop
               Source := In_Tree.Sources.Table (Source).Next_In_Project;
            end loop;

            In_Tree.Sources.Table (Source).Next_In_Project :=
              Src_Data.Next_In_Project;

            if Src_Data.Next_In_Project = No_Source then
               In_Tree.Projects.Table (Src_Data.Project).Last_Source := Source;
            end if;
         end if;
      end if;

      --  Remove source from the language list

      Source := In_Tree.Languages_Data.Table (Src_Data.Language).First_Source;

      if Source = Id then
         In_Tree.Languages_Data.Table (Src_Data.Language).First_Source :=
           Src_Data.Next_In_Lang;

      else
         while In_Tree.Sources.Table (Source).Next_In_Lang /= Id loop
            Source := In_Tree.Sources.Table (Source).Next_In_Lang;
         end loop;

         In_Tree.Sources.Table (Source).Next_In_Lang :=
           Src_Data.Next_In_Lang;
      end if;
   end Remove_Source;

   -----------------------
   -- Report_No_Sources --
   -----------------------

   procedure Report_No_Sources
     (Project      : Project_Id;
      Lang_Name    : String;
      In_Tree      : Project_Tree_Ref;
      Location     : Source_Ptr;
      Continuation : Boolean := False)
   is
   begin
      case When_No_Sources is
         when Silent =>
            null;

         when Warning | Error =>
            declare
               Msg : constant String :=
                       "<there are no " &
                       Lang_Name &
                       " sources in this project";

            begin
               Error_Msg_Warn := When_No_Sources = Warning;

               if Continuation then
                  Error_Msg
                    (Project, In_Tree, "\" & Msg, Location);

               else
                  Error_Msg
                    (Project, In_Tree, Msg, Location);
               end if;
            end;
      end case;
   end Report_No_Sources;

   ----------------------
   -- Show_Source_Dirs --
   ----------------------

   procedure Show_Source_Dirs
     (Data    : Project_Data;
      In_Tree : Project_Tree_Ref)
   is
      Current : String_List_Id;
      Element : String_Element;

   begin
      Write_Line ("Source_Dirs:");

      Current := Data.Source_Dirs;
      while Current /= Nil_String loop
         Element := In_Tree.String_Elements.Table (Current);
         Write_Str  ("   ");
         Write_Line (Get_Name_String (Element.Value));
         Current := Element.Next;
      end loop;

      Write_Line ("end Source_Dirs.");
   end Show_Source_Dirs;

   -------------------------
   -- Warn_If_Not_Sources --
   -------------------------

   --  comments needed in this body ???

   procedure Warn_If_Not_Sources
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Conventions : Array_Element_Id;
      Specs       : Boolean;
      Extending   : Boolean)
   is
      Conv          : Array_Element_Id;
      Unit          : Name_Id;
      The_Unit_Id   : Unit_Index;
      The_Unit_Data : Unit_Data;
      Location      : Source_Ptr;

   begin
      Conv := Conventions;
      while Conv /= No_Array_Element loop
         Unit := In_Tree.Array_Elements.Table (Conv).Index;
         Error_Msg_Name_1 := Unit;
         Get_Name_String (Unit);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Unit := Name_Find;
         The_Unit_Id := Units_Htable.Get (In_Tree.Units_HT, Unit);
         Location := In_Tree.Array_Elements.Table (Conv).Value.Location;

         if The_Unit_Id = No_Unit_Index then
            Error_Msg (Project, In_Tree, "?unknown unit %%", Location);

         else
            The_Unit_Data := In_Tree.Units.Table (The_Unit_Id);
            Error_Msg_Name_2 :=
              In_Tree.Array_Elements.Table (Conv).Value.Value;

            if Specs then
               if not Check_Project
                 (The_Unit_Data.File_Names (Specification).Project,
                  Project, In_Tree, Extending)
               then
                  Error_Msg
                    (Project, In_Tree,
                     "?source of spec of unit %% (%%)" &
                     " cannot be found in this project",
                     Location);
               end if;

            else
               if not Check_Project
                 (The_Unit_Data.File_Names (Body_Part).Project,
                  Project, In_Tree, Extending)
               then
                  Error_Msg
                    (Project, In_Tree,
                     "?source of body of unit %% (%%)" &
                     " cannot be found in this project",
                     Location);
               end if;
            end if;
         end if;

         Conv := In_Tree.Array_Elements.Table (Conv).Next;
      end loop;
   end Warn_If_Not_Sources;

end Prj.Nmsc;
