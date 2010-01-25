------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2009, Free Software Foundation, Inc.         --
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

--  The following package declares the data types for GNAT project.
--  These data types may be used by GNAT Project-aware tools.

--  Children of these package implements various services on these data types.
--  See in particular Prj.Pars and Prj.Env.

with Casing; use Casing;
with Namet;  use Namet;
with Osint;
with Scans;  use Scans;
with Types;  use Types;

with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;
with GNAT.Dynamic_Tables;
with GNAT.OS_Lib;          use GNAT.OS_Lib;

package Prj is

   All_Other_Names : constant Name_Id := Names_High_Bound;
   --  Name used to replace others as an index of an associative array
   --  attribute in situations where this is allowed.

   Subdirs : String_Ptr := null;
   --  The value after the equal sign in switch --subdirs=...
   --  Contains the relative subdirectory.

   type Library_Support is (None, Static_Only, Full);
   --  Support for Library Project File.
   --  - None: Library Project Files are not supported at all
   --  - Static_Only: Library Project Files are only supported for static
   --    libraries.
   --  - Full: Library Project Files are supported for static and dynamic
   --    (shared) libraries.

   type Yes_No_Unknown is (Yes, No, Unknown);
   --  Tri-state to decide if -lgnarl is needed when linking

   type Project_Qualifier is
     (Unspecified,
      Standard,
      Library,
      Configuration,
      Dry,
      Aggregate,
      Aggregate_Library);
   --  Qualifiers that can prefix the reserved word "project" in a project
   --  file:
   --    Standard:             standard project ...
   --    Library:              library project is ...
   --    Dry:                  abstract project is
   --    Aggregate:            aggregate project is
   --    Aggregate_Library:    aggregate library project is ...
   --    Configuration:        configuration project is ...

   All_Packages : constant String_List_Access;
   --  Default value of parameter Packages of procedures Parse, in Prj.Pars and
   --  Prj.Part, indicating that all packages should be checked.

   type Project_Tree_Data;
   type Project_Tree_Ref is access all Project_Tree_Data;
   --  Reference to a project tree. Several project trees may exist in memory
   --  at the same time.

   No_Project_Tree : constant Project_Tree_Ref;

   procedure Free (Tree : in out Project_Tree_Ref);
   --  Free memory associated with the tree

   Config_Project_File_Extension : String := ".cgpr";
   Project_File_Extension : String := ".gpr";
   --  The standard config and user project file name extensions. They are not
   --  constants, because Canonical_Case_File_Name is called on these variables
   --  in the body of Prj.

   function Empty_File   return File_Name_Type;
   function Empty_String return Name_Id;
   --  Return the id for an empty string ""

   type Path_Information is record
      Name         : Path_Name_Type := No_Path;
      Display_Name : Path_Name_Type := No_Path;
   end record;
   --  Directory names always end with a directory separator

   No_Path_Information : constant Path_Information := (No_Path, No_Path);

   type Project_Data;
   type Project_Id is access all Project_Data;
   No_Project : constant Project_Id := null;
   --  Id of a Project File

   type String_List_Id is new Nat;
   Nil_String : constant String_List_Id := 0;
   type String_Element is record
      Value         : Name_Id        := No_Name;
      Index         : Int            := 0;
      Display_Value : Name_Id        := No_Name;
      Location      : Source_Ptr     := No_Location;
      Flag          : Boolean        := False;
      Next          : String_List_Id := Nil_String;
   end record;
   --  To hold values for string list variables and array elements.
   --  Component Flag may be used for various purposes. For source
   --  directories, it indicates if the directory contains Ada source(s).

   package String_Element_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => String_Element,
      Table_Index_Type     => String_List_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100);
   --  The table for string elements in string lists

   type Variable_Kind is (Undefined, List, Single);
   --  Different kinds of variables

   subtype Defined_Variable_Kind is Variable_Kind range List .. Single;
   --  The defined kinds of variables

   Ignored : constant Variable_Kind;
   --  Used to indicate that a package declaration must be ignored
   --  while processing the project tree (unknown package name).

   type Variable_Value (Kind : Variable_Kind := Undefined) is record
      Project  : Project_Id := No_Project;
      Location : Source_Ptr := No_Location;
      Default  : Boolean    := False;
      case Kind is
         when Undefined =>
            null;
         when List =>
            Values : String_List_Id := Nil_String;
         when Single =>
            Value : Name_Id := No_Name;
            Index : Int     := 0;
      end case;
   end record;
   --  Values for variables and array elements. Default is True if the
   --  current value is the default one for the variable.

   Nil_Variable_Value : constant Variable_Value;
   --  Value of a non existing variable or array element

   type Variable_Id is new Nat;
   No_Variable : constant Variable_Id := 0;
   type Variable is record
      Next  : Variable_Id := No_Variable;
      Name  : Name_Id;
      Value : Variable_Value;
   end record;
   --  To hold the list of variables in a project file and in packages

   package Variable_Element_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Variable,
      Table_Index_Type     => Variable_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100);
   --  The table of variable in list of variables

   type Array_Element_Id is new Nat;
   No_Array_Element : constant Array_Element_Id := 0;
   type Array_Element is record
      Index                : Name_Id;
      Src_Index            : Int := 0;
      Index_Case_Sensitive : Boolean := True;
      Value                : Variable_Value;
      Next                 : Array_Element_Id := No_Array_Element;
   end record;
   --  Each Array_Element represents an array element and is linked (Next)
   --  to the next array element, if any, in the array.

   package Array_Element_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Array_Element,
      Table_Index_Type     => Array_Element_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100);
   --  The table that contains all array elements

   type Array_Id is new Nat;
   No_Array : constant Array_Id := 0;
   type Array_Data is record
      Name     : Name_Id          := No_Name;
      Location : Source_Ptr       := No_Location;
      Value    : Array_Element_Id := No_Array_Element;
      Next     : Array_Id         := No_Array;
   end record;
   --  Each Array_Data value represents an array.
   --  Value is the id of the first element.
   --  Next is the id of the next array in the project file or package.

   package Array_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Array_Data,
      Table_Index_Type     => Array_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100);
   --  The table that contains all arrays

   type Package_Id is new Nat;
   No_Package : constant Package_Id := 0;
   type Declarations is record
      Variables  : Variable_Id := No_Variable;
      Attributes : Variable_Id := No_Variable;
      Arrays     : Array_Id    := No_Array;
      Packages   : Package_Id  := No_Package;
   end record;
   --  Contains the declarations (variables, single and array attributes,
   --  packages) for a project or a package in a project.

   No_Declarations : constant Declarations :=
     (Variables  => No_Variable,
      Attributes => No_Variable,
      Arrays     => No_Array,
      Packages   => No_Package);
   --  Default value of Declarations: indicates that there is no declarations

   type Package_Element is record
      Name   : Name_Id      := No_Name;
      Decl   : Declarations := No_Declarations;
      Parent : Package_Id   := No_Package;
      Next   : Package_Id   := No_Package;
   end record;
   --  A package (includes declarations that may include other packages)

   package Package_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Package_Element,
      Table_Index_Type     => Package_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100);
   --  The table that contains all packages

   type Language_Data;
   type Language_Ptr is access all Language_Data;
   --  Index of language data

   No_Language_Index : constant Language_Ptr := null;
   --  Constant indicating that there is no language data

   function Get_Language_From_Name
     (Project : Project_Id;
      Name    : String) return Language_Ptr;
   --  Get a language from a project. This might return null if no such
   --  language exists in the project

   Max_Header_Num : constant := 6150;
   type Header_Num is range 0 .. Max_Header_Num;
   --  Size for hash table below. The upper bound is an arbitrary value, the
   --  value here was chosen after testing to determine a good compromise
   --  between speed of access and memory usage.

   function Hash (Name : Name_Id)        return Header_Num;
   function Hash (Name : File_Name_Type) return Header_Num;
   function Hash (Name : Path_Name_Type) return Header_Num;
   function Hash (Project : Project_Id)  return Header_Num;
   --  Used for computing hash values for names put into hash tables

   type Language_Kind is (File_Based, Unit_Based);
   --  Type for the kind of language. All languages are file based, except Ada
   --  which is unit based.

   type Dependency_File_Kind is (None, Makefile, ALI_File);
   --  Type of dependency to be checked: no dependency file, Makefile fragment
   --  or ALI file (for Ada).

   Makefile_Dependency_Suffix : constant String := ".d";
   ALI_Dependency_Suffix      : constant String := ".ali";

   Switches_Dependency_Suffix : constant String := ".cswi";

   Binder_Exchange_Suffix     : constant String := ".bexch";
   --  Suffix for binder exchange files

   Library_Exchange_Suffix     : constant String := ".lexch";
   --  Suffix for library exchange files

   type Name_List_Index is new Nat;
   No_Name_List            : constant Name_List_Index := 0;

   type Name_Node is record
      Name : Name_Id         := No_Name;
      Next : Name_List_Index := No_Name_List;
   end record;

   package Name_List_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Name_Node,
      Table_Index_Type     => Name_List_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  The table for lists of names

   function Length
     (Table : Name_List_Table.Instance;
      List  : Name_List_Index) return Natural;
   --  Return the number of elements in specified list

   type Number_List_Index is new Nat;
   No_Number_List : constant Number_List_Index := 0;

   type Number_Node is record
      Number : Natural           := 0;
      Next   : Number_List_Index := No_Number_List;
   end record;

   package Number_List_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Number_Node,
      Table_Index_Type     => Number_List_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  The table for lists of numbers

   package Mapping_Files_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Path_Name_Type,
      No_Element => No_Path,
      Key        => Path_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store the mapping files that are not used

   --  The following record ???

   type Lang_Naming_Data is record
      Dot_Replacement : File_Name_Type := No_File;
      --  The string to replace '.' in the source file name (for Ada)

      Casing : Casing_Type := All_Lower_Case;
      --  The casing of the source file name (for Ada)

      Separate_Suffix : File_Name_Type := No_File;
      --  String to append to unit name for source file name of an Ada subunit

      Spec_Suffix : File_Name_Type := No_File;
      --  The string to append to the unit name for the
      --  source file name of a spec.

      Body_Suffix : File_Name_Type := No_File;
      --  The string to append to the unit name for the
      --  source file name of a body.
   end record;

   No_Lang_Naming_Data : constant Lang_Naming_Data :=
                           (Dot_Replacement => No_File,
                            Casing          => All_Lower_Case,
                            Separate_Suffix => No_File,
                            Spec_Suffix     => No_File,
                            Body_Suffix     => No_File);

   function Is_Standard_GNAT_Naming (Naming : Lang_Naming_Data) return Boolean;
   --  True if the naming scheme is GNAT's default naming scheme. This
   --  is to take into account shortened names like "Ada." (a-), "System." (s-)
   --  and so on.

   type Source_Data;
   type Source_Id is access all Source_Data;

   function Is_Compilable (Source : Source_Id) return Boolean;
   pragma Inline (Is_Compilable);
   --  Return True if we know how to compile Source (i.e. if a compiler is
   --  defined). This doesn't indicate whether the source should be compiled.

   function Object_To_Global_Archive (Source : Source_Id) return Boolean;
   pragma Inline (Object_To_Global_Archive);
   --  Return True if the object file should be put in the global archive.
   --  This is for Ada, when only the closure of a main needs to be
   --  (re)compiled.

   function Other_Part (Source : Source_Id) return Source_Id;
   pragma Inline (Other_Part);
   --  Source ID for the other part, if any: for a spec, indicates its body;
   --  for a body, indicates its spec.

   No_Source : constant Source_Id := null;

   type Path_Syntax_Kind is
     (Canonical,
      --  Unix style
      Host);
      --  Host specific syntax, for example on VMS (the default)

   --  The following record describes the configuration of a language

   type Language_Config is record
      Kind : Language_Kind := File_Based;
      --  Kind of language. All languages are file based, except Ada which is
      --  unit based.

      Naming_Data : Lang_Naming_Data;
      --  The naming data for the languages (prefixes, etc.)

      Include_Compatible_Languages : Name_List_Index := No_Name_List;
      --  List of languages that are "include compatible" with this language. A
      --  language B (for example "C") is "include compatible" with a language
      --  A (for example "C++") if it is expected that sources of language A
      --  may "include" header files from language B.

      Compiler_Driver : File_Name_Type := No_File;
      --  The name of the executable for the compiler of the language

      Compiler_Driver_Path : String_Access := null;
      --  The path name of the executable for the compiler of the language

      Compiler_Leading_Required_Switches : Name_List_Index := No_Name_List;
      --  The list of initial switches that are required as a minimum to invoke
      --  the compiler driver.

      Compiler_Trailing_Required_Switches : Name_List_Index := No_Name_List;
      --  The list of final switches that are required as a minimum to invoke
      --  the compiler driver.

      Multi_Unit_Switches : Name_List_Index := No_Name_List;
      --  The switch(es) to indicate the index of a unit in a multi-source file

      Multi_Unit_Object_Separator : Character := ' ';
      --  The string separating the base name of a source from the index of the
      --  unit in a multi-source file, in the object file name.

      Path_Syntax : Path_Syntax_Kind := Host;
      --  Value may be Canonical (Unix style) or Host (host syntax, for example
      --  on VMS for DEC C).

      Object_File_Suffix : Name_Id := No_Name;
      --  Optional alternate object file suffix

      Object_File_Switches : Name_List_Index := No_Name_List;
      --  Optional object file switches. When this is defined, the switches
      --  are used to specify the object file. The object file name is appended
      --  to the last switch in the list. Example: ("-o", "").

      Compilation_PIC_Option : Name_List_Index := No_Name_List;
      --  The option(s) to compile a source in Position Independent Code for
      --  shared libraries. Specified in the configuration. When not specified,
      --  there is no need for such switch.

      Object_Generated : Boolean := True;
      --  False in no object file is generated

      Objects_Linked : Boolean := True;
      --  False if object files are not use to link executables and build
      --  libraries.

      Runtime_Library_Dir : Name_Id := No_Name;
      --  Path name of the runtime library directory, if any

      Runtime_Source_Dir : Name_Id := No_Name;
      --  Path name of the runtime source directory, if any

      Mapping_File_Switches : Name_List_Index := No_Name_List;
      --  The option(s) to provide a mapping file to the compiler. Specified in
      --  the configuration. When value is No_Name_List, there is no mapping
      --  file.

      Mapping_Spec_Suffix : File_Name_Type := No_File;
      --  Placeholder representing the spec suffix in a mapping file

      Mapping_Body_Suffix : File_Name_Type := No_File;
      --  Placeholder representing the body suffix in a mapping file

      Config_File_Switches : Name_List_Index := No_Name_List;
      --  The option(s) to provide a config file to the compiler. Specified in
      --  the configuration. If value is No_Name_List there is no config file.

      Dependency_Kind : Dependency_File_Kind := None;
      --  The kind of dependency to be checked: none, Makefile fragment or
      --  ALI file (for Ada).

      Dependency_Option : Name_List_Index := No_Name_List;
      --  The option(s) to be used to create the dependency file. When value is
      --  No_Name_List, there is not such option(s).

      Compute_Dependency : Name_List_Index := No_Name_List;
      --  Hold the value of attribute Dependency_Driver, if declared for the
      --  language.

      Include_Option : Name_List_Index := No_Name_List;
      --  Hold the value of attribute Include_Switches, if declared for the
      --  language.

      Include_Path : Name_Id := No_Name;
      --  Name of environment variable declared by attribute Include_Path for
      --  the language.

      Include_Path_File : Name_Id := No_Name;
      --  Name of environment variable declared by attribute Include_Path_File
      --  for the language.

      Objects_Path : Name_Id := No_Name;
      --  Name of environment variable declared by attribute Objects_Path for
      --  the language.

      Objects_Path_File : Name_Id := No_Name;
      --  Name of environment variable declared by attribute Objects_Path_File
      --  for the language.

      Config_Body : Name_Id := No_Name;
      --  The template for a pragma Source_File_Name(_Project) for a specific
      --  file name of a body.

      Config_Body_Index : Name_Id := No_Name;
      --  The template for a pragma Source_File_Name(_Project) for a specific
      --  file name of a body in a multi-source file.

      Config_Body_Pattern : Name_Id := No_Name;
      --  The template for a pragma Source_File_Name(_Project) for a naming
      --  body pattern.

      Config_Spec : Name_Id := No_Name;
      --  The template for a pragma Source_File_Name(_Project) for a specific
      --  file name of a spec.

      Config_Spec_Index : Name_Id := No_Name;
      --  The template for a pragma Source_File_Name(_Project) for a specific
      --  file name of a spec in a multi-source file.

      Config_Spec_Pattern : Name_Id := No_Name;
      --  The template for a pragma Source_File_Name(_Project) for a naming
      --  spec pattern.

      Config_File_Unique : Boolean := False;
      --  Indicate if the config file specified to the compiler needs to be
      --  unique. If it is unique, then all config files are concatenated into
      --  a temp config file.

      Binder_Driver : File_Name_Type := No_File;
      --  The name of the binder driver for the language, if any

      Binder_Driver_Path : Path_Name_Type := No_Path;
      --  The path name of the binder driver

      Binder_Required_Switches : Name_List_Index := No_Name_List;
      --  Hold the value of attribute Binder'Required_Switches for the language

      Binder_Prefix : Name_Id := No_Name;
      --  Hold the value of attribute Binder'Prefix for the language

      Toolchain_Version : Name_Id := No_Name;
      --  Hold the value of attribute Toolchain_Version for the language

      Toolchain_Description : Name_Id := No_Name;
      --  Hold the value of attribute Toolchain_Description for the language

   end record;

   No_Language_Config : constant Language_Config :=
                          (Kind                         => File_Based,
                           Naming_Data                  => No_Lang_Naming_Data,
                           Include_Compatible_Languages => No_Name_List,
                           Compiler_Driver              => No_File,
                           Compiler_Driver_Path         => null,
                           Compiler_Leading_Required_Switches  => No_Name_List,
                           Compiler_Trailing_Required_Switches => No_Name_List,
                           Multi_Unit_Switches          => No_Name_List,
                           Multi_Unit_Object_Separator  => ' ',
                           Path_Syntax                  => Canonical,
                           Object_File_Suffix           => No_Name,
                           Object_File_Switches         => No_Name_List,
                           Compilation_PIC_Option       => No_Name_List,
                           Object_Generated             => True,
                           Objects_Linked               => True,
                           Runtime_Library_Dir          => No_Name,
                           Runtime_Source_Dir           => No_Name,
                           Mapping_File_Switches        => No_Name_List,
                           Mapping_Spec_Suffix          => No_File,
                           Mapping_Body_Suffix          => No_File,
                           Config_File_Switches         => No_Name_List,
                           Dependency_Kind              => None,
                           Dependency_Option            => No_Name_List,
                           Compute_Dependency           => No_Name_List,
                           Include_Option               => No_Name_List,
                           Include_Path                 => No_Name,
                           Include_Path_File            => No_Name,
                           Objects_Path                 => No_Name,
                           Objects_Path_File            => No_Name,
                           Config_Body                  => No_Name,
                           Config_Body_Index            => No_Name,
                           Config_Body_Pattern          => No_Name,
                           Config_Spec                  => No_Name,
                           Config_Spec_Index            => No_Name,
                           Config_Spec_Pattern          => No_Name,
                           Config_File_Unique           => False,
                           Binder_Driver                => No_File,
                           Binder_Driver_Path           => No_Path,
                           Binder_Required_Switches     => No_Name_List,
                           Binder_Prefix                => No_Name,
                           Toolchain_Version            => No_Name,
                           Toolchain_Description        => No_Name);

   --  The following record ???

   type Language_Data is record
      Name          : Name_Id         := No_Name;
      Display_Name  : Name_Id         := No_Name;
      Config        : Language_Config := No_Language_Config;
      First_Source  : Source_Id       := No_Source;
      Mapping_Files : Mapping_Files_Htable.Instance :=
                        Mapping_Files_Htable.Nil;
      Next          : Language_Ptr  := No_Language_Index;
   end record;

   No_Language_Data : constant Language_Data :=
                        (Name          => No_Name,
                         Display_Name  => No_Name,
                         Config        => No_Language_Config,
                         First_Source  => No_Source,
                         Mapping_Files => Mapping_Files_Htable.Nil,
                         Next          => No_Language_Index);

   type Language_List_Element;
   type Language_List is access all Language_List_Element;
   type Language_List_Element is record
      Language : Language_Ptr := No_Language_Index;
      Next     : Language_List;
   end record;

   type Source_Kind is (Spec, Impl, Sep);
   subtype Spec_Or_Body is Source_Kind range Spec .. Impl;

   --  The following declarations declare a structure used to store the Name
   --  and File and Path names of a unit, with a reference to its GNAT Project
   --  File(s). Some units might have neither Spec nor Impl when they were
   --  created for a "separate".

   type File_Names_Data is array (Spec_Or_Body) of Source_Id;

   type Unit_Data is record
      Name       : Name_Id := No_Name;
      File_Names : File_Names_Data;
   end record;

   type Unit_Index is access all Unit_Data;

   No_Unit_Index : constant Unit_Index := null;
   --  Used to indicate a null entry for no unit

   --  Structure to define source data

   type Source_Data is record
      Project : Project_Id := No_Project;
      --  Project of the source

      Source_Dir_Rank : Natural := 0;
      --  The rank of the source directory in list declared with attribute
      --  Source_Dirs. Two source files with the same name cannot appears in
      --  different directory with the same rank. That can happen when the
      --  recursive notation <dir>/** is used in attribute Source_Dirs.

      Language : Language_Ptr := No_Language_Index;
      --  Index of the language. This is an index into
      --  Project_Tree.Languages_Data.

      In_Interfaces : Boolean := True;
      --  False when the source is not included in interfaces, when attribute
      --  Interfaces is declared.

      Declared_In_Interfaces : Boolean := False;
      --  True when source is declared in attribute Interfaces

      Alternate_Languages : Language_List := null;
      --  List of languages a header file may also be, in addition of language
      --  Language_Name.

      Kind : Source_Kind := Spec;
      --  Kind of the source: spec, body or subunit

      Unit : Unit_Index := No_Unit_Index;
      --  Name of the unit, if language is unit based. This is only set for
      --  those files that are part of the compilation set (for instance a
      --  file in an extended project that is overridden will not have this
      --  field set).

      Index : Int := 0;
      --  Index of the source in a multi unit source file (the same Source_Data
      --  is duplicated several times when there are several units in the same
      --  file). Index is 0 if there is either no unit or a single one, and
      --  starts at 1 when there are multiple units

      Locally_Removed : Boolean := False;
      --  True if the source has been "excluded"

      Replaced_By : Source_Id := No_Source;
      --  Missing comment ???

      File : File_Name_Type := No_File;
      --  Canonical file name of the source

      Display_File : File_Name_Type := No_File;
      --  File name of the source, for display purposes

      Path : Path_Information := No_Path_Information;
      --  Path name of the source

      Source_TS : Time_Stamp_Type := Empty_Time_Stamp;
      --  Time stamp of the source file

      Object_Project : Project_Id := No_Project;
      --  Project where the object file is. This might be different from
      --  Project when using extending project files.

      Object : File_Name_Type := No_File;
      --  File name of the object file

      Current_Object_Path : Path_Name_Type := No_Path;
      --  Object path of an existing object file

      Object_Path : Path_Name_Type := No_Path;
      --  Object path of the real object file

      Object_TS : Time_Stamp_Type := Empty_Time_Stamp;
      --  Object file time stamp

      Dep_Name : File_Name_Type := No_File;
      --  Dependency file simple name

      Current_Dep_Path : Path_Name_Type := No_Path;
      --  Path name of an existing dependency file

      Dep_Path : Path_Name_Type := No_Path;
      --  Path name of the real dependency file

      Dep_TS : aliased Osint.File_Attributes := Osint.Unknown_Attributes;
      --  Dependency file time stamp

      Switches : File_Name_Type := No_File;
      --  File name of the switches file. For all languages, this is a file
      --  that ends with the .cswi extension.

      Switches_Path : Path_Name_Type := No_Path;
      --  Path name of the switches file

      Switches_TS : Time_Stamp_Type := Empty_Time_Stamp;
      --  Switches file time stamp

      Naming_Exception : Boolean := False;
      --  True if the source has an exceptional name

      Next_In_Lang : Source_Id := No_Source;
      --  Link to another source of the same language in the same project
   end record;

   No_Source_Data : constant Source_Data :=
                      (Project                => No_Project,
                       Source_Dir_Rank        => 0,
                       Language               => No_Language_Index,
                       In_Interfaces          => True,
                       Declared_In_Interfaces => False,
                       Alternate_Languages    => null,
                       Kind                   => Spec,
                       Unit                   => No_Unit_Index,
                       Index                  => 0,
                       Locally_Removed        => False,
                       Replaced_By            => No_Source,
                       File                   => No_File,
                       Display_File           => No_File,
                       Path                   => No_Path_Information,
                       Source_TS              => Empty_Time_Stamp,
                       Object_Project         => No_Project,
                       Object                 => No_File,
                       Current_Object_Path    => No_Path,
                       Object_Path            => No_Path,
                       Object_TS              => Empty_Time_Stamp,
                       Dep_Name               => No_File,
                       Current_Dep_Path       => No_Path,
                       Dep_Path               => No_Path,
                       Dep_TS                 => Osint.Unknown_Attributes,
                       Switches               => No_File,
                       Switches_Path          => No_Path,
                       Switches_TS            => Empty_Time_Stamp,
                       Naming_Exception       => False,
                       Next_In_Lang           => No_Source);

   package Source_Paths_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Source_Id,
      No_Element => No_Source,
      Key        => Path_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping of source paths to source ids

   package Unit_Sources_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Source_Id,
      No_Element => No_Source,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

   type Verbosity is (Default, Medium, High);
   --  Verbosity when parsing GNAT Project Files
   --    Default is default (very quiet, if no errors).
   --    Medium is more verbose.
   --    High is extremely verbose.

   Current_Verbosity : Verbosity := Default;
   --  The current value of the verbosity the project files are parsed with

   type Lib_Kind is (Static, Dynamic, Relocatable);

   type Policy is (Autonomous, Compliant, Controlled, Restricted, Direct);
   --  Type to specify the symbol policy, when symbol control is supported.
   --  See full explanation about this type in package Symbols.
   --    Autonomous: Create a symbol file without considering any reference
   --    Compliant:  Try to be as compatible as possible with an existing ref
   --    Controlled: Fail if symbols are not the same as those in the reference
   --    Restricted: Restrict the symbols to those in the symbol file
   --    Direct:     The symbol file is used as is

   type Symbol_Record is record
      Symbol_File   : Path_Name_Type := No_Path;
      Reference     : Path_Name_Type := No_Path;
      Symbol_Policy : Policy  := Autonomous;
   end record;
   --  Type to keep the symbol data to be used when building a shared library

   No_Symbols : constant Symbol_Record :=
     (Symbol_File   => No_Path,
      Reference     => No_Path,
      Symbol_Policy => Autonomous);
   --  The default value of the symbol data

   function Image (The_Casing : Casing_Type) return String;
   --  Similar to 'Image (but avoid use of this attribute in compiler)

   function Value (Image : String) return Casing_Type;
   --  Similar to 'Value (but avoid use of this attribute in compiler)
   --  Raises Constraint_Error if not a Casing_Type image.

   --  The following record contains data for a naming scheme

   function Get_Object_Directory
     (Project             : Project_Id;
      Including_Libraries : Boolean;
      Only_If_Ada         : Boolean := False) return Path_Name_Type;
   --  Return the object directory to use for the project. This depends on
   --  whether we have a library project or a standard project. This function
   --  might return No_Name when no directory applies.
   --  If we have a a library project file and Including_Libraries is True then
   --  the library dir is returned instead of the object dir.
   --  If Only_If_Ada is True, then No_Name will be returned when the project
   --  doesn't Ada sources.

   procedure Compute_All_Imported_Projects (Tree : Project_Tree_Ref);
   --  For all projects in the tree, compute the list of the projects imported
   --  directly or indirectly by project Project. The result is stored in
   --  Project.All_Imported_Projects for each project

   function Ultimate_Extending_Project_Of
     (Proj : Project_Id) return Project_Id;
   --  Returns the ultimate extending project of project Proj. If project Proj
   --  is not extended, returns Proj.

   type Project_List_Element;
   type Project_List is access all Project_List_Element;
   type Project_List_Element is record
      Project : Project_Id   := No_Project;
      Next    : Project_List := null;
   end record;
   --  A list of projects

   procedure Free_List
     (List         : in out Project_List;
      Free_Project : Boolean);
   --  Free the list of projects, if Free_Project, each project is also freed

   type Response_File_Format is
     (None,
      GNU,
      Object_List,
      Option_List);
   --  The format of the different response files

   type Project_Configuration is record
      Target : Name_Id := No_Name;
      --  The target of the configuration, when specified

      Run_Path_Option : Name_List_Index := No_Name_List;
      --  The option to use when linking to specify the path where to look for
      --  libraries.

      Run_Path_Origin_Supported : Boolean := False;
      --  Specify if the run path option support $ORIGIN to indicate paths
      --  reative to the directory of the executable.

      Separate_Run_Path_Options : Boolean := False;
      --  True if each directory needs to be specified in a separate run path
      --  option.

      Executable_Suffix : Name_Id := No_Name;
      --  The suffix of executables, when specified in the configuration or in
      --  package Builder of the main project. When this is not specified, the
      --  executable suffix is the default for the platform.

      --  Linking

      Linker : Path_Name_Type := No_Path;
      --  Path name of the linker driver. Specified in the configuration or in
      --  the package Builder of the main project.

      Map_File_Option : Name_Id := No_Name;
      --  Option to use when invoking the linker to build a map file

      Minimum_Linker_Options : Name_List_Index := No_Name_List;
      --  The minimum options for the linker driver. Specified in the
      --  configuration.

      Linker_Executable_Option : Name_List_Index := No_Name_List;
      --  The option(s) to indicate the name of the executable in the linker
      --  command. Specified in the configuration. When not specified, default
      --  to -o <executable name>.

      Linker_Lib_Dir_Option : Name_Id := No_Name;
      --  The option to specify where to find a library for linking. Specified
      --  in the configuration. When not specified, defaults to "-L".

      Linker_Lib_Name_Option : Name_Id := No_Name;
      --  The option to specify the name of a library for linking. Specified in
      --  the configuration. When not specified, defaults to "-l".

      Max_Command_Line_Length : Natural := 0;
      --  When positive and when Resp_File_Format (see below) is not None,
      --  if the command line for the invocation of the linker would be greater
      --  than this value, a response file is used to invoke the linker.

      Resp_File_Format : Response_File_Format := None;
      --  The format of a response file, when linking with a response file is
      --  supported.

      Resp_File_Options : Name_List_Index := No_Name_List;
      --  The switches, if any, that precede the path name of the response
      --  file in the invocation of the linker.

      --  Libraries

      Library_Builder : Path_Name_Type  := No_Path;
      --  The executable to build library (specified in the configuration)

      Lib_Support : Library_Support := None;
      --  The level of library support. Specified in the configuration. Support
      --  is none, static libraries only or both static and shared libraries.

      Archive_Builder : Name_List_Index := No_Name_List;
      --  The name of the executable to build archives, with the minimum
      --  switches. Specified in the configuration.

      Archive_Builder_Append_Option : Name_List_Index := No_Name_List;
      --  The options to append object files to an archive

      Archive_Indexer : Name_List_Index := No_Name_List;
      --  The name of the executable to index archives, with the minimum
      --  switches. Specified in the configuration.

      Archive_Suffix : File_Name_Type := No_File;
      --  The suffix of archives. Specified in the configuration. When not
      --  specified, defaults to ".a".

      Lib_Partial_Linker : Name_List_Index := No_Name_List;

      --  Shared libraries

      Shared_Lib_Driver : File_Name_Type := No_File;
      --  The driver to link shared libraries. Set with attribute Library_GCC.
      --  Default to gcc.

      Shared_Lib_Prefix : File_Name_Type := No_File;
      --  Part of a shared library file name that precedes the name of the
      --  library. Specified in the configuration. When not specified, defaults
      --  to "lib".

      Shared_Lib_Suffix : File_Name_Type := No_File;
      --  Suffix of shared libraries, after the library name in the shared
      --  library name. Specified in the configuration. When not specified,
      --  default to ".so".

      Shared_Lib_Min_Options : Name_List_Index := No_Name_List;
      --  The minimum options to use when building a shared library

      Lib_Version_Options : Name_List_Index := No_Name_List;
      --  The options to use to specify a library version

      Symbolic_Link_Supported : Boolean := False;
      --  True if the platform supports symbolic link files

      Lib_Maj_Min_Id_Supported : Boolean := False;
      --  True if platform supports library major and minor options, such as
      --  libname.so -> libname.so.2 -> libname.so.2.4

      Auto_Init_Supported : Boolean := False;
      --  True if automatic initialisation is supported for shared stand-alone
      --  libraries.
   end record;

   Default_Project_Config : constant Project_Configuration :=
                              (Target                        => No_Name,
                               Run_Path_Option               => No_Name_List,
                               Run_Path_Origin_Supported     => False,
                               Separate_Run_Path_Options     => False,
                               Executable_Suffix             => No_Name,
                               Linker                        => No_Path,
                               Map_File_Option               => No_Name,
                               Minimum_Linker_Options        => No_Name_List,
                               Linker_Executable_Option      => No_Name_List,
                               Linker_Lib_Dir_Option         => No_Name,
                               Linker_Lib_Name_Option        => No_Name,
                               Library_Builder               => No_Path,
                               Max_Command_Line_Length       => 0,
                               Resp_File_Format              => None,
                               Resp_File_Options             => No_Name_List,
                               Lib_Support                   => None,
                               Archive_Builder               => No_Name_List,
                               Archive_Builder_Append_Option => No_Name_List,
                               Archive_Indexer               => No_Name_List,
                               Archive_Suffix                => No_File,
                               Lib_Partial_Linker            => No_Name_List,
                               Shared_Lib_Driver             => No_File,
                               Shared_Lib_Prefix             => No_File,
                               Shared_Lib_Suffix             => No_File,
                               Shared_Lib_Min_Options        => No_Name_List,
                               Lib_Version_Options           => No_Name_List,
                               Symbolic_Link_Supported       => False,
                               Lib_Maj_Min_Id_Supported      => False,
                               Auto_Init_Supported           => False);

   --  The following record describes a project file representation

   --  Note that it is not specified if the path names of directories (source,
   --  object, library or exec directories) end with or without a directory
   --  separator.

   type Project_Data is record

      -------------
      -- General --
      -------------

      Name : Name_Id := No_Name;
      --  The name of the project

      Display_Name : Name_Id := No_Name;
      --  The name of the project with the spelling of its declaration

      Qualifier : Project_Qualifier := Unspecified;
      --  The eventual qualifier for this project

      Externally_Built : Boolean := False;
      --  True if the project is externally built. In such case, the Project
      --  Manager will not modify anything in this project.

      Config : Project_Configuration;

      Path : Path_Information := No_Path_Information;
      --  The path name of the project file. This include base name of the
      --  project file.

      Virtual : Boolean := False;
      --  True for virtual extending projects

      Location : Source_Ptr := No_Location;
      --  The location in the project file source of the reserved word project

      ---------------
      -- Languages --
      ---------------

      Languages : Language_Ptr := No_Language_Index;
      --  First index of the language data in the project.
      --  This is an index into the project_tree_data.languages_data.
      --  Traversing the list gives access to all the languages supported by
      --  the project.

      --------------
      -- Projects --
      --------------

      Mains : String_List_Id := Nil_String;
      --  List of mains specified by attribute Main

      Extends : Project_Id := No_Project;
      --  The reference of the project file, if any, that this project file
      --  extends.

      Extended_By : Project_Id := No_Project;
      --  The reference of the project file, if any, that extends this project
      --  file.

      Decl : Declarations := No_Declarations;
      --  The declarations (variables, attributes and packages) of this project
      --  file.

      Imported_Projects : Project_List;
      --  The list of all directly imported projects, if any

      All_Imported_Projects : Project_List;
      --  The list of all projects imported directly or indirectly, if any.
      --  This does not include the project itself.

      -----------------
      -- Directories --
      -----------------

      Directory : Path_Information := No_Path_Information;
      --  Path name of the directory where the project file resides

      Object_Directory : Path_Information := No_Path_Information;
      --  The path name of the object directory of this project file

      Exec_Directory : Path_Information := No_Path_Information;
      --  The path name of the exec directory of this project file. Default is
      --  equal to Object_Directory.

      -------------
      -- Library --
      -------------

      Library : Boolean := False;
      --  True if this is a library project

      Library_Name : Name_Id := No_Name;
      --  If a library project, name of the library

      Library_Kind : Lib_Kind := Static;
      --  If a library project, kind of library

      Library_Dir : Path_Information := No_Path_Information;
      --  If a library project, path name of the directory where the library
      --  resides.

      Library_TS : Time_Stamp_Type := Empty_Time_Stamp;
      --  The timestamp of a library file in a library project

      Library_Src_Dir : Path_Information := No_Path_Information;
      --  If a Stand-Alone Library project, path name of the directory where
      --  the sources of the interfaces of the library are copied. By default,
      --  if attribute Library_Src_Dir is not specified, sources of the
      --  interfaces are not copied anywhere.

      Library_ALI_Dir : Path_Information := No_Path_Information;
      --  In a library project, path name of the directory where the ALI files
      --  are copied. If attribute Library_ALI_Dir is not specified, ALI files
      --  are copied in the Library_Dir.

      Lib_Internal_Name : Name_Id := No_Name;
      --  If a library project, internal name store inside the library

      Standalone_Library : Boolean := False;
      --  Indicate that this is a Standalone Library Project File

      Lib_Interface_ALIs : String_List_Id := Nil_String;
      --  For Standalone Library Project Files, indicate the list of Interface
      --  ALI files.

      Lib_Auto_Init : Boolean := False;
      --  For non static Stand-Alone Library Project Files, indicate if
      --  the library initialisation should be automatic.

      Symbol_Data : Symbol_Record := No_Symbols;
      --  Symbol file name, reference symbol file name, symbol policy

      Need_To_Build_Lib : Boolean := False;
      --  Indicates that the library of a Library Project needs to be built or
      --  rebuilt.

      -------------
      -- Sources --
      -------------
      --  The sources for all languages including Ada are accessible through
      --  the Source_Iterator type

      Interfaces_Defined : Boolean := False;
      --  True if attribute Interfaces is declared for the project or any
      --  project it extends.

      Include_Path_File : Path_Name_Type := No_Path;
      --  The path name of the of the source search directory file.
      --  This is only used by gnatmake

      Source_Dirs : String_List_Id := Nil_String;
      --  The list of all the source directories

      Source_Dir_Ranks : Number_List_Index := No_Number_List;

      Ada_Include_Path : String_Access := null;
      --  The cached value of source search path for this project file. Set by
      --  the first call to Prj.Env.Ada_Include_Path for the project. Do not
      --  use this field directly outside of the project manager, use
      --  Prj.Env.Ada_Include_Path instead.

      Has_Multi_Unit_Sources : Boolean := False;
      --  Whether there is at least one source file containing multiple units

      -------------------
      -- Miscellaneous --
      -------------------

      Ada_Objects_Path : String_Access := null;
      --  The cached value of ADA_OBJECTS_PATH for this project file. Do not
      --  use this field directly outside of the compiler, use
      --  Prj.Env.Ada_Objects_Path instead.

      Libgnarl_Needed : Yes_No_Unknown := Unknown;
      --  Set to True when libgnarl is needed to link

      Objects_Path : String_Access := null;
      --  The cached value of the object dir path, used during the binding
      --  phase of gprbuild.

      Objects_Path_File_With_Libs : Path_Name_Type := No_Path;
      --  The cached value of the object path temp file (including library
      --  dirs) for this project file.

      Objects_Path_File_Without_Libs : Path_Name_Type := No_Path;
      --  The cached value of the object path temp file (excluding library
      --  dirs) for this project file.

      Config_File_Name : Path_Name_Type := No_Path;
      --  The path name of the configuration pragmas file, if any

      Config_File_Temp : Boolean := False;
      --  An indication that the configuration pragmas file is a temporary file
      --  that must be deleted at the end.

      Config_Checked : Boolean := False;
      --  A flag to avoid checking repetitively the configuration pragmas file

      Depth : Natural := 0;
      --  The maximum depth of a project in the project graph. Depth of main
      --  project is 0.

      Unkept_Comments : Boolean := False;
      --  True if there are comments in the project sources that cannot be kept
      --  in the project tree.

   end record;

   function Empty_Project return Project_Data;
   --  Return the representation of an empty project

   function Is_Extending
     (Extending : Project_Id;
      Extended  : Project_Id) return Boolean;
   --  Return True if Extending is extending the Extended project

   function Has_Ada_Sources (Data : Project_Id) return Boolean;
   --  Return True if the project has Ada sources

   Project_Error : exception;
   --  Raised by some subprograms in Prj.Attr

   package Units_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Unit_Index,
      No_Element => No_Unit_Index,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping of unit names to indexes in the Units table

   ---------------------
   -- Source_Iterator --
   ---------------------

   type Source_Iterator is private;

   function For_Each_Source
     (In_Tree  : Project_Tree_Ref;
      Project  : Project_Id := No_Project;
      Language : Name_Id := No_Name) return Source_Iterator;
   --  Returns an iterator for all the sources of a project tree, or a specific
   --  project, or a specific language.

   function Element (Iter : Source_Iterator) return Source_Id;
   --  Return the current source (or No_Source if there are no more sources)

   procedure Next (Iter : in out Source_Iterator);
   --  Move on to the next source

   function Find_Source
     (In_Tree          : Project_Tree_Ref;
      Project          : Project_Id;
      In_Imported_Only : Boolean := False;
      In_Extended_Only : Boolean := False;
      Base_Name        : File_Name_Type) return Source_Id;
   --  Find the first source file with the given name either in the whole tree
   --  (if In_Imported_Only is False) or in the projects imported or extended
   --  by Project otherwise. In_Extended_Only implies In_Imported_Only, and
   --  will only look in Project and the projects it extends

   -----------------------
   -- Project_Tree_Data --
   -----------------------

   type Private_Project_Tree_Data is private;
   --  Data for a project tree that is used only by the Project Manager

   type Project_Tree_Data is
      record
         Name_Lists        : Name_List_Table.Instance;
         Number_Lists      : Number_List_Table.Instance;
         String_Elements   : String_Element_Table.Instance;
         Variable_Elements : Variable_Element_Table.Instance;
         Array_Elements    : Array_Element_Table.Instance;
         Arrays            : Array_Table.Instance;
         Packages          : Package_Table.Instance;
         Projects          : Project_List;

         Units_HT          : Units_Htable.Instance;
         --  Unit name to Unit_Index (and from there so Source_Id)

         Source_Paths_HT   : Source_Paths_Htable.Instance;
         --  Full path to Source_Id

         Private_Part      : Private_Project_Tree_Data;
      end record;
   --  Data for a project tree

   procedure Expect (The_Token : Token_Type; Token_Image : String);
   --  Check that the current token is The_Token. If it is not, then output
   --  an error message.

   procedure Initialize (Tree : Project_Tree_Ref);
   --  This procedure must be called before using any services from the Prj
   --  hierarchy. Namet.Initialize must be called before Prj.Initialize.

   procedure Reset (Tree : Project_Tree_Ref);
   --  This procedure resets all the tables that are used when processing a
   --  project file tree. Initialize must be called before the call to Reset.

   package Project_Boolean_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Project_Id,
      Hash       => Hash,
      Equal      => "=");
   --  A table that associates a project to a boolean. This is used to detect
   --  whether a project was already processed for instance.

   generic
      type State is limited private;
      with procedure Action
        (Project    : Project_Id;
         With_State : in out State);
   procedure For_Every_Project_Imported
     (By             : Project_Id;
      With_State     : in out State;
      Imported_First : Boolean := False);
   --  Call Action for each project imported directly or indirectly by project
   --  By, as well as extended projects.
   --  The order of processing depends on Imported_First:
   --  If False, Action is called according to the order of importation: if A
   --  imports B, directly or indirectly, Action will be called for A before
   --  it is called for B. If two projects import each other directly or
   --  indirectly (using at least one "limited with"), it is not specified
   --  for which of these two projects Action will be called first.
   --  The order is reversed if Imported_First is True.
   --  With_State may be used by Action to choose a behavior or to report some
   --  global result.

   function Extend_Name
     (File        : File_Name_Type;
      With_Suffix : String) return File_Name_Type;
   --  Replace the extension of File with With_Suffix

   function Object_Name
     (Source_File_Name   : File_Name_Type;
      Object_File_Suffix : Name_Id := No_Name) return File_Name_Type;
   --  Returns the object file name corresponding to a source file name

   function Object_Name
     (Source_File_Name   : File_Name_Type;
      Source_Index       : Int;
      Index_Separator    : Character;
      Object_File_Suffix : Name_Id := No_Name) return File_Name_Type;
   --  Returns the object file name corresponding to a unit in a multi-source
   --  file.

   function Dependency_Name
     (Source_File_Name : File_Name_Type;
      Dependency       : Dependency_File_Kind) return File_Name_Type;
   --  Returns the dependency file name corresponding to a source file name

   function Switches_Name
     (Source_File_Name : File_Name_Type) return File_Name_Type;
   --  Returns the switches file name corresponding to a source file name

   -----------
   -- Flags --
   -----------

   type Processing_Flags is private;
   --  Flags used while parsing and processing a project tree to configure the
   --  behavior of the parser, and indicate how to report error messages. This
   --  structure does not allocate memory and never needs to be freed

   type Error_Warning is (Silent, Warning, Error);
   --  Severity of some situations, such as: no Ada sources in a project where
   --  Ada is one of the language.
   --
   --  When the situation occurs, the behaviour depends on the setting:
   --
   --    - Silent:  no action
   --    - Warning: issue a warning, does not cause the tool to fail
   --    - Error:   issue an error, causes the tool to fail

   type Error_Handler is access procedure
     (Project    : Project_Id;
      Is_Warning : Boolean);
   --  This warngs when an error was found when parsing a project. The error
   --  itself is handled through Prj.Err (and Prj.Err.Finalize should be called
   --  to actually print the error). This ensures that duplicate error messages
   --  are always correctly removed, that errors msgs are sorted, and that all
   --  tools will report the same error to the user.

   function Create_Flags
     (Report_Error               : Error_Handler;
      When_No_Sources            : Error_Warning;
      Require_Sources_Other_Lang : Boolean := True;
      Allow_Duplicate_Basenames  : Boolean := True;
      Compiler_Driver_Mandatory  : Boolean := False;
      Error_On_Unknown_Language  : Boolean := True;
      Require_Obj_Dirs           : Error_Warning := Error)
      return Processing_Flags;
   --  Function used to create Processing_Flags structure
   --
   --  If Allow_Duplicate_Basenames, then files with the same base names are
   --  authorized within a project for source-based languages (never for unit
   --  based languages).
   --
   --  If Compiler_Driver_Mandatory is true, then a Compiler.Driver attribute
   --  for each language must be defined, or we will not look for its source
   --  files.
   --
   --  When_No_Sources indicates what should be done when no sources of a
   --  language are found in a project where this language is declared.
   --  If Require_Sources_Other_Lang is true, then all languages must have at
   --  least one source file, or an error is reported via When_No_Sources. If
   --  it is false, this is only required for Ada (and only if it is a language
   --  of the project). When this parameter is set to False, we do not check
   --  that a proper naming scheme is defined for languages other than Ada.
   --
   --  If Report_Error is null, use the standard error reporting mechanism
   --  (Errout). Otherwise, report errors using Report_Error.
   --
   --  If Error_On_Unknown_Language is true, an error is displayed if some of
   --  the source files listed in the project do not match any naming scheme
   --
   --  If Require_Obj_Dirs is true, then all object directories must exist
   --  (possibly after they have been created automatically if the appropriate
   --  switches were specified), or an error is raised.

   Gprbuild_Flags : constant Processing_Flags;
   Gprclean_Flags : constant Processing_Flags;
   Gnatmake_Flags : constant Processing_Flags;
   --  Flags used by the various tools. They all display the error messages
   --  through Prj.Err.

   ----------------
   -- Temp Files --
   ----------------

   procedure Record_Temp_File
     (Tree : Project_Tree_Ref;
      Path : Path_Name_Type);
   --  Record the path of a newly created temporary file, so that it can be
   --  deleted later.

   procedure Delete_All_Temp_Files (Tree : Project_Tree_Ref);
   --  Delete all recorded temporary files.
   --  Does nothing if Debug.Debug_Flag_N is set

   procedure Delete_Temporary_File
     (Tree : Project_Tree_Ref;
      Path : Path_Name_Type);
   --  Delete a temporary file from the disk. The file is also removed from the
   --  list of temporary files to delete at the end of the program, in case
   --  another program running on the same machine has recreated it.
   --  Does nothing if Debug.Debug_Flag_N is set

private

   All_Packages : constant String_List_Access := null;

   No_Project_Tree : constant Project_Tree_Ref := null;

   Ignored : constant Variable_Kind := Single;

   Nil_Variable_Value : constant Variable_Value :=
                          (Project  => No_Project,
                           Kind     => Undefined,
                           Location => No_Location,
                           Default  => False);

   Virtual_Prefix : constant String := "v$";
   --  The prefix for virtual extending projects. Because of the '$', which is
   --  normally forbidden for project names, there cannot be any name clash.

   type Source_Iterator is record
      In_Tree : Project_Tree_Ref;

      Project      : Project_List;
      All_Projects : Boolean;
      --  Current project and whether we should move on to the next

      Language : Language_Ptr;
      --  Current language processed

      Language_Name : Name_Id;
      --  Only sources of this language will be returned (or all if No_Name)

      Current : Source_Id;
   end record;

   procedure Add_To_Buffer
     (S    : String;
      To   : in out String_Access;
      Last : in out Natural);
   --  Append a String to the Buffer

   package Temp_Files_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Path_Name_Type,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 10);
   --  Table to store the path name of all the created temporary files, so that
   --  they can be deleted at the end, or when the program is interrupted.

   type Private_Project_Tree_Data is record
      Temp_Files   : Temp_Files_Table.Instance;
      --  Temporary files created as part of running tools (pragma files,
      --  mapping files,...)

      Current_Source_Path_File : Path_Name_Type := No_Path;
      --  Current value of project source path file env var. Used to avoid
      --  setting the env var to the same value. When different from No_Path,
      --  this indicates that logical names (VMS) or environment variables were
      --  created and should be deassigned to avoid polluting the environment
      --  on VMS.
      --  gnatmake only

      Current_Object_Path_File : Path_Name_Type := No_Path;
      --  Current value of project object path file env var. Used to avoid
      --  setting the env var to the same value.
      --  gnatmake only

   end record;
   --  Type to represent the part of a project tree which is private to the
   --  Project Manager.

   type Processing_Flags is record
      Require_Sources_Other_Lang : Boolean;
      Report_Error               : Error_Handler;
      When_No_Sources            : Error_Warning;
      Allow_Duplicate_Basenames  : Boolean;
      Compiler_Driver_Mandatory  : Boolean;
      Error_On_Unknown_Language  : Boolean;
      Require_Obj_Dirs           : Error_Warning;
   end record;

   Gprbuild_Flags : constant Processing_Flags :=
     (Report_Error               => null,
      When_No_Sources            => Warning,
      Require_Sources_Other_Lang => True,
      Allow_Duplicate_Basenames  => False,
      Compiler_Driver_Mandatory  => True,
      Error_On_Unknown_Language  => True,
      Require_Obj_Dirs           => Error);

   Gprclean_Flags : constant Processing_Flags :=
     (Report_Error               => null,
      When_No_Sources            => Warning,
      Require_Sources_Other_Lang => True,
      Allow_Duplicate_Basenames  => False,
      Compiler_Driver_Mandatory  => True,
      Error_On_Unknown_Language  => True,
      Require_Obj_Dirs           => Warning);

   Gnatmake_Flags : constant Processing_Flags :=
     (Report_Error               => null,
      When_No_Sources            => Error,
      Require_Sources_Other_Lang => False,
      Allow_Duplicate_Basenames  => False,
      Compiler_Driver_Mandatory  => False,
      Error_On_Unknown_Language  => False,
      Require_Obj_Dirs           => Error);

end Prj;
