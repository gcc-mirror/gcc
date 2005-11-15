------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P R J                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2005, Free Software Foundation, Inc.         --
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

--  The following package declares the data types for GNAT project.
--  These data types may be used by GNAT Project-aware tools.

--  Children of these package implements various services on these data types.
--  See in particular Prj.Pars and Prj.Env.

with Casing; use Casing;
with Scans;  use Scans;
with Table;
with Types;  use Types;

with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;
with GNAT.Dynamic_Tables;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

with System.HTable;

package Prj is

   All_Packages : constant String_List_Access;
   --  Default value of parameter Packages of procedures Parse, in Prj.Pars and
   --  Prj.Part, indicating that all packages should be checked.

   type Project_Tree_Data;
   type Project_Tree_Ref is access all Project_Tree_Data;
   --  Reference to a project tree.
   --  Several project trees may exist in memory at the same time.

   No_Project_Tree : constant Project_Tree_Ref;

   function Default_Ada_Spec_Suffix return Name_Id;
   pragma Inline (Default_Ada_Spec_Suffix);
   --  The Name_Id for the standard GNAT suffix for Ada spec source file
   --  name ".ads". Initialized by Prj.Initialize.

   function Default_Ada_Body_Suffix return Name_Id;
   pragma Inline (Default_Ada_Body_Suffix);
   --  The Name_Id for the standard GNAT suffix for Ada body source file
   --  name ".adb". Initialized by Prj.Initialize.

   function Slash return Name_Id;
   pragma Inline (Slash);
   --  "/", used as the path of locally removed files

   Project_File_Extension : String := ".gpr";
   --  The standard project file name extension. It is not a constant, because
   --  Canonical_Case_File_Name is called on this variable in the body of Prj.

   -----------------------------------------------------
   -- Multi-language Stuff That Will be Modified Soon --
   -----------------------------------------------------

   --  Still should be properly commented ???

   type Language_Index is new Nat;

   No_Language_Index           : constant Language_Index := 0;
   First_Language_Index        : constant Language_Index := 1;
   First_Language_Indexes_Last : constant Language_Index := 5;

   Ada_Language_Index         : constant Language_Index :=
                                  First_Language_Index;
   C_Language_Index           : constant Language_Index :=
                                  Ada_Language_Index + 1;
   C_Plus_Plus_Language_Index : constant Language_Index :=
                                  C_Language_Index + 1;

   Last_Language_Index : Language_Index := No_Language_Index;

   subtype First_Language_Indexes is Language_Index
      range First_Language_Index .. First_Language_Indexes_Last;

   type Header_Num is range 0 .. 2047;

   function Hash is new System.HTable.Hash (Header_Num => Header_Num);

   function Hash (Name : Name_Id) return Header_Num;

   package Language_Indexes is new System.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Language_Index,
      No_Element => No_Language_Index,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping of language names to language indexes

   package Language_Names is new Table.Table
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Language_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 4,
      Table_Increment      => 100,
      Table_Name           => "Prj.Language_Names");
   --  The table for the name of programming languages

   procedure Add_Language_Name (Name : Name_Id);

   procedure Display_Language_Name (Language : Language_Index);

   type Languages_In_Project is array (First_Language_Indexes) of Boolean;
   --  Set of supported languages used in a project

   No_Languages : constant Languages_In_Project := (others => False);
   --  No supported languages are used

   type Supp_Language_Index is new Nat;
   No_Supp_Language_Index  : constant Supp_Language_Index := 0;

   type Supp_Language is record
      Index   : Language_Index := No_Language_Index;
      Present : Boolean := False;
      Next    : Supp_Language_Index := No_Supp_Language_Index;
   end record;

   package Present_Language_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Supp_Language,
      Table_Index_Type     => Supp_Language_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 4,
      Table_Increment      => 100);
   --  The table for the presence of languages with an index that is outside
   --  of First_Language_Indexes.

   type Impl_Suffix_Array is array (First_Language_Indexes) of Name_Id;
   --  Suffixes for the non spec sources of the different supported languages
   --  in a project.

   No_Impl_Suffixes : constant Impl_Suffix_Array := (others => No_Name);
   --  A default value for the non spec source suffixes

   type Supp_Suffix is record
      Index   : Language_Index := No_Language_Index;
      Suffix  : Name_Id := No_Name;
      Next    : Supp_Language_Index := No_Supp_Language_Index;
   end record;

   package Supp_Suffix_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Supp_Suffix,
      Table_Index_Type     => Supp_Language_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 4,
      Table_Increment      => 100);
   --  The table for the presence of languages with an index that is outside
   --  of First_Language_Indexes.

   type Language_Kind is (GNU, other);

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
   --  The table for lists of names used in package Language_Processing

   type Language_Processing_Data is record
      Compiler_Drivers     : Name_List_Index := No_Name_List;
      Compiler_Paths       : Name_Id         := No_Name;
      Compiler_Kinds       : Language_Kind   := GNU;
      Dependency_Options   : Name_List_Index := No_Name_List;
      Compute_Dependencies : Name_List_Index := No_Name_List;
      Include_Options      : Name_List_Index := No_Name_List;
      Binder_Drivers       : Name_Id         := No_Name;
      Binder_Driver_Paths  : Name_Id         := No_Name;
   end record;

   Default_Language_Processing_Data :
     constant Language_Processing_Data :=
       (Compiler_Drivers     => No_Name_List,
        Compiler_Paths       => No_Name,
        Compiler_Kinds       => GNU,
        Dependency_Options   => No_Name_List,
        Compute_Dependencies => No_Name_List,
        Include_Options      => No_Name_List,
        Binder_Drivers       => No_Name,
        Binder_Driver_Paths  => No_Name);

   type First_Language_Processing_Data is
     array (First_Language_Indexes) of Language_Processing_Data;

   Default_First_Language_Processing_Data :
      constant First_Language_Processing_Data :=
                 (others => Default_Language_Processing_Data);

   type Supp_Language_Data is record
      Index : Language_Index := No_Language_Index;
      Data  : Language_Processing_Data := Default_Language_Processing_Data;
      Next  : Supp_Language_Index := No_Supp_Language_Index;
   end record;

   package Supp_Language_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Supp_Language_Data,
      Table_Index_Type     => Supp_Language_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 4,
      Table_Increment      => 100);
   --  The table for language data when there are more languages than
   --  in First_Language_Indexes.

   type Other_Source_Id is new Nat;
   No_Other_Source : constant Other_Source_Id := 0;

   type Other_Source is record
      Language         : Language_Index;       --  language of the source
      File_Name        : Name_Id;              --  source file simple name
      Path_Name        : Name_Id;              --  source full path name
      Source_TS        : Time_Stamp_Type;      --  source file time stamp
      Object_Name      : Name_Id;              --  object file simple name
      Object_Path      : Name_Id;              --  object full path name
      Object_TS        : Time_Stamp_Type;      --  object file time stamp
      Dep_Name         : Name_Id;              --  dependency file simple name
      Dep_Path         : Name_Id;              --  dependency full path name
      Dep_TS           : Time_Stamp_Type;      --  dependency file time stamp
      Naming_Exception : Boolean := False;     --  True if a naming exception
      Next             : Other_Source_Id := No_Other_Source;
   end record;
   --  Data for a source in a language other than Ada

   package Other_Source_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Other_Source,
      Table_Index_Type     => Other_Source_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100);
   --  The table for sources of languages other than Ada

   ----------------------------------
   --  End of multi-language stuff --
   ----------------------------------

   type Verbosity is (Default, Medium, High);
   --  Verbosity when parsing GNAT Project Files
   --    Default is default (very quiet, if no errors).
   --    Medium is more verbose.
   --    High is extremely verbose.

   Current_Verbosity : Verbosity := Default;
   --  The current value of the verbosity the project files are parsed with

   type Lib_Kind is (Static, Dynamic, Relocatable);
   type Policy is (Autonomous, Compliant, Controlled, Restricted);
   --  Type to specify the symbol policy, when symbol control is supported.
   --  See full explanation about this type in package Symbols.
   --    Autonomous: Create a symbol file without considering any reference
   --    Compliant:  Try to be as compatible as possible with an existing ref
   --    Controlled: Fail if symbols are not the same as those in the reference
   --    Restricted: Restrict the symbols to those in the symbol file

   type Symbol_Record is record
      Symbol_File   : Name_Id := No_Name;
      Reference     : Name_Id := No_Name;
      Symbol_Policy : Policy  := Autonomous;
   end record;
   --  Type to keep the symbol data to be used when building a shared library

   No_Symbols : constant Symbol_Record :=
     (Symbol_File   => No_Name,
      Reference     => No_Name,
      Symbol_Policy => Autonomous);
   --  The default value of the symbol data

   function Empty_String return Name_Id;
   --  Return the Name_Id for an empty string ""

   type Project_Id is new Nat;
   No_Project : constant Project_Id := 0;
   --  Id of a Project File

   type String_List_Id is new Nat;
   Nil_String : constant String_List_Id := 0;
   type String_Element is record
      Value         : Name_Id        := No_Name;
      Index         : Int            := 0;
      Display_Value : Name_Id   := No_Name;
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
   --  current value is the default one for the variable

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
      Name  : Name_Id          := No_Name;
      Value : Array_Element_Id := No_Array_Element;
      Next  : Array_Id         := No_Array;
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

   function Image (Casing : Casing_Type) return String;
   --  Similar to 'Image (but avoid use of this attribute in compiler)

   function Value (Image : String) return Casing_Type;
   --  Similar to 'Value (but avoid use of this attribute in compiler)
   --  Raises Constraint_Error if not a Casing_Type image.

   --  The following record contains data for a naming scheme

   type Naming_Data is record

      Dot_Replacement : Name_Id := No_Name;
      --  The string to replace '.' in the source file name (for Ada)

      Dot_Repl_Loc : Source_Ptr := No_Location;
      --  The position in the project file source where Dot_Replacement is
      --  defined.

      Casing : Casing_Type := All_Lower_Case;
      --  The casing of the source file name (for Ada)

      Spec_Suffix : Array_Element_Id := No_Array_Element;
      --  The string to append to the unit name for the
      --  source file name of a spec.
      --  Indexed by the programming language.

      Ada_Spec_Suffix : Name_Id := No_Name;
      --  The suffix of the Ada spec sources

      Spec_Suffix_Loc : Source_Ptr := No_Location;
      --  The position in the project file source where
      --  Ada_Spec_Suffix is defined.

      Impl_Suffixes : Impl_Suffix_Array   := No_Impl_Suffixes;
      Supp_Suffixes : Supp_Language_Index := No_Supp_Language_Index;
      --  The source suffixes of the different languages

      Body_Suffix : Array_Element_Id := No_Array_Element;
      --  The string to append to the unit name for the
      --  source file name of a body.
      --  Indexed by the programming language.

      Ada_Body_Suffix : Name_Id := No_Name;
      --  The suffix of the Ada body sources

      Body_Suffix_Loc : Source_Ptr := No_Location;
      --  The position in the project file source where
      --  Ada_Body_Suffix is defined.

      Separate_Suffix : Name_Id := No_Name;
      --  String to append to unit name for source file name of an Ada subunit

      Sep_Suffix_Loc : Source_Ptr := No_Location;
      --  Position in the project file source where Separate_Suffix is defined

      Specs : Array_Element_Id := No_Array_Element;
      --  An associative array mapping individual specs to source file names
      --  This is specific to Ada.

      Bodies : Array_Element_Id := No_Array_Element;
      --  An associative array mapping individual bodies to source file names
      --  This is specific to Ada.

      Specification_Exceptions : Array_Element_Id := No_Array_Element;
      --  An associative array listing spec file names that do not have the
      --  spec suffix. Not used by Ada. Indexed by programming language name.

      Implementation_Exceptions : Array_Element_Id := No_Array_Element;
      --  An associative array listing body file names that do not have the
      --  body suffix. Not used by Ada. Indexed by programming language name.

   end record;

   function Standard_Naming_Data
     (Tree : Project_Tree_Ref := No_Project_Tree) return Naming_Data;
   pragma Inline (Standard_Naming_Data);
   --  The standard GNAT naming scheme when Tree is No_Project_Tree.
   --  Otherwise, return the default naming scheme for the project tree Tree,
   --  which must have been Initialized.

   function Same_Naming_Scheme
     (Left, Right : Naming_Data) return Boolean;
   --  Returns True if Left and Right are the same naming scheme
   --  not considering Specs and Bodies.

   type Project_List is new Nat;
   Empty_Project_List : constant Project_List := 0;
   --  A list of project files

   type Project_Element is record
      Project : Project_Id   := No_Project;
      Next    : Project_List := Empty_Project_List;
   end record;
   --  Element in a list of project files. Next is the id of the next
   --  project file in the list.

   package Project_List_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Project_Element,
      Table_Index_Type     => Project_List,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100);
   --  The table that contains the lists of project files

   --  The following record describes a project file representation

   type Project_Data is record
      Externally_Built : Boolean := False;

      Languages      : Languages_In_Project := No_Languages;
      Supp_Languages : Supp_Language_Index  := No_Supp_Language_Index;
      --  Indicate the different languages of the source of this project

      First_Referred_By : Project_Id := No_Project;
      --  The project, if any, that was the first to be known as importing or
      --  extending this project. Set by Prj.Proc.Process.

      Name : Name_Id := No_Name;
      --  The name of the project. Set by Prj.Proc.Process

      Display_Name : Name_Id := No_Name;
      --  The name of the project with the spelling of its declaration.
      --  Set by Prj.Proc.Process.

      Path_Name : Name_Id := No_Name;
      --  The path name of the project file. Set by Prj.Proc.Process

      Display_Path_Name : Name_Id := No_Name;
      --  The path name used for display purposes. May be different from
      --  Path_Name for platforms where the file names are case-insensitive.

      Virtual : Boolean := False;
      --  True for virtual extending projects

      Location : Source_Ptr := No_Location;
      --  The location in the project file source of the reserved word
      --  project. Set by Prj.Proc.Process.

      Mains : String_List_Id := Nil_String;
      --  List of mains specified by attribute Main. Set by Prj.Nmsc.Check

      Directory : Name_Id := No_Name;
      --  Directory where the project file resides. Set by Prj.Proc.Process

      Display_Directory : Name_Id := No_Name;
      --  comment ???

      Dir_Path : String_Access;
      --  Same as Directory, but as an access to String. Set by
      --  Make.Compile_Sources.Collect_Arguments_And_Compile.

      Library : Boolean := False;
      --  True if this is a library project. Set by
      --  Prj.Nmsc.Language_Independent_Check.

      Library_Dir : Name_Id := No_Name;
      --  If a library project, directory where resides the library Set by
      --  Prj.Nmsc.Language_Independent_Check.

      Display_Library_Dir : Name_Id := No_Name;
      --  The name of the library directory, for display purposes. May be
      --  different from Library_Dir for platforms where the file names are
      --  case-insensitive.

      Library_TS : Time_Stamp_Type := Empty_Time_Stamp;
      --  The timestamp of a library file in a library project.
      --  Set by MLib.Prj.Check_Library.

      Library_Src_Dir : Name_Id := No_Name;
      --  If a Stand-Alone Library project, directory where the sources
      --  of the interfaces of the library are copied. By default, if
      --  attribute Library_Src_Dir is not specified, sources of the interfaces
      --  are not copied anywhere. Set by Prj.Nmsc.Check_Stand_Alone_Library.

      Display_Library_Src_Dir : Name_Id := No_Name;
      --  The name of the library source directory, for display purposes.
      --  May be different from Library_Src_Dir for platforms where the file
      --  names are case-insensitive.

      Library_ALI_Dir : Name_Id := No_Name;
      --  In a library project, directory where the ALI files are copied.
      --  If attribute Library_ALI_Dir is not specified, ALI files are
      --  copied in the Library_Dir. Set by Prj.Nmsc.Check_Library_Attributes.

      Display_Library_ALI_Dir : Name_Id := No_Name;
      --  The name of the library ALI directory, for display purposes. May be
      --  different from Library_ALI_Dir for platforms where the file names are
      --  case-insensitive.

      Library_Name : Name_Id := No_Name;
      --  If a library project, name of the library
      --  Set by Prj.Nmsc.Language_Independent_Check.

      Library_Kind : Lib_Kind := Static;
      --  If a library project, kind of library
      --  Set by Prj.Nmsc.Language_Independent_Check.

      Lib_Internal_Name : Name_Id := No_Name;
      --  If a library project, internal name store inside the library Set by
      --  Prj.Nmsc.Language_Independent_Check.

      Standalone_Library : Boolean := False;
      --  Indicate that this is a Standalone Library Project File. Set by
      --  Prj.Nmsc.Check.

      Lib_Interface_ALIs : String_List_Id := Nil_String;
      --  For Standalone Library Project Files, indicate the list of Interface
      --  ALI files. Set by Prj.Nmsc.Check.

      Lib_Auto_Init : Boolean := False;
      --  For non static Standalone Library Project Files, indicate if
      --  the library initialisation should be automatic.

      Symbol_Data : Symbol_Record := No_Symbols;
      --  Symbol file name, reference symbol file name, symbol policy

      Ada_Sources_Present : Boolean := True;
      --  A flag that indicates if there are Ada sources in this project file.
      --  There are no sources if any of the following is true:
      --    1) Source_Dirs is specified as an empty list
      --    2) Source_Files is specified as an empty list
      --    3) Ada is not in the list of the specified Languages

      Other_Sources_Present : Boolean := True;
      --  A flag that indicates that there are non-Ada sources in this project

      Sources : String_List_Id := Nil_String;
      --  The list of all the source file names.
      --  Set by Prj.Nmsc.Check_Ada_Naming_Scheme.

      First_Other_Source : Other_Source_Id := No_Other_Source;
      Last_Other_Source  : Other_Source_Id := No_Other_Source;
      --  Head and tail of the list of sources of languages other than Ada

      Imported_Directories_Switches : Argument_List_Access := null;
      --  List of the -I switches to be used when compiling sources of
      --  languages other than Ada.

      Include_Path : String_Access := null;
      --  Value to be used as CPATH, when using a GCC, instead of a list of
      --  -I switches.

      Include_Data_Set : Boolean := False;
      --  Set True when Imported_Directories_Switches or Include_Path are set

      Source_Dirs : String_List_Id := Nil_String;
      --  The list of all the source directories.
      --  Set by Prj.Nmsc.Language_Independent_Check.

      Known_Order_Of_Source_Dirs : Boolean := True;
      --  False, if there is any /** in the Source_Dirs, because in this case
      --  the ordering of the source subdirs depend on the OS. If True,
      --  duplicate file names in the same project file are allowed.

      Object_Directory : Name_Id := No_Name;
      --  The object directory of this project file.
      --  Set by Prj.Nmsc.Language_Independent_Check.

      Display_Object_Dir : Name_Id := No_Name;
      --  The name of the object directory, for display purposes.
      --  May be different from Object_Directory for platforms where the file
      --  names are case-insensitive.

      Exec_Directory : Name_Id := No_Name;
      --  The exec directory of this project file. Default is equal to
      --  Object_Directory. Set by Prj.Nmsc.Language_Independent_Check.

      Display_Exec_Dir : Name_Id := No_Name;
      --  The name of the exec directory, for display purposes. May be
      --  different from Exec_Directory for platforms where the file names are
      --  case-insensitive.

      Extends : Project_Id := No_Project;
      --  The reference of the project file, if any, that this project file
      --  extends. Set by Prj.Proc.Process.

      Extended_By : Project_Id := No_Project;
      --  The reference of the project file, if any, that extends this project
      --  file. Set by Prj.Proc.Process.

      Naming : Naming_Data := Standard_Naming_Data;
      --  The naming scheme of this project file.
      --  Set by Prj.Nmsc.Check_Naming_Scheme.

      First_Language_Processing : First_Language_Processing_Data :=
                                    Default_First_Language_Processing_Data;
      --  Comment needed ???

      Supp_Language_Processing : Supp_Language_Index := No_Supp_Language_Index;
      --  Comment needed

      Default_Linker      : Name_Id := No_Name;
      Default_Linker_Path : Name_Id := No_Name;

      Decl : Declarations := No_Declarations;
      --  The declarations (variables, attributes and packages) of this
      --  project file. Set by Prj.Proc.Process.

      Imported_Projects : Project_List := Empty_Project_List;
      --  The list of all directly imported projects, if any. Set by
      --  Prj.Proc.Process.

      All_Imported_Projects : Project_List := Empty_Project_List;
      --  The list of all projects imported directly or indirectly, if any.
      --  Set by Make.Initialize.

      Ada_Include_Path : String_Access := null;
      --  The cached value of ADA_INCLUDE_PATH for this project file. Do not
      --  use this field directly outside of the compiler, use
      --  Prj.Env.Ada_Include_Path instead. Set by Prj.Env.Ada_Include_Path.

      Ada_Objects_Path : String_Access := null;
      --  The cached value of ADA_OBJECTS_PATH for this project file. Do not
      --  use this field directly outside of the compiler, use
      --  Prj.Env.Ada_Objects_Path instead. Set by Prj.Env.Ada_Objects_Path

      Include_Path_File : Name_Id := No_Name;
      --  The cached value of the source path temp file for this project file.
      --  Set by gnatmake (Prj.Env.Set_Ada_Paths).

      Objects_Path_File_With_Libs : Name_Id := No_Name;
      --  The cached value of the object path temp file (including library
      --  dirs) for this project file. Set by gnatmake (Prj.Env.Set_Ada_Paths).

      Objects_Path_File_Without_Libs : Name_Id := No_Name;
      --  The cached value of the object path temp file (excluding library
      --  dirs) for this project file. Set by gnatmake (Prj.Env.Set_Ada_Paths).

      Config_File_Name : Name_Id := No_Name;
      --  The name of the configuration pragmas file, if any.
      --  Set by gnatmake (Prj.Env.Create_Config_Pragmas_File).

      Config_File_Temp : Boolean := False;
      --  An indication that the configuration pragmas file is
      --  a temporary file that must be deleted at the end.
      --  Set by gnatmake (Prj.Env.Create_Config_Pragmas_File).

      Config_Checked : Boolean := False;
      --  A flag to avoid checking repetitively the configuration pragmas file.
      --  Set by gnatmake (Prj.Env.Create_Config_Pragmas_File).

      Language_Independent_Checked : Boolean := False;
      --  A flag that indicates that the project file has been checked
      --  for language independent features: Object_Directory,
      --  Source_Directories, Library, non empty Naming Suffixes.

      Checked : Boolean := False;
      --  A flag to avoid checking repetitively the naming scheme of
      --  this project file. Set by Prj.Nmsc.Check_Ada_Naming_Scheme.

      Seen : Boolean := False;
      --  A flag to mark a project as "visited" to avoid processing the same
      --  project several time.

      Need_To_Build_Lib : Boolean := False;
      --  Indicates that the library of a Library Project needs to be built or
      --  rebuilt.

      Depth : Natural := 0;
      --  The maximum depth of a project in the project graph.
      --  Depth of main project is 0.

      Unkept_Comments : Boolean := False;
      --  True if there are comments in the project sources that cannot
      --  be kept in the project tree.

   end record;

   function Empty_Project (Tree : Project_Tree_Ref) return Project_Data;
   --  Return the representation of an empty project in project Tree tree.
   --  The project tree Tree must have been Initialized and/or Reset.

   Project_Error : exception;
   --  Raised by some subprograms in Prj.Attr

   package Project_Table is new GNAT.Dynamic_Tables (
     Table_Component_Type => Project_Data,
     Table_Index_Type     => Project_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 100);
   --  The set of all project files

   type Spec_Or_Body is
     (Specification, Body_Part);

   type File_Name_Data is record
      Name         : Name_Id := No_Name;
      Index        : Int     := 0;
      Display_Name : Name_Id := No_Name;
      Path         : Name_Id := No_Name;
      Display_Path : Name_Id := No_Name;
      Project      : Project_Id := No_Project;
      Needs_Pragma : Boolean := False;
   end record;
   --  File and Path name of a spec or body

   type File_Names_Data is array (Spec_Or_Body) of File_Name_Data;

   type Unit_Id is new Nat;
   No_Unit : constant Unit_Id := 0;
   type Unit_Data is record
      Name       : Name_Id    := No_Name;
      File_Names : File_Names_Data;
   end record;
   --  Name and File and Path names of a unit, with a reference to its
   --  GNAT Project File(s).

   package Unit_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Unit_Data,
      Table_Index_Type     => Unit_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100);
   --  Table of all units in a project tree

   package Units_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Unit_Id,
      No_Element => No_Unit,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping of unit names to indexes in the Units table

   type Unit_Project is record
      Unit    : Unit_Id    := No_Unit;
      Project : Project_Id := No_Project;
   end record;

   No_Unit_Project : constant Unit_Project := (No_Unit, No_Project);

   package Files_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Unit_Project,
      No_Element => No_Unit_Project,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping of file names to indexes in the Units table

   type Private_Project_Tree_Data is private;
   --  Data for a project tree that is used only by the Project Manager

   type Project_Tree_Data is
      record
         Present_Languages : Present_Language_Table.Instance;
         Supp_Suffixes     : Supp_Suffix_Table.Instance;
         Name_Lists        : Name_List_Table.Instance;
         Supp_Languages    : Supp_Language_Table.Instance;
         Other_Sources     : Other_Source_Table.Instance;
         String_Elements   : String_Element_Table.Instance;
         Variable_Elements : Variable_Element_Table.Instance;
         Array_Elements    : Array_Element_Table.Instance;
         Arrays            : Array_Table.Instance;
         Packages          : Package_Table.Instance;
         Project_Lists     : Project_List_Table.Instance;
         Projects          : Project_Table.Instance;
         Units             : Unit_Table.Instance;
         Units_HT          : Units_Htable.Instance;
         Files_HT          : Files_Htable.Instance;
         Private_Part      : Private_Project_Tree_Data;
      end record;
   --  Data for a project tree

   type Put_Line_Access is access procedure
     (Line    : String;
      Project : Project_Id;
      In_Tree : Project_Tree_Ref);
   --  Use to customize error reporting in Prj.Proc and Prj.Nmsc

   procedure Expect (The_Token : Token_Type; Token_Image : String);
   --  Check that the current token is The_Token. If it is not, then
   --  output an error message.

   procedure Initialize (Tree : Project_Tree_Ref);
   --  This procedure must be called before using any services from the Prj
   --  hierarchy. Namet.Initialize must be called before Prj.Initialize.

   procedure Reset (Tree : Project_Tree_Ref);
   --  This procedure resets all the tables that are used when processing a
   --  project file tree. Initialize must be called before the call to Reset.

   procedure Register_Default_Naming_Scheme
     (Language            : Name_Id;
      Default_Spec_Suffix : Name_Id;
      Default_Body_Suffix : Name_Id;
      In_Tree             : Project_Tree_Ref);
   --  Register the default suffixes for a given language. These extensions
   --  will be ignored if the user has specified a new naming scheme in a
   --  project file.
   --
   --  Otherwise, this information will be automatically added to Naming_Data
   --  when a project is processed, in the lists Spec_Suffix and Body_Suffix.

   generic
      type State is limited private;
      with procedure Action
        (Project    : Project_Id;
         With_State : in out State);
   procedure For_Every_Project_Imported
     (By         : Project_Id;
      In_Tree    : Project_Tree_Ref;
      With_State : in out State);
   --  Call Action for each project imported directly or indirectly by project
   --  By. Action is called according to the order of importation: if A
   --  imports B, directly or indirectly, Action will be called for A before
   --  it is called for B. If two projects import each other directly or
   --  indirectly (using at least one "limited with"), it is not specified
   --  for which of these two projects Action will be called first. Projects
   --  that are extended by other projects are not considered. With_State may
   --  be used by Action to choose a behavior or to report some global result.

   ----------------------------------------------------------
   -- Other multi-language stuff that may be modified soon --
   ----------------------------------------------------------

   function Is_Present
     (Language   : Language_Index;
      In_Project : Project_Data;
      In_Tree    : Project_Tree_Ref) return Boolean;
   --  Return True when Language is one of the languages used in
   --  project In_Project.

   procedure Set
     (Language   : Language_Index;
      Present    : Boolean;
      In_Project : in out Project_Data;
      In_Tree    : Project_Tree_Ref);
   --  Indicate if Language is or not a language used in project In_Project

   function Language_Processing_Data_Of
     (Language   : Language_Index;
      In_Project : Project_Data;
      In_Tree    : Project_Tree_Ref) return Language_Processing_Data;
   --  Return the Language_Processing_Data for language Language in project
   --  In_Project. Return the default when no Language_Processing_Data are
   --  defined for the language.

   procedure Set
     (Language_Processing : Language_Processing_Data;
      For_Language        : Language_Index;
      In_Project          : in out Project_Data;
      In_Tree             : Project_Tree_Ref);
   --  Set the Language_Processing_Data for language Language in project
   --  In_Project.

   function Suffix_Of
     (Language   : Language_Index;
      In_Project : Project_Data;
      In_Tree    : Project_Tree_Ref) return Name_Id;
   --  Return the suffix for language Language in project In_Project. Return
   --  No_Name when no suffix is defined for the language.

   procedure Set
     (Suffix       : Name_Id;
      For_Language : Language_Index;
      In_Project   : in out Project_Data;
      In_Tree      : Project_Tree_Ref);
   --  Set the suffix for language Language in project In_Project

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

   Empty_Name : Name_Id;
   --  Name_Id for an empty name (no characters). Initialized by the call
   --  to procedure Initialize.

   procedure Add_To_Buffer
     (S    : String;
      To   : in out String_Access;
      Last : in out Natural);
   --  Append a String to the Buffer

   type Naming_Id is new Nat;

   package Naming_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Naming_Data,
      Table_Index_Type     => Naming_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 100);
   --  Comment ???

   package Path_File_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50);
   --  Table storing all the temp path file names.
   --  Used by Delete_All_Path_Files.

   package Source_Path_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50);
   --  A table to store the source dirs before creating the source path file

   package Object_Path_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50);
   --  A table to store the object dirs, before creating the object path file

   type Private_Project_Tree_Data is record
      Namings        : Naming_Table.Instance;
      Path_Files     : Path_File_Table.Instance;
      Source_Paths   : Source_Path_Table.Instance;
      Object_Paths   : Object_Path_Table.Instance;
      Default_Naming : Naming_Data;
   end record;
   --  Comment ???
end Prj;
