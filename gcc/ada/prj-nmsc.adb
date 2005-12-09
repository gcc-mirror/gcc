------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2005, Free Software Foundation, Inc.         --
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

with Err_Vars; use Err_Vars;
with Fmap;     use Fmap;
with Hostparm;
with MLib.Tgt;
with Namet;    use Namet;
with Osint;    use Osint;
with Output;   use Output;
with MLib.Tgt; use MLib.Tgt;
with Prj.Env;  use Prj.Env;
with Prj.Err;
with Prj.Util; use Prj.Util;
with Sinput.P;
with Snames;   use Snames;
with Table;    use Table;
with Targparm; use Targparm;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.HTable;

package body Prj.Nmsc is

   Error_Report : Put_Line_Access := null;
   --  Set to point to error reporting procedure

   ALI_Suffix   : constant String := ".ali";
   --  File suffix for ali files

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  File suffix for object files

   type Name_Location is record
      Name     : Name_Id;
      Location : Source_Ptr;
      Found    : Boolean := False;
   end record;
   --  Information about file names found in string list attribute
   --  Source_Files or in a source list file, stored in hash table
   --  Source_Names, used by procedure Get_Path_Names_And_Record_Sources.

   No_Name_Location : constant Name_Location :=
     (Name => No_Name, Location => No_Location, Found => False);

   package Source_Names is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_Location,
      No_Element => No_Name_Location,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table to store file names found in string list attribute
   --  Source_Files or in a source list file, stored in hash table
   --  Source_Names, used by procedure Get_Path_Names_And_Record_Sources.

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
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store naming exceptions for Ada. For each file name
   --  there is one or several unit in table Ada_Naming_Exception_Table.

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
   --  A table to check if a unit with an exceptional name will hide
   --  a source with a file name following the naming convention.

   function ALI_File_Name (Source : String) return String;
   --  Return the ALI file name corresponding to a source

   procedure Check_Ada_Name (Name : String; Unit : out Name_Id);
   --  Check that a name is a valid Ada unit name

   procedure Check_Naming_Scheme
     (Data    : in out Project_Data;
      Project : Project_Id;
      In_Tree : Project_Tree_Ref);
   --  Check the naming scheme part of Data

   procedure Check_Ada_Naming_Scheme_Validity
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Naming  : Naming_Data);
   --  Check that the package Naming is correct

   procedure Check_For_Source
     (File_Name        : Name_Id;
      Path_Name        : Name_Id;
      Project          : Project_Id;
      In_Tree          : Project_Tree_Ref;
      Data             : in out Project_Data;
      Location         : Source_Ptr;
      Language         : Language_Index;
      Suffix           : String;
      Naming_Exception : Boolean);
   --  Check if a file, with name File_Name and path Path_Name, in a source
   --  directory is a source for language Language in project Project of
   --  project tree In_Tree. ???

   procedure Check_If_Externally_Built
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Check attribute Externally_Built of project Project in project tree
   --  In_Tree and modify its data Data if it has the value "true".

   procedure Check_Library_Attributes
     (Project   : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data      : in out Project_Data);
   --  Check the library attributes of project Project in project tree In_Tree
   --  and modify its data Data accordingly.

   procedure Check_Package_Naming
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Check package Naming of project Project in project tree In_Tree and
   --  modify its data Data accordingly.

   procedure Check_Programming_Languages
     (In_Tree : Project_Tree_Ref; Data : in out Project_Data);
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
     (Project   : Project_Id;
      In_Tree   : Project_Tree_Ref;
      Data      : in out Project_Data;
      Extending : Boolean);
   --  Check if project Project in project tree In_Tree is a Stand-Alone
   --  Library project, and modify its data Data accordingly if it is one.

   function Compute_Directory_Last (Dir : String) return Natural;
   --  Return the index of the last significant character in Dir. This is used
   --  to avoid duplicates '/' at the end of directory names

   function Body_Suffix_Of
     (Language   : Language_Index;
      In_Project : Project_Data;
      In_Tree    : Project_Tree_Ref)
      return String;
   --  Returns the suffix of sources of language Language in project In_Project
   --  in project tree In_Tree.

   procedure Error_Msg
     (Project       : Project_Id;
      In_Tree       : Project_Tree_Ref;
      Msg           : String;
      Flag_Location : Source_Ptr);
   --  Output an error message. If Error_Report is null, simply call
   --  Prj.Err.Error_Msg. Otherwise, disregard Flag_Location and use
   --  Error_Report.

   procedure Find_Sources
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Data         : in out Project_Data;
      For_Language : Language_Index;
      Follow_Links : Boolean := False);
   --  Find all the sources in all of the source directories of a project for
   --  a specified language.

   procedure Free_Ada_Naming_Exceptions;
   --  Free the internal hash tables used for checking naming exceptions

   procedure Get_Directories
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Get the object directory, the exec directory and the source directories
   --  of a project.

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

   procedure Get_Unit
     (Canonical_File_Name : Name_Id;
      Naming              : Naming_Data;
      Exception_Id        : out Ada_Naming_Exception_Id;
      Unit_Name           : out Name_Id;
      Unit_Kind           : out Spec_Or_Body;
      Needs_Pragma        : out Boolean);
   --  Find out, from a file name, the unit name, the unit kind and if a
   --  specific SFN pragma is needed. If the file name corresponds to no
   --  unit, then Unit_Name will be No_Name. If the file is a multi-unit source
   --  or an exception to the naming scheme, then Exception_Id is set to
   --  the unit or units that the source contains.

   function Is_Illegal_Suffix
     (Suffix                          : String;
      Dot_Replacement_Is_A_Single_Dot : Boolean) return Boolean;
   --  Returns True if the string Suffix cannot be used as
   --  a spec suffix, a body suffix or a separate suffix.

   procedure Locate_Directory
     (Name    : Name_Id;
      Parent  : Name_Id;
      Dir     : out Name_Id;
      Display : out Name_Id);
   --  Locate a directory (returns No_Name for Dir and Display if directory
   --  does not exist). Name is the directory name. Parent is the root
   --  directory, if Name is a relative path name. Dir is the canonical case
   --  path name of the directory, Display is the directory path name for
   --  display purposes.

   procedure Look_For_Sources
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Data         : in out Project_Data;
      Follow_Links : Boolean);
   --  Find all the sources of a project

   function Path_Name_Of
     (File_Name : Name_Id;
      Directory : Name_Id) return String;
   --  Returns the path name of a (non project) file.
   --  Returns an empty string if file cannot be found.

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
     (File_Name       : Name_Id;
      Path_Name       : Name_Id;
      Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Data            : in out Project_Data;
      Location        : Source_Ptr;
      Current_Source  : in out String_List_Id;
      Source_Recorded : in out Boolean;
      Follow_Links    : Boolean);
   --  Put a unit in the list of units of a project, if the file name
   --  corresponds to a valid unit name.

   procedure Record_Other_Sources
     (Project           : Project_Id;
      In_Tree           : Project_Tree_Ref;
      Data              : in out Project_Data;
      Language          : Language_Index;
      Naming_Exceptions : Boolean);
   --  Record the sources of a language in a project.
   --  When Naming_Exceptions is True, mark the found sources as such, to
   --  later remove those that are not named in a list of sources.

   procedure Show_Source_Dirs
     (Project : Project_Id; In_Tree : Project_Tree_Ref);
   --  List all the source directories of a project

   function Suffix_For
     (Language : Language_Index;
      Naming   : Naming_Data;
      In_Tree  : Project_Tree_Ref) return Name_Id;
   --  Get the suffix for the source of a language from a package naming.
   --  If not specified, return the default for the language.

   procedure Warn_If_Not_Sources
     (Project     : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Conventions : Array_Element_Id;
      Specs       : Boolean;
      Extending   : Boolean);
   --  Check that individual naming conventions apply to immediate
   --  sources of the project; if not, issue a warning.

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
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Report_Error : Put_Line_Access;
      Follow_Links : Boolean)
   is
      Data      : Project_Data := In_Tree.Projects.Table (Project);
      Extending : Boolean := False;

   begin
      Error_Report := Report_Error;

      Recursive_Dirs.Reset;

      --  Object, exec and source directories

      Get_Directories (Project, In_Tree, Data);

      --  Get the programming languages

      Check_Programming_Languages (In_Tree, Data);

      --  Library attributes

      Check_Library_Attributes (Project, In_Tree, Data);

      Check_If_Externally_Built (Project, In_Tree, Data);

      if Current_Verbosity = High then
         Show_Source_Dirs (Project, In_Tree);
      end if;

      Check_Package_Naming (Project, In_Tree, Data);

      Extending := Data.Extends /= No_Project;

      Check_Naming_Scheme (Data, Project, In_Tree);

      Prepare_Ada_Naming_Exceptions
        (Data.Naming.Bodies, In_Tree, Body_Part);
      Prepare_Ada_Naming_Exceptions
        (Data.Naming.Specs, In_Tree, Specification);

      --  Find the sources

      if Data.Source_Dirs /= Nil_String then
         Look_For_Sources (Project, In_Tree, Data, Follow_Links);
      end if;

      if Data.Ada_Sources_Present then

         --  Check that all individual naming conventions apply to sources of
         --  this project file.

         Warn_If_Not_Sources
           (Project, In_Tree, Data.Naming.Bodies,
            Specs     => False,
            Extending => Extending);
         Warn_If_Not_Sources
           (Project, In_Tree, Data.Naming.Specs,
            Specs     => True,
            Extending => Extending);
      end if;

      --  If it is a library project file, check if it is a standalone library

      if Data.Library then
         Check_Stand_Alone_Library (Project, In_Tree, Data, Extending);
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

   begin
      To_Lower (The_Name);

      Name_Len := The_Name'Length;
      Name_Buffer (1 .. Name_Len) := The_Name;
      Real_Name := Name_Find;

      --  Check first that the given name is not an Ada reserved word

      if Get_Name_Table_Byte (Real_Name) /= 0
        and then Real_Name /= Name_Project
        and then Real_Name /= Name_Extends
        and then Real_Name /= Name_External
      then
         Unit := No_Name;

         if Current_Verbosity = High then
            Write_Str (The_Name);
            Write_Line (" is an Ada reserved word.");
         end if;

         return;
      end if;

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
            Dot_Replacement       : constant String :=
                                     Get_Name_String
                                       (Naming.Dot_Replacement);

            Spec_Suffix : constant String :=
                                     Get_Name_String
                                       (Naming.Ada_Spec_Suffix);

            Body_Suffix : constant String :=
                                     Get_Name_String
                                       (Naming.Ada_Body_Suffix);

            Separate_Suffix       : constant String :=
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
               Err_Vars.Error_Msg_Name_1 := Naming.Ada_Spec_Suffix;
               Error_Msg
                 (Project, In_Tree,
                  "{ is illegal for Spec_Suffix",
                  Naming.Spec_Suffix_Loc);
            end if;

            if Is_Illegal_Suffix
                 (Body_Suffix, Dot_Replacement = ".")
            then
               Err_Vars.Error_Msg_Name_1 := Naming.Ada_Body_Suffix;
               Error_Msg
                 (Project, In_Tree,
                  "{ is illegal for Body_Suffix",
                  Naming.Body_Suffix_Loc);
            end if;

            if Body_Suffix /= Separate_Suffix then
               if Is_Illegal_Suffix
                    (Separate_Suffix, Dot_Replacement = ".")
               then
                  Err_Vars.Error_Msg_Name_1 := Naming.Separate_Suffix;
                  Error_Msg
                    (Project, In_Tree,
                     "{ is illegal for Separate_Suffix",
                     Naming.Sep_Suffix_Loc);
               end if;
            end if;

            --  Spec_Suffix cannot have the same termination as
            --  Body_Suffix or Separate_Suffix

            if Spec_Suffix'Length <= Body_Suffix'Length
              and then
                Body_Suffix (Body_Suffix'Last -
                             Spec_Suffix'Length + 1 ..
                             Body_Suffix'Last) = Spec_Suffix
            then
               Error_Msg
                 (Project, In_Tree,
                  "Body_Suffix (""" &
                  Body_Suffix &
                  """) cannot end with" &
                  " Spec_Suffix  (""" &
                  Spec_Suffix & """).",
                  Naming.Body_Suffix_Loc);
            end if;

            if Body_Suffix /= Separate_Suffix
              and then Spec_Suffix'Length <= Separate_Suffix'Length
              and then
                Separate_Suffix
                  (Separate_Suffix'Last - Spec_Suffix'Length + 1
                    ..
                   Separate_Suffix'Last) = Spec_Suffix
            then
               Error_Msg
                 (Project, In_Tree,
                  "Separate_Suffix (""" &
                  Separate_Suffix &
                  """) cannot end with" &
                  " Spec_Suffix (""" &
                  Spec_Suffix & """).",
                  Naming.Sep_Suffix_Loc);
            end if;
         end;
      end if;
   end Check_Ada_Naming_Scheme_Validity;

   ----------------------
   -- Check_For_Source --
   ----------------------

   procedure Check_For_Source
     (File_Name        : Name_Id;
      Path_Name        : Name_Id;
      Project          : Project_Id;
      In_Tree          : Project_Tree_Ref;
      Data             : in out Project_Data;
      Location         : Source_Ptr;
      Language         : Language_Index;
      Suffix           : String;
      Naming_Exception : Boolean)
   is
      Name : String := Get_Name_String (File_Name);
      Real_Location : Source_Ptr := Location;

   begin
      Canonical_Case_File_Name (Name);

      --  A file is a source of a language if Naming_Exception is True (case
      --  of naming exceptions) or if its file name ends with the suffix.

      if Naming_Exception or else
        (Name'Length > Suffix'Length and then
         Name (Name'Last - Suffix'Length + 1 .. Name'Last) = Suffix)
      then
         if Real_Location = No_Location then
            Real_Location := Data.Location;
         end if;

         declare
            Path : String := Get_Name_String (Path_Name);

            Path_Id : Name_Id;
            --  The path name id (in canonical case)

            File_Id : Name_Id;
            --  The file name id (in canonical case)

            Obj_Id : Name_Id;
            --  The object file name

            Obj_Path_Id : Name_Id;
            --  The object path name

            Dep_Id : Name_Id;
            --  The dependency file name

            Dep_Path_Id : Name_Id;
            --  The dependency path name

            Dot_Pos : Natural := 0;
            --  Position of the last dot in Name

            Source    : Other_Source;
            Source_Id : Other_Source_Id := Data.First_Other_Source;

         begin
            Canonical_Case_File_Name (Path);

            --  Get the file name id

            Name_Len := Name'Length;
            Name_Buffer (1 .. Name_Len) := Name;
            File_Id := Name_Find;

            --  Get the path name id

            Name_Len := Path'Length;
            Name_Buffer (1 .. Name_Len) := Path;
            Path_Id := Name_Find;

            --  Find the position of the last dot

            for J in reverse Name'Range loop
               if Name (J) = '.' then
                  Dot_Pos := J;
                  exit;
               end if;
            end loop;

            if Dot_Pos <= Name'First then
               Dot_Pos := Name'Last + 1;
            end if;

            --  Compute the object file name

            Get_Name_String (File_Id);
            Name_Len := Dot_Pos - Name'First;

            for J in Object_Suffix'Range loop
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := Object_Suffix (J);
            end loop;

            Obj_Id := Name_Find;

            --  Compute the object path name

            Get_Name_String (Data.Object_Directory);

            if Name_Buffer (Name_Len) /= Directory_Separator and then
              Name_Buffer (Name_Len) /= '/'
            then
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := Directory_Separator;
            end if;

            Add_Str_To_Name_Buffer (Get_Name_String (Obj_Id));
            Obj_Path_Id := Name_Find;

            --  Compute the dependency file name

            Get_Name_String (File_Id);
            Name_Len := Dot_Pos - Name'First + 1;
            Name_Buffer (Name_Len) := '.';
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := 'd';
            Dep_Id := Name_Find;

            --  Compute the dependency path name

            Get_Name_String (Data.Object_Directory);

            if Name_Buffer (Name_Len) /= Directory_Separator and then
              Name_Buffer (Name_Len) /= '/'
            then
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := Directory_Separator;
            end if;

            Add_Str_To_Name_Buffer (Get_Name_String (Dep_Id));
            Dep_Path_Id := Name_Find;

            --  Check if source is already in the list of source for this
            --  project: it may have already been specified as a naming
            --  exception for the same language or an other language, or
            --  they may be two identical file names in different source
            --  directories.

            while Source_Id /= No_Other_Source loop
               Source := In_Tree.Other_Sources.Table (Source_Id);

               if Source.File_Name = File_Id then

                  --  Two sources of different languages cannot have the same
                  --  file name.

                  if Source.Language /= Language then
                     Error_Msg_Name_1 := File_Name;
                     Error_Msg
                       (Project, In_Tree,
                        "{ cannot be a source of several languages",
                        Real_Location);
                     return;

                  --  No problem if a file has already been specified as
                  --  a naming exception of this language.

                  elsif Source.Path_Name = Path_Id then

                     --  Reset the naming exception flag, if this is not a
                     --  naming exception.

                     if not Naming_Exception then
                        In_Tree.Other_Sources.Table
                          (Source_Id).Naming_Exception := False;
                     end if;

                     return;

                  --  There are several files with the same names, but the
                  --  order of the source directories is known (no /**):
                  --  only the first one encountered is kept, the other ones
                  --  are ignored.

                  elsif Data.Known_Order_Of_Source_Dirs then
                     return;

                  --  But it is an error if the order of the source directories
                  --  is not known.

                  else
                     Error_Msg_Name_1 := File_Name;
                     Error_Msg
                       (Project, In_Tree,
                        "{ is found in several source directories",
                        Real_Location);
                     return;
                  end if;

               --  Two sources with different file names cannot have the same
               --  object file name.

               elsif Source.Object_Name = Obj_Id then
                  Error_Msg_Name_1 := File_Id;
                  Error_Msg_Name_2 := Source.File_Name;
                  Error_Msg_Name_3 := Obj_Id;
                  Error_Msg
                       (Project, In_Tree,
                        "{ and { have the same object file {",
                        Real_Location);
                     return;
               end if;

               Source_Id := Source.Next;
            end loop;

            if Current_Verbosity = High then
               Write_Str ("      found ");
               Display_Language_Name (Language);
               Write_Str (" source """);
               Write_Str (Get_Name_String (File_Name));
               Write_Line ("""");
               Write_Str ("      object path = ");
               Write_Line (Get_Name_String (Obj_Path_Id));
            end if;

            --  Create the Other_Source record

            Source :=
              (Language         => Language,
               File_Name        => File_Id,
               Path_Name        => Path_Id,
               Source_TS        => File_Stamp (Path_Id),
               Object_Name      => Obj_Id,
               Object_Path      => Obj_Path_Id,
               Object_TS        => File_Stamp (Obj_Path_Id),
               Dep_Name         => Dep_Id,
               Dep_Path         => Dep_Path_Id,
               Dep_TS           => File_Stamp (Dep_Path_Id),
               Naming_Exception => Naming_Exception,
               Next             => No_Other_Source);

            --  And add it to the Other_Sources table

            Other_Source_Table.Increment_Last
              (In_Tree.Other_Sources);
            In_Tree.Other_Sources.Table
              (Other_Source_Table.Last (In_Tree.Other_Sources)) :=
                 Source;

            --  There are sources of languages other than Ada in this project

            Data.Other_Sources_Present := True;

            --  And there are sources of this language in this project

            Set (Language, True, Data, In_Tree);

            --  Add this source to the list of sources of languages other than
            --  Ada of the project.

            if Data.First_Other_Source = No_Other_Source then
               Data.First_Other_Source :=
                 Other_Source_Table.Last (In_Tree.Other_Sources);

            else
               In_Tree.Other_Sources.Table (Data.Last_Other_Source).Next :=
                 Other_Source_Table.Last (In_Tree.Other_Sources);
            end if;

            Data.Last_Other_Source :=
              Other_Source_Table.Last (In_Tree.Other_Sources);
         end;
      end if;
   end Check_For_Source;

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

      if Current_Verbosity = High then
         Write_Str ("Project is ");

         if not Data.Externally_Built then
            Write_Str ("not ");
         end if;

         Write_Line ("externally built.");
      end if;
   end Check_If_Externally_Built;

   -----------------------------
   -- Check_Naming_Scheme --
   -----------------------------

   procedure Check_Naming_Scheme
     (Data    : in out Project_Data;
      Project : Project_Id;
      In_Tree : Project_Tree_Ref)
   is
      Naming_Id : constant Package_Id :=
                    Util.Value_Of (Name_Naming, Data.Decl.Packages, In_Tree);

      Naming : Package_Element;

      procedure Check_Unit_Names (List : Array_Element_Id);
      --  Check that a list of unit names contains only valid names

      ----------------------
      -- Check_Unit_Names --
      ----------------------

      procedure Check_Unit_Names (List : Array_Element_Id) is
         Current   : Array_Element_Id := List;
         Element   : Array_Element;
         Unit_Name : Name_Id;

      begin
         --  Loop through elements of the string list

         while Current /= No_Array_Element loop
            Element := In_Tree.Array_Elements.Table (Current);

            --  Put file name in canonical case

            Get_Name_String (Element.Value.Value);
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            Element.Value.Value := Name_Find;

            --  Check that it contains a valid unit name

            Get_Name_String (Element.Index);
            Check_Ada_Name (Name_Buffer (1 .. Name_Len), Unit_Name);

            if Unit_Name = No_Name then
               Err_Vars.Error_Msg_Name_1 := Element.Index;
               Error_Msg
                 (Project, In_Tree,
                  "{ is not a valid unit name.",
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

   --  Start of processing for Check_Naming_Scheme

   begin
      --  If there is a package Naming, we will put in Data.Naming what is in
      --  this package Naming.

      if Naming_Id /= No_Package then
         Naming := In_Tree.Packages.Table (Naming_Id);

         if Current_Verbosity = High then
            Write_Line ("Checking ""Naming"" for Ada.");
         end if;

         declare
            Bodies : constant Array_Element_Id :=
                       Util.Value_Of (Name_Body, Naming.Decl.Arrays, In_Tree);

            Specs : constant Array_Element_Id :=
                      Util.Value_Of (Name_Spec, Naming.Decl.Arrays, In_Tree);

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
         --  Spec_Suffix, Body_Suffix and/or Separate_Suffix
         --  exist.

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
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                  Data.Naming.Dot_Replacement := Name_Find;
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
                                (Name_Casing, Naming.Decl.Attributes, In_Tree);

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
                           "{ is not a correct Casing",
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
               Data.Naming.Ada_Spec_Suffix := Name_Find;
               Data.Naming.Spec_Suffix_Loc := Ada_Spec_Suffix.Location;

            else
               Data.Naming.Ada_Spec_Suffix := Default_Ada_Spec_Suffix;
            end if;
         end;

         if Current_Verbosity = High then
            Write_Str  ("  Spec_Suffix = """);
            Write_Str  (Get_Name_String (Data.Naming.Ada_Spec_Suffix));
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
               Data.Naming.Ada_Body_Suffix := Name_Find;
               Data.Naming.Body_Suffix_Loc := Ada_Body_Suffix.Location;

            else
               Data.Naming.Ada_Body_Suffix := Default_Ada_Body_Suffix;
            end if;
         end;

         if Current_Verbosity = High then
            Write_Str  ("  Body_Suffix = """);
            Write_Str  (Get_Name_String (Data.Naming.Ada_Body_Suffix));
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
                 Data.Naming.Ada_Body_Suffix;

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

      else
         Data.Naming.Ada_Spec_Suffix := Default_Ada_Spec_Suffix;
         Data.Naming.Ada_Body_Suffix := Default_Ada_Body_Suffix;
         Data.Naming.Separate_Suffix := Default_Ada_Body_Suffix;
      end if;
   end Check_Naming_Scheme;

   ------------------------------
   -- Check_Library_Attributes --
   ------------------------------

   procedure Check_Library_Attributes
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Attributes : constant Prj.Variable_Id := Data.Decl.Attributes;

      Lib_Dir : constant Prj.Variable_Value :=
                  Prj.Util.Value_Of
                    (Snames.Name_Library_Dir, Attributes, In_Tree);

      Lib_Name : constant Prj.Variable_Value :=
                   Prj.Util.Value_Of
                     (Snames.Name_Library_Name, Attributes, In_Tree);

      Lib_Version : constant Prj.Variable_Value :=
                      Prj.Util.Value_Of
                        (Snames.Name_Library_Version, Attributes, In_Tree);

      Lib_ALI_Dir : constant Prj.Variable_Value :=
                      Prj.Util.Value_Of
                        (Snames.Name_Library_Ali_Dir, Attributes, In_Tree);

      The_Lib_Kind : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Kind, Attributes, In_Tree);

   begin
      --  Special case of extending project

      if Data.Extends /= No_Project then
         declare
            Extended_Data : constant Project_Data :=
                           In_Tree.Projects.Table (Data.Extends);

         begin
            --  If the project extended is a library project, we inherit
            --  the library name, if it is not redefined; we check that
            --  the library directory is specified; and we reset the
            --  library flag for the extended project.

            if Extended_Data.Library then
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
                  end if;
               end if;

               In_Tree.Projects.Table (Data.Extends).Library :=
                 False;
            end if;
         end;
      end if;

      pragma Assert (Lib_Dir.Kind = Single);

      if Lib_Dir.Value = Empty_String then
         if Current_Verbosity = High then
            Write_Line ("No library directory");
         end if;

      else
         --  Find path name, check that it is a directory

         Locate_Directory
           (Lib_Dir.Value, Data.Display_Directory,
            Data.Library_Dir, Data.Display_Library_Dir);

         if Data.Library_Dir = No_Name then

            --  Get the absolute name of the library directory that
            --  does not exist, to report an error.

            declare
               Dir_Name : constant String := Get_Name_String (Lib_Dir.Value);

            begin
               if Is_Absolute_Path (Dir_Name) then
                  Err_Vars.Error_Msg_Name_1 := Lib_Dir.Value;

               else
                  Get_Name_String (Data.Display_Directory);

                  if Name_Buffer (Name_Len) /= Directory_Separator then
                     Name_Len := Name_Len + 1;
                     Name_Buffer (Name_Len) := Directory_Separator;
                  end if;

                  Name_Buffer
                    (Name_Len + 1 .. Name_Len + Dir_Name'Length) :=
                    Dir_Name;
                  Name_Len := Name_Len + Dir_Name'Length;
                  Err_Vars.Error_Msg_Name_1 := Name_Find;
               end if;

               --  Report the error

               Error_Msg
                 (Project, In_Tree,
                  "library directory { does not exist",
                  Lib_Dir.Location);
            end;

         --  The library directory cannot be the same as the Object directory

         elsif Data.Library_Dir = Data.Object_Directory then
            Error_Msg
              (Project, In_Tree,
               "library directory cannot be the same " &
               "as object directory",
               Lib_Dir.Location);
            Data.Library_Dir := No_Name;
            Data.Display_Library_Dir := No_Name;

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

                  if Data.Library_Dir = Dir_Elem.Value then
                     Err_Vars.Error_Msg_Name_1 := Dir_Elem.Value;
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
                           Dir_Elem := In_Tree.String_Elements.Table (Dirs_Id);
                           Dirs_Id  := Dir_Elem.Next;

                           if Data.Library_Dir = Dir_Elem.Value then
                              Err_Vars.Error_Msg_Name_1 := Dir_Elem.Value;
                              Err_Vars.Error_Msg_Name_2 :=
                                In_Tree.Projects.Table (Pid).Name;

                              Error_Msg
                                (Project, In_Tree,
                                 "library directory cannot be the same " &
                                 "as source directory { of project {",
                                 Lib_Dir.Location);
                              OK := False;
                              exit Project_Loop;
                           end if;
                        end loop Dir_Loop;
                     end if;
                  end loop Project_Loop;
               end if;

               if not OK then
                  Data.Library_Dir := No_Name;
                  Data.Display_Library_Dir := No_Name;

               elsif Current_Verbosity = High then

                  --  Display the Library directory in high verbosity

                  Write_Str ("Library directory =""");
                  Write_Str (Get_Name_String (Data.Display_Library_Dir));
                  Write_Line ("""");
               end if;
            end;
         end if;
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

      if Data.Library_Name /= No_Name
        and then Current_Verbosity = High
      then
         Write_Str ("Library name = """);
         Write_Str (Get_Name_String (Data.Library_Name));
         Write_Line ("""");
      end if;

      Data.Library :=
        Data.Library_Dir /= No_Name
        and then
      Data.Library_Name /= No_Name;

      if Data.Library then
         if MLib.Tgt.Support_For_Libraries = MLib.Tgt.None then
            Error_Msg
              (Project, In_Tree,
               "?libraries are not supported on this platform",
               Lib_Name.Location);
            Data.Library := False;

         else
            if Lib_ALI_Dir.Value = Empty_String then
               if Current_Verbosity = High then
                  Write_Line ("No library 'A'L'I directory specified");
               end if;
               Data.Library_ALI_Dir := Data.Library_Dir;
               Data.Display_Library_ALI_Dir := Data.Display_Library_Dir;

            else
               --  Find path name, check that it is a directory

               Locate_Directory
                 (Lib_ALI_Dir.Value, Data.Display_Directory,
                  Data.Library_ALI_Dir, Data.Display_Library_ALI_Dir);

               if Data.Library_ALI_Dir = No_Name then

                  --  Get the absolute name of the library ALI directory that
                  --  does not exist, to report an error.

                  declare
                     Dir_Name : constant String :=
                                  Get_Name_String (Lib_ALI_Dir.Value);

                  begin
                     if Is_Absolute_Path (Dir_Name) then
                        Err_Vars.Error_Msg_Name_1 := Lib_Dir.Value;

                     else
                        Get_Name_String (Data.Display_Directory);

                        if Name_Buffer (Name_Len) /= Directory_Separator then
                           Name_Len := Name_Len + 1;
                           Name_Buffer (Name_Len) := Directory_Separator;
                        end if;

                        Name_Buffer
                          (Name_Len + 1 .. Name_Len + Dir_Name'Length) :=
                          Dir_Name;
                        Name_Len := Name_Len + Dir_Name'Length;
                        Err_Vars.Error_Msg_Name_1 := Name_Find;
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
                     Data.Library_ALI_Dir := No_Name;
                     Data.Display_Library_ALI_Dir := No_Name;

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

                           if Data.Library_ALI_Dir = Dir_Elem.Value then
                              Err_Vars.Error_Msg_Name_1 := Dir_Elem.Value;
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

                                    if
                                      Data.Library_ALI_Dir = Dir_Elem.Value
                                    then
                                       Err_Vars.Error_Msg_Name_1 :=
                                         Dir_Elem.Value;
                                       Err_Vars.Error_Msg_Name_2 :=
                                         In_Tree.Projects.Table (Pid).Name;

                                       Error_Msg
                                         (Project, In_Tree,
                                          "library 'A'L'I directory cannot " &
                                          "be the same as source directory " &
                                          "{ of project {",
                                          Lib_ALI_Dir.Location);
                                       OK := False;
                                       exit ALI_Project_Loop;
                                    end if;
                                 end loop ALI_Dir_Loop;
                              end if;
                           end loop ALI_Project_Loop;
                        end if;

                        if not OK then
                           Data.Library_ALI_Dir := No_Name;
                           Data.Display_Library_ALI_Dir := No_Name;

                        elsif Current_Verbosity = High then

                           --  Display the Library ALI directory in high
                           --  verbosity.

                           Write_Str ("Library ALI directory =""");
                           Write_Str
                             (Get_Name_String (Data.Display_Library_ALI_Dir));
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
                    MLib.Tgt.Support_For_Libraries = MLib.Tgt.Static_Only
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

            if Data.Library and then Current_Verbosity = High then
               Write_Line ("This is a library project file");
            end if;

         end if;
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

      Naming : Package_Element;

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
            --  If some suffixs have been specified, we make sure that
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

               --  Put the resulting array as the specification suffixs

               Data.Naming.Spec_Suffix := Spec_Suffixs;
            end if;
         end;

         declare
            Current : Array_Element_Id := Data.Naming.Spec_Suffix;
            Element : Array_Element;

         begin
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

            Suffix       : Array_Element_Id;
            Element      : Array_Element;
            Suffix2      : Array_Element_Id;

         begin
            --  If some suffixes have been specified, we make sure that
            --  for each language for which a default suffix has been
            --  specified, there is a suffix specified, either the one
            --  in the project file or if there were noe, the default.

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
                  --  specified in the project file. Add the default to the
                  --  array.

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

               --  Put the resulting array as the implementation suffixs

               Data.Naming.Body_Suffix := Impl_Suffixs;
            end if;
         end;

         declare
            Current : Array_Element_Id := Data.Naming.Body_Suffix;
            Element : Array_Element;

         begin
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
      Data    : in out Project_Data)
   is
      Languages : Variable_Value := Nil_Variable_Value;

   begin
      Languages :=
        Prj.Util.Value_Of (Name_Languages, Data.Decl.Attributes, In_Tree);
      Data.Ada_Sources_Present   := Data.Source_Dirs /= Nil_String;
      Data.Other_Sources_Present := Data.Source_Dirs /= Nil_String;

      if Data.Source_Dirs /= Nil_String then

         --  Check if languages are specified in this project

         if Languages.Default then

            --  Attribute Languages is not specified. So, it defaults to
            --  a project of language Ada only.

            Data.Languages (Ada_Language_Index) := True;

            --  No sources of languages other than Ada

            Data.Other_Sources_Present := False;

         else
            declare
               Current   : String_List_Id := Languages.Values;
               Element   : String_Element;
               Lang_Name : Name_Id;
               Index     : Language_Index;

            begin
               --  Assume that there is no language specified yet

               Data.Other_Sources_Present := False;
               Data.Ada_Sources_Present   := False;

               --  Look through all the languages specified in attribute
               --  Languages, if any

               while Current /= Nil_String loop
                  Element :=
                    In_Tree.String_Elements.Table (Current);
                  Get_Name_String (Element.Value);
                  To_Lower (Name_Buffer (1 .. Name_Len));
                  Lang_Name := Name_Find;
                  Index := Language_Indexes.Get (Lang_Name);

                  if Index = No_Language_Index then
                     Add_Language_Name (Lang_Name);
                     Index := Last_Language_Index;
                  end if;

                  Set (Index, True, Data, In_Tree);
                  Set (Language_Processing => Default_Language_Processing_Data,
                       For_Language        => Index,
                       In_Project          => Data,
                       In_Tree             => In_Tree);

                  if Index = Ada_Language_Index then
                     Data.Ada_Sources_Present := True;

                  else
                     Data.Other_Sources_Present := True;
                  end if;

                  Current := Element.Next;
               end loop;
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
     (Project   : Project_Id;
      In_Tree   : Project_Tree_Ref;
      Data      : in out Project_Data;
      Extending : Boolean)
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

      Auto_Init_Supported : constant Boolean :=
                              MLib.Tgt.
                                Standalone_Library_Auto_Init_Is_Supported;

      OK : Boolean := True;

   begin
      pragma Assert (Lib_Interfaces.Kind = List);

      --  It is a stand-alone library project file if attribute
      --  Library_Interface is defined.

      if not Lib_Interfaces.Default then
         SAL_Library : declare
            Interfaces     : String_List_Id := Lib_Interfaces.Values;
            Interface_ALIs : String_List_Id := Nil_String;
            Unit           : Name_Id;
            The_Unit_Id    : Unit_Id;
            The_Unit_Data  : Unit_Data;

            procedure Add_ALI_For (Source : Name_Id);
            --  Add an ALI file name to the list of Interface ALIs

            -----------------
            -- Add_ALI_For --
            -----------------

            procedure Add_ALI_For (Source : Name_Id) is
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
                 (In_Tree.String_Elements.Table
                                                     (Interfaces).Value);
               To_Lower (Name_Buffer (1 .. Name_Len));

               if Name_Len = 0 then
                  Error_Msg
                    (Project, In_Tree,
                     "an interface cannot be an empty string",
                     In_Tree.String_Elements.Table
                                                   (Interfaces).Location);

               else
                  Unit := Name_Find;
                  Error_Msg_Name_1 := Unit;
                  The_Unit_Id :=
                    Units_Htable.Get (In_Tree.Units_HT, Unit);

                  if The_Unit_Id = No_Unit then
                     Error_Msg
                       (Project, In_Tree,
                        "unknown unit {",
                        In_Tree.String_Elements.Table
                          (Interfaces).Location);

                  else
                     --  Check that the unit is part of the project

                     The_Unit_Data :=
                       In_Tree.Units.Table (The_Unit_Id);

                     if The_Unit_Data.File_Names (Body_Part).Name /= No_Name
                       and then The_Unit_Data.File_Names (Body_Part).Path /=
                                                                        Slash
                     then
                        if Check_Project
                          (The_Unit_Data.File_Names (Body_Part).Project,
                           Project, In_Tree, Extending)
                        then
                           --  There is a body for this unit.
                           --  If there is no spec, we need to check
                           --  that it is not a subunit.

                           if The_Unit_Data.File_Names
                             (Specification).Name = No_Name
                           then
                              declare
                                 Src_Ind : Source_File_Index;

                              begin
                                 Src_Ind := Sinput.P.Load_Project_File
                                   (Get_Name_String
                                      (The_Unit_Data.File_Names
                                         (Body_Part).Path));

                                 if Sinput.P.Source_File_Is_Subunit
                                   (Src_Ind)
                                 then
                                    Error_Msg
                                      (Project, In_Tree,
                                       "{ is a subunit; " &
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
                              "{ is not an unit of this project",
                              In_Tree.String_Elements.Table
                                (Interfaces).Location);
                        end if;

                     elsif The_Unit_Data.File_Names
                       (Specification).Name /= No_Name
                       and then The_Unit_Data.File_Names
                         (Specification).Path /= Slash
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
                           "{ is not an unit of this project",
                           In_Tree.String_Elements.Table
                                                    (Interfaces).Location);
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

               --  If no attribute Library_Auto_Init is declared, then
               --  set auto init only if it is supported.

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
                     --  Library_Auto_Init cannot be "true" if auto init
                     --  is not supported

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

         --  If attribute Library_Src_Dir is defined and not the
         --  empty string, check if the directory exist and is not
         --  the object directory or one of the source directories.
         --  This is the directory where copies of the interface
         --  sources will be copied. Note that this directory may be
         --  the library directory.

         if Lib_Src_Dir.Value /= Empty_String then
            declare
               Dir_Id : constant Name_Id := Lib_Src_Dir.Value;

            begin
               Locate_Directory
                 (Dir_Id, Data.Display_Directory,
                  Data.Library_Src_Dir,
                  Data.Display_Library_Src_Dir);

               --  If directory does not exist, report an error

               if Data.Library_Src_Dir = No_Name then

                  --  Get the absolute name of the library directory
                  --  that does not exist, to report an error.

                  declare
                     Dir_Name : constant String :=
                       Get_Name_String (Dir_Id);

                  begin
                     if Is_Absolute_Path (Dir_Name) then
                        Err_Vars.Error_Msg_Name_1 := Dir_Id;

                     else
                        Get_Name_String (Data.Directory);

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

                     Error_Msg
                       (Project, In_Tree,
                        "Directory { does not exist",
                        Lib_Src_Dir.Location);
                  end;

                  --  Report an error if it is the same as the object
                  --  directory.

               elsif Data.Library_Src_Dir = Data.Object_Directory then
                  Error_Msg
                    (Project, In_Tree,
                     "directory to copy interfaces cannot be " &
                     "the object directory",
                     Lib_Src_Dir.Location);
                  Data.Library_Src_Dir := No_Name;

               else
                  declare
                     Src_Dirs : String_List_Id;
                     Src_Dir  : String_Element;

                  begin
                     --  Interface copy directory cannot be one of the source
                     --  directory of the current project.

                     Src_Dirs := Data.Source_Dirs;
                     while Src_Dirs /= Nil_String loop
                        Src_Dir := In_Tree.String_Elements.Table
                                                          (Src_Dirs);

                        --  Report error if it is one of the source directories

                        if Data.Library_Src_Dir = Src_Dir.Value then
                           Error_Msg
                             (Project, In_Tree,
                              "directory to copy interfaces cannot " &
                              "be one of the source directories",
                              Lib_Src_Dir.Location);
                           Data.Library_Src_Dir := No_Name;
                           exit;
                        end if;

                        Src_Dirs := Src_Dir.Next;
                     end loop;

                     if Data.Library_Src_Dir /= No_Name then

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

                              if Data.Library_Src_Dir = Src_Dir.Value then
                                 Error_Msg_Name_1 := Src_Dir.Value;
                                 Error_Msg_Name_2 :=
                                   In_Tree.Projects.Table (Pid).Name;
                                 Error_Msg
                                   (Project, In_Tree,
                                    "directory to copy interfaces cannot " &
                                    "be the same as source directory { of " &
                                    "project {",
                                    Lib_Src_Dir.Location);
                                 Data.Library_Src_Dir := No_Name;
                                 exit Project_Loop;
                              end if;

                              Src_Dirs := Src_Dir.Next;
                           end loop Dir_Loop;
                        end loop Project_Loop;
                     end if;
                  end;

                  --  In high verbosity, if there is a valid Library_Src_Dir,
                  --  display its path name.

                  if Data.Library_Src_Dir /= No_Name
                    and then Current_Verbosity = High
                  then
                     Write_Str ("Directory to copy interfaces =""");
                     Write_Str (Get_Name_String (Data.Library_Src_Dir));
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
            --  Library_Symbol_File is defined. Check that the file exists

            Data.Symbol_Data.Symbol_File := Lib_Symbol_File.Value;

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
                  Error_Msg_Name_1 := Lib_Symbol_File.Value;
                  Error_Msg
                    (Project, In_Tree,
                     "symbol file name { is illegal. " &
                     "Name canot include directory info.",
                     Lib_Symbol_File.Location);
               end if;
            end if;
         end if;

         --  If attribute Library_Reference_Symbol_File is not defined,
         --  symbol policy cannot be Compilant or Controlled.

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

            Data.Symbol_Data.Reference := Lib_Ref_Symbol_File.Value;

            Get_Name_String (Lib_Ref_Symbol_File.Value);

            if Name_Len = 0 then
               Error_Msg
                 (Project, In_Tree,
                  "reference symbol file name cannot be an empty string",
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
                  Error_Msg_Name_1 := Lib_Ref_Symbol_File.Value;
                  Error_Msg
                    (Project, In_Tree,
                     "reference symbol file { name is illegal. " &
                     "Name canot include directory info.",
                     Lib_Ref_Symbol_File.Location);
               end if;

               if not Is_Regular_File
                 (Get_Name_String (Data.Object_Directory) &
                  Directory_Separator &
                  Get_Name_String (Lib_Ref_Symbol_File.Value))
               then
                  Error_Msg_Name_1 := Lib_Ref_Symbol_File.Value;

                  --  For controlled symbol policy, it is an error if the
                  --  reference symbol file does not exist. For other symbol
                  --  policies, this is just a warning

                  Error_Msg_Warn :=
                    Data.Symbol_Data.Symbol_Policy /= Controlled;

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

   --------------------
   -- Body_Suffix_Of --
   --------------------

   function Body_Suffix_Of
     (Language   : Language_Index;
      In_Project : Project_Data;
      In_Tree    : Project_Tree_Ref) return String
   is
      Suffix_Id : constant Name_Id :=
                    Suffix_Of (Language, In_Project, In_Tree);
   begin
      if Suffix_Id /= No_Name then
         return Get_Name_String (Suffix_Id);
      else
         return "." & Get_Name_String (Language_Names.Table (Language));
      end if;
   end Body_Suffix_Of;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg
     (Project       : Project_Id;
      In_Tree       : Project_Tree_Ref;
      Msg           : String;
      Flag_Location : Source_Ptr)
   is
      Error_Buffer : String (1 .. 5_000);
      Error_Last   : Natural := 0;
      Msg_Name     : Natural := 0;
      First        : Positive := Msg'First;

      procedure Add (C : Character);
      --  Add a character to the buffer

      procedure Add (S : String);
      --  Add a string to the buffer

      procedure Add (Id : Name_Id);
      --  Add a name to the buffer

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

      procedure Add (Id : Name_Id) is
      begin
         Get_Name_String (Id);
         Add (Name_Buffer (1 .. Name_Len));
      end Add;

   --  Start of processing for Error_Msg

   begin
      if Error_Report = null then
         Prj.Err.Error_Msg (Msg, Flag_Location);
         return;
      end if;

      --  Ignore continuation character

      if Msg (First) = '\' then
         First := First + 1;

         --  Warniung character is always the first one in this package
         --  this is an undoocumented kludge!!!

      elsif Msg (First) = '?' then
         First := First + 1;
         Add ("Warning: ");

      elsif Msg (First) = '<' then
         First := First + 1;

         if Err_Vars.Error_Msg_Warn then
            Add ("Warning: ");
         end if;
      end if;

      for Index in First .. Msg'Last loop
         if Msg (Index) = '{' or else Msg (Index) = '%' then

            --  Include a name between double quotes

            Msg_Name := Msg_Name + 1;
            Add ('"');

            case Msg_Name is
               when 1 => Add (Err_Vars.Error_Msg_Name_1);
               when 2 => Add (Err_Vars.Error_Msg_Name_2);
               when 3 => Add (Err_Vars.Error_Msg_Name_3);

               when others => null;
            end case;

            Add ('"');

         else
            Add (Msg (Index));
         end if;

      end loop;

      Error_Report (Error_Buffer (1 .. Error_Last), Project, In_Tree);
   end Error_Msg;

   ------------------
   -- Find_Sources --
   ------------------

   procedure Find_Sources
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Data         : in out Project_Data;
      For_Language : Language_Index;
      Follow_Links : Boolean := False)
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

                  --  We look to every entry in the source directory

                  Open (Dir, Source_Directory
                               (Source_Directory'First .. Dir_Last));

                  loop
                     Read (Dir, Name_Buffer, Name_Len);

                     if Current_Verbosity = High then
                        Write_Str  ("   Checking ");
                        Write_Line (Name_Buffer (1 .. Name_Len));
                     end if;

                     exit when Name_Len = 0;

                     declare
                        File_Name : constant Name_Id := Name_Find;
                        Path      : constant String :=
                          Normalize_Pathname
                            (Name      => Name_Buffer (1 .. Name_Len),
                             Directory => Source_Directory
                               (Source_Directory'First .. Dir_Last),
                             Resolve_Links => Follow_Links,
                             Case_Sensitive => True);
                        Path_Name : Name_Id;

                     begin
                        Name_Len := Path'Length;
                        Name_Buffer (1 .. Name_Len) := Path;
                        Path_Name := Name_Find;

                        if For_Language = Ada_Language_Index then

                           --  We attempt to register it as a source. However,
                           --  there is no error if the file does not contain
                           --  a valid source. But there is an error if we have
                           --  a duplicate unit name.

                           Record_Ada_Source
                             (File_Name       => File_Name,
                              Path_Name       => Path_Name,
                              Project         => Project,
                              In_Tree         => In_Tree,
                              Data            => Data,
                              Location        => No_Location,
                              Current_Source  => Current_Source,
                              Source_Recorded => Source_Recorded,
                              Follow_Links    => Follow_Links);

                        else
                           Check_For_Source
                             (File_Name        => File_Name,
                              Path_Name        => Path_Name,
                              Project          => Project,
                              In_Tree          => In_Tree,
                              Data             => Data,
                              Location         => No_Location,
                              Language         => For_Language,
                              Suffix           =>
                                Body_Suffix_Of (For_Language, Data, In_Tree),
                              Naming_Exception => False);
                        end if;
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

      if For_Language = Ada_Language_Index then

         --  If we have looked for sources and found none, then
         --  it is an error, except if it is an extending project.
         --  If a non extending project is not supposed to contain
         --  any source, then we never call Find_Sources.

         if Current_Source /= Nil_String then
            Data.Ada_Sources_Present := True;

         elsif Data.Extends = No_Project then
            Error_Msg
              (Project, In_Tree,
               "there are no Ada sources in this project",
               Data.Location);
         end if;
      end if;
   end Find_Sources;

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
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Object_Dir : constant Variable_Value :=
                     Util.Value_Of
                       (Name_Object_Dir, Data.Decl.Attributes, In_Tree);

      Exec_Dir   : constant Variable_Value :=
                     Util.Value_Of
                       (Name_Exec_Dir, Data.Decl.Attributes, In_Tree);

      Source_Dirs : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Source_Dirs, Data.Decl.Attributes, In_Tree);

      Last_Source_Dir : String_List_Id  := Nil_String;

      procedure Find_Source_Dirs (From : Name_Id; Location : Source_Ptr);
      --  Find one or several source directories, and add them
      --  to the list of source directories of the project.

      ----------------------
      -- Find_Source_Dirs --
      ----------------------

      procedure Find_Source_Dirs (From : Name_Id; Location : Source_Ptr) is
         Directory : constant String := Get_Name_String (From);
         Element   : String_Element;

         procedure Recursive_Find_Dirs (Path : Name_Id);
         --  Find all the subdirectories (recursively) of Path and add them
         --  to the list of source directories of the project.

         -------------------------
         -- Recursive_Find_Dirs --
         -------------------------

         procedure Recursive_Find_Dirs (Path : Name_Id) is
            Dir      : Dir_Type;
            Name     : String (1 .. 250);
            Last     : Natural;
            List     : String_List_Id := Data.Source_Dirs;
            Element  : String_Element;
            Found    : Boolean := False;

            Non_Canonical_Path : Name_Id := No_Name;
            Canonical_Path     : Name_Id := No_Name;

            The_Path : constant String :=
                         Normalize_Pathname (Get_Name_String (Path)) &
                         Directory_Separator;

            The_Path_Last : constant Natural :=
                              Compute_Directory_Last (The_Path);

         begin
            Name_Len := The_Path_Last - The_Path'First + 1;
            Name_Buffer (1 .. Name_Len) :=
              The_Path (The_Path'First .. The_Path_Last);
            Non_Canonical_Path := Name_Find;
            Get_Name_String (Non_Canonical_Path);
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            Canonical_Path := Name_Find;

            --  To avoid processing the same directory several times, check
            --  if the directory is already in Recursive_Dirs. If it is,
            --  then there is nothing to do, just return. If it is not, put
            --  it there and continue recursive processing.

            if Recursive_Dirs.Get (Canonical_Path) then
               return;

            else
               Recursive_Dirs.Set (Canonical_Path, True);
            end if;

            --  Check if directory is already in list

            while List /= Nil_String loop
               Element := In_Tree.String_Elements.Table (List);

               if Element.Value /= No_Name then
                  Found := Element.Value = Canonical_Path;
                  exit when Found;
               end if;

               List := Element.Next;
            end loop;

            --  If directory is not already in list, put it there

            if not Found then
               if Current_Verbosity = High then
                  Write_Str  ("   ");
                  Write_Line (The_Path (The_Path'First .. The_Path_Last));
               end if;

               String_Element_Table.Increment_Last
                 (In_Tree.String_Elements);
               Element :=
                 (Value    => Canonical_Path,
                  Display_Value => Non_Canonical_Path,
                  Location => No_Location,
                  Flag     => False,
                  Next     => Nil_String,
                  Index    => 0);

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
                                        The_Path
                                          (The_Path'First .. The_Path_Last),
                                      Resolve_Links  => False,
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
         if Current_Verbosity = High then
            Write_Str ("Find_Source_Dirs (""");
            Write_Str (Directory);
            Write_Line (""")");
         end if;

         --  First, check if we are looking for a directory tree,
         --  indicated by "/**" at the end.

         if Directory'Length >= 3
           and then Directory (Directory'Last - 1 .. Directory'Last) = "**"
           and then (Directory (Directory'Last - 2) = '/'
                       or else
                     Directory (Directory'Last - 2) = Directory_Separator)
         then
            Data.Known_Order_Of_Source_Dirs := False;

            Name_Len := Directory'Length - 3;

            if Name_Len = 0 then

               --  This is the case of "/**": all directories
               --  in the file system.

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
               Base_Dir : constant Name_Id := Name_Find;
               Root_Dir : constant String :=
                            Normalize_Pathname
                              (Name      => Get_Name_String (Base_Dir),
                               Directory =>
                                 Get_Name_String (Data.Display_Directory),
                               Resolve_Links  => False,
                               Case_Sensitive => True);

            begin
               if Root_Dir'Length = 0 then
                  Err_Vars.Error_Msg_Name_1 := Base_Dir;

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
                  --  We have an existing directory, we register it and all
                  --  of its subdirectories.

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
               Path_Name         : Name_Id;
               Display_Path_Name : Name_Id;

            begin
               Locate_Directory
                 (From, Data.Display_Directory, Path_Name, Display_Path_Name);

               if Path_Name = No_Name then
                  Err_Vars.Error_Msg_Name_1 := From;

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
                  --  As it is an existing directory, we add it to
                  --  the list of directories.

                  String_Element_Table.Increment_Last
                    (In_Tree.String_Elements);
                  Element.Value := Path_Name;
                  Element.Display_Value := Display_Path_Name;

                  if Last_Source_Dir = Nil_String then

                     --  This is the first source directory

                     Data.Source_Dirs := String_Element_Table.Last
                                        (In_Tree.String_Elements);

                  else
                     --  We already have source directories,
                     --  link the previous last to the new one.

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

      Data.Object_Directory   := Data.Directory;
      Data.Display_Object_Dir := Data.Display_Directory;

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
              (Object_Dir.Value, Data.Display_Directory,
               Data.Object_Directory, Data.Display_Object_Dir);

            if Data.Object_Directory = No_Name then

               --  The object directory does not exist, report an error

               Err_Vars.Error_Msg_Name_1 := Object_Dir.Value;
               Error_Msg
                 (Project, In_Tree,
                  "the object directory { cannot be found",
                  Data.Location);

               --  Do not keep a nil Object_Directory. Set it to the specified
               --  (relative or absolute) path. This is for the benefit of
               --  tools that recover from errors; for example, these tools
               --  could create the non existent directory.

               Data.Display_Object_Dir := Object_Dir.Value;
               Get_Name_String (Object_Dir.Value);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Data.Object_Directory := Name_Find;
            end if;
         end if;
      end if;

      if Current_Verbosity = High then
         if Data.Object_Directory = No_Name then
            Write_Line ("No object directory");
         else
            Write_Str ("Object directory: """);
            Write_Str (Get_Name_String (Data.Display_Object_Dir));
            Write_Line ("""");
         end if;
      end if;

      --  Check the exec directory

      pragma Assert (Exec_Dir.Kind = Single,
                     "Exec_Dir is not a single string");

      --  We set the object directory to its default

      Data.Exec_Directory   := Data.Object_Directory;
      Data.Display_Exec_Dir := Data.Display_Object_Dir;

      if Exec_Dir.Value /= Empty_String then
         Get_Name_String (Exec_Dir.Value);

         if Name_Len = 0 then
            Error_Msg
              (Project, In_Tree,
               "Exec_Dir cannot be empty",
               Exec_Dir.Location);

         else
            --  We check that the specified object directory
            --  does exist.

            Locate_Directory
              (Exec_Dir.Value, Data.Directory,
               Data.Exec_Directory, Data.Display_Exec_Dir);

            if Data.Exec_Directory = No_Name then
               Err_Vars.Error_Msg_Name_1 := Exec_Dir.Value;
               Error_Msg
                 (Project, In_Tree,
                  "the exec directory { cannot be found",
                  Data.Location);
            end if;
         end if;
      end if;

      if Current_Verbosity = High then
         if Data.Exec_Directory = No_Name then
            Write_Line ("No exec directory");
         else
            Write_Str ("Exec directory: """);
            Write_Str (Get_Name_String (Data.Display_Exec_Dir));
            Write_Line ("""");
         end if;
      end if;

      --  Look for the source directories

      if Current_Verbosity = High then
         Write_Line ("Starting to look for source directories");
      end if;

      pragma Assert (Source_Dirs.Kind = List, "Source_Dirs is not a list");

      if Source_Dirs.Default then

         --  No Source_Dirs specified: the single source directory
         --  is the one containing the project file

         String_Element_Table.Increment_Last
           (In_Tree.String_Elements);
         Data.Source_Dirs := String_Element_Table.Last
           (In_Tree.String_Elements);
         In_Tree.String_Elements.Table (Data.Source_Dirs) :=
           (Value         => Data.Directory,
            Display_Value => Data.Display_Directory,
            Location      => No_Location,
            Flag          => False,
            Next          => Nil_String,
            Index         => 0);

         if Current_Verbosity = High then
            Write_Line ("Single source directory:");
            Write_Str ("    """);
            Write_Str (Get_Name_String (Data.Display_Directory));
            Write_Line ("""");
         end if;

      elsif Source_Dirs.Values = Nil_String then

         --  If Source_Dirs is an empty string list, this means
         --  that this project contains no source. For projects that
         --  don't extend other projects, this also means that there is no
         --  need for an object directory, if not specified.

         if Data.Extends = No_Project
           and then  Data.Object_Directory = Data.Directory
         then
            Data.Object_Directory := No_Name;
         end if;

         Data.Source_Dirs           := Nil_String;
         Data.Ada_Sources_Present   := False;
         Data.Other_Sources_Present := False;

      else
         declare
            Source_Dir : String_List_Id := Source_Dirs.Values;
            Element    : String_Element;

         begin
            --  We will find the source directories for each
            --  element of the list

            while Source_Dir /= Nil_String loop
               Element :=
                 In_Tree.String_Elements.Table (Source_Dir);
               Find_Source_Dirs (Element.Value, Element.Location);
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
               Get_Name_String (Element.Value);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Element.Value := Name_Find;
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
      Data    : in out Project_Data) is
      Mains : constant Variable_Value :=
                Prj.Util.Value_Of (Name_Main, Data.Decl.Attributes, In_Tree);

   begin
      Data.Mains := Mains.Values;

      --  If no Mains were specified, and if we are an extending
      --  project, inherit the Mains from the project we are extending.

      if Mains.Default then
         if Data.Extends /= No_Project then
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
      Source_Name : Name_Id;

   begin
      Source_Names.Reset;

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
               --  ??? we should check that there is no directory information

               Name_Len := Last;
               Name_Buffer (1 .. Name_Len) := Line (1 .. Last);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Source_Name := Name_Find;
               Source_Names.Set
                 (K => Source_Name,
                  E =>
                    (Name     => Source_Name,
                     Location => Location,
                     Found    => False));
            end if;
         end loop;

         Prj.Util.Close (File);

      end if;
   end Get_Sources_From_File;

   --------------
   -- Get_Unit --
   --------------

   procedure Get_Unit
     (Canonical_File_Name : Name_Id;
      Naming              : Naming_Data;
      Exception_Id        : out Ada_Naming_Exception_Id;
      Unit_Name           : out Name_Id;
      Unit_Kind           : out Spec_Or_Body;
      Needs_Pragma        : out Boolean)
   is
      Info_Id  : Ada_Naming_Exception_Id
        := Ada_Naming_Exceptions.Get (Canonical_File_Name);
      VMS_Name : Name_Id;

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

      declare
         File          : String := Name_Buffer (1 .. Name_Len);
         First         : constant Positive := File'First;
         Last          : Natural           := File'Last;
         Standard_GNAT : Boolean;

      begin
         Standard_GNAT :=
           Naming.Ada_Spec_Suffix = Default_Ada_Spec_Suffix
             and then Naming.Ada_Body_Suffix = Default_Ada_Body_Suffix;

         --  Check if the end of the file name is Specification_Append

         Get_Name_String (Naming.Ada_Spec_Suffix);

         if File'Length > Name_Len
           and then File (Last - Name_Len + 1 .. Last) =
                                                Name_Buffer (1 .. Name_Len)
         then
            --  We have a spec

            Unit_Kind := Specification;
            Last := Last - Name_Len;

            if Current_Verbosity = High then
               Write_Str  ("   Specification: ");
               Write_Line (File (First .. Last));
            end if;

         else
            Get_Name_String (Naming.Ada_Body_Suffix);

            --  Check if the end of the file name is Body_Append

            if File'Length > Name_Len
              and then File (Last - Name_Len + 1 .. Last) =
                                                Name_Buffer (1 .. Name_Len)
            then
               --  We have a body

               Unit_Kind := Body_Part;
               Last := Last - Name_Len;

               if Current_Verbosity = High then
                  Write_Str  ("   Body: ");
                  Write_Line (File (First .. Last));
               end if;

            elsif Naming.Separate_Suffix /= Naming.Ada_Spec_Suffix then
               Get_Name_String (Naming.Separate_Suffix);

               --  Check if the end of the file name is Separate_Append

               if File'Length > Name_Len
                 and then File (Last - Name_Len + 1 .. Last) =
                                                Name_Buffer (1 .. Name_Len)
               then
                  --  We have a separate (a body)

                  Unit_Kind := Body_Part;
                  Last := Last - Name_Len;

                  if Current_Verbosity = High then
                     Write_Str  ("   Separate: ");
                     Write_Line (File (First .. Last));
                  end if;

               else
                  Last := 0;
               end if;

            else
               Last := 0;
            end if;
         end if;

         if Last = 0 then

            --  This is not a source file

            Unit_Name := No_Name;
            Unit_Kind := Specification;

            if Current_Verbosity = High then
               Write_Line ("   Not a valid file name.");
            end if;

            return;
         end if;

         Get_Name_String (Naming.Dot_Replacement);
         Standard_GNAT :=
           Standard_GNAT and then Name_Buffer (1 .. Name_Len) = "-";

         if Name_Buffer (1 .. Name_Len) /= "." then

            --  If Dot_Replacement is not a single dot, then there should
            --  not be any dot in the name.

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
            Src : String := File (First .. Last);

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
                  if S1 = 'a' or else S1 = 'g'
                    or else S1 = 'i' or else S1 = 's'
                  then
                     --  Children or separates of packages A, G, I or S

                     if (OpenVMS_On_Target
                         and then S2 = '_'
                         and then S3 = '_')
                        or else
                         S2 = '~'
                     then
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
               Write_Line (Src);
            end if;

            --  Now, we check if this name is a valid unit name

            Check_Ada_Name (Name => Src, Unit => Unit_Name);
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

      --  If dot replacement is a single dot, and first character of
      --  suffix is also a dot

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
     (Name    : Name_Id;
      Parent  : Name_Id;
      Dir     : out Name_Id;
      Display : out Name_Id)
   is
      The_Name   : constant String := Get_Name_String (Name);

      The_Parent : constant String :=
                     Get_Name_String (Parent) & Directory_Separator;

      The_Parent_Last : constant Natural :=
                     Compute_Directory_Last (The_Parent);

   begin
      if Current_Verbosity = High then
         Write_Str ("Locate_Directory (""");
         Write_Str (The_Name);
         Write_Str (""", """);
         Write_Str (The_Parent);
         Write_Line (""")");
      end if;

      Dir     := No_Name;
      Display := No_Name;

      if Is_Absolute_Path (The_Name) then
         if Is_Directory (The_Name) then
            declare
               Normed : constant String :=
                          Normalize_Pathname
                            (The_Name,
                             Resolve_Links  => False,
                             Case_Sensitive => True);

               Canonical_Path : constant String :=
                                  Normalize_Pathname
                                    (Normed,
                                     Resolve_Links  => True,
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

      else
         declare
            Full_Path : constant String :=
                          The_Parent (The_Parent'First .. The_Parent_Last) &
                          The_Name;

         begin
            if Is_Directory (Full_Path) then
               declare
                  Normed : constant String :=
                             Normalize_Pathname
                               (Full_Path,
                                Resolve_Links  => False,
                                Case_Sensitive => True);

                  Canonical_Path : constant String :=
                                     Normalize_Pathname
                                       (Normed,
                                        Resolve_Links  => True,
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
      end if;
   end Locate_Directory;

   ----------------------
   -- Look_For_Sources --
   ----------------------

   procedure Look_For_Sources
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Data         : in out Project_Data;
      Follow_Links : Boolean)
   is
      procedure Get_Path_Names_And_Record_Sources (Follow_Links : Boolean);
      --  Find the path names of the source files in the Source_Names table
      --  in the source directories and record those that are Ada sources.

      procedure Get_Sources_From_File
        (Path     : String;
         Location : Source_Ptr);
      --  Get the sources of a project from a text file

      ---------------------------------------
      -- Get_Path_Names_And_Record_Sources --
      ---------------------------------------

      procedure Get_Path_Names_And_Record_Sources (Follow_Links : Boolean) is
         Source_Dir : String_List_Id := Data.Source_Dirs;
         Element    : String_Element;
         Path       : Name_Id;

         Dir      : Dir_Type;
         Name     : Name_Id;
         Canonical_Name : Name_Id;
         Name_Str : String (1 .. 1_024);
         Last     : Natural := 0;
         NL       : Name_Location;

         Current_Source : String_List_Id := Nil_String;

         First_Error : Boolean := True;

         Source_Recorded : Boolean := False;

      begin
         --  We look in all source directories for the file names in the
         --  hash table Source_Names

         while Source_Dir /= Nil_String loop
            Source_Recorded := False;
            Element := In_Tree.String_Elements.Table (Source_Dir);

            declare
               Dir_Path : constant String := Get_Name_String (Element.Value);
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
                  Canonical_Case_File_Name (Name_Str (1 .. Last));
                  Name_Len := Last;
                  Name_Buffer (1 .. Name_Len) := Name_Str (1 .. Last);
                  Canonical_Name := Name_Find;
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
                        Follow_Links    => Follow_Links);
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
               Err_Vars.Error_Msg_Name_1 := NL.Name;

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
      end Get_Path_Names_And_Record_Sources;

      ---------------------------
      -- Get_Sources_From_File --
      ---------------------------

      procedure Get_Sources_From_File
        (Path     : String;
         Location : Source_Ptr)
      is
      begin
         --  Get the list of sources from the file and put them in hash table
         --  Source_Names.

         Get_Sources_From_File (Path, Location, Project, In_Tree);

         --  Look in the source directories to find those sources

         Get_Path_Names_And_Record_Sources (Follow_Links);

         --  We should have found at least one source.
         --  If not, report an error.

         if Data.Sources = Nil_String then
            Error_Msg (Project, In_Tree,
                       "there are no Ada sources in this project",
                       Location);
         end if;
      end Get_Sources_From_File;

   begin
      if Data.Ada_Sources_Present then
         declare
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

            Locally_Removed  : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Locally_Removed_Files,
                                    Data.Decl.Attributes,
                                    In_Tree);

         begin
            pragma Assert
              (Sources.Kind = List,
               "Source_Files is not a list");

            pragma Assert
              (Source_List_File.Kind = Single,
               "Source_List_File is not a single string");

            if not Sources.Default then
               if not Source_List_File.Default then
                  Error_Msg
                    (Project, In_Tree,
                     "?both variables source_files and " &
                     "source_list_file are present",
                     Source_List_File.Location);
               end if;

               --  Sources is a list of file names

               declare
                  Current  : String_List_Id := Sources.Values;
                  Element  : String_Element;
                  Location : Source_Ptr;
                  Name     : Name_Id;

               begin
                  Source_Names.Reset;

                  Data.Ada_Sources_Present := Current /= Nil_String;

                  while Current /= Nil_String loop
                     Element :=
                       In_Tree.String_Elements.Table (Current);
                     Get_Name_String (Element.Value);
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Name := Name_Find;

                     --  If the element has no location, then use the
                     --  location of Sources to report possible errors.

                     if Element.Location = No_Location then
                        Location := Sources.Location;
                     else
                        Location := Element.Location;
                     end if;

                     Source_Names.Set
                       (K => Name,
                        E =>
                          (Name     => Name,
                           Location => Location,
                           Found    => False));

                     Current := Element.Next;
                  end loop;

                  Get_Path_Names_And_Record_Sources (Follow_Links);
               end;

               --  No source_files specified

               --  We check Source_List_File has been specified

            elsif not Source_List_File.Default then

               --  Source_List_File is the name of the file
               --  that contains the source file names

               declare
                  Source_File_Path_Name : constant String :=
                                            Path_Name_Of
                                              (Source_List_File.Value,
                                               Data.Directory);

               begin
                  if Source_File_Path_Name'Length = 0 then
                     Err_Vars.Error_Msg_Name_1 := Source_List_File.Value;
                     Error_Msg
                       (Project, In_Tree,
                        "file with sources { does not exist",
                        Source_List_File.Location);

                  else
                     Get_Sources_From_File
                       (Source_File_Path_Name,
                        Source_List_File.Location);
                  end if;
               end;

            else
               --  Neither Source_Files nor Source_List_File has been
               --  specified. Find all the files that satisfy the naming
               --  scheme in all the source directories.

               Find_Sources
                 (Project, In_Tree, Data, Ada_Language_Index, Follow_Links);
            end if;

            --  If there are sources that are locally removed, mark them as
            --  such in the Units table.

            if not Locally_Removed.Default then

               --  Sources can be locally removed only in extending
               --  project files.

               if Data.Extends = No_Project then
                  Error_Msg
                    (Project, In_Tree,
                     "Locally_Removed_Files can only be used " &
                     "in an extending project file",
                     Locally_Removed.Location);

               else
                  declare
                     Current  : String_List_Id := Locally_Removed.Values;
                     Element  : String_Element;
                     Location : Source_Ptr;
                     OK       : Boolean;
                     Unit     : Unit_Data;
                     Name     : Name_Id;
                     Extended : Project_Id;

                  begin
                     while Current /= Nil_String loop
                        Element :=
                          In_Tree.String_Elements.Table (Current);
                        Get_Name_String (Element.Value);
                        Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                        Name := Name_Find;

                        --  If the element has no location, then use the
                        --  location of Locally_Removed to report
                        --  possible errors.

                        if Element.Location = No_Location then
                           Location := Locally_Removed.Location;
                        else
                           Location := Element.Location;
                        end if;

                        OK := False;

                        for Index in Unit_Table.First ..
                                  Unit_Table.Last (In_Tree.Units)
                        loop
                           Unit := In_Tree.Units.Table (Index);

                           if Unit.File_Names (Specification).Name = Name then
                              OK := True;

                              --  Check that this is from a project that
                              --  the current project extends, but not the
                              --  current project.

                              Extended := Unit.File_Names
                                (Specification).Project;

                              if Extended = Project then
                                 Error_Msg
                                   (Project, In_Tree,
                                    "cannot remove a source " &
                                    "of the same project",
                                    Location);

                              elsif
                                Project_Extends (Project, Extended, In_Tree)
                              then
                                 Unit.File_Names
                                   (Specification).Path := Slash;
                                 Unit.File_Names
                                   (Specification).Needs_Pragma := False;
                                 In_Tree.Units.Table (Index) :=
                                   Unit;
                                 Add_Forbidden_File_Name
                                   (Unit.File_Names (Specification).Name);
                                 exit;

                              else
                                 Error_Msg
                                   (Project, In_Tree,
                                    "cannot remove a source from " &
                                    "another project",
                                    Location);
                              end if;

                           elsif
                             Unit.File_Names (Body_Part).Name = Name
                           then
                              OK := True;

                              --  Check that this is from a project that
                              --  the current project extends, but not the
                              --  current project.

                              Extended := Unit.File_Names
                                (Body_Part).Project;

                              if Extended = Project then
                                 Error_Msg
                                   (Project, In_Tree,
                                    "cannot remove a source " &
                                    "of the same project",
                                    Location);

                              elsif
                                Project_Extends (Project, Extended, In_Tree)
                              then
                                 Unit.File_Names (Body_Part).Path := Slash;
                                 Unit.File_Names (Body_Part).Needs_Pragma
                                   := False;
                                 In_Tree.Units.Table (Index) :=
                                   Unit;
                                 Add_Forbidden_File_Name
                                   (Unit.File_Names (Body_Part).Name);
                                 exit;
                              end if;

                           end if;
                        end loop;

                        if not OK then
                           Err_Vars.Error_Msg_Name_1 := Name;
                           Error_Msg
                             (Project, In_Tree, "unknown file {", Location);
                        end if;

                        Current := Element.Next;
                     end loop;
                  end;
               end if;
            end if;
         end;
      end if;

      if Data.Other_Sources_Present then

         --  Set Source_Present to False. It will be set back to True
         --  whenever a source is found.

         Data.Other_Sources_Present := False;
         for Lang in Ada_Language_Index + 1 .. Last_Language_Index loop

            --  For each language (other than Ada) in the project file

            if Is_Present (Lang, Data, In_Tree) then

               --  Reset the indication that there are sources of this
               --  language. It will be set back to True whenever we find a
               --  source of the language.

               Set (Lang, False, Data, In_Tree);

               --  First, get the source suffix for the language

               Set (Suffix       => Suffix_For (Lang, Data.Naming, In_Tree),
                    For_Language => Lang,
                    In_Project   => Data,
                    In_Tree      => In_Tree);

               --  Then, deal with the naming exceptions, if any

               Source_Names.Reset;

               declare
                  Naming_Exceptions : constant Variable_Value :=
                    Value_Of
                      (Index     => Language_Names.Table (Lang),
                       Src_Index => 0,
                       In_Array  => Data.Naming.Implementation_Exceptions,
                       In_Tree   => In_Tree);
                  Element_Id        : String_List_Id;
                  Element           : String_Element;
                  File_Id           : Name_Id;
                  Source_Found      : Boolean := False;

               begin
                  --  If there are naming exceptions, look through them one
                  --  by one.

                  if Naming_Exceptions /= Nil_Variable_Value then
                     Element_Id := Naming_Exceptions.Values;

                     while Element_Id /= Nil_String loop
                        Element := In_Tree.String_Elements.Table
                                                          (Element_Id);
                        Get_Name_String (Element.Value);
                        Canonical_Case_File_Name
                          (Name_Buffer (1 .. Name_Len));
                        File_Id := Name_Find;

                        --  Put each naming exception in the Source_Names
                        --  hash table, but if there are repetition, don't
                        --  bother after the first instance.

                        if
                          Source_Names.Get (File_Id) = No_Name_Location
                        then
                           Source_Found := True;
                           Source_Names.Set
                             (File_Id,
                              (Name     => File_Id,
                               Location => Element.Location,
                               Found    => False));
                        end if;

                        Element_Id := Element.Next;
                     end loop;

                     --  If there is at least one naming exception, record
                     --  those that are found in the source directories.

                     if Source_Found then
                        Record_Other_Sources
                          (Project           => Project,
                           In_Tree           => In_Tree,
                           Data              => Data,
                           Language          => Lang,
                           Naming_Exceptions => True);
                     end if;

                  end if;
               end;

               --  Now, check if a list of sources is declared either through
               --  a string list (attribute Source_Files) or a text file
               --  (attribute Source_List_File). If a source list is declared,
               --  we will consider only those naming exceptions that are
               --  on the list.

               declare
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

               begin
                  pragma Assert
                    (Sources.Kind = List,
                     "Source_Files is not a list");

                  pragma Assert
                    (Source_List_File.Kind = Single,
                     "Source_List_File is not a single string");

                  if not Sources.Default then
                     if not Source_List_File.Default then
                        Error_Msg
                          (Project, In_Tree,
                           "?both variables source_files and " &
                           "source_list_file are present",
                           Source_List_File.Location);
                     end if;

                     --  Sources is a list of file names

                     declare
                        Current  : String_List_Id := Sources.Values;
                        Element  : String_Element;
                        Location : Source_Ptr;
                        Name     : Name_Id;

                     begin
                        Source_Names.Reset;

                        --  Put all the sources in the Source_Names hash table

                        while Current /= Nil_String loop
                           Element :=
                             In_Tree.String_Elements.Table
                               (Current);
                           Get_Name_String (Element.Value);
                           Canonical_Case_File_Name
                             (Name_Buffer (1 .. Name_Len));
                           Name := Name_Find;

                           --  If the element has no location, then use the
                           --  location of Sources to report possible errors.

                           if Element.Location = No_Location then
                              Location := Sources.Location;
                           else
                              Location := Element.Location;
                           end if;

                           Source_Names.Set
                             (K => Name,
                              E =>
                                (Name     => Name,
                                 Location => Location,
                                 Found    => False));

                           Current := Element.Next;
                        end loop;

                        --  And look for their directories

                        Record_Other_Sources
                          (Project           => Project,
                           In_Tree           => In_Tree,
                           Data              => Data,
                           Language          => Lang,
                           Naming_Exceptions => False);
                     end;

                     --  No source_files specified

                     --  We check if Source_List_File has been specified

                  elsif not Source_List_File.Default then

                     --  Source_List_File is the name of the file
                     --  that contains the source file names

                     declare
                        Source_File_Path_Name : constant String :=
                          Path_Name_Of
                            (Source_List_File.Value,
                             Data.Directory);

                     begin
                        if Source_File_Path_Name'Length = 0 then
                           Err_Vars.Error_Msg_Name_1 :=
                             Source_List_File.Value;
                           Error_Msg
                             (Project, In_Tree,
                              "file with sources { does not exist",
                              Source_List_File.Location);

                        else
                           --  Read the file, putting each source in the
                           --  Source_Names hash table.

                           Get_Sources_From_File
                             (Source_File_Path_Name,
                              Source_List_File.Location,
                              Project, In_Tree);

                           --  And look for their directories

                           Record_Other_Sources
                             (Project           => Project,
                              In_Tree           => In_Tree,
                              Data              => Data,
                              Language          => Lang,
                              Naming_Exceptions => False);
                        end if;
                     end;

                  --  Neither Source_Files nor Source_List_File was specified

                  else
                     --  Find all the files that satisfy the naming scheme in
                     --  all the source directories. All the naming exceptions
                     --  that effectively exist are also part of the source
                     --  of this language.

                     Find_Sources (Project, In_Tree, Data, Lang);
                  end if;
               end;
            end if;
         end loop;
      end if;
   end Look_For_Sources;

   ------------------
   -- Path_Name_Of --
   ------------------

   function Path_Name_Of
     (File_Name : Name_Id;
      Directory : Name_Id) return String
   is
      Result : String_Access;

      The_Directory : constant String := Get_Name_String (Directory);

   begin
      Get_Name_String (File_Name);
      Result := Locate_Regular_File
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
      Current : Array_Element_Id := List;
      Element : Array_Element;

      Unit : Unit_Info;

   begin
      --  Traverse the list

      while Current /= No_Array_Element loop
         Element := In_Tree.Array_Elements.Table (Current);

         if Element.Index /= No_Name then
            Unit :=
              (Kind => Kind,
               Unit => Element.Index,
               Next => No_Ada_Naming_Exception);
            Reverse_Ada_Naming_Exceptions.Set
              (Unit, (Element.Value.Value, Element.Value.Index));
            Unit.Next := Ada_Naming_Exceptions.Get (Element.Value.Value);
            Ada_Naming_Exception_Table.Increment_Last;
            Ada_Naming_Exception_Table.Table
              (Ada_Naming_Exception_Table.Last) := Unit;
            Ada_Naming_Exceptions.Set
              (Element.Value.Value, Ada_Naming_Exception_Table.Last);
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
     (File_Name       : Name_Id;
      Path_Name       : Name_Id;
      Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Data            : in out Project_Data;
      Location        : Source_Ptr;
      Current_Source  : in out String_List_Id;
      Source_Recorded : in out Boolean;
      Follow_Links    : Boolean)
   is
      Canonical_File_Name : Name_Id;
      Canonical_Path_Name : Name_Id;

      Exception_Id : Ada_Naming_Exception_Id;
      Unit_Name    : Name_Id;
      Unit_Kind    : Spec_Or_Body;
      Unit_Index   : Int := 0;
      Info         : Unit_Info;
      Name_Index   : Name_And_Index;
      Needs_Pragma : Boolean;

      The_Location    : Source_Ptr              := Location;
      Previous_Source : constant String_List_Id := Current_Source;
      Except_Name     : Name_And_Index          := No_Name_And_Index;

      Unit_Prj : Unit_Project;

      File_Name_Recorded : Boolean := False;

   begin
      Get_Name_String (File_Name);
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      Canonical_File_Name := Name_Find;

      declare
         Canonical_Path : constant String :=
                            Normalize_Pathname
                              (Get_Name_String (Path_Name),
                               Resolve_Links => Follow_Links,
                               Case_Sensitive => False);
      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Canonical_Path);
         Canonical_Path_Name := Name_Find;
      end;

      --  Find out the unit name, the unit kind and if it needs
      --  a specific SFN pragma.

      Get_Unit
        (Canonical_File_Name => Canonical_File_Name,
         Naming              => Data.Naming,
         Exception_Id        => Exception_Id,
         Unit_Name           => Unit_Name,
         Unit_Kind           => Unit_Kind,
         Needs_Pragma        => Needs_Pragma);

      if Exception_Id = No_Ada_Naming_Exception and then
        Unit_Name = No_Name
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

               --  The file is not included in the source of the project,
               --  because it is hidden by the exception.
               --  So, there is nothing else to do.

               return;
            end if;
         end if;

         loop
            if Exception_Id /= No_Ada_Naming_Exception then
               Info := Ada_Naming_Exception_Table.Table (Exception_Id);
               Exception_Id := Info.Next;
               Info.Next := No_Ada_Naming_Exception;
               Name_Index := Reverse_Ada_Naming_Exceptions.Get (Info);

               Unit_Name  := Info.Unit;
               Unit_Index := Name_Index.Index;
               Unit_Kind  := Info.Kind;
            end if;

            --  Put the file name in the list of sources of the project

            if not File_Name_Recorded then
               String_Element_Table.Increment_Last
                 (In_Tree.String_Elements);
               In_Tree.String_Elements.Table
                 (String_Element_Table.Last
                   (In_Tree.String_Elements)) :=
                 (Value         => Canonical_File_Name,
                  Display_Value => File_Name,
                  Location      => No_Location,
                  Flag          => False,
                  Next          => Nil_String,
                  Index         => Unit_Index);
            end if;

            if Current_Source = Nil_String then
               Data.Sources := String_Element_Table.Last
                                 (In_Tree.String_Elements);
            else
               In_Tree.String_Elements.Table
                 (Current_Source).Next :=
                 String_Element_Table.Last
                   (In_Tree.String_Elements);
            end if;

            Current_Source := String_Element_Table.Last
                                (In_Tree.String_Elements);

            --  Put the unit in unit list

            declare
               The_Unit      : Unit_Id :=
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

               if The_Unit /= No_Unit then
                  The_Unit_Data := In_Tree.Units.Table (The_Unit);

                  if The_Unit_Data.File_Names (Unit_Kind).Name = No_Name
                    or else Project_Extends
                      (Data.Extends,
                       The_Unit_Data.File_Names (Unit_Kind).Project,
                       In_Tree)
                  then
                     if The_Unit_Data.File_Names (Unit_Kind).Path = Slash then
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
                        Index        => Unit_Index,
                        Display_Name => File_Name,
                        Path         => Canonical_Path_Name,
                        Display_Path => Path_Name,
                        Project      => Project,
                        Needs_Pragma => Needs_Pragma);
                     In_Tree.Units.Table (The_Unit) :=
                       The_Unit_Data;
                     Source_Recorded := True;

                  elsif The_Unit_Data.File_Names (Unit_Kind).Project = Project
                    and then (Data.Known_Order_Of_Source_Dirs or else
                              The_Unit_Data.File_Names (Unit_Kind).Path =
                                Canonical_Path_Name)
                  then
                     if Previous_Source = Nil_String then
                        Data.Sources := Nil_String;
                     else
                        In_Tree.String_Elements.Table
                          (Previous_Source).Next := Nil_String;
                        String_Element_Table.Decrement_Last
                          (In_Tree.String_Elements);
                     end if;

                     Current_Source := Previous_Source;

                  else
                     --  It is an error to have two units with the same name
                     --  and the same kind (spec or body).

                     if The_Location = No_Location then
                        The_Location :=
                          In_Tree.Projects.Table
                            (Project).Location;
                     end if;

                     Err_Vars.Error_Msg_Name_1 := Unit_Name;
                     Error_Msg
                       (Project, In_Tree, "duplicate source {", The_Location);

                     Err_Vars.Error_Msg_Name_1 :=
                       In_Tree.Projects.Table
                         (The_Unit_Data.File_Names (Unit_Kind).Project).Name;
                     Err_Vars.Error_Msg_Name_2 :=
                       The_Unit_Data.File_Names (Unit_Kind).Path;
                     Error_Msg
                       (Project, In_Tree,
                        "\   project file {, {", The_Location);

                     Err_Vars.Error_Msg_Name_1 :=
                       In_Tree.Projects.Table (Project).Name;
                     Err_Vars.Error_Msg_Name_2 := Canonical_Path_Name;
                     Error_Msg
                       (Project, In_Tree,
                        "\   project file {, {", The_Location);
                  end if;

               --  It is a new unit, create a new record

               else
                  --  First, check if there is no other unit with this file
                  --  name in another project. If it is, report an error.
                  --  Of course, we do that only for the first unit in the
                  --  source file.

                  Unit_Prj := Files_Htable.Get
                    (In_Tree.Files_HT, Canonical_File_Name);

                  if not File_Name_Recorded and then
                    Unit_Prj /= No_Unit_Project
                  then
                     Error_Msg_Name_1 := File_Name;
                     Error_Msg_Name_2 :=
                       In_Tree.Projects.Table
                         (Unit_Prj.Project).Name;
                     Error_Msg
                       (Project, In_Tree,
                        "{ is already a source of project {",
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
                        Index        => Unit_Index,
                        Display_Name => File_Name,
                        Path         => Canonical_Path_Name,
                        Display_Path => Path_Name,
                        Project      => Project,
                        Needs_Pragma => Needs_Pragma);
                     In_Tree.Units.Table (The_Unit) :=
                       The_Unit_Data;
                     Source_Recorded := True;
                  end if;
               end if;
            end;

            exit when Exception_Id = No_Ada_Naming_Exception;
            File_Name_Recorded := True;
         end loop;
      end if;
   end Record_Ada_Source;

   --------------------------
   -- Record_Other_Sources --
   --------------------------

   procedure Record_Other_Sources
     (Project           : Project_Id;
      In_Tree           : Project_Tree_Ref;
      Data              : in out Project_Data;
      Language          : Language_Index;
      Naming_Exceptions : Boolean)
   is
      Source_Dir : String_List_Id := Data.Source_Dirs;
      Element    : String_Element;
      Path       : Name_Id;

      Dir            : Dir_Type;
      Canonical_Name : Name_Id;

      Name_Str : String (1 .. 1_024);
      Last     : Natural := 0;
      NL       : Name_Location;

      First_Error : Boolean := True;

      Suffix : constant String := Body_Suffix_Of (Language, Data, In_Tree);

   begin
      while Source_Dir /= Nil_String loop
         Element := In_Tree.String_Elements.Table (Source_Dir);

         declare
            Dir_Path : constant String := Get_Name_String (Element.Value);

         begin
            if Current_Verbosity = High then
               Write_Str ("checking directory """);
               Write_Str (Dir_Path);
               Write_Str (""" for ");

               if Naming_Exceptions then
                  Write_Str ("naming exceptions");

               else
                  Write_Str ("sources");
               end if;

               Write_Str (" of Language ");
               Display_Language_Name (Language);
            end if;

            Open (Dir, Dir_Path);

            loop
               Read (Dir, Name_Str, Last);
               exit when Last = 0;

               if Is_Regular_File
                 (Dir_Path & Directory_Separator & Name_Str (1 .. Last))
               then
                  Name_Len := Last;
                  Name_Buffer (1 .. Name_Len) := Name_Str (1 .. Last);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                  Canonical_Name := Name_Find;
                  NL := Source_Names.Get (Canonical_Name);

                  if NL /= No_Name_Location then
                     if NL.Found then
                        if not Data.Known_Order_Of_Source_Dirs then
                           Error_Msg_Name_1 := Canonical_Name;
                           Error_Msg
                             (Project, In_Tree,
                              "{ is found in several source directories",
                              NL.Location);
                        end if;

                     else
                        NL.Found := True;
                        Source_Names.Set (Canonical_Name, NL);
                        Name_Len := Dir_Path'Length;
                        Name_Buffer (1 .. Name_Len) := Dir_Path;
                        Add_Char_To_Name_Buffer (Directory_Separator);
                        Add_Str_To_Name_Buffer (Name_Str (1 .. Last));
                        Path := Name_Find;

                        Check_For_Source
                          (File_Name        => Canonical_Name,
                           Path_Name        => Path,
                           Project          => Project,
                           In_Tree          => In_Tree,
                           Data             => Data,
                           Location         => NL.Location,
                           Language         => Language,
                           Suffix           => Suffix,
                           Naming_Exception => Naming_Exceptions);
                     end if;
                  end if;
               end if;
            end loop;

            Close (Dir);
         end;

         Source_Dir := Element.Next;
      end loop;

      if not Naming_Exceptions then
         NL := Source_Names.Get_First;

         --  It is an error if a source file name in a source list or
         --  in a source list file is not found.

         while NL /= No_Name_Location loop
            if not NL.Found then
               Err_Vars.Error_Msg_Name_1 := NL.Name;

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

         --  Any naming exception of this language that is not in a list
         --  of sources must be removed.

         declare
            Source_Id : Other_Source_Id := Data.First_Other_Source;
            Prev_Id   : Other_Source_Id := No_Other_Source;
            Source    : Other_Source;

         begin
            while Source_Id /= No_Other_Source loop
               Source := In_Tree.Other_Sources.Table (Source_Id);

               if Source.Language = Language
                 and then Source.Naming_Exception
               then
                  if Current_Verbosity = High then
                     Write_Str ("Naming exception """);
                     Write_Str (Get_Name_String (Source.File_Name));
                     Write_Str (""" is not in the list of sources,");
                     Write_Line (" so it is removed.");
                  end if;

                  if Prev_Id = No_Other_Source then
                     Data.First_Other_Source := Source.Next;

                  else
                     In_Tree.Other_Sources.Table
                       (Prev_Id).Next := Source.Next;
                  end if;

                  Source_Id := Source.Next;

                  if Source_Id = No_Other_Source then
                     Data.Last_Other_Source := Prev_Id;
                  end if;

               else
                  Prev_Id := Source_Id;
                  Source_Id := Source.Next;
               end if;
            end loop;
         end;
      end if;
   end Record_Other_Sources;

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

      Current := In_Tree.Projects.Table (Project).Source_Dirs;
      while Current /= Nil_String loop
         Element := In_Tree.String_Elements.Table (Current);
         Write_Str  ("   ");
         Write_Line (Get_Name_String (Element.Value));
         Current := Element.Next;
      end loop;

      Write_Line ("end Source_Dirs.");
   end Show_Source_Dirs;

   ----------------
   -- Suffix_For --
   ----------------

   function Suffix_For
     (Language : Language_Index;
      Naming   : Naming_Data;
      In_Tree  : Project_Tree_Ref) return Name_Id
   is
      Suffix : constant Variable_Value :=
        Value_Of
          (Index     => Language_Names.Table (Language),
           Src_Index => 0,
           In_Array  => Naming.Body_Suffix,
           In_Tree   => In_Tree);
   begin
      --  If no suffix for this language in package Naming, use the default

      if Suffix = Nil_Variable_Value then
         Name_Len := 0;

         case Language is
            when Ada_Language_Index =>
               Add_Str_To_Name_Buffer (".adb");

            when C_Language_Index =>
               Add_Str_To_Name_Buffer (".c");

            when C_Plus_Plus_Language_Index =>
               Add_Str_To_Name_Buffer (".cpp");

            when others =>
               return No_Name;
         end case;

      --  Otherwise use the one specified

      else
         Get_Name_String (Suffix.Value);
      end if;

      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end Suffix_For;

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
      Conv          : Array_Element_Id := Conventions;
      Unit          : Name_Id;
      The_Unit_Id   : Unit_Id;
      The_Unit_Data : Unit_Data;
      Location      : Source_Ptr;

   begin
      while Conv /= No_Array_Element loop
         Unit := In_Tree.Array_Elements.Table (Conv).Index;
         Error_Msg_Name_1 := Unit;
         Get_Name_String (Unit);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Unit := Name_Find;
         The_Unit_Id := Units_Htable.Get
           (In_Tree.Units_HT, Unit);
         Location := In_Tree.Array_Elements.Table
                                            (Conv).Value.Location;

         if The_Unit_Id = No_Unit then
            Error_Msg
              (Project, In_Tree,
               "?unknown unit {",
               Location);

         else
            The_Unit_Data := In_Tree.Units.Table (The_Unit_Id);

            if Specs then
               if not Check_Project
                 (The_Unit_Data.File_Names (Specification).Project,
                  Project, In_Tree, Extending)
               then
                  Error_Msg
                    (Project, In_Tree,
                     "?unit{ has no spec in this project",
                     Location);
               end if;

            else
               if not Check_Project
                 (The_Unit_Data.File_Names (Body_Part).Project,
                  Project, In_Tree, Extending)
               then
                  Error_Msg
                    (Project, In_Tree,
                     "?unit{ has no body in this project",
                     Location);
               end if;
            end if;
         end if;

         Conv := In_Tree.Array_Elements.Table (Conv).Next;
      end loop;
   end Warn_If_Not_Sources;

end Prj.Nmsc;
