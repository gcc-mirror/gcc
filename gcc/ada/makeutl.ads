------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              M A K E U T L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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

--  This package contains various subprograms used by the builders, in
--  particular those subprograms related to project management and build
--  queue management.

with ALI;
with Namet;    use Namet;
with Opt;
with Osint;
with Prj;         use Prj;
with Prj.Tree;
with Types;    use Types;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Makeutl is

   type Fail_Proc is access procedure (S : String);

   Source_Info_Option : constant String := "--source-info=";
   --  Switch to indicate the source info file

   Subdirs_Option : constant String := "--subdirs=";
   --  Switch used to indicate that the real directories (object, exec,
   --  library, ...) are subdirectories of those in the project file.

   Unchecked_Shared_Lib_Imports : constant String :=
                                    "--unchecked-shared-lib-imports";
   --  Command line switch to allow shared library projects to import projects
   --  that are not shared library projects.

   Single_Compile_Per_Obj_Dir_Switch : constant String :=
                                         "--single-compile-per-obj-dir";
   --  Switch to forbid simultaneous compilations for the same object directory
   --  when project files are used.

   Create_Map_File_Switch : constant String := "--create-map-file";
   --  Switch to create a map file when an executable is linked

   procedure Add
     (Option : String_Access;
      To     : in out String_List_Access;
      Last   : in out Natural);
   procedure Add
     (Option : String;
      To     : in out String_List_Access;
      Last   : in out Natural);
   --  Add a string to a list of strings

   function Create_Binder_Mapping_File
     (Project_Tree : Project_Tree_Ref) return Path_Name_Type;
   --  Create a binder mapping file and returns its path name

   function Create_Name (Name : String) return File_Name_Type;
   function Create_Name (Name : String) return Name_Id;
   function Create_Name (Name : String) return Path_Name_Type;
   --  Get an id for a name

   function Base_Name_Index_For
     (Main            : String;
      Main_Index      : Int;
      Index_Separator : Character) return File_Name_Type;
   --  Returns the base name of Main, without the extension, followed by the
   --  Index_Separator followed by the Main_Index if it is non-zero.

   function Executable_Prefix_Path return String;
   --  Return the absolute path parent directory of the directory where the
   --  current executable resides, if its directory is named "bin", otherwise
   --  return an empty string. When a directory is returned, it is guaranteed
   --  to end with a directory separator.

   procedure Inform (N : Name_Id := No_Name; Msg : String);
   procedure Inform (N : File_Name_Type; Msg : String);
   --  Prints out the program name followed by a colon, N and S

   function File_Not_A_Source_Of
     (Project_Tree : Project_Tree_Ref;
      Uname        : Name_Id;
      Sfile        : File_Name_Type) return Boolean;
   --  Check that file name Sfile is one of the source of unit Uname. Returns
   --  True if the unit is in one of the project file, but the file name is not
   --  one of its source. Returns False otherwise.

   function Check_Source_Info_In_ALI
     (The_ALI      : ALI.ALI_Id;
      Tree         : Project_Tree_Ref) return Boolean;
   --  Check whether all file references in ALI are still valid (i.e. the
   --  source files are still associated with the same units). Return True
   --  if everything is still valid.

   function Is_Subunit (Source : Source_Id) return Boolean;
   --  Return True if source is a subunit

   procedure Initialize_Source_Record (Source : Source_Id);
   --  Get information either about the source file, the object and
   --  dependency file, as well as their timestamps. This includes timestamps.

   function Is_External_Assignment
     (Env  : Prj.Tree.Environment;
      Argv : String) return Boolean;
   --  Verify that an external assignment switch is syntactically correct
   --
   --  Correct forms are:
   --
   --      -Xname=value
   --      -X"name=other value"
   --
   --  Assumptions: 'First = 1, Argv (1 .. 2) = "-X"
   --
   --  When this function returns True, the external assignment has been
   --  entered by a call to Prj.Ext.Add, so that in a project file, External
   --  ("name") will return "value".

   procedure Verbose_Msg
     (N1                : Name_Id;
      S1                : String;
      N2                : Name_Id := No_Name;
      S2                : String  := "";
      Prefix            : String  := "  -> ";
      Minimum_Verbosity : Opt.Verbosity_Level_Type := Opt.Low);
   procedure Verbose_Msg
     (N1                : File_Name_Type;
      S1                : String;
      N2                : File_Name_Type := No_File;
      S2                : String  := "";
      Prefix            : String  := "  -> ";
      Minimum_Verbosity : Opt.Verbosity_Level_Type := Opt.Low);
   --  If the verbose flag (Verbose_Mode) is set and the verbosity level is at
   --  least equal to Minimum_Verbosity, then print Prefix to standard output
   --  followed by N1 and S1. If N2 /= No_Name then N2 is printed after S1. S2
   --  is printed last. Both N1 and N2 are printed in quotation marks. The two
   --  forms differ only in taking Name_Id or File_name_Type arguments.

   procedure Get_Switches
     (Source       : Source_Id;
      Pkg_Name     : Name_Id;
      Project_Tree : Project_Tree_Ref;
      Value        : out Variable_Value;
      Is_Default   : out Boolean);
   procedure Get_Switches
     (Source_File         : File_Name_Type;
      Source_Lang         : Name_Id;
      Source_Prj          : Project_Id;
      Pkg_Name            : Name_Id;
      Project_Tree        : Project_Tree_Ref;
      Value               : out Variable_Value;
      Is_Default          : out Boolean;
      Test_Without_Suffix : Boolean := False;
      Check_ALI_Suffix    : Boolean := False);
   --  Compute the switches (Compilation switches for instance) for the given
   --  file. This checks various attributes to see if there are file specific
   --  switches, or else defaults on the switches for the corresponding
   --  language. Is_Default is set to False if there were file-specific
   --  switches Source_File can be set to No_File to force retrieval of the
   --  default switches. If Test_Without_Suffix is True, and there is no " for
   --  Switches(Source_File) use", then this procedure also tests without the
   --  extension of the filename. If Test_Without_Suffix is True and
   --  Check_ALI_Suffix is True, then we also replace the file extension with
   --  ".ali" when testing.

   function Linker_Options_Switches
     (Project  : Project_Id;
      Do_Fail  : Fail_Proc;
      In_Tree  : Project_Tree_Ref) return String_List;
   --  Collect the options specified in the Linker'Linker_Options attributes
   --  of project Project, in project tree In_Tree, and in the projects that
   --  it imports directly or indirectly, and returns the result.

   function Unit_Index_Of (ALI_File : File_Name_Type) return Int;
   --  Find the index of a unit in a source file. Return zero if the file is
   --  not a multi-unit source file.

   procedure Test_If_Relative_Path
     (Switch               : in out String_Access;
      Parent               : String;
      Do_Fail              : Fail_Proc;
      Including_L_Switch   : Boolean := True;
      Including_Non_Switch : Boolean := True;
      Including_RTS        : Boolean := False);
   --  Test if Switch is a relative search path switch. If so, fail if Parent
   --  is the empty string, otherwise prepend the path with Parent. This
   --  subprogram is only used when using project files. For gnatbind switches,
   --  Including_L_Switch is False, because the argument of the -L switch is
   --  not a path. If Including_RTS is True, process also switches --RTS=.
   --  Do_Fail is called in case of error. Using Osint.Fail might be
   --  appropriate.

   function Path_Or_File_Name (Path : Path_Name_Type) return String;
   --  Returns a file name if -df is used, otherwise return a path name

   -------------------------
   -- Program termination --
   -------------------------

   procedure Fail_Program
     (Project_Tree   : Project_Tree_Ref;
      S              : String;
      Flush_Messages : Boolean := True);
   --  Terminate program with a message and a fatal status code

   procedure Finish_Program
     (Project_Tree : Project_Tree_Ref;
      Exit_Code    : Osint.Exit_Code_Type := Osint.E_Success;
      S            : String := "");
   --  Terminate program, with or without a message, setting the status code
   --  according to Fatal.
   --  This properly removes all temporary files

   -----------
   -- Mains --
   -----------

   --  Package Mains is used to store the mains specified on the command line
   --  and to retrieve them when a project file is used, to verify that the
   --  files exist and that they belong to a project file.

   --  Mains are stored in a table. An index is used to retrieve the mains
   --  from the table.

   type Main_Info is record
      File      : File_Name_Type;  --  Always canonical casing
      Index     : Int := 0;
      Location  : Source_Ptr := No_Location;

      Source    : Prj.Source_Id := No_Source;
      Project   : Project_Id;
      Tree      : Project_Tree_Ref;
   end record;
   No_Main_Info : constant Main_Info :=
     (No_File, 0, No_Location, No_Source, No_Project, null);

   package Mains is
      procedure Add_Main
        (Name     : String;
         Index    : Int := 0;
         Location : Source_Ptr := No_Location;
         Project  : Project_Id := No_Project;
         Tree     : Project_Tree_Ref := null);
      --  Add one main to the table.
      --  This is in general used to add the main files specified on the
      --  command line.
      --  Index is used for multi-unit source files, and indicates which unit
      --  within the source is concerned.
      --  Location is the location within the project file (if a project file
      --  is used).
      --  Project and Tree indicate to which project the main should belong.
      --  In particular, for aggregate projects, this isn't necessarily the
      --  main project tree. These can be set to No_Project and null when not
      --  using projects.

      procedure Delete;
      --  Empty the table

      procedure Reset;
      --  Reset the cursor to the beginning of the table

      procedure Set_Multi_Unit_Index
        (Project_Tree : Project_Tree_Ref := null;
         Index        : Int := 0);
      --  If a single main file was defined, this subprogram indicates which
      --  unit inside it is the main (case of a multi-unit source files).
      --  Errors are raised if zero or more than one main file was defined,
      --  and Index is not 0.
      --  This subprogram is used for the handling of the command line switch.

      function Next_Main return String;
      function Next_Main return Main_Info;
      --  Moves the cursor forward and returns the new current entry.
      --  Returns No_File_And_Loc if there are no more mains in the table.

      function Number_Of_Mains return Natural;
      --  Returns the number of mains in the table.

      procedure Fill_From_Project
        (Root_Project : Project_Id;
         Project_Tree : Project_Tree_Ref);
      --  If no main was already added (presumably from the command line), add
      --  the main units from root_project (or in the case of an aggregate
      --  project from all the aggregated projects).
      --
      --  If some main units were already added from the command line, check
      --  that they all belong to the root project, and that they are full
      --  full paths rather than (partial) base names (e.g. no body suffix was
      --  specified).

   end Mains;

   -----------
   -- Queue --
   -----------

   type Source_Info_Format is (Format_Gprbuild, Format_Gnatmake);

   package Queue is
      --  The queue of sources to be checked for compilation.
      --  There can be a single such queue per application.

      type Source_Info (Format : Source_Info_Format := Format_Gprbuild) is
         record
            case Format is
            when Format_Gprbuild =>
               Tree : Project_Tree_Ref := null;
               Id   : Source_Id := null;

            when Format_Gnatmake =>
               File      : File_Name_Type := No_File;
               Unit      : Unit_Name_Type := No_Unit_Name;
               Index     : Int := 0;
               Project   : Project_Id := No_Project;
            end case;
         end record;
      --  Information about files stored in the queue. The exact information
      --  depends on the builder, and in particular whether it only supports
      --  project-based files (in which case we have a full Source_Id record).

      procedure Initialize
        (Queue_Per_Obj_Dir : Boolean;
         Force : Boolean := False);
      --  Initialize the queue.
      --  Queue_Per_Obj_Dir matches the --single-compile-per-obj-dir switch:
      --  when True, there cannot be simultaneous compilations with the object
      --  files in the same object directory when project files are used.
      --
      --  Nothing is done if Force is False and the queue was already
      --  initialized.

      procedure Remove_Marks;
      --  Remove all marks set for the files.
      --  This means that the files will be handed to the compiler if they are
      --  added to the queue, and is mostly useful when recompiling several
      --  executables in non-project mode, as the switches may be different
      --  and -s may be in use.

      function Is_Empty return Boolean;
      --  Returns True if the queue is empty

      function Is_Virtually_Empty return Boolean;
      --  Returns True if the queue is empty or if all object directories are
      --  busy.

      procedure Insert (Source  : Source_Info; With_Roots : Boolean := False);
      function Insert
        (Source  : Source_Info; With_Roots : Boolean := False) return Boolean;
      --  Insert source in the queue.
      --  The second version returns False if the Source was already marked in
      --  the queue.
      --  If With_Roots is True and the source is in Format_Gprbuild mode (ie
      --  with a project), this procedure also includes the "Roots" for this
      --  main, ie all the other files that must be included in the library or
      --  binary (in particular to combine Ada and C files connected through
      --  pragma Export/Import). When the roots are computed, they are also
      --  stored in the corresponding Source_Id for later reuse by the binder.

      procedure Insert_Project_Sources
        (Project      : Project_Id;
         Project_Tree : Project_Tree_Ref;
         All_Projects : Boolean;
         Unit_Based   : Boolean);
      --  Insert all the compilable sources of the project in the queue. If
      --  All_Project is true, then all sources from imported projects are also
      --  inserted.
      --  When Unit_Based is True, put in the queue all compilable sources
      --  including the unit based (Ada) one. When Unit_Based is False, put the
      --  Ada sources only when they are in a library project.

      procedure Insert_Withed_Sources_For
        (The_ALI               : ALI.ALI_Id;
         Project_Tree          : Project_Tree_Ref;
         Excluding_Shared_SALs : Boolean := False);
      --  Insert in the queue those sources withed by The_ALI, if there are not
      --  already in the queue and Only_Interfaces is False or they are part of
      --  the interfaces of their project.

      procedure Extract
        (Found  : out Boolean;
         Source : out Source_Info);
      --  Get the first source that can be compiled from the queue. If no
      --  source may be compiled, sets Found to False. In this case, the value
      --  for Source is undefined.

      function Size return Natural;
      --  Return the total size of the queue, including the sources already
      --  extracted.

      function Processed return Natural;
      --  Return the number of source in the queue that have aready been
      --  processed.

      procedure Set_Obj_Dir_Busy (Obj_Dir : Path_Name_Type);
      procedure Set_Obj_Dir_Free (Obj_Dir : Path_Name_Type);
      --  Mark Obj_Dir as busy or free (see the parameter to Initialize)

      function Element (Rank : Positive) return File_Name_Type;
      --  Get the file name for element of index Rank in the queue

   end Queue;

end Makeutl;
