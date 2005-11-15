------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M A K E                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with ALI;      use ALI;
with ALI.Util; use ALI.Util;
with Csets;
with Debug;
with Fmap;
with Fname;    use Fname;
with Fname.SF; use Fname.SF;
with Fname.UF; use Fname.UF;
with Gnatvsn;  use Gnatvsn;
with Hostparm; use Hostparm;
with Makeusg;
with Makeutl;  use Makeutl;
with MLib.Prj;
with MLib.Tgt; use MLib.Tgt;
with MLib.Utl;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint.M;  use Osint.M;
with Osint;    use Osint;
with Output;   use Output;
with Prj;      use Prj;
with Prj.Com;
with Prj.Env;
with Prj.Pars;
with Prj.Util;
with SFN_Scan;
with Sinput.P;
with Snames;   use Snames;
with Switch;   use Switch;
with Switch.M; use Switch.M;
with Targparm;
with Tempdir;

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Command_Line;          use Ada.Command_Line;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Case_Util;            use GNAT.Case_Util;

with System.HTable;

package body Make is

   use ASCII;
   --  Make control characters visible

   Standard_Library_Package_Body_Name : constant String := "s-stalib.adb";
   --  Every program depends on this package, that must then be checked,
   --  especially when -f and -a are used.

   type Sigint_Handler is access procedure;

   procedure Install_Int_Handler (Handler : Sigint_Handler);
   pragma Import (C, Install_Int_Handler, "__gnat_install_int_handler");
   --  Called by Gnatmake to install the SIGINT handler below

   procedure Sigint_Intercepted;
   --  Called when the program is interrupted by Ctrl-C to delete the
   --  temporary mapping files and configuration pragmas files.

   -------------------------
   -- Note on terminology --
   -------------------------

   --  In this program, we use the phrase "termination" of a file name to
   --  refer to the suffix that appears after the unit name portion. Very
   --  often this is simply the extension, but in some cases, the sequence
   --  may be more complex, for example in main.1.ada, the termination in
   --  this name is ".1.ada" and in main_.ada the termination is "_.ada".

   -------------------------------------
   -- Queue (Q) Manipulation Routines --
   -------------------------------------

   --  The Q is used in Compile_Sources below. Its implementation uses the
   --  GNAT generic package Table (basically an extensible array). Q_Front
   --  points to the first valid element in the Q, whereas Q.First is the first
   --  element ever enqueued, while Q.Last - 1 is the last element in the Q.
   --
   --        +---+--------------+---+---+---+-----------+---+--------
   --    Q   |   |  ........    |   |   |   | .......   |   |
   --        +---+--------------+---+---+---+-----------+---+--------
   --          ^                  ^                       ^
   --       Q.First             Q_Front               Q.Last - 1
   --
   --  The elements comprised between Q.First and Q_Front - 1 are the
   --  elements that have been enqueued and then dequeued, while the
   --  elements between Q_Front and Q.Last - 1 are the elements currently
   --  in the Q. When the Q is initialized Q_Front = Q.First = Q.Last.
   --  After Compile_Sources has terminated its execution, Q_Front = Q.Last
   --  and the elements contained between Q.Front and Q.Last-1 are those that
   --  were explored and thus marked by Compile_Sources. Whenever the Q is
   --  reinitialized, the elements between Q.First and Q.Last - 1 are unmarked.

   procedure Init_Q;
   --  Must be called to (re)initialize the Q

   procedure Insert_Q
     (Source_File : File_Name_Type;
      Source_Unit : Unit_Name_Type := No_Name;
      Index       : Int            := 0);
   --  Inserts Source_File at the end of Q. Provide Source_Unit when possible
   --  for external use (gnatdist). Provide index for multi-unit sources.

   function Empty_Q return Boolean;
   --  Returns True if Q is empty

   procedure Extract_From_Q
     (Source_File  : out File_Name_Type;
      Source_Unit  : out Unit_Name_Type;
      Source_Index : out Int);
   --  Extracts the first element from the Q

   procedure Insert_Project_Sources
     (The_Project  : Project_Id;
      All_Projects : Boolean;
      Into_Q       : Boolean);
   --  If Into_Q is True, insert all sources of the project file(s) that are
   --  not already marked into the Q. If Into_Q is False, call Osint.Add_File
   --  for the first source, then insert all other sources that are not already
   --  marked into the Q. If All_Projects is True, all sources of all projects
   --  are concerned; otherwise, only sources of The_Project are concerned,
   --  including, if The_Project is an extending project, sources inherited
   --  from projects being extended.

   First_Q_Initialization : Boolean := True;
   --  Will be set to false after Init_Q has been called once

   Q_Front : Natural;
   --  Points to the first valid element in the Q

   Unique_Compile : Boolean := False;
   --  Set to True if -u or -U or a project file with no main is used

   Unique_Compile_All_Projects : Boolean := False;
   --  Set to True if -U is used

   RTS_Specified : String_Access := null;
   --  Used to detect multiple --RTS= switches

   type Q_Record is record
      File  : File_Name_Type;
      Unit  : Unit_Name_Type;
      Index : Int;
   end record;
   --  File is the name of the file to compile. Unit is for gnatdist
   --  use in order to easily get the unit name of a file to compile
   --  when its name is krunched or declared in gnat.adc. Index, when not 0,
   --  is the index of the unit in a multi-unit source.

   package Q is new Table.Table (
     Table_Component_Type => Q_Record,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 0,
     Table_Initial        => 4000,
     Table_Increment      => 100,
     Table_Name           => "Make.Q");
   --  This is the actual Q

   --  The following instantiations and variables are necessary to save what
   --  is found on the command line, in case there is a project file specified.

   package Saved_Gcc_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Saved_Gcc_Switches");

   package Saved_Binder_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Saved_Binder_Switches");

   package Saved_Linker_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Make.Saved_Linker_Switches");

   package Switches_To_Check is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Switches_To_Check");

   package Library_Paths is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Library_Paths");

   package Failed_Links is new Table.Table (
     Table_Component_Type => File_Name_Type,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Make.Failed_Links");

   package Successful_Links is new Table.Table (
     Table_Component_Type => File_Name_Type,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Make.Successful_Links");

   package Library_Projs is new Table.Table (
     Table_Component_Type => Project_Id,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Make.Library_Projs");

   --  Two variables to keep the last binder and linker switch index in tables
   --  Binder_Switches and Linker_Switches, before adding switches from the
   --  project file (if any) and switches from the command line (if any).

   Last_Binder_Switch : Integer := 0;
   Last_Linker_Switch : Integer := 0;

   Normalized_Switches : Argument_List_Access := new Argument_List (1 .. 10);
   Last_Norm_Switch    : Natural := 0;

   Saved_Maximum_Processes : Natural := 0;

   type Arg_List_Ref is access Argument_List;
   The_Saved_Gcc_Switches : Arg_List_Ref;

   Project_File_Name : String_Access  := null;
   --  The path name of the main project file, if any

   Project_File_Name_Present : Boolean := False;
   --  True when -P is used with a space between -P and the project file name

   Current_Verbosity : Prj.Verbosity  := Prj.Default;
   --  Verbosity to parse the project files

   Project_Tree : constant Project_Tree_Ref := new Project_Tree_Data;

   Main_Project : Prj.Project_Id := No_Project;
   --  The project id of the main project file, if any

   Project_Object_Directory : Project_Id := No_Project;
   --  The object directory of the project for the last compilation. Avoid
   --  calling Change_Dir if the current working directory is already this
   --  directory

   --  Packages of project files where unknown attributes are errors

   Naming_String   : aliased String := "naming";
   Builder_String  : aliased String := "builder";
   Compiler_String : aliased String := "compiler";
   Binder_String   : aliased String := "binder";
   Linker_String   : aliased String := "linker";

   Gnatmake_Packages : aliased String_List :=
     (Naming_String   'Access,
      Builder_String  'Access,
      Compiler_String 'Access,
      Binder_String   'Access,
      Linker_String   'Access);

   Packages_To_Check_By_Gnatmake : constant String_List_Access :=
     Gnatmake_Packages'Access;

   procedure Add_Library_Search_Dir
     (Path            : String;
      On_Command_Line : Boolean);
   --  Call Add_Lib_Search_Dir with an absolute directory path. If Path is a
   --  relative path, when On_Command_Line is True, it is relative to the
   --  current working directory; when On_Command_Line is False, it is relative
   --  to the project directory of the main project.

   procedure Add_Source_Search_Dir
     (Path            : String;
      On_Command_Line : Boolean);
   --  Call Add_Src_Search_Dir with an absolute directory path. If Path is a
   --  relative path, when On_Command_Line is True, it is relative to the
   --  current working directory; when On_Command_Line is False, it is relative
   --  to the project directory of the main project.

   procedure Add_Source_Dir (N : String);
   --  Call Add_Src_Search_Dir (output one line when in verbose mode)

   procedure Add_Source_Directories is
     new Prj.Env.For_All_Source_Dirs (Action => Add_Source_Dir);

   procedure Add_Object_Dir (N : String);
   --  Call Add_Lib_Search_Dir (output one line when in verbose mode)

   procedure Add_Object_Directories is
     new Prj.Env.For_All_Object_Dirs (Action => Add_Object_Dir);

   procedure Change_To_Object_Directory (Project : Project_Id);
   --  Change to the object directory of project Project, if this is not
   --  already the current working directory.

   type Bad_Compilation_Info is record
      File  : File_Name_Type;
      Unit  : Unit_Name_Type;
      Found : Boolean;
   end record;
   --  File is the name of the file for which a compilation failed. Unit is for
   --  gnatdist use in order to easily get the unit name of a file when its
   --  name is krunched or declared in gnat.adc. Found is False if the
   --  compilation failed because the file could not be found.

   package Bad_Compilation is new Table.Table (
     Table_Component_Type => Bad_Compilation_Info,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Bad_Compilation");
   --  Full name of all the source files for which compilation fails

   Do_Compile_Step : Boolean := True;
   Do_Bind_Step    : Boolean := True;
   Do_Link_Step    : Boolean := True;
   --  Flags to indicate what step should be executed.
   --  Can be set to False with the switches -c, -b and -l.
   --  These flags are reset to True for each invokation of procedure Gnatmake.

   Shared_String           : aliased String := "-shared";
   Force_Elab_Flags_String : aliased String := "-F";

   No_Shared_Switch : aliased Argument_List := (1 .. 0 => null);
   Shared_Switch    : aliased Argument_List := (1 => Shared_String'Access);
   Bind_Shared      : Argument_List_Access := No_Shared_Switch'Access;
   --  Switch to added in front of gnatbind switches. By default no switch is
   --  added. Switch "-shared" is added if there is a non-static Library
   --  Project File.

   Shared_Libgcc : aliased String := "-shared-libgcc";

   No_Shared_Libgcc_Switch : aliased Argument_List := (1 .. 0 => null);
   Shared_Libgcc_Switch    : aliased Argument_List :=
                               (1 => Shared_Libgcc'Access);
   Link_With_Shared_Libgcc : Argument_List_Access :=
                               No_Shared_Libgcc_Switch'Access;

   procedure Make_Failed (S1 : String; S2 : String := ""; S3 : String := "");
   --  Delete all temp files created by Gnatmake and call Osint.Fail,
   --  with the parameter S1, S2 and S3 (see osint.ads).
   --  This is called from the Prj hierarchy and the MLib hierarchy.

   --------------------------
   -- Obsolete Executables --
   --------------------------

   Executable_Obsolete : Boolean := False;
   --  Executable_Obsolete is initially set to False for each executable,
   --  and is set to True whenever one of the source of the executable is
   --  compiled, or has already been compiled for another executable.

   Max_Header : constant := 200;
   --  This needs a proper comment, it used to say "arbitrary"
   --  that's not an adequate comment ???

   type Header_Num is range 1 .. Max_Header;
   --  Header_Num for the hash table Obsoleted below

   function Hash (F : Name_Id) return Header_Num;
   --  Hash function for the hash table Obsoleted below

   package Obsoleted is new System.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to keep all files that have been compiled, to detect
   --  if an executable is up to date or not.

   procedure Enter_Into_Obsoleted (F : Name_Id);
   --  Enter a file name, without directory information, into the hash table
   --  Obsoleted.

   function Is_In_Obsoleted (F : Name_Id) return Boolean;
   --  Check if a file name, without directory information, has already been
   --  entered into the hash table Obsoleted.

   type Dependency is record
      This       : Name_Id;
      Depends_On : Name_Id;
   end record;
   --  Components of table Dependencies below

   package Dependencies is new Table.Table (
     Table_Component_Type => Dependency,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Dependencies");
   --  A table to keep dependencies, to be able to decide if an executable
   --  is obsolete.

   procedure Add_Dependency (S : Name_Id; On : Name_Id);
   --  Add one entry in table Dependencies

   ----------------------------
   -- Arguments and Switches --
   ----------------------------

   Arguments : Argument_List_Access;
   --  Used to gather the arguments for invocation of the compiler

   Last_Argument : Natural := 0;
   --  Last index of arguments in Arguments above

   Arguments_Collected : Boolean := False;
   --  Set to True when the arguments for the next invocation of the compiler
   --  have been collected.

   Arguments_Project : Project_Id;
   --  Project id, if any, of the source to be compiled

   Arguments_Path_Name : File_Name_Type;
   --  Full path of the source to be compiled, when Arguments_Project is not
   --  No_Project.

   Dummy_Switch : constant String_Access := new String'("- ");
   --  Used to initialized Prev_Switch in procedure Check

   procedure Add_Arguments (Args : Argument_List);
   --  Add arguments to global variable Arguments, increasing its size
   --  if necessary and adjusting Last_Argument.

   function Configuration_Pragmas_Switch
     (For_Project : Project_Id) return Argument_List;
   --  Return an argument list of one element, if there is a configuration
   --  pragmas file to be specified for For_Project,
   --  otherwise return an empty argument list.

   -------------------
   -- Misc Routines --
   -------------------

   procedure List_Depend;
   --  Prints to standard output the list of object dependencies. This list
   --  can be used directly in a Makefile. A call to Compile_Sources must
   --  precede the call to List_Depend. Also because this routine uses the
   --  ALI files that were originally loaded and scanned by Compile_Sources,
   --  no additional ALI files should be scanned between the two calls (i.e.
   --  between the call to Compile_Sources and List_Depend.)

   procedure Inform (N : Name_Id := No_Name; Msg : String);
   --  Prints out the program name followed by a colon, N and S

   procedure List_Bad_Compilations;
   --  Prints out the list of all files for which the compilation failed

   procedure Verbose_Msg
     (N1                : Name_Id;
      S1                : String;
      N2                : Name_Id := No_Name;
      S2                : String  := "";
      Prefix            : String  := "  -> ";
      Minimum_Verbosity : Verbosity_Level_Type := Opt.Low);
   --  If the verbose flag (Verbose_Mode) is set and the verbosity level is
   --  at least equal to Minimum_Verbosity, then print Prefix to standard
   --  output followed by N1 and S1. If N2 /= No_Name then N2 is printed after
   --  S1. S2 is printed last. Both N1 and N2 are printed in quotation marks.

   Usage_Needed : Boolean := True;
   --  Flag used to make sure Makeusg is call at most once

   procedure Usage;
   --  Call Makeusg, if Usage_Needed is True.
   --  Set Usage_Needed to False.

   procedure Debug_Msg (S : String; N : Name_Id);
   --  If Debug.Debug_Flag_W is set outputs string S followed by name N

   procedure Recursive_Compute_Depth
     (Project : Project_Id;
      Depth   : Natural);
   --  Compute depth of Project and of the projects it depends on

   procedure Compute_All_Imported_Projects (Project : Project_Id);
   --  Compute, the list of the projects imported directly or indirectly by
   --  project Project.

   -----------------------
   -- Gnatmake Routines --
   -----------------------

   Gnatmake_Called : Boolean := False;
   --  Set to True when procedure Gnatmake is called.
   --  Attempt to delete temporary files is made only when Gnatmake_Called
   --  is True.

   subtype Lib_Mark_Type is Byte;
   --  Used in Mark_Directory

   Ada_Lib_Dir : constant Lib_Mark_Type := 1;
   --  Used to mark a directory as a GNAT lib dir

   --  Note that the notion of GNAT lib dir is no longer used. The code related
   --  to it has not been removed to give an idea on how to use the directory
   --  prefix marking mechanism.

   --  An Ada library directory is a directory containing ali and object files
   --  but no source files for the bodies (the specs can be in the same or some
   --  other directory). These directories are specified in the Gnatmake
   --  command line with the switch "-Adir" (to specify the spec location -Idir
   --  cab be used). Gnatmake skips the missing sources whose ali are in Ada
   --  library directories. For an explanation of why Gnatmake behaves that
   --  way, see the spec of Make.Compile_Sources. The directory lookup penalty
   --  is incurred every single time this routine is called.

   procedure Check_Steps;
   --  Check what steps (Compile, Bind, Link) must be executed.
   --  Set the step flags accordingly.

   function In_Ada_Lib_Dir  (File : File_Name_Type) return Boolean;
   --  Get directory prefix of this file and get lib mark stored in name
   --  table for this directory. Then check if an Ada lib mark has been set.

   procedure Mark_Directory
     (Dir             : String;
      Mark            : Lib_Mark_Type;
      On_Command_Line : Boolean);
   --  Store the absolute path from Dir in name table and set lib mark as name
   --  info to identify Ada libraries.
   --
   --  If Dir is a relative path, when On_Command_Line is True, it is relative
   --  to the current working directory; when On_Command_Line is False, it is
   --  relative to the project directory of the main project.

   Output_Is_Object : Boolean := True;
   --  Set to False when using a switch -S for the compiler

   procedure Check_For_S_Switch;
   --  Set Output_Is_Object to False when the -S switch is used for the
   --  compiler.

   function Switches_Of
     (Source_File      : Name_Id;
      Source_File_Name : String;
      Source_Index     : Int;
      Naming           : Naming_Data;
      In_Package       : Package_Id;
      Allow_ALI        : Boolean) return Variable_Value;
   --  Return the switches for the source file in the specified package of a
   --  project file. If the Source_File ends with a standard GNAT extension
   --  (".ads" or ".adb"), try first the full name, then the name without the
   --  extension, then, if Allow_ALI is True, the name with the extension
   --  ".ali". If there is no switches for either names, try the default
   --  switches for Ada. If all failed, return No_Variable_Value.

   function Is_In_Object_Directory
     (Source_File   : File_Name_Type;
      Full_Lib_File : File_Name_Type) return Boolean;
   --  Check if, when using a project file, the ALI file is in the project
   --  directory of the ultimate extending project. If it is not, we ignore
   --  the fact that this ALI file is read-only.

   ----------------------------------------------------
   -- Compiler, Binder & Linker Data and Subprograms --
   ----------------------------------------------------

   Gcc             : String_Access := Program_Name ("gcc");
   Gnatbind        : String_Access := Program_Name ("gnatbind");
   Gnatlink        : String_Access := Program_Name ("gnatlink");
   --  Default compiler, binder, linker programs

   Saved_Gcc       : String_Access := null;
   Saved_Gnatbind  : String_Access := null;
   Saved_Gnatlink  : String_Access := null;
   --  Given by the command line. Will be used, if non null

   Gcc_Path        : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);
   Gnatbind_Path   : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gnatbind.all);
   Gnatlink_Path   : String_Access :=
                       GNAT.OS_Lib.Locate_Exec_On_Path (Gnatlink.all);
   --  Path for compiler, binder, linker programs, defaulted now for gnatdist.
   --  Changed later if overridden on command line.

   Comp_Flag         : constant String_Access := new String'("-c");
   Output_Flag       : constant String_Access := new String'("-o");
   Ada_Flag_1        : constant String_Access := new String'("-x");
   Ada_Flag_2        : constant String_Access := new String'("ada");
   No_gnat_adc       : constant String_Access := new String'("-gnatA");
   GNAT_Flag         : constant String_Access := new String'("-gnatpg");
   Do_Not_Check_Flag : constant String_Access := new String'("-x");

   Object_Suffix     : constant String := Get_Object_Suffix.all;
   Executable_Suffix : constant String := Get_Executable_Suffix.all;

   Syntax_Only : Boolean := False;
   --  Set to True when compiling with -gnats

   Display_Executed_Programs : Boolean := True;
   --  Set to True if name of commands should be output on stderr

   Output_File_Name_Seen : Boolean := False;
   --  Set to True after having scanned the file_name for
   --  switch "-o file_name"

   Object_Directory_Seen : Boolean := False;
   --  Set to True after having scanned the object directory for
   --  switch "-D obj_dir".

   Object_Directory_Path : String_Access := null;
   --  The path name of the object directory, set with switch -D

   type Make_Program_Type is (None, Compiler, Binder, Linker);

   Program_Args : Make_Program_Type := None;
   --  Used to indicate if we are scanning gnatmake, gcc, gnatbind, or gnatbind
   --  options within the gnatmake command line. Used in Scan_Make_Arg only,
   --  but must be global since value preserved from one call to another.

   Temporary_Config_File : Boolean := False;
   --  Set to True when there is a temporary config file used for a project
   --  file, to avoid displaying the -gnatec switch for a temporary file.

   procedure Add_Switches
     (The_Package : Package_Id;
      File_Name   : String;
      Index       : Int;
      Program     : Make_Program_Type);
   procedure Add_Switch
     (S             : String_Access;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True;
      And_Save      : Boolean := True);
   procedure Add_Switch
     (S             : String;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True;
      And_Save      : Boolean := True);
   --  Make invokes one of three programs (the compiler, the binder or the
   --  linker). For the sake of convenience, some program specific switches
   --  can be passed directly on the gnatmake commande line. This procedure
   --  records these switches so that gnamake can pass them to the right
   --  program.  S is the switch to be added at the end of the command line
   --  for Program if Append_Switch is True. If Append_Switch is False S is
   --  added at the beginning of the command line.

   procedure Check
     (Source_File  : File_Name_Type;
      Source_Index : Int;
      The_Args     : Argument_List;
      Lib_File     : File_Name_Type;
      Read_Only    : Boolean;
      ALI          : out ALI_Id;
      O_File       : out File_Name_Type;
      O_Stamp      : out Time_Stamp_Type);
   --  Determines whether the library file Lib_File is up-to-date or not. The
   --  full name (with path information) of the object file corresponding to
   --  Lib_File is returned in O_File. Its time stamp is saved in O_Stamp.
   --  ALI is the ALI_Id corresponding to Lib_File. If Lib_File in not
   --  up-to-date, then the corresponding source file needs to be recompiled.
   --  In this case ALI = No_ALI_Id.

   procedure Check_Linker_Options
     (E_Stamp : Time_Stamp_Type;
      O_File  : out File_Name_Type;
      O_Stamp : out Time_Stamp_Type);
   --  Checks all linker options for linker files that are newer
   --  than E_Stamp. If such objects are found, the youngest object
   --  is returned in O_File and its stamp in O_Stamp.
   --
   --  If no obsolete linker files were found, the first missing
   --  linker file is returned in O_File and O_Stamp is empty.
   --  Otherwise O_File is No_File.

   procedure Collect_Arguments
     (Source_File  : File_Name_Type;
      Source_Index : Int;
      Args         : Argument_List);
   --  Collect all arguments for a source to be compiled, including those
   --  that come from a project file.

   procedure Display (Program : String; Args : Argument_List);
   --  Displays Program followed by the arguments in Args if variable
   --  Display_Executed_Programs is set. The lower bound of Args must be 1.

   -----------------
   --  Mapping files
   -----------------

   type Temp_File_Names is
     array (Project_Id range <>, Positive range <>) of Name_Id;

   type Temp_Files_Ptr is access Temp_File_Names;

   type Indices is array (Project_Id range <>) of Natural;

   type Indices_Ptr is access Indices;

   type Free_File_Indices is array
     (Project_Id range <>, Positive range <>) of Positive;

   type Free_Indices_Ptr is access Free_File_Indices;

   The_Mapping_File_Names : Temp_Files_Ptr;
   --  For each project, the name ids of the temporary mapping files used

   Last_Mapping_File_Names : Indices_Ptr;
   --  For each project, the index of the last mapping file created

   The_Free_Mapping_File_Indices : Free_Indices_Ptr;
   --  For each project, the indices in The_Mapping_File_Names of the mapping
   --  file names that can be reused for subsequent compilations.

   Last_Free_Indices : Indices_Ptr;
   --  For each project, the number of mapping files that can be reused

   Gnatmake_Mapping_File : String_Access := null;
   --  The path name of a mapping file specified by switch -C=

   procedure Delete_Mapping_Files;
   --  Delete all temporary mapping files

   procedure Init_Mapping_File
     (Project : Project_Id;
      File_Index : in out Natural);
   --  Create a new temporary mapping file, and fill it with the project file
   --  mappings, when using project file(s). The out parameter File_Index is
   --  the index to the name of the file in the array The_Mapping_File_Names.

   procedure Delete_Temp_Config_Files;
   --  Delete all temporary config files

   procedure Delete_All_Temp_Files;
   --  Delete all temp files (config files, mapping files, path files)

   -------------------
   -- Add_Arguments --
   -------------------

   procedure Add_Arguments (Args : Argument_List) is
   begin
      if Arguments = null then
         Arguments := new Argument_List (1 .. Args'Length + 10);

      else
         while Last_Argument + Args'Length > Arguments'Last loop
            declare
               New_Arguments : constant Argument_List_Access :=
                                 new Argument_List (1 .. Arguments'Last * 2);
            begin
               New_Arguments (1 .. Last_Argument) :=
                 Arguments (1 .. Last_Argument);
               Arguments := New_Arguments;
            end;
         end loop;
      end if;

      Arguments (Last_Argument + 1 .. Last_Argument + Args'Length) := Args;
      Last_Argument := Last_Argument + Args'Length;
   end Add_Arguments;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency (S : Name_Id; On : Name_Id) is
   begin
      Dependencies.Increment_Last;
      Dependencies.Table (Dependencies.Last) := (S, On);
   end Add_Dependency;

   ----------------------------
   -- Add_Library_Search_Dir --
   ----------------------------

   procedure Add_Library_Search_Dir
     (Path            : String;
      On_Command_Line : Boolean)
   is
   begin
      if On_Command_Line then
         Add_Lib_Search_Dir
           (Normalize_Pathname (Path));

      else
         Get_Name_String
           (Project_Tree.Projects.Table (Main_Project).Directory);
         Add_Lib_Search_Dir
           (Normalize_Pathname
              (Path, Name_Buffer (1 .. Name_Len)));
      end if;
   end Add_Library_Search_Dir;

   --------------------
   -- Add_Object_Dir --
   --------------------

   procedure Add_Object_Dir (N : String) is
   begin
      Add_Lib_Search_Dir (N);

      if Verbose_Mode then
         Write_Str ("Adding object directory """);
         Write_Str (N);
         Write_Str (""".");
         Write_Eol;
      end if;
   end Add_Object_Dir;

   --------------------
   -- Add_Source_Dir --
   --------------------

   procedure Add_Source_Dir (N : String) is
   begin
      Add_Src_Search_Dir (N);

      if Verbose_Mode then
         Write_Str ("Adding source directory """);
         Write_Str (N);
         Write_Str (""".");
         Write_Eol;
      end if;
   end Add_Source_Dir;

   ---------------------------
   -- Add_Source_Search_Dir --
   ---------------------------

   procedure Add_Source_Search_Dir
     (Path            : String;
      On_Command_Line : Boolean)
   is
   begin
      if On_Command_Line then
         Add_Src_Search_Dir
           (Normalize_Pathname (Path));

      else
         Get_Name_String
           (Project_Tree.Projects.Table (Main_Project).Directory);
         Add_Src_Search_Dir
           (Normalize_Pathname
              (Path, Name_Buffer (1 .. Name_Len)));
      end if;
   end Add_Source_Search_Dir;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (S             : String_Access;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True;
      And_Save      : Boolean := True)
   is
      generic
         with package T is new Table.Table (<>);
      procedure Generic_Position (New_Position : out Integer);
      --  Generic procedure that chooses a position for S in T at the
      --  beginning or the end, depending on the boolean Append_Switch.
      --  Calling this procedure may expand the table.

      ----------------------
      -- Generic_Position --
      ----------------------

      procedure Generic_Position (New_Position : out Integer) is
      begin
         T.Increment_Last;

         if Append_Switch then
            New_Position := Integer (T.Last);
         else
            for J in reverse T.Table_Index_Type'Succ (T.First) .. T.Last loop
               T.Table (J) := T.Table (T.Table_Index_Type'Pred (J));
            end loop;

            New_Position := Integer (T.First);
         end if;
      end Generic_Position;

      procedure Gcc_Switches_Pos    is new Generic_Position (Gcc_Switches);
      procedure Binder_Switches_Pos is new Generic_Position (Binder_Switches);
      procedure Linker_Switches_Pos is new Generic_Position (Linker_Switches);

      procedure Saved_Gcc_Switches_Pos is new
        Generic_Position (Saved_Gcc_Switches);

      procedure Saved_Binder_Switches_Pos is new
        Generic_Position (Saved_Binder_Switches);

      procedure Saved_Linker_Switches_Pos is new
        Generic_Position (Saved_Linker_Switches);

      New_Position : Integer;

   --  Start of processing for Add_Switch

   begin
      if And_Save then
         case Program is
            when Compiler =>
               Saved_Gcc_Switches_Pos (New_Position);
               Saved_Gcc_Switches.Table (New_Position) := S;

            when Binder   =>
               Saved_Binder_Switches_Pos (New_Position);
               Saved_Binder_Switches.Table (New_Position) := S;

            when Linker   =>
               Saved_Linker_Switches_Pos (New_Position);
               Saved_Linker_Switches.Table (New_Position) := S;

            when None =>
               raise Program_Error;
         end case;

      else
         case Program is
            when Compiler =>
               Gcc_Switches_Pos (New_Position);
               Gcc_Switches.Table (New_Position) := S;

            when Binder   =>
               Binder_Switches_Pos (New_Position);
               Binder_Switches.Table (New_Position) := S;

            when Linker   =>
               Linker_Switches_Pos (New_Position);
               Linker_Switches.Table (New_Position) := S;

            when None =>
               raise Program_Error;
         end case;
      end if;
   end Add_Switch;

   procedure Add_Switch
     (S             : String;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True;
      And_Save      : Boolean := True)
   is
   begin
      Add_Switch (S             => new String'(S),
                  Program       => Program,
                  Append_Switch => Append_Switch,
                  And_Save      => And_Save);
   end Add_Switch;

   ------------------
   -- Add_Switches --
   ------------------

   procedure Add_Switches
     (The_Package : Package_Id;
      File_Name   : String;
      Index       : Int;
      Program     : Make_Program_Type)
   is
      Switches    : Variable_Value;
      Switch_List : String_List_Id;
      Element     : String_Element;

   begin
      if File_Name'Length > 0 then
         Name_Len := File_Name'Length;
         Name_Buffer (1 .. Name_Len) := File_Name;
         Switches :=
           Switches_Of
           (Source_File      => Name_Find,
            Source_File_Name => File_Name,
            Source_Index     => Index,
            Naming           => Project_Tree.Projects.Table
                                  (Main_Project).Naming,
            In_Package       => The_Package,
            Allow_ALI        =>
              Program = Binder or else Program = Linker);

         case Switches.Kind is
            when Undefined =>
               null;

            when List =>
               Program_Args := Program;

               Switch_List := Switches.Values;

               while Switch_List /= Nil_String loop
                  Element :=
                    Project_Tree.String_Elements.Table (Switch_List);
                  Get_Name_String (Element.Value);

                  if Name_Len > 0 then
                     declare
                        Argv : constant String := Name_Buffer (1 .. Name_Len);
                        --  We need a copy, because Name_Buffer may be modified

                     begin
                        if Verbose_Mode then
                           Write_Str ("   Adding ");
                           Write_Line (Argv);
                        end if;

                        Scan_Make_Arg (Argv, And_Save => False);
                     end;
                  end if;

                  Switch_List := Element.Next;
               end loop;

            when Single =>
               Program_Args := Program;
               Get_Name_String (Switches.Value);

               if Name_Len > 0 then
                  declare
                     Argv : constant String := Name_Buffer (1 .. Name_Len);
                     --  We need a copy, because Name_Buffer may be modified

                  begin
                     if Verbose_Mode then
                        Write_Str ("   Adding ");
                        Write_Line (Argv);
                     end if;

                     Scan_Make_Arg (Argv, And_Save => False);
                  end;
               end if;
         end case;
      end if;
   end Add_Switches;

   ----------
   -- Bind --
   ----------

   procedure Bind (ALI_File : File_Name_Type; Args : Argument_List) is
      Bind_Args : Argument_List (1 .. Args'Last + 2);
      Bind_Last : Integer;
      Success   : Boolean;

   begin
      pragma Assert (Args'First = 1);

      --  Optimize the simple case where the gnatbind command line looks like
      --     gnatbind -aO. -I- file.ali   --into->   gnatbind file.adb

      if Args'Length = 2
        and then Args (Args'First).all = "-aO" & Normalized_CWD
        and then Args (Args'Last).all = "-I-"
        and then ALI_File = Strip_Directory (ALI_File)
      then
         Bind_Last := Args'First - 1;

      else
         Bind_Last := Args'Last;
         Bind_Args (Args'Range) := Args;
      end if;

      --  It is completely pointless to re-check source file time stamps. This
      --  has been done already by gnatmake

      Bind_Last := Bind_Last + 1;
      Bind_Args (Bind_Last) := Do_Not_Check_Flag;

      Get_Name_String (ALI_File);

      Bind_Last := Bind_Last + 1;
      Bind_Args (Bind_Last) := new String'(Name_Buffer (1 .. Name_Len));

      GNAT.OS_Lib.Normalize_Arguments (Bind_Args (Args'First .. Bind_Last));

      Display (Gnatbind.all, Bind_Args (Args'First .. Bind_Last));

      if Gnatbind_Path = null then
         Make_Failed ("error, unable to locate ", Gnatbind.all);
      end if;

      GNAT.OS_Lib.Spawn
        (Gnatbind_Path.all, Bind_Args (Args'First .. Bind_Last), Success);

      if not Success then
         raise Bind_Failed;
      end if;
   end Bind;

   --------------------------------
   -- Change_To_Object_Directory --
   --------------------------------

   procedure Change_To_Object_Directory (Project : Project_Id) is
      Actual_Project : Project_Id;

   begin
      --  For sources outside of any project, compilation occurs in the object
      --  directory of the main project, otherwise we use the project given.

      if Project = No_Project then
         Actual_Project := Main_Project;
      else
         Actual_Project := Project;
      end if;

      --  Nothing to do if the current working directory is already the correct
      --  object directory.

      if Project_Object_Directory /= Actual_Project then
         Project_Object_Directory := Actual_Project;

         --  Set the working directory to the object directory of the actual
         --  project.

         Change_Dir
           (Get_Name_String
              (Project_Tree.Projects.Table
                 (Actual_Project).Object_Directory));

      end if;

   exception
      --  Fail if unable to change to the object directory

      when Directory_Error =>
         Make_Failed ("unable to change to object directory """ &
                      Get_Name_String
                        (Project_Tree.Projects.Table
                           (Actual_Project).Object_Directory) &
                      """ of project " &
                      Get_Name_String (Project_Tree.Projects.Table
                                         (Actual_Project).Display_Name));
   end Change_To_Object_Directory;

   -----------
   -- Check --
   -----------

   procedure Check
     (Source_File  : File_Name_Type;
      Source_Index : Int;
      The_Args     : Argument_List;
      Lib_File     : File_Name_Type;
      Read_Only    : Boolean;
      ALI          : out ALI_Id;
      O_File       : out File_Name_Type;
      O_Stamp      : out Time_Stamp_Type)
   is
      function First_New_Spec (A : ALI_Id) return File_Name_Type;
      --  Looks in the with table entries of A and returns the spec file name
      --  of the first withed unit (subprogram) for which no spec existed when
      --  A was generated but for which there exists one now, implying that A
      --  is now obsolete. If no such unit is found No_File is returned.
      --  Otherwise the spec file name of the unit is returned.
      --
      --  **WARNING** in the event of Uname format modifications, one *MUST*
      --  make sure this function is also updated.
      --
      --  Note: This function should really be in ali.adb and use Uname
      --  services, but this causes the whole compiler to be dragged along
      --  for gnatbind and gnatmake.

      --------------------
      -- First_New_Spec --
      --------------------

      function First_New_Spec (A : ALI_Id) return File_Name_Type is
         Spec_File_Name : File_Name_Type := No_File;

         function New_Spec (Uname : Unit_Name_Type) return Boolean;
         --  Uname is the name of the spec or body of some ada unit. This
         --  function returns True if the Uname is the name of a body which has
         --  a spec not mentioned inali file A. If True is returned
         --  Spec_File_Name above is set to the name of this spec file.

         --------------
         -- New_Spec --
         --------------

         function New_Spec (Uname : Unit_Name_Type) return Boolean is
            Spec_Name : Unit_Name_Type;
            File_Name : File_Name_Type;

         begin
            --  Test whether Uname is the name of a body unit (ie ends with %b)

            Get_Name_String (Uname);
            pragma
              Assert (Name_Len > 2 and then Name_Buffer (Name_Len - 1) = '%');

            if Name_Buffer (Name_Len) /= 'b' then
               return False;
            end if;

            --  Convert unit name into spec name

            --  ??? this code seems dubious in presence of pragma
            --  Source_File_Name since there is no more direct relationship
            --  between unit name and file name.

            --  ??? Further, what about alternative subunit naming

            Name_Buffer (Name_Len) := 's';
            Spec_Name := Name_Find;
            File_Name := Get_File_Name (Spec_Name, Subunit => False);

            --  Look if File_Name is mentioned in A's sdep list.
            --  If not look if the file exists. If it does return True.

            for D in
              ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
            loop
               if Sdep.Table (D).Sfile = File_Name then
                  return False;
               end if;
            end loop;

            if Full_Source_Name (File_Name) /= No_File then
               Spec_File_Name := File_Name;
               return True;
            end if;

            return False;
         end New_Spec;

      --  Start of processing for First_New_Spec

      begin
         U_Chk : for U in
           ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit
         loop
            exit U_Chk when Units.Table (U).Utype = Is_Body_Only
               and then New_Spec (Units.Table (U).Uname);

            for W in Units.Table (U).First_With
                       ..
                     Units.Table (U).Last_With
            loop
               exit U_Chk when
                 Withs.Table (W).Afile /= No_File
                 and then New_Spec (Withs.Table (W).Uname);
            end loop;
         end loop U_Chk;

         return Spec_File_Name;
      end First_New_Spec;

      ---------------------------------
      -- Data declarations for Check --
      ---------------------------------

      Full_Lib_File : File_Name_Type;
      --  Full name of current library file

      Full_Obj_File : File_Name_Type;
      --  Full name of the object file corresponding to Lib_File

      Lib_Stamp : Time_Stamp_Type;
      --  Time stamp of the current ada library file

      Obj_Stamp : Time_Stamp_Type;
      --  Time stamp of the current object file

      Modified_Source : File_Name_Type;
      --  The first source in Lib_File whose current time stamp differs
      --  from that stored in Lib_File.

      New_Spec : File_Name_Type;
      --  If Lib_File contains in its W (with) section a body (for a
      --  subprogram) for which there exists a spec and the spec did not
      --  appear in the Sdep section of Lib_File, New_Spec contains the file
      --  name of this new spec.

      Source_Name : Name_Id;
      Text        : Text_Buffer_Ptr;

      Prev_Switch : String_Access;
      --  Previous switch processed

      Arg : Arg_Id := Arg_Id'First;
      --  Current index in Args.Table for a given unit (init to stop warning)

      Switch_Found : Boolean;
      --  True if a given switch has been found

   --  Start of processing for Check

   begin
      pragma Assert (Lib_File /= No_File);

      --  If ALI file is read-only, temporarily set Check_Object_Consistency to
      --  False. We don't care if the object file is not there (presumably a
      --  library will be used for linking.)

      if Read_Only then
         declare
            Saved_Check_Object_Consistency : constant Boolean :=
                                               Check_Object_Consistency;
         begin
            Check_Object_Consistency := False;
            Text := Read_Library_Info (Lib_File);
            Check_Object_Consistency := Saved_Check_Object_Consistency;
         end;

      else
         Text := Read_Library_Info (Lib_File);
      end if;

      Full_Lib_File := Full_Library_Info_Name;
      Full_Obj_File := Full_Object_File_Name;
      Lib_Stamp     := Current_Library_File_Stamp;
      Obj_Stamp     := Current_Object_File_Stamp;

      if Full_Lib_File = No_File then
         Verbose_Msg
           (Lib_File,
            "being checked ...",
            Prefix => "  ",
            Minimum_Verbosity => Opt.Medium);
      else
         Verbose_Msg
           (Full_Lib_File,
            "being checked ...",
            Prefix => "  ",
            Minimum_Verbosity => Opt.Medium);
      end if;

      ALI     := No_ALI_Id;
      O_File  := Full_Obj_File;
      O_Stamp := Obj_Stamp;

      if Text = null then
         if Full_Lib_File = No_File then
            Verbose_Msg (Lib_File, "missing.");

         elsif Obj_Stamp (Obj_Stamp'First) = ' ' then
            Verbose_Msg (Full_Obj_File, "missing.");

         else
            Verbose_Msg
              (Full_Lib_File, "(" & String (Lib_Stamp) & ") newer than",
               Full_Obj_File, "(" & String (Obj_Stamp) & ")");
         end if;

      else
         ALI := Scan_ALI (Lib_File, Text, Ignore_ED => False, Err => True);
         Free (Text);

         if ALI = No_ALI_Id then
            Verbose_Msg (Full_Lib_File, "incorrectly formatted ALI file");
            return;

         elsif ALIs.Table (ALI).Ver (1 .. ALIs.Table (ALI).Ver_Len) /=
                 Verbose_Library_Version
         then
            Verbose_Msg (Full_Lib_File, "compiled with old GNAT version");
            ALI := No_ALI_Id;
            return;
         end if;

         --  Don't take Ali file into account if it was generated with
         --  errors.

         if ALIs.Table (ALI).Compile_Errors then
            Verbose_Msg (Full_Lib_File, "had errors, must be recompiled");
            ALI := No_ALI_Id;
            return;
         end if;

         --  Don't take Ali file into account if it was generated without
         --  object.

         if Operating_Mode /= Check_Semantics
           and then ALIs.Table (ALI).No_Object
         then
            Verbose_Msg (Full_Lib_File, "has no corresponding object");
            ALI := No_ALI_Id;
            return;
         end if;

         --  Check for matching compiler switches if needed

         if Check_Switches then

            --  First, collect all the switches

            Collect_Arguments (Source_File, Source_Index, The_Args);

            Prev_Switch := Dummy_Switch;

            Get_Name_String (ALIs.Table (ALI).Sfile);

            Switches_To_Check.Set_Last (0);

            for J in 1 .. Last_Argument loop

               --  Skip non switches -c, -I and -o switches

               if Arguments (J) (1) = '-'
                 and then Arguments (J) (2) /= 'c'
                 and then Arguments (J) (2) /= 'o'
                 and then Arguments (J) (2) /= 'I'
               then
                  Normalize_Compiler_Switches
                    (Arguments (J).all,
                     Normalized_Switches,
                     Last_Norm_Switch);

                  for K in 1 .. Last_Norm_Switch loop
                     Switches_To_Check.Increment_Last;
                     Switches_To_Check.Table (Switches_To_Check.Last) :=
                       Normalized_Switches (K);
                  end loop;
               end if;
            end loop;

            for J in 1 .. Switches_To_Check.Last loop

               --  Comparing switches is delicate because gcc reorders a number
               --  of switches, according to lang-specs.h, but gnatmake doesn't
               --  have the sufficient knowledge to perform the same
               --  reordering. Instead, we ignore orders between different
               --  "first letter" switches, but keep orders between same
               --  switches, e.g -O -O2 is different than -O2 -O, but -g -O is
               --  equivalent to -O -g.

               if Switches_To_Check.Table (J) (2) /= Prev_Switch (2) or else
                   (Prev_Switch'Length >= 6 and then
                    Prev_Switch (2 .. 5) = "gnat" and then
                    Switches_To_Check.Table (J)'Length >= 6 and then
                    Switches_To_Check.Table (J) (2 .. 5) = "gnat" and then
                    Prev_Switch (6) /= Switches_To_Check.Table (J) (6))
               then
                  Prev_Switch := Switches_To_Check.Table (J);
                  Arg :=
                    Units.Table (ALIs.Table (ALI).First_Unit).First_Arg;
               end if;

               Switch_Found := False;

               for K in Arg ..
                 Units.Table (ALIs.Table (ALI).First_Unit).Last_Arg
               loop
                  if
                    Switches_To_Check.Table (J).all = Args.Table (K).all
                  then
                     Arg := K + 1;
                     Switch_Found := True;
                     exit;
                  end if;
               end loop;

               if not Switch_Found then
                  if Verbose_Mode then
                     Verbose_Msg (ALIs.Table (ALI).Sfile,
                                  "switch mismatch """ &
                                  Switches_To_Check.Table (J).all & '"');
                  end if;

                  ALI := No_ALI_Id;
                  return;
               end if;
            end loop;

            if Switches_To_Check.Last /=
              Integer (Units.Table (ALIs.Table (ALI).First_Unit).Last_Arg -
                       Units.Table (ALIs.Table (ALI).First_Unit).First_Arg + 1)
            then
               if Verbose_Mode then
                  Verbose_Msg (ALIs.Table (ALI).Sfile,
                               "different number of switches");

                  for K in Units.Table (ALIs.Table (ALI).First_Unit).First_Arg
                    .. Units.Table (ALIs.Table (ALI).First_Unit).Last_Arg
                  loop
                     Write_Str (Args.Table (K).all);
                     Write_Char (' ');
                  end loop;

                  Write_Eol;

                  for J in 1 .. Switches_To_Check.Last loop
                     Write_Str (Switches_To_Check.Table (J).all);
                     Write_Char (' ');
                  end loop;

                  Write_Eol;
               end if;

               ALI := No_ALI_Id;
               return;
            end if;
         end if;

         --  Get the source files and their message digests. Note that some
         --  sources may be missing if ALI is out-of-date.

         Set_Source_Table (ALI);

         Modified_Source := Time_Stamp_Mismatch (ALI, Read_Only);

         if Modified_Source /= No_File then
            ALI := No_ALI_Id;

            if Verbose_Mode then
               Source_Name := Full_Source_Name (Modified_Source);

               if Source_Name /= No_File then
                  Verbose_Msg (Source_Name, "time stamp mismatch");
               else
                  Verbose_Msg (Modified_Source, "missing");
               end if;
            end if;

         else
            New_Spec := First_New_Spec (ALI);

            if New_Spec /= No_File then
               ALI := No_ALI_Id;

               if Verbose_Mode then
                  Source_Name := Full_Source_Name (New_Spec);

                  if Source_Name /= No_File then
                     Verbose_Msg (Source_Name, "new spec");
                  else
                     Verbose_Msg (New_Spec, "old spec missing");
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Check;

   ------------------------
   -- Check_For_S_Switch --
   ------------------------

   procedure Check_For_S_Switch is
   begin
      --  By default, we generate an object file

      Output_Is_Object := True;

      for Arg in 1 .. Last_Argument loop
         if Arguments (Arg).all = "-S" then
            Output_Is_Object := False;

         elsif Arguments (Arg).all = "-c" then
            Output_Is_Object := True;
         end if;
      end loop;
   end Check_For_S_Switch;

   --------------------------
   -- Check_Linker_Options --
   --------------------------

   procedure Check_Linker_Options
     (E_Stamp   : Time_Stamp_Type;
      O_File    : out File_Name_Type;
      O_Stamp   : out Time_Stamp_Type)
   is
      procedure Check_File (File : File_Name_Type);
      --  Update O_File and O_Stamp if the given file is younger than E_Stamp
      --  and O_Stamp, or if O_File is No_File and File does not exist.

      function Get_Library_File (Name : String) return File_Name_Type;
      --  Return the full file name including path of a library based
      --  on the name specified with the -l linker option, using the
      --  Ada object path. Return No_File if no such file can be found.

      type Char_Array is array (Natural) of Character;
      type Char_Array_Access is access constant Char_Array;

      Template : Char_Array_Access;
      pragma Import (C, Template, "__gnat_library_template");

      ----------------
      -- Check_File --
      ----------------

      procedure Check_File (File : File_Name_Type) is
         Stamp : Time_Stamp_Type;
         Name  : File_Name_Type := File;

      begin
         Get_Name_String (Name);

         --  Remove any trailing NUL characters

         while Name_Len >= Name_Buffer'First
           and then Name_Buffer (Name_Len) = NUL
         loop
            Name_Len := Name_Len - 1;
         end loop;

         if Name_Len = 0 then
            return;

         elsif Name_Buffer (1) = '-' then

            --  Do not check if File is a switch other than "-l"

            if Name_Buffer (2) /= 'l' then
               return;
            end if;

            --  The argument is a library switch, get actual name. It
            --  is necessary to make a copy of the relevant part of
            --  Name_Buffer as Get_Library_Name uses Name_Buffer as well.

            declare
               Base_Name : constant String := Name_Buffer (3 .. Name_Len);

            begin
               Name := Get_Library_File (Base_Name);
            end;

            if Name = No_File then
               return;
            end if;
         end if;

         Stamp := File_Stamp (Name);

         --  Find the youngest object file that is younger than the
         --  executable. If no such file exist, record the first object
         --  file that is not found.

         if (O_Stamp < Stamp and then E_Stamp < Stamp)
           or else (O_File = No_File and then Stamp (Stamp'First) = ' ')
         then
            O_Stamp := Stamp;
            O_File := Name;

            --  Strip the trailing NUL if present

            Get_Name_String (O_File);

            if Name_Buffer (Name_Len) = NUL then
               Name_Len := Name_Len - 1;
               O_File := Name_Find;
            end if;
         end if;
      end Check_File;

      ----------------------
      -- Get_Library_Name --
      ----------------------

      --  See comments in a-adaint.c about template syntax

      function Get_Library_File (Name : String) return File_Name_Type is
         File : File_Name_Type := No_File;

      begin
         Name_Len := 0;

         for Ptr in Template'Range loop
            case Template (Ptr) is
               when '*'    =>
                  Add_Str_To_Name_Buffer (Name);

               when ';'    =>
                  File := Full_Lib_File_Name (Name_Find);
                  exit when File /= No_File;
                  Name_Len := 0;

               when NUL    =>
                  exit;

               when others =>
                  Add_Char_To_Name_Buffer (Template (Ptr));
            end case;
         end loop;

         --  The for loop exited because the end of the template
         --  was reached. File contains the last possible file name
         --  for the library.

         if File = No_File and then Name_Len > 0 then
            File := Full_Lib_File_Name (Name_Find);
         end if;

         return File;
      end Get_Library_File;

   --  Start of processing for Check_Linker_Options

   begin
      O_File  := No_File;
      O_Stamp := (others => ' ');

      --  Process linker options from the ALI files

      for Opt in 1 .. Linker_Options.Last loop
         Check_File (Linker_Options.Table (Opt).Name);
      end loop;

      --  Process options given on the command line

      for Opt in Linker_Switches.First .. Linker_Switches.Last loop

         --  Check if the previous Opt has one of the two switches
         --  that take an extra parameter. (See GCC manual.)

         if Opt = Linker_Switches.First
           or else (Linker_Switches.Table (Opt - 1).all /= "-u"
                      and then
                    Linker_Switches.Table (Opt - 1).all /= "-Xlinker"
                      and then
                    Linker_Switches.Table (Opt - 1).all /= "-L")
         then
            Name_Len := 0;
            Add_Str_To_Name_Buffer (Linker_Switches.Table (Opt).all);
            Check_File (Name_Find);
         end if;
      end loop;

   end Check_Linker_Options;

   -----------------
   -- Check_Steps --
   -----------------

   procedure Check_Steps is
   begin
      --  If either -c, -b or -l has been specified, we will not necessarily
      --  execute all steps.

      if Make_Steps then
         Do_Compile_Step := Do_Compile_Step and Compile_Only;
         Do_Bind_Step    := Do_Bind_Step    and Bind_Only;
         Do_Link_Step    := Do_Link_Step    and Link_Only;

         --  If -c has been specified, but not -b, ignore any potential -l

         if Do_Compile_Step and then not Do_Bind_Step then
            Do_Link_Step := False;
         end if;
      end if;
   end Check_Steps;

   -----------------------
   -- Collect_Arguments --
   -----------------------

   procedure Collect_Arguments
     (Source_File  : File_Name_Type;
      Source_Index : Int;
      Args         : Argument_List)
   is
   begin
      Arguments_Collected := True;
      Arguments_Project := No_Project;
      Last_Argument := 0;
      Add_Arguments (Args);

      if Main_Project /= No_Project then
         declare
            Source_File_Name : constant String :=
                                 Get_Name_String (Source_File);
            Compiler_Package : Prj.Package_Id;
            Switches         : Prj.Variable_Value;
            Data             : Project_Data;

         begin
            Prj.Env.
              Get_Reference
              (Source_File_Name => Source_File_Name,
               Project          => Arguments_Project,
               Path             => Arguments_Path_Name,
               In_Tree          => Project_Tree);

            --  If the source is not a source of a project file, check if
            --  this is allowed.

            if Arguments_Project = No_Project then
               if not External_Unit_Compilation_Allowed then
                  Make_Failed ("external source (", Source_File_Name,
                               ") is not part of any project; cannot be " &
                               "compiled without gnatmake switch -x");
               end if;

               --  If it is allowed, simply add the saved gcc switches

               Add_Arguments (The_Saved_Gcc_Switches.all);

            else
               --  We get the project directory for the relative path
               --  switches and arguments.

               Data := Project_Tree.Projects.Table (Arguments_Project);

               --  If the source is in an extended project, we go to
               --  the ultimate extending project.

               while Data.Extended_By /= No_Project loop
                  Arguments_Project := Data.Extended_By;
                  Data :=
                    Project_Tree.Projects.Table (Arguments_Project);
               end loop;

               --  If building a dynamic or relocatable library, compile with
               --  PIC option, if it exists.

               if Data.Library and then Data.Library_Kind /= Static then
                  declare
                     PIC : constant String := MLib.Tgt.PIC_Option;

                  begin
                     if PIC /= "" then
                        Add_Arguments ((1 => new String'(PIC)));
                     end if;
                  end;
               end if;

               if Data.Dir_Path = null then
                  Data.Dir_Path :=
                    new String'(Get_Name_String (Data.Display_Directory));
                  Project_Tree.Projects.Table (Arguments_Project) :=
                    Data;
               end if;

               --  We now look for package Compiler
               --  and get the switches from this package.

               Compiler_Package :=
                 Prj.Util.Value_Of
                   (Name        => Name_Compiler,
                    In_Packages => Data.Decl.Packages,
                    In_Tree     => Project_Tree);

               if Compiler_Package /= No_Package then

                  --  If package Gnatmake.Compiler exists, we get
                  --  the specific switches for the current source,
                  --  or the global switches, if any.

                  Switches := Switches_Of
                    (Source_File      => Source_File,
                     Source_File_Name => Source_File_Name,
                     Source_Index     => Source_Index,
                     Naming           => Data.Naming,
                     In_Package       => Compiler_Package,
                     Allow_ALI        => False);

               end if;

               case Switches.Kind is

                  --  We have a list of switches. We add these switches,
                  --  plus the saved gcc switches.

                  when List =>

                     declare
                        Current : String_List_Id := Switches.Values;
                        Element : String_Element;
                        Number  : Natural := 0;

                     begin
                        while Current /= Nil_String loop
                           Element := Project_Tree.String_Elements.
                                        Table (Current);
                           Number  := Number + 1;
                           Current := Element.Next;
                        end loop;

                        declare
                           New_Args : Argument_List (1 .. Number);
                           Last_New : Natural := 0;

                        begin
                           Current := Switches.Values;

                           for Index in New_Args'Range loop
                              Element := Project_Tree.String_Elements.
                                           Table (Current);
                              Get_Name_String (Element.Value);

                              if Name_Len > 0 then
                                 Last_New := Last_New + 1;
                                 New_Args (Last_New) :=
                                   new String'(Name_Buffer (1 .. Name_Len));
                                 Test_If_Relative_Path
                                   (New_Args (Last_New),
                                    Parent => Data.Dir_Path);
                              end if;

                              Current := Element.Next;
                           end loop;

                           Add_Arguments
                             (Configuration_Pragmas_Switch
                                (Arguments_Project) &
                              New_Args (1 .. Last_New) &
                              The_Saved_Gcc_Switches.all);
                        end;
                     end;

                     --  We have a single switch. We add this switch,
                     --  plus the saved gcc switches.

                  when Single =>
                     Get_Name_String (Switches.Value);

                     declare
                        New_Args : Argument_List :=
                                     (1 => new String'
                                            (Name_Buffer (1 .. Name_Len)));

                     begin
                        Test_If_Relative_Path
                          (New_Args (1), Parent => Data.Dir_Path);
                        Add_Arguments
                          (Configuration_Pragmas_Switch (Arguments_Project) &
                           New_Args & The_Saved_Gcc_Switches.all);
                     end;

                     --  We have no switches from Gnatmake.Compiler.
                     --  We add the saved gcc switches.

                  when Undefined =>
                     Add_Arguments
                       (Configuration_Pragmas_Switch (Arguments_Project) &
                        The_Saved_Gcc_Switches.all);
               end case;
            end if;
         end;
      end if;

      --  Set Output_Is_Object, depending if there is a -S switch.
      --  If the bind step is not performed, and there is a -S switch,
      --  then we will not check for a valid object file.

      Check_For_S_Switch;
   end Collect_Arguments;

   ---------------------
   -- Compile_Sources --
   ---------------------

   procedure Compile_Sources
     (Main_Source           : File_Name_Type;
      Args                  : Argument_List;
      First_Compiled_File   : out Name_Id;
      Most_Recent_Obj_File  : out Name_Id;
      Most_Recent_Obj_Stamp : out Time_Stamp_Type;
      Main_Unit             : out Boolean;
      Compilation_Failures  : out Natural;
      Main_Index            : Int      := 0;
      Check_Readonly_Files  : Boolean  := False;
      Do_Not_Execute        : Boolean  := False;
      Force_Compilations    : Boolean  := False;
      Keep_Going            : Boolean  := False;
      In_Place_Mode         : Boolean  := False;
      Initialize_ALI_Data   : Boolean  := True;
      Max_Process           : Positive := 1)
   is
      No_Mapping_File : constant Natural := 0;

      type Compilation_Data is record
         Pid              : Process_Id;
         Full_Source_File : File_Name_Type;
         Lib_File         : File_Name_Type;
         Source_Unit      : Unit_Name_Type;
         Mapping_File     : Natural := No_Mapping_File;
         Project          : Project_Id := No_Project;
         Syntax_Only      : Boolean := False;
         Output_Is_Object : Boolean := True;
      end record;

      Running_Compile : array (1 .. Max_Process) of Compilation_Data;
      --  Used to save information about outstanding compilations

      Outstanding_Compiles : Natural := 0;
      --  Current number of outstanding compiles

      Source_Unit : Unit_Name_Type;
      --  Current source unit

      Source_File : File_Name_Type;
      --  Current source file

      Full_Source_File : File_Name_Type;
      --  Full name of the current source file

      Lib_File : File_Name_Type;
      --  Current library file

      Full_Lib_File : File_Name_Type;
      --  Full name of the current library file

      Obj_File : File_Name_Type;
      --  Full name of the object file corresponding to Lib_File

      Obj_Stamp : Time_Stamp_Type;
      --  Time stamp of the current object file

      Sfile : File_Name_Type;
      --  Contains the source file of the units withed by Source_File

      ALI : ALI_Id;
      --  ALI Id of the current ALI file

      --  Comment following declarations ???

      Read_Only : Boolean := False;

      Compilation_OK  : Boolean;
      Need_To_Compile : Boolean;

      Pid  : Process_Id;
      Text : Text_Buffer_Ptr;

      Mfile : Natural := No_Mapping_File;

      Need_To_Check_Standard_Library : Boolean :=
                                         Check_Readonly_Files
                                           and not Unique_Compile;

      Mapping_File_Arg : String_Access;

      Process_Created : Boolean := False;

      procedure Add_Process
        (Pid    : Process_Id;
         Sfile  : File_Name_Type;
         Afile  : File_Name_Type;
         Uname  : Unit_Name_Type;
         Mfile  : Natural := No_Mapping_File);
      --  Adds process Pid to the current list of outstanding compilation
      --  processes and record the full name of the source file Sfile that
      --  we are compiling, the name of its library file Afile and the
      --  name of its unit Uname. If Mfile is not equal to No_Mapping_File,
      --  it is the index of the mapping file used during compilation in the
      --  array The_Mapping_File_Names.

      procedure Await_Compile
        (Sfile : out File_Name_Type;
         Afile : out File_Name_Type;
         Uname : out Unit_Name_Type;
         OK    : out Boolean);
      --  Awaits that an outstanding compilation process terminates. When
      --  it does set Sfile to the name of the source file that was compiled
      --  Afile to the name of its library file and Uname to the name of its
      --  unit. Note that this time stamp can be used to check whether the
      --  compilation did generate an object file. OK is set to True if the
      --  compilation succeeded. Note that Sfile, Afile and Uname could be
      --  resp. No_File, No_File and No_Name  if there were no compilations
      --  to wait for.

      function Bad_Compilation_Count return Natural;
      --  Returns the number of compilation failures

      procedure Check_Standard_Library;
      --  Check if s-stalib.adb needs to be compiled

      procedure Collect_Arguments_And_Compile
        (Source_File : File_Name_Type; Source_Index : Int);
      --  Collect arguments from project file (if any) and compile

      function Compile
        (S            : Name_Id;
         L            : Name_Id;
         Source_Index : Int;
         Args         : Argument_List) return Process_Id;
      --  Compiles S using Args. If S is a GNAT predefined source
      --  "-gnatpg" is added to Args. Non blocking call. L corresponds to the
      --  expected library file name. Process_Id of the process spawned to
      --  execute the compile.

      package Good_ALI is new Table.Table (
        Table_Component_Type => ALI_Id,
        Table_Index_Type     => Natural,
        Table_Low_Bound      => 1,
        Table_Initial        => 50,
        Table_Increment      => 100,
        Table_Name           => "Make.Good_ALI");
      --  Contains the set of valid ALI files that have not yet been scanned

      function Good_ALI_Present return Boolean;
      --  Returns True if any ALI file was recorded in the previous set

      procedure Get_Mapping_File (Project : Project_Id);
      --  Get a mapping file name. If there is one to be reused, reuse it.
      --  Otherwise, create a new mapping file.

      function Get_Next_Good_ALI return ALI_Id;
      --  Returns the next good ALI_Id record

      procedure Record_Failure
        (File  : File_Name_Type;
         Unit  : Unit_Name_Type;
         Found : Boolean := True);
      --  Records in the previous table that the compilation for File failed.
      --  If Found is False then the compilation of File failed because we
      --  could not find it. Records also Unit when possible.

      procedure Record_Good_ALI (A : ALI_Id);
      --  Records in the previous set the Id of an ALI file

      -----------------
      -- Add_Process --
      -----------------

      procedure Add_Process
        (Pid    : Process_Id;
         Sfile  : File_Name_Type;
         Afile  : File_Name_Type;
         Uname  : Unit_Name_Type;
         Mfile  : Natural := No_Mapping_File)
      is
         OC1 : constant Positive := Outstanding_Compiles + 1;

      begin
         pragma Assert (OC1 <= Max_Process);
         pragma Assert (Pid /= Invalid_Pid);

         Running_Compile (OC1).Pid              := Pid;
         Running_Compile (OC1).Full_Source_File := Sfile;
         Running_Compile (OC1).Lib_File         := Afile;
         Running_Compile (OC1).Source_Unit      := Uname;
         Running_Compile (OC1).Mapping_File     := Mfile;
         Running_Compile (OC1).Project          := Arguments_Project;
         Running_Compile (OC1).Syntax_Only      := Syntax_Only;
         Running_Compile (OC1).Output_Is_Object := Output_Is_Object;

         Outstanding_Compiles := OC1;
      end Add_Process;

      --------------------
      -- Await_Compile --
      -------------------

      procedure Await_Compile
        (Sfile  : out File_Name_Type;
         Afile  : out File_Name_Type;
         Uname  : out File_Name_Type;
         OK     : out Boolean)
      is
         Pid : Process_Id;
         Project : Project_Id;

      begin
         pragma Assert (Outstanding_Compiles > 0);

         Sfile := No_File;
         Afile := No_File;
         Uname := No_Name;
         OK    := False;

         --  The loop here is a work-around for a problem on VMS; in some
         --  circumstances (shared library and several executables, for
         --  example), there are child processes other than compilation
         --  processes that are received. Until this problem is resolved,
         --  we will ignore such processes.

         loop
            Wait_Process (Pid, OK);

            if Pid = Invalid_Pid then
               return;
            end if;

            for J in Running_Compile'First .. Outstanding_Compiles loop
               if Pid = Running_Compile (J).Pid then
                  Sfile := Running_Compile (J).Full_Source_File;
                  Afile := Running_Compile (J).Lib_File;
                  Uname := Running_Compile (J).Source_Unit;
                  Syntax_Only := Running_Compile (J).Syntax_Only;
                  Output_Is_Object := Running_Compile (J).Output_Is_Object;
                  Project := Running_Compile (J).Project;

                  --  If a mapping file was used by this compilation,
                  --  get its file name for reuse by a subsequent compilation

                  if Running_Compile (J).Mapping_File /= No_Mapping_File then
                     Last_Free_Indices (Project) :=
                       Last_Free_Indices (Project) + 1;
                     The_Free_Mapping_File_Indices
                       (Project, Last_Free_Indices (Project)) :=
                       Running_Compile (J).Mapping_File;
                  end if;

                  --  To actually remove this Pid and related info from
                  --  Running_Compile replace its entry with the last valid
                  --  entry in Running_Compile.

                  if J = Outstanding_Compiles then
                     null;

                  else
                     Running_Compile (J) :=
                       Running_Compile (Outstanding_Compiles);
                  end if;

                  Outstanding_Compiles := Outstanding_Compiles - 1;
                  return;
               end if;
            end loop;

            --  This child process was not one of our compilation processes;
            --  just ignore it for now.

            --  raise Program_Error;
         end loop;
      end Await_Compile;

      ---------------------------
      -- Bad_Compilation_Count --
      ---------------------------

      function Bad_Compilation_Count return Natural is
      begin
         return Bad_Compilation.Last - Bad_Compilation.First + 1;
      end Bad_Compilation_Count;

      ----------------------------
      -- Check_Standard_Library --
      ----------------------------

      procedure Check_Standard_Library is
      begin
         Need_To_Check_Standard_Library := False;

         if not Targparm.Suppress_Standard_Library_On_Target then
            declare
               Sfile  : Name_Id;
               Add_It : Boolean := True;

            begin
               Name_Len := Standard_Library_Package_Body_Name'Length;
               Name_Buffer (1 .. Name_Len) :=
                 Standard_Library_Package_Body_Name;
               Sfile := Name_Enter;

               --  If we have a special runtime, we add the standard
               --  library only if we can find it.

               if RTS_Switch then
                  Add_It :=
                    Find_File (Sfile, Osint.Source) /= No_File;
               end if;

               if Add_It then
                  if Is_Marked (Sfile) then
                     if Is_In_Obsoleted (Sfile) then
                        Executable_Obsolete := True;
                     end if;

                  else
                     Insert_Q (Sfile, Index => 0);
                     Mark (Sfile, Index => 0);
                  end if;
               end if;
            end;
         end if;
      end Check_Standard_Library;

      -----------------------------------
      -- Collect_Arguments_And_Compile --
      -----------------------------------

      procedure Collect_Arguments_And_Compile
        (Source_File : File_Name_Type; Source_Index : Int)
      is
      begin
         --  Process_Created will be set True if an attempt is made to compile
         --  the source, that is if it is not in an externally built project.

         Process_Created := False;

         --  If arguments not yet collected (in Check), collect them now

         if not Arguments_Collected then
            Collect_Arguments (Source_File, Source_Index, Args);
         end if;

         --  If we use mapping file (-P or -C switches), then get one

         if Create_Mapping_File then
            Get_Mapping_File (Arguments_Project);
         end if;

         --  If the source is part of a project file, we set the ADA_*_PATHs,
         --  check for an eventual library project, and use the full path.

         if Arguments_Project /= No_Project then
            if not Project_Tree.Projects.Table
                     (Arguments_Project).Externally_Built
            then
               Prj.Env.Set_Ada_Paths
                 (Arguments_Project, Project_Tree, True);

               if not Unique_Compile
                 and then MLib.Tgt.Support_For_Libraries /= MLib.Tgt.None
               then
                  declare
                     The_Data : Project_Data :=
                                  Project_Tree.Projects.Table
                                    (Arguments_Project);

                     Prj : Project_Id := Arguments_Project;

                  begin
                     while The_Data.Extended_By /= No_Project loop
                        Prj := The_Data.Extended_By;
                        The_Data := Project_Tree.Projects.Table (Prj);
                     end loop;

                     if The_Data.Library
                       and then not The_Data.Need_To_Build_Lib
                     then
                        --  Add to the Q all sources of the project that
                        --  have not been marked

                        Insert_Project_Sources
                          (The_Project  => Prj,
                           All_Projects => False,
                           Into_Q       => True);

                        --  Now mark the project as processed

                        Project_Tree.Projects.Table
                          (Prj).Need_To_Build_Lib := True;
                     end if;
                  end;
               end if;

               --  Change to the object directory of the project file,
               --  if necessary.

               Change_To_Object_Directory (Arguments_Project);

               Pid := Compile (Arguments_Path_Name, Lib_File, Source_Index,
                               Arguments (1 .. Last_Argument));
               Process_Created := True;
            end if;

         else
            --  If this is a source outside of any project file, make sure it
            --  will be compiled in object directory of the main project file.

            if Main_Project /= No_Project then
               Change_To_Object_Directory (Arguments_Project);
            end if;

            Pid := Compile (Full_Source_File, Lib_File, Source_Index,
                            Arguments (1 .. Last_Argument));
            Process_Created := True;
         end if;
      end Collect_Arguments_And_Compile;

      -------------
      -- Compile --
      -------------

      function Compile
        (S            : Name_Id;
         L            : Name_Id;
         Source_Index : Int;
         Args         : Argument_List) return Process_Id
      is
         Comp_Args : Argument_List (Args'First .. Args'Last + 9);
         Comp_Next : Integer := Args'First;
         Comp_Last : Integer;
         Arg_Index : Integer;

         function Ada_File_Name (Name : Name_Id) return Boolean;
         --  Returns True if Name is the name of an ada source file
         --  (i.e. suffix is .ads or .adb)

         -------------------
         -- Ada_File_Name --
         -------------------

         function Ada_File_Name (Name : Name_Id) return Boolean is
         begin
            Get_Name_String (Name);
            return
              Name_Len > 4
                and then Name_Buffer (Name_Len - 3 .. Name_Len - 1) = ".ad"
                and then (Name_Buffer (Name_Len) = 'b'
                            or else
                          Name_Buffer (Name_Len) = 's');
         end Ada_File_Name;

      --  Start of processing for Compile

      begin
         Enter_Into_Obsoleted (S);

         --  By default, Syntax_Only is False

         Syntax_Only := False;

         for J in Args'Range loop
            if Args (J).all = "-gnats" then

               --  If we compile with -gnats, the bind step and the link step
               --  are inhibited. Also, we set Syntax_Only to True, so that
               --  we don't fail when we don't find the ALI file, after
               --  compilation.

               Do_Bind_Step := False;
               Do_Link_Step := False;
               Syntax_Only  := True;

            elsif Args (J).all = "-gnatc" then

               --  If we compile with -gnatc, the bind step and the link step
               --  are inhibited. We set Syntax_Only to False for the case when
               --  -gnats was previously specified.

               Do_Bind_Step := False;
               Do_Link_Step := False;
               Syntax_Only  := False;
            end if;
         end loop;

         Comp_Args (Comp_Next) := Comp_Flag;
         Comp_Next := Comp_Next + 1;

         --  Optimize the simple case where the gcc command line looks like
         --     gcc -c -I. ... -I- file.adb  --into->  gcc -c ... file.adb

         if Args (Args'First).all = "-I" & Normalized_CWD
           and then Args (Args'Last).all = "-I-"
           and then S = Strip_Directory (S)
         then
            Comp_Last := Comp_Next + Args'Length - 3;
            Arg_Index := Args'First + 1;

         else
            Comp_Last := Comp_Next + Args'Length - 1;
            Arg_Index := Args'First;
         end if;

         --  Make a deep copy of the arguments, because Normalize_Arguments
         --  may deallocate some arguments.

         for J in Comp_Next .. Comp_Last loop
            Comp_Args (J) := new String'(Args (Arg_Index).all);
            Arg_Index := Arg_Index + 1;
         end loop;

         --  Set -gnatpg for predefined files (for this purpose the renamings
         --  such as Text_IO do not count as predefined). Note that we strip
         --  the directory name from the source file name becase the call to
         --  Fname.Is_Predefined_File_Name cannot deal with directory prefixes.

         declare
            Fname : constant File_Name_Type := Strip_Directory (S);

         begin
            if Is_Predefined_File_Name (Fname, False) then
               if Check_Readonly_Files then
                  Comp_Last := Comp_Last + 1;
                  Comp_Args (Comp_Last) := GNAT_Flag;

               else
                  Make_Failed
                    ("not allowed to compile """ &
                     Get_Name_String (Fname) &
                     """; use -a switch, or compile file with " &
                     """-gnatg"" switch");
               end if;
            end if;
         end;

         --  Now check if the file name has one of the suffixes familiar to
         --  the gcc driver. If this is not the case then add the ada flag
         --  "-x ada".

         if not Ada_File_Name (S) and then not Targparm.AAMP_On_Target then
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := Ada_Flag_1;
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := Ada_Flag_2;
         end if;

         if Source_Index /= 0 then
            declare
               Num : constant String := Source_Index'Img;
            begin
               Comp_Last := Comp_Last + 1;
               Comp_Args (Comp_Last) :=
                 new String'("-gnateI" & Num (Num'First + 1 .. Num'Last));
            end;
         end if;

         if Source_Index /= 0 or else
           L /= Strip_Directory (L) or else
           Object_Directory_Path /= null
         then
            --  Build -o argument

            Get_Name_String (L);

            for J in reverse 1 .. Name_Len loop
               if Name_Buffer (J) = '.' then
                  Name_Len := J + Object_Suffix'Length - 1;
                  Name_Buffer (J .. Name_Len) := Object_Suffix;
                  exit;
               end if;
            end loop;

            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := Output_Flag;
            Comp_Last := Comp_Last + 1;

            --  If an object directory was specified, prepend the object file
            --  name with this object directory.

            if Object_Directory_Path /= null then
               Comp_Args (Comp_Last) :=
                 new String'(Object_Directory_Path.all &
                               Name_Buffer (1 .. Name_Len));

            else
               Comp_Args (Comp_Last) :=
                 new String'(Name_Buffer (1 .. Name_Len));
            end if;
         end if;

         if Create_Mapping_File then
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := Mapping_File_Arg;
         end if;

         Get_Name_String (S);

         Comp_Last := Comp_Last + 1;
         Comp_Args (Comp_Last) := new String'(Name_Buffer (1 .. Name_Len));

         GNAT.OS_Lib.Normalize_Arguments (Comp_Args (Args'First .. Comp_Last));

         Comp_Last := Comp_Last + 1;
         Comp_Args (Comp_Last) := new String'("-gnatez");

         Display (Gcc.all, Comp_Args (Args'First .. Comp_Last));

         if Gcc_Path = null then
            Make_Failed ("error, unable to locate ", Gcc.all);
         end if;

         return
           GNAT.OS_Lib.Non_Blocking_Spawn
             (Gcc_Path.all, Comp_Args (Args'First .. Comp_Last));
      end Compile;

      ----------------------
      -- Get_Mapping_File --
      ----------------------

      procedure Get_Mapping_File (Project : Project_Id) is
      begin
         --  If there is a mapping file ready to be reused, reuse it

         if Last_Free_Indices (Project) > 0 then
            Mfile := The_Free_Mapping_File_Indices
                       (Project, Last_Free_Indices (Project));
            Last_Free_Indices (Project) := Last_Free_Indices (Project) - 1;

         --  Otherwise, create and initialize a new one

         else
            Init_Mapping_File (Project => Project, File_Index => Mfile);
         end if;

         --  Put the name in the mapping file argument for the invocation
         --  of the compiler.

         Free (Mapping_File_Arg);
         Mapping_File_Arg :=
           new String'("-gnatem=" &
                       Get_Name_String
                         (The_Mapping_File_Names (Project, Mfile)));

      end Get_Mapping_File;

      -----------------------
      -- Get_Next_Good_ALI --
      -----------------------

      function Get_Next_Good_ALI return ALI_Id is
         ALI : ALI_Id;

      begin
         pragma Assert (Good_ALI_Present);
         ALI := Good_ALI.Table (Good_ALI.Last);
         Good_ALI.Decrement_Last;
         return ALI;
      end Get_Next_Good_ALI;

      ----------------------
      -- Good_ALI_Present --
      ----------------------

      function Good_ALI_Present return Boolean is
      begin
         return Good_ALI.First <= Good_ALI.Last;
      end Good_ALI_Present;

      --------------------
      -- Record_Failure --
      --------------------

      procedure Record_Failure
        (File  : File_Name_Type;
         Unit  : Unit_Name_Type;
         Found : Boolean := True)
      is
      begin
         Bad_Compilation.Increment_Last;
         Bad_Compilation.Table (Bad_Compilation.Last) := (File, Unit, Found);
      end Record_Failure;

      ---------------------
      -- Record_Good_ALI --
      ---------------------

      procedure Record_Good_ALI (A : ALI_Id) is
      begin
         Good_ALI.Increment_Last;
         Good_ALI.Table (Good_ALI.Last) := A;
      end Record_Good_ALI;

   --  Start of processing for Compile_Sources

   begin
      pragma Assert (Args'First = 1);

      --  Package and Queue initializations

      Good_ALI.Init;
      Output.Set_Standard_Error;

      if First_Q_Initialization then
         Init_Q;
      end if;

      if Initialize_ALI_Data then
         Initialize_ALI;
         Initialize_ALI_Source;
      end if;

      --  The following two flags affect the behavior of ALI.Set_Source_Table.
      --  We set Check_Source_Files to True to ensure that source file
      --  time stamps are checked, and we set All_Sources to False to
      --  avoid checking the presence of the source files listed in the
      --  source dependency section of an ali file (which would be a mistake
      --  since the ali file may be obsolete).

      Check_Source_Files := True;
      All_Sources        := False;

      --  Only insert in the Q if it is not already done, to avoid simultaneous
      --  compilations if -jnnn is used.

      if not Is_Marked (Main_Source, Main_Index) then
         Insert_Q (Main_Source, Index => Main_Index);
         Mark (Main_Source, Main_Index);
      end if;

      First_Compiled_File   := No_File;
      Most_Recent_Obj_File  := No_File;
      Most_Recent_Obj_Stamp := Empty_Time_Stamp;
      Main_Unit             := False;

      --  Keep looping until there is no more work to do (the Q is empty)
      --  and all the outstanding compilations have terminated

      Make_Loop : while not Empty_Q or else Outstanding_Compiles > 0 loop

         --  If the user does not want to keep going in case of errors then
         --  wait for the remaining outstanding compiles and then exit.

         if Bad_Compilation_Count > 0 and then not Keep_Going then
            while Outstanding_Compiles > 0 loop
               Await_Compile
                 (Full_Source_File, Lib_File, Source_Unit, Compilation_OK);

               if not Compilation_OK then
                  Record_Failure (Full_Source_File, Source_Unit);
               end if;
            end loop;

            exit Make_Loop;
         end if;

         --  PHASE 1: Check if there is more work that we can do (ie the Q
         --  is non empty). If there is, do it only if we have not yet used
         --  up all the available processes.

         if not Empty_Q and then Outstanding_Compiles < Max_Process then
            declare
               Source_Index : Int;
               --  Index of the current unit in the current source file

            begin
               Extract_From_Q (Source_File, Source_Unit, Source_Index);
               Full_Source_File := Osint.Full_Source_Name (Source_File);
               Lib_File         := Osint.Lib_File_Name
                 (Source_File, Source_Index);
               Full_Lib_File    := Osint.Full_Lib_File_Name (Lib_File);

               --  If this source has already been compiled, the executable is
               --  obsolete.

               if Is_In_Obsoleted (Source_File) then
                  Executable_Obsolete := True;
               end if;

               --  If the library file is an Ada library skip it

               if Full_Lib_File /= No_File
                 and then In_Ada_Lib_Dir (Full_Lib_File)
               then
                  Verbose_Msg
                    (Lib_File,
                     "is in an Ada library",
                     Prefix => "  ",
                     Minimum_Verbosity => Opt.High);

                  --  If the library file is a read-only library skip it, but
                  --  only if, when using project files, this library file is
                  --  in the right object directory (a read-only ALI file
                  --  in the object directory of a project being extended
                  --  should not be skipped).

               elsif Full_Lib_File /= No_File
                 and then not Check_Readonly_Files
                 and then Is_Readonly_Library (Full_Lib_File)
                 and then Is_In_Object_Directory (Source_File, Full_Lib_File)
               then
                  Verbose_Msg
                    (Lib_File,
                     "is a read-only library",
                     Prefix => "  ",
                     Minimum_Verbosity => Opt.High);

                  --  The source file that we are checking cannot be located

               elsif Full_Source_File = No_File then
                  Record_Failure (Source_File, Source_Unit, False);

                  --  Source and library files can be located but are internal
                  --  files

               elsif not Check_Readonly_Files
                 and then Full_Lib_File /= No_File
                 and then Is_Internal_File_Name (Source_File)
               then
                  if Force_Compilations then
                     Fail
                       ("not allowed to compile """ &
                        Get_Name_String (Source_File) &
                        """; use -a switch, or compile file with " &
                        """-gnatg"" switch");
                  end if;

                  Verbose_Msg
                    (Lib_File,
                     "is an internal library",
                     Prefix => "  ",
                     Minimum_Verbosity => Opt.High);

               --  The source file that we are checking can be located

               else
                  Arguments_Collected := False;

                  --  Don't waste any time if we have to recompile anyway

                  Obj_Stamp       := Empty_Time_Stamp;
                  Need_To_Compile := Force_Compilations;

                  if not Force_Compilations then
                     Read_Only :=
                       Full_Lib_File /= No_File
                       and then not Check_Readonly_Files
                       and then Is_Readonly_Library (Full_Lib_File);
                     Check (Source_File, Source_Index, Args, Lib_File,
                            Read_Only, ALI, Obj_File, Obj_Stamp);
                     Need_To_Compile := (ALI = No_ALI_Id);
                  end if;

                  if not Need_To_Compile then

                     --  The ALI file is up-to-date. Record its Id

                     Record_Good_ALI (ALI);

                     --  Record the time stamp of the most recent object file
                     --  as long as no (re)compilations are needed.

                     if First_Compiled_File = No_File
                       and then (Most_Recent_Obj_File = No_File
                                   or else Obj_Stamp > Most_Recent_Obj_Stamp)
                     then
                        Most_Recent_Obj_File  := Obj_File;
                        Most_Recent_Obj_Stamp := Obj_Stamp;
                     end if;

                  else
                     --  Do nothing if project of source is externally built

                     if not Arguments_Collected then
                        Collect_Arguments (Source_File, Source_Index, Args);
                     end if;

                     if Arguments_Project = No_Project
                       or else not Project_Tree.Projects.Table
                                     (Arguments_Project).Externally_Built
                     then
                        --  Is this the first file we have to compile?

                        if First_Compiled_File = No_File then
                           First_Compiled_File  := Full_Source_File;
                           Most_Recent_Obj_File := No_File;

                           if Do_Not_Execute then
                              exit Make_Loop;
                           end if;
                        end if;

                        if In_Place_Mode then

                           --  If the library file was not found, then save
                           --  the library file near the source file.

                           if Full_Lib_File = No_File then
                              Lib_File := Osint.Lib_File_Name
                                (Full_Source_File, Source_Index);

                              --  If the library file was found, then save the
                              --  library file in the same place.

                           else
                              Lib_File := Full_Lib_File;
                           end if;

                        end if;

                        --  Start the compilation and record it. We can do
                        --  this because there is at least one free process.

                        Collect_Arguments_And_Compile
                          (Source_File, Source_Index);

                        --  Make sure we could successfully start
                        --  the Compilation.

                        if Process_Created then
                           if Pid = Invalid_Pid then
                              Record_Failure (Full_Source_File, Source_Unit);
                           else
                              Add_Process
                                (Pid,
                                 Full_Source_File,
                                 Lib_File,
                                 Source_Unit,
                                 Mfile);
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end;
         end if;

         --  PHASE 2: Now check if we should wait for a compilation to
         --  finish. This is the case if all the available processes are
         --  busy compiling sources or there is nothing else to do
         --  (that is the Q is empty and there are no good ALIs to process).

         if Outstanding_Compiles = Max_Process
           or else (Empty_Q
                     and then not Good_ALI_Present
                     and then Outstanding_Compiles > 0)
         then
            Await_Compile
              (Full_Source_File, Lib_File, Source_Unit, Compilation_OK);

            if not Compilation_OK then
               Record_Failure (Full_Source_File, Source_Unit);
            end if;

            if Compilation_OK or else Keep_Going then

               --  Re-read the updated library file

               declare
                  Saved_Object_Consistency : constant Boolean :=
                                               Check_Object_Consistency;

               begin
                  --  If compilation was not OK, or if output is not an
                  --  object file and we don't do the bind step, don't check
                  --  for object consistency.

                  Check_Object_Consistency :=
                    Check_Object_Consistency
                    and Compilation_OK
                    and (Output_Is_Object or Do_Bind_Step);
                  Text := Read_Library_Info (Lib_File);

                  --  Restore Check_Object_Consistency to its initial value

                  Check_Object_Consistency := Saved_Object_Consistency;
               end;

               --  If an ALI file was generated by this compilation, scan
               --  the ALI file and record it.
               --  If the scan fails, a previous ali file is inconsistent with
               --  the unit just compiled.

               if Text /= null then
                  ALI :=
                    Scan_ALI (Lib_File, Text, Ignore_ED => False, Err => True);

                  if ALI = No_ALI_Id then

                     --  Record a failure only if not already done

                     if Compilation_OK then
                        Inform
                          (Lib_File,
                           "incompatible ALI file, please recompile");
                        Record_Failure (Full_Source_File, Source_Unit);
                     end if;
                  else
                     Free (Text);
                     Record_Good_ALI (ALI);
                  end if;

               --  If we could not read the ALI file that was just generated
               --  then there could be a problem reading either the ALI or the
               --  corresponding object file (if Check_Object_Consistency
               --  is set Read_Library_Info checks that the time stamp of the
               --  object file is more recent than that of the ALI). For an
               --  example of problems caught by this test see [6625-009].
               --  However, we record a failure only if not already done.

               else
                  if Compilation_OK and not Syntax_Only then
                     Inform
                       (Lib_File,
                        "WARNING: ALI or object file not found after compile");
                     Record_Failure (Full_Source_File, Source_Unit);
                  end if;
               end if;
            end if;
         end if;

         --  PHASE 3: Check if we recorded good ALI files. If yes process
         --  them now in the order in which they have been recorded. There
         --  are two occasions in which we record good ali files. The first is
         --  in phase 1 when, after scanning an existing ALI file we realize
         --  it is up-to-date, the second instance is after a successful
         --  compilation.

         while Good_ALI_Present loop
            ALI := Get_Next_Good_ALI;

            declare
               Source_Index : Int := Unit_Index_Of (ALIs.Table (ALI).Afile);

            begin
               --  If we are processing the library file corresponding to the
               --  main source file check if this source can be a main unit.

               if ALIs.Table (ALI).Sfile = Main_Source and then
                 Source_Index = Main_Index
               then
                  Main_Unit := ALIs.Table (ALI).Main_Program /= None;
               end if;

               --  The following adds the standard library (s-stalib) to the
               --  list of files to be handled by gnatmake: this file and any
               --  files it depends on are always included in every bind,
               --  even if they are not in the explicit dependency list.
               --  Of course, it is not added if Suppress_Standard_Library
               --  is True.

               --  However, to avoid annoying output about s-stalib.ali being
               --  read only, when "-v" is used, we add the standard library
               --  only when "-a" is used.

               if Need_To_Check_Standard_Library then
                  Check_Standard_Library;
               end if;

               --  Now insert in the Q the unmarked source files (i.e. those
               --  which have never been inserted in the Q and hence never
               --  considered). Only do that if Unique_Compile is False.

               if not Unique_Compile then
                  for J in
                    ALIs.Table (ALI).First_Unit .. ALIs.Table (ALI).Last_Unit
                  loop
                     for K in
                       Units.Table (J).First_With .. Units.Table (J).Last_With
                     loop
                        Sfile := Withs.Table (K).Sfile;
                        Add_Dependency (ALIs.Table (ALI).Sfile, Sfile);

                        if Is_In_Obsoleted (Sfile) then
                           Executable_Obsolete := True;
                        end if;

                        if Sfile = No_File then
                           Debug_Msg
                             ("Skipping generic:", Withs.Table (K).Uname);

                        else
                           Source_Index :=
                             Unit_Index_Of (Withs.Table (K).Afile);

                           if Is_Marked (Sfile, Source_Index) then
                              Debug_Msg ("Skipping marked file:", Sfile);

                           elsif not Check_Readonly_Files
                             and then Is_Internal_File_Name (Sfile)
                           then
                              Debug_Msg ("Skipping internal file:", Sfile);

                           else
                              Insert_Q
                                (Sfile, Withs.Table (K).Uname, Source_Index);
                              Mark (Sfile, Source_Index);
                           end if;
                        end if;
                     end loop;
                  end loop;
               end if;
            end;
         end loop;

         if Display_Compilation_Progress then
            Write_Str ("completed ");
            Write_Int (Int (Q_Front));
            Write_Str (" out of ");
            Write_Int (Int (Q.Last));
            Write_Str (" (");
            Write_Int (Int ((Q_Front * 100) / (Q.Last - Q.First)));
            Write_Str ("%)...");
            Write_Eol;
         end if;
      end loop Make_Loop;

      Compilation_Failures := Bad_Compilation_Count;

      --  Compilation is finished

      --  Delete any temporary configuration pragma file

      Delete_Temp_Config_Files;

   end Compile_Sources;

   -----------------------------------
   -- Compute_All_Imported_Projects --
   -----------------------------------

   procedure Compute_All_Imported_Projects (Project : Project_Id) is
      procedure Add_To_List (Prj : Project_Id);
      --  Add a project to the list All_Imported_Projects of project Project

      procedure Recursive_Add_Imported (Project : Project_Id);
      --  Recursively add the projects imported by project Project, but not
      --  those that are extended.

      -----------------
      -- Add_To_List --
      -----------------

      procedure Add_To_List (Prj : Project_Id) is
         Element : constant Project_Element :=
           (Prj, Project_Tree.Projects.Table (Project).All_Imported_Projects);
         List : Project_List;
      begin
         Project_List_Table.Increment_Last (Project_Tree.Project_Lists);
         List := Project_List_Table.Last (Project_Tree.Project_Lists);
         Project_Tree.Project_Lists.Table (List) := Element;
         Project_Tree.Projects.Table (Project).All_Imported_Projects := List;
      end Add_To_List;

      ----------------------------
      -- Recursive_Add_Imported --
      ----------------------------

      procedure Recursive_Add_Imported (Project : Project_Id) is
         List    : Project_List;
         Element : Project_Element;
         Prj     : Project_Id;

      begin
         if Project /= No_Project then

            --  For all the imported projects

            List := Project_Tree.Projects.Table (Project).Imported_Projects;
            while List /= Empty_Project_List loop
               Element := Project_Tree.Project_Lists.Table (List);
               Prj := Element.Project;

               --  Get the ultimate extending project

               while
                 Project_Tree.Projects.Table (Prj).Extended_By /= No_Project
               loop
                  Prj := Project_Tree.Projects.Table (Prj).Extended_By;
               end loop;

               --  If project has not yet been visited, add to list and recurse

               if not Project_Tree.Projects.Table (Prj).Seen then
                  Project_Tree.Projects.Table (Prj).Seen := True;
                  Add_To_List (Prj);
                  Recursive_Add_Imported (Prj);
               end if;

               List := Element.Next;
            end loop;

            --  Recurse on projects being imported, if any

            Recursive_Add_Imported
              (Project_Tree.Projects.Table (Project).Extends);
         end if;
      end Recursive_Add_Imported;

   begin
      --  Reset the Seen flag for all projects

      for Index in 1 .. Project_Table.Last (Project_Tree.Projects) loop
         Project_Tree.Projects.Table (Index).Seen := False;
      end loop;

      --  Make sure the list is empty

      Project_Tree.Projects.Table (Project).All_Imported_Projects :=
        Empty_Project_List;

      --  Add to the list all projects imported directly or indirectly

      Recursive_Add_Imported (Project);
   end Compute_All_Imported_Projects;

   ----------------------------------
   -- Configuration_Pragmas_Switch --
   ----------------------------------

   function Configuration_Pragmas_Switch
     (For_Project : Project_Id) return Argument_List
   is
      The_Packages : Package_Id;
      Gnatmake     : Package_Id;
      Compiler     : Package_Id;

      Global_Attribute : Variable_Value := Nil_Variable_Value;
      Local_Attribute  : Variable_Value := Nil_Variable_Value;

      Global_Attribute_Present : Boolean := False;
      Local_Attribute_Present  : Boolean := False;

      Result : Argument_List (1 .. 3);
      Last   : Natural := 0;

      function Absolute_Path
        (Path    : Name_Id;
         Project : Project_Id) return String;
      --  Returns an absolute path for a configuration pragmas file

      -------------------
      -- Absolute_Path --
      -------------------

      function Absolute_Path
        (Path    : Name_Id;
         Project : Project_Id) return String
      is
      begin
         Get_Name_String (Path);

         declare
            Path_Name : constant String := Name_Buffer (1 .. Name_Len);

         begin
            if Is_Absolute_Path (Path_Name) then
               return Path_Name;

            else
               declare
                  Parent_Directory : constant String :=
                    Get_Name_String
                      (Project_Tree.Projects.Table
                           (Project).Directory);

               begin
                  if Parent_Directory (Parent_Directory'Last) =
                                                 Directory_Separator
                  then
                     return Parent_Directory & Path_Name;

                  else
                     return Parent_Directory & Directory_Separator & Path_Name;
                  end if;
               end;
            end if;
         end;
      end Absolute_Path;

   --  Start of processing for Configuration_Pragmas_Switch

   begin
      Prj.Env.Create_Config_Pragmas_File
        (For_Project, Main_Project, Project_Tree);

      if Project_Tree.Projects.Table
           (For_Project).Config_File_Name /= No_Name
      then
         Temporary_Config_File :=
           Project_Tree.Projects.Table (For_Project).Config_File_Temp;
         Last := 1;
         Result (1) :=
           new String'
                 ("-gnatec=" &
                  Get_Name_String
                    (Project_Tree.Projects.Table
                       (For_Project).Config_File_Name));

      else
         Temporary_Config_File := False;
      end if;

      --  Check for attribute Builder'Global_Configuration_Pragmas

      The_Packages := Project_Tree.Projects.Table
                        (Main_Project).Decl.Packages;
      Gnatmake :=
        Prj.Util.Value_Of
          (Name        => Name_Builder,
           In_Packages => The_Packages,
           In_Tree     => Project_Tree);

      if Gnatmake /= No_Package then
         Global_Attribute := Prj.Util.Value_Of
           (Variable_Name => Name_Global_Configuration_Pragmas,
            In_Variables  => Project_Tree.Packages.Table
                               (Gnatmake).Decl.Attributes,
            In_Tree       => Project_Tree);
         Global_Attribute_Present :=
           Global_Attribute /= Nil_Variable_Value
           and then Get_Name_String (Global_Attribute.Value) /= "";

         if Global_Attribute_Present then
            declare
               Path : constant String :=
                        Absolute_Path
                          (Global_Attribute.Value, Global_Attribute.Project);
            begin
               if not Is_Regular_File (Path) then
                  Make_Failed
                    ("cannot find configuration pragmas file ", Path);
               end if;

               Last := Last + 1;
               Result (Last) := new String'("-gnatec=" &  Path);
            end;
         end if;
      end if;

      --  Check for attribute Compiler'Local_Configuration_Pragmas

      The_Packages :=
        Project_Tree.Projects.Table (For_Project).Decl.Packages;
      Compiler :=
        Prj.Util.Value_Of
          (Name        => Name_Compiler,
           In_Packages => The_Packages,
           In_Tree     => Project_Tree);

      if Compiler /= No_Package then
         Local_Attribute := Prj.Util.Value_Of
           (Variable_Name => Name_Local_Configuration_Pragmas,
            In_Variables  => Project_Tree.Packages.Table
                               (Compiler).Decl.Attributes,
            In_Tree       => Project_Tree);
         Local_Attribute_Present :=
           Local_Attribute /= Nil_Variable_Value
           and then Get_Name_String (Local_Attribute.Value) /= "";

         if Local_Attribute_Present then
            declare
               Path : constant String :=
                        Absolute_Path
                          (Local_Attribute.Value, Local_Attribute.Project);
            begin
               if not Is_Regular_File (Path) then
                  Make_Failed
                    ("cannot find configuration pragmas file ", Path);
               end if;

               Last := Last + 1;
               Result (Last) := new String'("-gnatec=" & Path);
            end;
         end if;
      end if;

      return Result (1 .. Last);
   end Configuration_Pragmas_Switch;

   ---------------
   -- Debug_Msg --
   ---------------

   procedure Debug_Msg (S : String; N : Name_Id) is
   begin
      if Debug.Debug_Flag_W then
         Write_Str ("   ... ");
         Write_Str (S);
         Write_Str (" ");
         Write_Name (N);
         Write_Eol;
      end if;
   end Debug_Msg;

   ---------------------------
   -- Delete_All_Temp_Files --
   ---------------------------

   procedure Delete_All_Temp_Files is
   begin
      if Gnatmake_Called and not Debug.Debug_Flag_N then
         Delete_Mapping_Files;
         Delete_Temp_Config_Files;
         Prj.Env.Delete_All_Path_Files (Project_Tree);
      end if;
   end Delete_All_Temp_Files;

   --------------------------
   -- Delete_Mapping_Files --
   --------------------------

   procedure Delete_Mapping_Files is
      Success : Boolean;
   begin
      if not Debug.Debug_Flag_N then
         if The_Mapping_File_Names /= null then
            for Project in The_Mapping_File_Names'Range (1) loop
               for Index in 1 .. Last_Mapping_File_Names (Project) loop
                  Delete_File
                    (Name => Get_Name_String
                               (The_Mapping_File_Names (Project, Index)),
                     Success => Success);
               end loop;
            end loop;
         end if;
      end if;
   end Delete_Mapping_Files;

   ------------------------------
   -- Delete_Temp_Config_Files --
   ------------------------------

   procedure Delete_Temp_Config_Files is
      Success : Boolean;
   begin
      if (not Debug.Debug_Flag_N) and Main_Project /= No_Project then
         for Project in Project_Table.First ..
                        Project_Table.Last (Project_Tree.Projects)
         loop
            if
              Project_Tree.Projects.Table (Project).Config_File_Temp
            then
               if Verbose_Mode then
                  Write_Str ("Deleting temp configuration file """);
                  Write_Str (Get_Name_String
                               (Project_Tree.Projects.Table
                                  (Project).Config_File_Name));
                  Write_Line ("""");
               end if;

               Delete_File
                 (Name    => Get_Name_String
                               (Project_Tree.Projects.Table
                                  (Project).Config_File_Name),
                  Success => Success);

               --  Make sure that we don't have a config file for this
               --  project, in case when there are several mains.
               --  In this case, we will recreate another config file:
               --  we cannot reuse the one that we just deleted!

               Project_Tree.Projects.Table (Project).
                 Config_Checked := False;
               Project_Tree.Projects.Table (Project).
                 Config_File_Name := No_Name;
               Project_Tree.Projects.Table (Project).
                 Config_File_Temp := False;
            end if;
         end loop;
      end if;
   end Delete_Temp_Config_Files;

   -------------
   -- Display --
   -------------

   procedure Display (Program : String; Args : Argument_List) is
   begin
      pragma Assert (Args'First = 1);

      if Display_Executed_Programs then
         Write_Str (Program);

         for J in Args'Range loop

            --  Never display -gnatez

            if Args (J).all /= "-gnatez" then

               --  Do not display the mapping file argument automatically
               --  created when using a project file.

               if Main_Project = No_Project
                 or else Debug.Debug_Flag_N
                 or else Args (J)'Length < 8
                 or else
                   Args (J) (Args (J)'First .. Args (J)'First + 6) /= "-gnatem"
               then
                  --  When -dn is not specified, do not display the config
                  --  pragmas switch (-gnatec) for the temporary file created
                  --  by the project manager (always the first -gnatec switch).
                  --  Reset Temporary_Config_File to False so that the eventual
                  --  other -gnatec switches will be displayed.

                  if (not Debug.Debug_Flag_N)
                    and then Temporary_Config_File
                    and then Args (J)'Length > 7
                    and then Args (J) (Args (J)'First .. Args (J)'First + 6)
                    = "-gnatec"
                  then
                     Temporary_Config_File := False;

                     --  Do not display the -F=mapping_file switch for
                     --  gnatbind, if -dn is not specified.

                  elsif Debug.Debug_Flag_N
                    or else Args (J)'Length < 4
                    or else
                      Args (J) (Args (J)'First .. Args (J)'First + 2) /= "-F="
                  then
                     Write_Str (" ");
                     Write_Str (Args (J).all);
                  end if;
               end if;
            end if;
         end loop;

         Write_Eol;
      end if;
   end Display;

   ----------------------
   -- Display_Commands --
   ----------------------

   procedure Display_Commands (Display : Boolean := True) is
   begin
      Display_Executed_Programs := Display;
   end Display_Commands;

   -------------
   -- Empty_Q --
   -------------

   function Empty_Q return Boolean is
   begin
      if Debug.Debug_Flag_P then
         Write_Str ("   Q := [");

         for J in Q_Front .. Q.Last - 1 loop
            Write_Str (" ");
            Write_Name (Q.Table (J).File);
            Write_Eol;
            Write_Str ("         ");
         end loop;

         Write_Str ("]");
         Write_Eol;
      end if;

      return Q_Front >= Q.Last;
   end Empty_Q;

   --------------------------
   -- Enter_Into_Obsoleted --
   --------------------------

   procedure Enter_Into_Obsoleted (F : Name_Id) is
      Name  : constant String := Get_Name_String (F);
      First : Natural := Name'Last;
      F2    : Name_Id := F;

   begin
      while First > Name'First
        and then Name (First - 1) /= Directory_Separator
        and then Name (First - 1) /= '/'
      loop
         First := First - 1;
      end loop;

      if First /= Name'First then
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Name (First .. Name'Last));
         F2 := Name_Find;
      end if;

      Debug_Msg ("New entry in Obsoleted table:", F2);
      Obsoleted.Set (F2, True);
   end Enter_Into_Obsoleted;

   ---------------------
   -- Extract_Failure --
   ---------------------

   procedure Extract_Failure
     (File  : out File_Name_Type;
      Unit  : out Unit_Name_Type;
      Found : out Boolean)
   is
   begin
      File  := Bad_Compilation.Table (Bad_Compilation.Last).File;
      Unit  := Bad_Compilation.Table (Bad_Compilation.Last).Unit;
      Found := Bad_Compilation.Table (Bad_Compilation.Last).Found;
      Bad_Compilation.Decrement_Last;
   end Extract_Failure;

   --------------------
   -- Extract_From_Q --
   --------------------

   procedure Extract_From_Q
     (Source_File  : out File_Name_Type;
      Source_Unit  : out Unit_Name_Type;
      Source_Index : out Int)
   is
      File  : constant File_Name_Type := Q.Table (Q_Front).File;
      Unit  : constant Unit_Name_Type := Q.Table (Q_Front).Unit;
      Index : constant Int            := Q.Table (Q_Front).Index;

   begin
      if Debug.Debug_Flag_Q then
         Write_Str ("   Q := Q - [ ");
         Write_Name (File);

         if Index /= 0 then
            Write_Str (", ");
            Write_Int (Index);
         end if;

         Write_Str (" ]");
         Write_Eol;
      end if;

      Q_Front := Q_Front + 1;
      Source_File  := File;
      Source_Unit  := Unit;
      Source_Index := Index;
   end Extract_From_Q;

   --------------
   -- Gnatmake --
   --------------

   procedure Gnatmake is
      Main_Source_File : File_Name_Type;
      --  The source file containing the main compilation unit

      Compilation_Failures : Natural;

      Total_Compilation_Failures : Natural := 0;

      Is_Main_Unit : Boolean;
      --  Set to True by Compile_Sources if the Main_Source_File can be a
      --  main unit.

      Main_ALI_File : File_Name_Type;
      --  The ali file corresponding to Main_Source_File

      Executable : File_Name_Type := No_File;
      --  The file name of an executable

      Non_Std_Executable : Boolean := False;
      --  Non_Std_Executable is set to True when there is a possibility
      --  that the linker will not choose the correct executable file name.

      Current_Work_Dir : constant String_Access :=
                                    new String'(Get_Current_Dir);
      --  The current working directory, used to modify some relative path
      --  switches on the command line when a project file is used.

      Current_Main_Index : Int := 0;
      --  If not zero, the index of the current main unit in its source file

      There_Are_Stand_Alone_Libraries : Boolean := False;
      --  Set to True when there are Stand-Alone Libraries, so that gnatbind
      --  is invoked with the -F switch to force checking of elaboration flags.

      Mapping_Path : Name_Id := No_Name;
      --  The path name of the mapping file

      Discard : Boolean;

      procedure Check_Mains;
      --  Check that the main subprograms do exist and that they all
      --  belong to the same project file.

      procedure Create_Binder_Mapping_File
        (Args : in out Argument_List; Last_Arg : in out Natural);
      --  Create a binder mapping file and add the necessary switch

      -----------------
      -- Check_Mains --
      -----------------

      procedure Check_Mains is
         Real_Main_Project : Project_Id := No_Project;
         --  The project of the first main

         Proj              : Project_Id := No_Project;
         --  The project of the current main

         Data              : Project_Data;

         Real_Path         : String_Access;

      begin
         Mains.Reset;

         --  Check each main

         loop
            declare
               Main      : constant String := Mains.Next_Main;
               --  The name specified on the command line may include
               --  directory information.

               File_Name : constant String := Base_Name (Main);
               --  The simple file name of the current main main

            begin
               exit when Main = "";

               --  Get the project of the current main

               Proj := Prj.Env.Project_Of
                         (File_Name, Main_Project, Project_Tree);

               --  Fail if the current main is not a source of a
               --  project.

               if Proj = No_Project then
                  Make_Failed
                    ("""" & Main &
                     """ is not a source of any project");

               else
                  --  If there is directory information, check that
                  --  the source exists and, if it does, that the path
                  --  is the actual path of a source of a project.

                  if Main /= File_Name then
                     Data :=
                       Project_Tree.Projects.Table (Main_Project);

                     Real_Path :=
                       Locate_Regular_File
                         (Main &
                          Get_Name_String
                            (Data.Naming.Ada_Body_Suffix),
                          "");
                     if Real_Path = null then
                        Real_Path :=
                          Locate_Regular_File
                            (Main &
                             Get_Name_String
                               (Data.Naming.Ada_Spec_Suffix),
                             "");
                     end if;

                     if Real_Path = null then
                        Real_Path :=
                          Locate_Regular_File (Main, "");
                     end if;

                     --  Fail if the file cannot be found

                     if Real_Path = null then
                        Make_Failed
                          ("file """ & Main & """ does not exist");
                     end if;

                     declare
                        Project_Path : constant String :=
                                         Prj.Env.File_Name_Of_Library_Unit_Body
                                           (Name              => File_Name,
                                            Project           => Main_Project,
                                            In_Tree           => Project_Tree,
                                            Main_Project_Only => False,
                                            Full_Path         => True);
                        Normed_Path  : constant String :=
                                         Normalize_Pathname
                                           (Real_Path.all,
                                            Case_Sensitive => False);
                        Proj_Path    : constant String :=
                                         Normalize_Pathname
                                           (Project_Path,
                                            Case_Sensitive => False);

                     begin
                        Free (Real_Path);

                        --  Fail if it is not the correct path

                        if Normed_Path /= Proj_Path then
                           if Verbose_Mode then
                              Write_Str (Normed_Path);
                              Write_Str (" /= ");
                              Write_Line (Proj_Path);
                           end if;

                           Make_Failed
                             ("""" & Main &
                              """ is not a source of any project");
                        end if;
                     end;
                  end if;

                  if not Unique_Compile then

                     --  Record the project, if it is the first main

                     if Real_Main_Project = No_Project then
                        Real_Main_Project := Proj;

                     elsif Proj /= Real_Main_Project then

                        --  Fail, as the current main is not a source
                        --  of the same project as the first main.

                        Make_Failed
                          ("""" & Main &
                           """ is not a source of project " &
                           Get_Name_String
                             (Project_Tree.Projects.Table
                                (Real_Main_Project).Name));
                     end if;
                  end if;
               end if;

               --  If -u and -U are not used, we may have mains that
               --  are sources of a project that is not the one
               --  specified with switch -P.

               if not Unique_Compile then
                  Main_Project := Real_Main_Project;
               end if;
            end;
         end loop;
      end Check_Mains;

      --------------------------------
      -- Create_Binder_Mapping_File --
      --------------------------------

      procedure Create_Binder_Mapping_File
        (Args : in out Argument_List; Last_Arg : in out Natural)
      is
         Mapping_FD : File_Descriptor := Invalid_FD;
         --  A File Descriptor for an eventual mapping file

         ALI_Unit     : Name_Id := No_Name;
         --  The unit name of an ALI file

         ALI_Name     : Name_Id := No_Name;
         --  The file name of the ALI file

         ALI_Project  : Project_Id := No_Project;
         --  The project of the ALI file

         Bytes        : Integer;
         OK           : Boolean := True;

         Status       : Boolean;
         --  For call to Close

      begin
         Tempdir.Create_Temp_File (Mapping_FD, Mapping_Path);

         if Mapping_FD /= Invalid_FD then

            --  Traverse all units

            for J in Unit_Table.First ..
                     Unit_Table.Last (Project_Tree.Units)
            loop
               declare
                  Unit : constant Unit_Data :=
                           Project_Tree.Units.Table (J);
               begin
                  if Unit.Name /= No_Name then

                     --  If there is a body, put it in the mapping

                     if Unit.File_Names (Body_Part).Name /= No_Name
                       and then Unit.File_Names (Body_Part).Project
                       /= No_Project
                     then
                        Get_Name_String (Unit.Name);
                        Name_Buffer
                          (Name_Len + 1 .. Name_Len + 2) := "%b";
                        Name_Len := Name_Len + 2;
                        ALI_Unit := Name_Find;
                        ALI_Name :=
                          Lib_File_Name
                            (Unit.File_Names (Body_Part).Name);
                        ALI_Project :=
                          Unit.File_Names (Body_Part).Project;

                        --  Otherwise, if there is a spec, put it
                        --  in the mapping.

                     elsif Unit.File_Names (Specification).Name
                       /= No_Name
                       and then Unit.File_Names
                         (Specification).Project
                         /= No_Project
                     then
                        Get_Name_String (Unit.Name);
                        Name_Buffer
                          (Name_Len + 1 .. Name_Len + 2) := "%s";
                        Name_Len := Name_Len + 2;
                        ALI_Unit := Name_Find;
                        ALI_Name := Lib_File_Name
                          (Unit.File_Names (Specification).Name);
                        ALI_Project :=
                          Unit.File_Names (Specification).Project;

                     else
                        ALI_Name := No_Name;
                     end if;

                     --  If we have something to put in the mapping
                     --  then we do it now. However, if the project
                     --  is extended, we don't put anything in the
                     --  mapping file, because we do not know where
                     --  the ALI file is: it might be in the ext-
                     --  ended project obj dir as well as in the
                     --  extending project obj dir.

                     if ALI_Name /= No_Name
                       and then
                         Project_Tree.Projects.Table
                           (ALI_Project).Extended_By = No_Project
                       and then
                         Project_Tree.Projects.Table
                           (ALI_Project).Extends = No_Project
                     then
                        --  First line is the unit name

                        Get_Name_String (ALI_Unit);
                        Name_Len := Name_Len + 1;
                        Name_Buffer (Name_Len) := ASCII.LF;
                        Bytes :=
                          Write
                            (Mapping_FD,
                             Name_Buffer (1)'Address,
                             Name_Len);
                        OK := Bytes = Name_Len;

                        exit when not OK;

                        --  Second line it the ALI file name

                        Get_Name_String (ALI_Name);
                        Name_Len := Name_Len + 1;
                        Name_Buffer (Name_Len) := ASCII.LF;
                        Bytes :=
                          Write
                            (Mapping_FD,
                             Name_Buffer (1)'Address,
                             Name_Len);
                        OK := Bytes = Name_Len;

                        exit when not OK;

                        --  Third line it the ALI path name, concatenation
                        --  of either the library directory or the object
                        --  directory with the ALI file name.

                        declare
                           ALI : constant String :=
                                   Get_Name_String (ALI_Name);
                           PD  : Project_Data renames
                                   Project_Tree.Projects.Table (ALI_Project);

                        begin
                           --  For library projects, use the library directory,
                           --  for other projects, use the object directory.

                           if PD.Library then
                              Get_Name_String (PD.Library_Dir);
                           else
                              Get_Name_String (PD.Object_Directory);
                           end if;

                           if Name_Buffer (Name_Len) /=
                             Directory_Separator
                           then
                              Name_Len := Name_Len + 1;
                              Name_Buffer (Name_Len) :=
                                Directory_Separator;
                           end if;

                           Name_Buffer
                             (Name_Len + 1 ..
                                Name_Len + ALI'Length) := ALI;
                           Name_Len :=
                             Name_Len + ALI'Length + 1;
                           Name_Buffer (Name_Len) := ASCII.LF;
                           Bytes :=
                             Write
                               (Mapping_FD,
                                Name_Buffer (1)'Address,
                                Name_Len);
                           OK := Bytes = Name_Len;
                        end;

                        --  If OK is False, it means we were unable
                        --  to write a line. No point in continuing
                        --  with the other units.

                        exit when not OK;
                     end if;
                  end if;
               end;
            end loop;

            Close (Mapping_FD, Status);

            OK := OK and Status;

            --  If the creation of the mapping file was successful,
            --  we add the switch to the arguments of gnatbind.

            if OK then
               Last_Arg := Last_Arg + 1;
               Args (Last_Arg) :=
                 new String'("-F=" & Get_Name_String (Mapping_Path));
            end if;
         end if;
      end Create_Binder_Mapping_File;

   --  Start of processing for Gnatmake

   --  This body is very long, should be broken down ???

   begin
      Gnatmake_Called := True;

      Install_Int_Handler (Sigint_Intercepted'Access);

      Do_Compile_Step := True;
      Do_Bind_Step    := True;
      Do_Link_Step    := True;

      Obsoleted.Reset;

      Make.Initialize;

      Bind_Shared := No_Shared_Switch'Access;
      Link_With_Shared_Libgcc := No_Shared_Libgcc_Switch'Access;

      Failed_Links.Set_Last (0);
      Successful_Links.Set_Last (0);

      if Hostparm.Java_VM then
         Gcc := new String'("jgnat");
         Gnatbind := new String'("jgnatbind");
         Gnatlink := new String'("jgnatlink");

         --  Do not check for an object file (".o") when compiling to
         --  Java bytecode since ".class" files are generated instead.

         Check_Object_Consistency := False;
      end if;

      --  Special case when switch -B was specified

      if Build_Bind_And_Link_Full_Project then

         --  When switch -B is specified, there must be a project file

         if Main_Project = No_Project then
            Make_Failed ("-B cannot be used without a project file");

         --  No main program may be specified on the command line

         elsif Osint.Number_Of_Files /= 0 then
            Make_Failed ("-B cannot be used with a main specified on " &
                         "the command line");

         --  And the project file cannot be a library project file

         elsif Project_Tree.Projects.Table (Main_Project).Library then
            Make_Failed ("-B cannot be used for a library project file");

         else
            Insert_Project_Sources
              (The_Project  => Main_Project,
               All_Projects => Unique_Compile_All_Projects,
               Into_Q       => False);

            --  If there are no sources to compile, we fail

            if Osint.Number_Of_Files = 0 then
               Make_Failed ("no sources to compile");
            end if;

            --  Specify -n for gnatbind and add the ALI files of all the
            --  sources, except the one which is a fake main subprogram:
            --  this is the one for the binder generated file and it will be
            --  transmitted to gnatlink. These sources are those that are
            --  in the queue.

            Add_Switch ("-n", Binder, And_Save => True);

            for J in Q.First .. Q.Last - 1 loop
               Add_Switch
                 (Get_Name_String
                    (Lib_File_Name (Q.Table (J).File)),
                  Binder, And_Save => True);
            end loop;
         end if;

      elsif Main_Index /= 0 and then Osint.Number_Of_Files > 1 then
         Make_Failed ("cannot specify several mains with a multi-unit index");

      elsif Main_Project /= No_Project then

         --  If the main project file is a library project file, main(s)
         --  cannot be specified on the command line.

         if Osint.Number_Of_Files /= 0 then
            if Project_Tree.Projects.Table (Main_Project).Library
              and then not Unique_Compile
              and then ((not Make_Steps) or else Bind_Only or else Link_Only)
            then
               Make_Failed ("cannot specify a main program " &
                            "on the command line for a library project file");

            else
               --  Check that each main on the command line is a source of a
               --  project file and, if there are several mains, each of them
               --  is a source of the same project file.

               Check_Mains;
            end if;

         --  If no mains have been specified on the command line,
         --  and we are using a project file, we either find the main(s)
         --  in the attribute Main of the main project, or we put all
         --  the sources of the project file as mains.

         else
            if Main_Index /= 0 then
               Make_Failed ("cannot specify a multi-unit index but no main " &
                            "on the command line");
            end if;

            declare
               Value : String_List_Id :=
                 Project_Tree.Projects.Table (Main_Project).Mains;

            begin
               --  The attribute Main is an empty list or not specified,
               --  or else gnatmake was invoked with the switch "-u".

               if Value = Prj.Nil_String or else Unique_Compile then

                  if (not Make_Steps) or else Compile_Only
                    or else not Project_Tree.Projects.Table
                                  (Main_Project).Library
                  then
                     --  First make sure that the binder and the linker
                     --  will not be invoked.

                     Do_Bind_Step := False;
                     Do_Link_Step := False;

                     --  Put all the sources in the queue

                     Insert_Project_Sources
                       (The_Project  => Main_Project,
                        All_Projects => Unique_Compile_All_Projects,
                        Into_Q       => False);

                     --  If no sources to compile, then there is nothing to do

                     if Osint.Number_Of_Files = 0 then
                        if not Debug.Debug_Flag_N then
                           Delete_Mapping_Files;
                           Prj.Env.Delete_All_Path_Files (Project_Tree);
                        end if;

                        if not Quiet_Output then
                           Osint.Write_Program_Name;
                           Write_Line (": no sources to compile");
                        end if;

                        Exit_Program (E_Success);
                     end if;
                  end if;

               else
                  --  The attribute Main is not an empty list.
                  --  Put all the main subprograms in the list as if there
                  --  were specified on the command line. However, if attribute
                  --  Languages includes a language other than Ada, only
                  --  include the Ada mains; if there is no Ada main, compile
                  --  all the sources of the project.

                  declare
                     Data : constant Project_Data :=
                           Project_Tree.Projects.Table (Main_Project);

                     Languages : constant Variable_Value :=
                                   Prj.Util.Value_Of
                                    (Name_Languages,
                                     Data.Decl.Attributes,
                                     Project_Tree);

                     Current : String_List_Id;
                     Element : String_Element;

                     Foreign_Language  : Boolean := False;
                     At_Least_One_Main : Boolean := False;

                  begin
                     --  First, determine if there is a foreign language in
                     --  attribute Languages.

                     if not Languages.Default then
                        Current := Languages.Values;

                        Look_For_Foreign :
                        while Current /= Nil_String loop
                           Element := Project_Tree.String_Elements.
                                        Table (Current);
                           Get_Name_String (Element.Value);
                           To_Lower (Name_Buffer (1 .. Name_Len));

                           if Name_Buffer (1 .. Name_Len) /= "ada" then
                              Foreign_Language := True;
                              exit Look_For_Foreign;
                           end if;

                           Current := Element.Next;
                        end loop Look_For_Foreign;
                     end if;

                     --  Then, find all mains, or if there is a foreign
                     --  language, all the Ada mains.

                     while Value /= Prj.Nil_String loop
                        Get_Name_String
                          (Project_Tree.String_Elements.Table
                             (Value).Value);

                        --  To know if a main is an Ada main, get its project.
                        --  It should be the project specified on the command
                        --  line.

                        if (not Foreign_Language) or else
                            Prj.Env.Project_Of
                              (Name_Buffer (1 .. Name_Len),
                               Main_Project,
                               Project_Tree) =
                             Main_Project
                        then
                           At_Least_One_Main := True;
                           Osint.Add_File
                             (Get_Name_String
                                (Project_Tree.String_Elements.Table
                                   (Value).Value),
                              Index =>
                                Project_Tree.String_Elements.Table
                                  (Value).Index);
                        end if;

                        Value := Project_Tree.String_Elements.Table
                                   (Value).Next;
                     end loop;

                     --  If we did not get any main, it means that all mains
                     --  in attribute Mains are in a foreign language and -B
                     --  was not specified to gnatmake; so, we fail.

                     if not At_Least_One_Main then
                        Make_Failed
                          ("no Ada mains; use -B to build foreign main");
                     end if;
                  end;

               end if;
            end;
         end if;
      end if;

      if Verbose_Mode then
         Write_Eol;
         Write_Str ("GNATMAKE ");
         Write_Str (Gnatvsn.Gnat_Version_String);
         Write_Eol;
         Write_Str ("Copyright 1995-2004 Free Software Foundation, Inc.");
         Write_Eol;
      end if;

      if Main_Project /= No_Project
        and then Project_Tree.Projects.Table
                   (Main_Project).Externally_Built
      then
         Make_Failed
           ("nothing to do for a main project that is externally built");
      end if;

      if Osint.Number_Of_Files = 0 then
         if Main_Project /= No_Project
           and then Project_Tree.Projects.Table (Main_Project).Library
         then
            if Do_Bind_Step
              and then not Project_Tree.Projects.Table
                             (Main_Project).Standalone_Library
            then
               Make_Failed ("only stand-alone libraries may be bound");
            end if;

            --  Add the default search directories to be able to find libgnat

            Osint.Add_Default_Search_Dirs;

            --  And bind and or link the library

            MLib.Prj.Build_Library
              (For_Project   => Main_Project,
               In_Tree       => Project_Tree,
               Gnatbind      => Gnatbind.all,
               Gnatbind_Path => Gnatbind_Path,
               Gcc           => Gcc.all,
               Gcc_Path      => Gcc_Path,
               Bind          => Bind_Only,
               Link          => Link_Only);
            Exit_Program (E_Success);

         else
            --  Output usage information if no files to compile

            Usage;
            Exit_Program (E_Fatal);
         end if;
      end if;

      --  If -M was specified, behave as if -n was specified

      if List_Dependencies then
         Do_Not_Execute := True;
      end if;

      --  Note that Osint.Next_Main_Source will always return the (possibly
      --  abbreviated file) without any directory information.

      Main_Source_File := Next_Main_Source;

      if Current_File_Index /= No_Index then
         Main_Index := Current_File_Index;
      end if;

      Add_Switch ("-I-", Binder, And_Save => True);
      Add_Switch ("-I-", Compiler, And_Save => True);

      if Main_Project = No_Project then
         if Look_In_Primary_Dir then

            Add_Switch
              ("-I" &
               Normalize_Directory_Name
               (Get_Primary_Src_Search_Directory.all).all,
               Compiler, Append_Switch => False,
               And_Save => False);

            Add_Switch ("-aO" & Normalized_CWD,
                        Binder,
                        Append_Switch => False,
                        And_Save => False);
         end if;

      else
         --  If we use a project file, we have already checked that a main
         --  specified on the command line with directory information has the
         --  path name corresponding to a correct source in the project tree.
         --  So, we don't need the directory information to be taken into
         --  account by Find_File, and in fact it may lead to take the wrong
         --  sources for other compilation units, when there are extending
         --  projects.

         Look_In_Primary_Dir := False;
      end if;

      --  If the user wants a program without a main subprogram, add the
      --  appropriate switch to the binder.

      if No_Main_Subprogram then
         Add_Switch ("-z", Binder, And_Save => True);
      end if;

      if Main_Project /= No_Project then

         if Project_Tree.Projects.Table
              (Main_Project).Object_Directory /= No_Name
         then
            --  Change current directory to object directory of main project

            Project_Object_Directory := No_Project;
            Change_To_Object_Directory (Main_Project);
         end if;

         --  Source file lookups should be cached for efficiency.
         --  Source files are not supposed to change.

         Osint.Source_File_Data (Cache => True);

         --  Find the file name of the (first) main unit

         declare
            Main_Source_File_Name : constant String :=
                                      Get_Name_String (Main_Source_File);
            Main_Unit_File_Name   : constant String :=
                                      Prj.Env.File_Name_Of_Library_Unit_Body
                                        (Name    => Main_Source_File_Name,
                                         Project => Main_Project,
                                         In_Tree => Project_Tree,
                                         Main_Project_Only =>
                                           not Unique_Compile);

            The_Packages : constant Package_Id :=
              Project_Tree.Projects.Table (Main_Project).Decl.Packages;

            Builder_Package : constant Prj.Package_Id :=
                         Prj.Util.Value_Of
                           (Name        => Name_Builder,
                            In_Packages => The_Packages,
                            In_Tree     => Project_Tree);

            Binder_Package : constant Prj.Package_Id :=
                         Prj.Util.Value_Of
                           (Name        => Name_Binder,
                            In_Packages => The_Packages,
                            In_Tree     => Project_Tree);

            Linker_Package : constant Prj.Package_Id :=
                         Prj.Util.Value_Of
                           (Name        => Name_Linker,
                            In_Packages => The_Packages,
                            In_Tree     => Project_Tree);

         begin
            --  We fail if we cannot find the main source file

            if Main_Unit_File_Name = "" then
               Make_Failed ('"' & Main_Source_File_Name,
                            """ is not a unit of project ",
                            Project_File_Name.all & ".");
            else
               --  Remove any directory information from the main
               --  source file name.

               declare
                  Pos : Natural := Main_Unit_File_Name'Last;

               begin
                  loop
                     exit when Pos < Main_Unit_File_Name'First or else
                       Main_Unit_File_Name (Pos) = Directory_Separator;
                     Pos := Pos - 1;
                  end loop;

                  Name_Len := Main_Unit_File_Name'Last - Pos;

                  Name_Buffer (1 .. Name_Len) :=
                    Main_Unit_File_Name
                    (Pos + 1 .. Main_Unit_File_Name'Last);

                  Main_Source_File := Name_Find;

                  --  We only output the main source file if there is only one

                  if Verbose_Mode and then Osint.Number_Of_Files = 1 then
                     Write_Str ("Main source file: """);
                     Write_Str (Main_Unit_File_Name
                                (Pos + 1 .. Main_Unit_File_Name'Last));
                     Write_Line (""".");
                  end if;
               end;
            end if;

            --  If there is a package Builder in the main project file, add
            --  the switches from it.

            if Builder_Package /= No_Package then

               --  If there is only one main, we attempt to get the gnatmake
               --  switches for this main (if any). If there are no specific
               --  switch for this particular main, get the general gnatmake
               --  switches (if any).

               if Osint.Number_Of_Files = 1 then
                  if Verbose_Mode then
                     Write_Str ("Adding gnatmake switches for """);
                     Write_Str (Main_Unit_File_Name);
                     Write_Line (""".");
                  end if;

                  Add_Switches
                    (File_Name   => Main_Unit_File_Name,
                     Index       => Main_Index,
                     The_Package => Builder_Package,
                     Program     => None);

               else
                  --  If there are several mains, we always get the general
                  --  gnatmake switches (if any).

                  --  Warn the user, if necessary, so that he is not surprized
                  --  that specific switches are not taken into account.

                  declare
                     Defaults : constant Variable_Value :=
                       Prj.Util.Value_Of
                         (Name                    => Name_Ada,
                          Index                   => 0,
                          Attribute_Or_Array_Name => Name_Default_Switches,
                          In_Package              => Builder_Package,
                          In_Tree                 => Project_Tree);

                     Switches : constant Array_Element_Id :=
                          Prj.Util.Value_Of
                             (Name      => Name_Switches,
                              In_Arrays =>
                                Project_Tree.Packages.Table
                                  (Builder_Package).Decl.Arrays,
                              In_Tree   => Project_Tree);

                  begin
                     if Defaults /= Nil_Variable_Value then
                        if (not Quiet_Output)
                          and then Switches /= No_Array_Element
                        then
                           Write_Line
                             ("Warning: using Builder'Default_Switches" &
                              "(""Ada""), as there are several mains");
                        end if;

                        --  As there is never a source with name " ", we are
                        --  guaranteed to always get the general switches.

                        Add_Switches
                          (File_Name   => " ",
                           Index       => 0,
                           The_Package => Builder_Package,
                           Program     => None);

                     elsif (not Quiet_Output)
                       and then Switches /= No_Array_Element
                     then
                        Write_Line
                          ("Warning: using no switches from package Builder," &
                           " as there are several mains");
                     end if;
                  end;
               end if;
            end if;

            Osint.Add_Default_Search_Dirs;

            --  Record the current last switch index for table Binder_Switches
            --  and Linker_Switches, so that these tables may be reset before
            --  for each main, before adding swiches from the project file
            --  and from the command line.

            Last_Binder_Switch := Binder_Switches.Last;
            Last_Linker_Switch := Linker_Switches.Last;

            Check_Steps;

            --  Add binder switches from the project file for the first main

            if Do_Bind_Step and Binder_Package /= No_Package then
               if Verbose_Mode then
                  Write_Str ("Adding binder switches for """);
                  Write_Str (Main_Unit_File_Name);
                  Write_Line (""".");
               end if;

               Add_Switches
                 (File_Name   => Main_Unit_File_Name,
                  Index       => Main_Index,
                  The_Package => Binder_Package,
                  Program     => Binder);
            end if;

            --  Add linker switches from the project file for the first main

            if Do_Link_Step and Linker_Package /= No_Package then
               if Verbose_Mode then
                  Write_Str ("Adding linker switches for""");
                  Write_Str (Main_Unit_File_Name);
                  Write_Line (""".");
               end if;

               Add_Switches
                 (File_Name   => Main_Unit_File_Name,
                  Index       => Main_Index,
                  The_Package => Linker_Package,
                  Program     => Linker);
            end if;
         end;
      end if;

      --  Get the target parameters, which are only needed for a couple of
      --  cases in gnatmake. Protect against an exception, such as the case
      --  of system.ads missing from the library, and fail gracefully.

      begin
         Targparm.Get_Target_Parameters;

      exception
         when Unrecoverable_Error =>
            Make_Failed ("*** make failed.");
      end;

      Display_Commands (not Quiet_Output);

      Check_Steps;

      if Main_Project /= No_Project then

         --  For all library project, if the library file does not exist
         --  put all the project sources in the queue, and flag the project
         --  so that the library is generated.

         if not Unique_Compile
           and then MLib.Tgt.Support_For_Libraries /= MLib.Tgt.None
         then
            for Proj in Project_Table.First ..
                        Project_Table.Last (Project_Tree.Projects)
            loop
               if Project_Tree.Projects.Table (Proj).Library then
                  Project_Tree.Projects.Table
                    (Proj).Need_To_Build_Lib :=
                      (not MLib.Tgt.Library_Exists_For (Proj, Project_Tree))
                    and then (not Project_Tree.Projects.Table
                                (Proj).Externally_Built);

                  if Project_Tree.Projects.Table
                       (Proj).Need_To_Build_Lib
                  then
                     --  If there is no object directory, then it will be
                     --  impossible to build the library. So fail immediately.

                     if Project_Tree.Projects.Table
                       (Proj).Object_Directory = No_Name
                     then
                        Make_Failed
                          ("no object files to build library for project """,
                           Get_Name_String
                             (Project_Tree.Projects.Table (Proj).Name),
                           """");
                        Project_Tree.Projects.Table
                          (Proj).Need_To_Build_Lib := False;

                     else
                        if Verbose_Mode then
                           Write_Str
                             ("Library file does not exist for project """);
                           Write_Str
                             (Get_Name_String
                                (Project_Tree.Projects.Table
                                   (Proj).Name));
                           Write_Line ("""");
                        end if;

                        Insert_Project_Sources
                          (The_Project  => Proj,
                           All_Projects => False,
                           Into_Q       => True);
                     end if;
                  end if;
               end if;
            end loop;
         end if;

         --  If a relative path output file has been specified, we add
         --  the exec directory.

         for J in reverse 1 .. Saved_Linker_Switches.Last - 1 loop
            if Saved_Linker_Switches.Table (J).all = Output_Flag.all then
               declare
                  Exec_File_Name : constant String :=
                    Saved_Linker_Switches.Table (J + 1).all;

               begin
                  if not Is_Absolute_Path (Exec_File_Name) then
                     Get_Name_String
                       (Project_Tree.Projects.Table
                          (Main_Project).Exec_Directory);

                     if Name_Buffer (Name_Len) /= Directory_Separator then
                        Name_Len := Name_Len + 1;
                        Name_Buffer (Name_Len) := Directory_Separator;
                     end if;

                     Name_Buffer (Name_Len + 1 ..
                                    Name_Len + Exec_File_Name'Length) :=
                       Exec_File_Name;
                     Name_Len := Name_Len + Exec_File_Name'Length;
                     Saved_Linker_Switches.Table (J + 1) :=
                       new String'(Name_Buffer (1 .. Name_Len));
                  end if;
               end;

               exit;
            end if;
         end loop;

         --  If we are using a project file, for relative paths we add the
         --  current working directory for any relative path on the command
         --  line and the project directory, for any relative path in the
         --  project file.

         declare
            Dir_Path : constant String_Access :=
              new String'(Get_Name_String
                            (Project_Tree.Projects.Table
                               (Main_Project).Directory));
         begin
            for J in 1 .. Binder_Switches.Last loop
               Test_If_Relative_Path
                 (Binder_Switches.Table (J),
                  Parent => Dir_Path, Including_L_Switch => False);
            end loop;

            for J in 1 .. Saved_Binder_Switches.Last loop
               Test_If_Relative_Path
                 (Saved_Binder_Switches.Table (J),
                  Parent => Current_Work_Dir, Including_L_Switch => False);
            end loop;

            for J in 1 .. Linker_Switches.Last loop
               Test_If_Relative_Path
                 (Linker_Switches.Table (J), Parent => Dir_Path);
            end loop;

            for J in 1 .. Saved_Linker_Switches.Last loop
               Test_If_Relative_Path
                 (Saved_Linker_Switches.Table (J), Parent => Current_Work_Dir);
            end loop;

            for J in 1 .. Gcc_Switches.Last loop
               Test_If_Relative_Path
                 (Gcc_Switches.Table (J), Parent => Dir_Path);
            end loop;

            for J in 1 .. Saved_Gcc_Switches.Last loop
               Test_If_Relative_Path
                 (Saved_Gcc_Switches.Table (J), Parent => Current_Work_Dir);
            end loop;
         end;
      end if;

      --  We now put in the Binder_Switches and Linker_Switches tables, the
      --  binder and linker switches of the command line that have been put in
      --  the Saved_ tables. If a project file was used, then the command line
      --  switches will follow the project file switches.

      for J in 1 .. Saved_Binder_Switches.Last loop
         Add_Switch
           (Saved_Binder_Switches.Table (J),
            Binder,
            And_Save => False);
      end loop;

      for J in 1 .. Saved_Linker_Switches.Last loop
         Add_Switch
           (Saved_Linker_Switches.Table (J),
            Linker,
            And_Save => False);
      end loop;

      --  If no project file is used, we just put the gcc switches
      --  from the command line in the Gcc_Switches table.

      if Main_Project = No_Project then
         for J in 1 .. Saved_Gcc_Switches.Last loop
            Add_Switch
              (Saved_Gcc_Switches.Table (J),
               Compiler,
              And_Save => False);
         end loop;

      else
         --  And we put the command line gcc switches in the variable
         --  The_Saved_Gcc_Switches. They are going to be used later
         --  in procedure Compile_Sources.

         The_Saved_Gcc_Switches :=
           new Argument_List (1 .. Saved_Gcc_Switches.Last + 1);

         for J in 1 .. Saved_Gcc_Switches.Last loop
            The_Saved_Gcc_Switches (J) := Saved_Gcc_Switches.Table (J);
         end loop;

         --  We never use gnat.adc when a project file is used

         The_Saved_Gcc_Switches (The_Saved_Gcc_Switches'Last) :=
           No_gnat_adc;

      end if;

      --  If there was a --GCC, --GNATBIND or --GNATLINK switch on
      --  the command line, then we have to use it, even if there was
      --  another switch in the project file.

      if Saved_Gcc /= null then
         Gcc := Saved_Gcc;
      end if;

      if Saved_Gnatbind /= null then
         Gnatbind := Saved_Gnatbind;
      end if;

      if Saved_Gnatlink /= null then
         Gnatlink := Saved_Gnatlink;
      end if;

      Gcc_Path       := GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);
      Gnatbind_Path  := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatbind.all);
      Gnatlink_Path  := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatlink.all);

      --  If we have specified -j switch both from the project file
      --  and on the command line, the one from the command line takes
      --  precedence.

      if Saved_Maximum_Processes = 0 then
         Saved_Maximum_Processes := Maximum_Processes;
      end if;

      --  Allocate as many temporary mapping file names as the maximum
      --  number of compilation processed, for each possible project.

      The_Mapping_File_Names :=
        new Temp_File_Names
          (No_Project .. Project_Table.Last (Project_Tree.Projects),
           1 .. Saved_Maximum_Processes);
      Last_Mapping_File_Names :=
        new Indices'
          (No_Project .. Project_Table.Last (Project_Tree.Projects)
            => 0);

      The_Free_Mapping_File_Indices :=
        new Free_File_Indices
          (No_Project .. Project_Table.Last (Project_Tree.Projects),
           1 .. Saved_Maximum_Processes);
      Last_Free_Indices :=
        new Indices'(No_Project .. Project_Table.Last
                                     (Project_Tree.Projects) => 0);

      Bad_Compilation.Init;

      Current_Main_Index := Main_Index;

      --  Here is where the make process is started

      --  We do the same process for each main

      Multiple_Main_Loop : for N_File in 1 .. Osint.Number_Of_Files loop

         --  First, find the executable name and path

         Executable          := No_File;
         Executable_Obsolete := False;
         Non_Std_Executable  := False;

         --  Look inside the linker switches to see if the name
         --  of the final executable program was specified.

         for
           J in reverse Linker_Switches.First .. Linker_Switches.Last
         loop
            if Linker_Switches.Table (J).all = Output_Flag.all then
               pragma Assert (J < Linker_Switches.Last);

               --  We cannot specify a single executable for several
               --  main subprograms!

               if Osint.Number_Of_Files > 1 then
                  Fail
                    ("cannot specify a single executable " &
                     "for several mains");
               end if;

               Name_Len := Linker_Switches.Table (J + 1)'Length;
               Name_Buffer (1 .. Name_Len) :=
                 Linker_Switches.Table (J + 1).all;
               Executable := Name_Enter;

               Verbose_Msg (Executable, "final executable");
            end if;
         end loop;

         --  If the name of the final executable program was not
         --  specified then construct it from the main input file.

         if Executable = No_File then
            if Main_Project = No_Project then
               Executable :=
                 Executable_Name (Strip_Suffix (Main_Source_File));

            else
               --  If we are using a project file, we attempt to
               --  remove the body (or spec) termination of the main
               --  subprogram. We find it the the naming scheme of the
               --  project file. This will avoid to generate an
               --  executable "main.2" for a main subprogram
               --  "main.2.ada", when the body termination is ".2.ada".

               Executable :=
                 Prj.Util.Executable_Of
                   (Main_Project, Project_Tree, Main_Source_File, Main_Index);
            end if;
         end if;

         if Main_Project /= No_Project then
            declare
               Exec_File_Name : constant String :=
                 Get_Name_String (Executable);

            begin
               if not Is_Absolute_Path (Exec_File_Name) then

                  Get_Name_String (Project_Tree.Projects.Table
                                     (Main_Project).Exec_Directory);

                  if
                    Name_Buffer (Name_Len) /= Directory_Separator
                  then
                     Name_Len := Name_Len + 1;
                     Name_Buffer (Name_Len) := Directory_Separator;
                  end if;

                  Name_Buffer (Name_Len + 1 ..
                                       Name_Len + Exec_File_Name'Length) :=
                    Exec_File_Name;

                  Name_Len := Name_Len + Exec_File_Name'Length;
                  Executable := Name_Find;
               end if;

               Non_Std_Executable := True;
            end;
         end if;

         if Do_Compile_Step then
            Recursive_Compilation_Step : declare
               Args : Argument_List (1 .. Gcc_Switches.Last);

               First_Compiled_File : Name_Id;
               Youngest_Obj_File   : Name_Id;
               Youngest_Obj_Stamp  : Time_Stamp_Type;

               Executable_Stamp : Time_Stamp_Type;
               --  Executable is the final executable program

               Library_Rebuilt : Boolean := False;

            begin
               for J in 1 .. Gcc_Switches.Last loop
                  Args (J) := Gcc_Switches.Table (J);
               end loop;

               --  Now we invoke Compile_Sources for the current main

               Compile_Sources
                 (Main_Source           => Main_Source_File,
                  Args                  => Args,
                  First_Compiled_File   => First_Compiled_File,
                  Most_Recent_Obj_File  => Youngest_Obj_File,
                  Most_Recent_Obj_Stamp => Youngest_Obj_Stamp,
                  Main_Unit             => Is_Main_Unit,
                  Main_Index            => Current_Main_Index,
                  Compilation_Failures  => Compilation_Failures,
                  Check_Readonly_Files  => Check_Readonly_Files,
                  Do_Not_Execute        => Do_Not_Execute,
                  Force_Compilations    => Force_Compilations,
                  In_Place_Mode         => In_Place_Mode,
                  Keep_Going            => Keep_Going,
                  Initialize_ALI_Data   => True,
                  Max_Process           => Saved_Maximum_Processes);

               if Verbose_Mode then
                  Write_Str ("End of compilation");
                  Write_Eol;
               end if;

               --  Make sure the queue will be reinitialized for the next round

               First_Q_Initialization := True;

               Total_Compilation_Failures :=
                 Total_Compilation_Failures + Compilation_Failures;

               if Total_Compilation_Failures /= 0 then
                  if Keep_Going then
                     goto Next_Main;
                  else
                     List_Bad_Compilations;
                     raise Compilation_Failed;
                  end if;
               end if;

               --  Regenerate libraries, if any, and if object files
               --  have been regenerated.

               if Main_Project /= No_Project
                 and then MLib.Tgt.Support_For_Libraries /= MLib.Tgt.None
                 and then (Do_Bind_Step
                             or Unique_Compile_All_Projects
                             or not Compile_Only)
                 and then (Do_Link_Step or N_File = Osint.Number_Of_Files)
               then
                  Library_Projs.Init;

                  declare
                     Depth   : Natural;
                     Current : Natural;

                     procedure Add_To_Library_Projs (Proj : Project_Id);
                     --  Add project Project to table Library_Projs
                     --  in decreasing depth order.

                     --------------------------
                     -- Add_To_Library_Projs --
                     --------------------------

                     procedure Add_To_Library_Projs (Proj : Project_Id) is
                        Prj   : Project_Id;

                     begin
                        Library_Projs.Increment_Last;
                        Depth := Project_Tree.Projects.Table (Proj).Depth;

                        --  Put the projects in decreasing depth order,
                        --  so that if libA depends on libB, libB is first
                        --  in order.

                        Current := Library_Projs.Last;
                        while Current > 1 loop
                           Prj := Library_Projs.Table (Current - 1);
                           exit when Project_Tree.Projects.Table
                             (Prj).Depth >= Depth;
                           Library_Projs.Table (Current) := Prj;
                           Current := Current - 1;
                        end loop;

                        Library_Projs.Table (Current) := Proj;
                     end Add_To_Library_Projs;

                  --  Start of processing for ??? (should name declare block
                  --  or probably better, break this out as a nested proc.

                  begin
                     --  Put in Library_Projs table all library project
                     --  file ids when the library need to be rebuilt.

                     for Proj1 in Project_Table.First ..
                                  Project_Table.Last (Project_Tree.Projects)
                     loop
                        if Project_Tree.Projects.Table
                          (Proj1).Standalone_Library
                        then
                           There_Are_Stand_Alone_Libraries := True;
                        end if;

                        if Project_Tree.Projects.Table (Proj1).Library then
                           MLib.Prj.Check_Library (Proj1, Project_Tree);
                        end if;

                        if Project_Tree.Projects.Table
                             (Proj1).Need_To_Build_Lib
                        then
                           Add_To_Library_Projs (Proj1);
                        end if;
                     end loop;

                     --  Check if importing libraries should be regenerated
                     --  because at least an imported library will be
                     --  regenerated or is more recent.

                     for Proj1 in Project_Table.First ..
                                  Project_Table.Last (Project_Tree.Projects)
                     loop
                        if Project_Tree.Projects.Table (Proj1).Library
                          and then not Project_Tree.Projects.Table
                            (Proj1).Need_To_Build_Lib
                          and then not Project_Tree.Projects.Table
                            (Proj1).Externally_Built
                        then
                           declare
                              List    : Project_List;
                              Element : Project_Element;
                              Proj2   : Project_Id;
                              Rebuild : Boolean := False;

                              Lib_Timestamp1 : constant Time_Stamp_Type :=
                                                 Project_Tree.Projects.Table
                                                   (Proj1). Library_TS;

                           begin
                              List := Project_Tree.Projects.Table (Proj1).
                                                      All_Imported_Projects;
                              while List /= Empty_Project_List loop
                                 Element :=
                                   Project_Tree.Project_Lists.Table (List);
                                 Proj2 := Element.Project;

                                 if
                                   Project_Tree.Projects.Table (Proj2).Library
                                 then
                                    if Project_Tree.Projects.Table (Proj2).
                                          Need_To_Build_Lib
                                      or else
                                        (Lib_Timestamp1 <
                                             Project_Tree.Projects.Table
                                               (Proj2).Library_TS)
                                    then
                                       Rebuild := True;
                                       exit;
                                    end if;
                                 end if;

                                 List := Element.Next;
                              end loop;

                              if Rebuild then
                                 Project_Tree.Projects.Table
                                   (Proj1).Need_To_Build_Lib := True;
                                 Add_To_Library_Projs (Proj1);
                              end if;
                           end;
                        end if;
                     end loop;

                     --  Reset the flags Need_To_Build_Lib for the next main,
                     --  to avoid rebuilding libraries uselessly.

                     for Proj1 in Project_Table.First ..
                                  Project_Table.Last (Project_Tree.Projects)
                     loop
                        Project_Tree.Projects.Table
                          (Proj1).Need_To_Build_Lib := False;
                     end loop;
                  end;

                  --  Build the libraries, if any need to be built

                  for J in 1 .. Library_Projs.Last loop
                     Library_Rebuilt := True;
                     MLib.Prj.Build_Library
                       (For_Project   => Library_Projs.Table (J),
                        In_Tree       => Project_Tree,
                        Gnatbind      => Gnatbind.all,
                        Gnatbind_Path => Gnatbind_Path,
                        Gcc           => Gcc.all,
                        Gcc_Path      => Gcc_Path);
                  end loop;
               end if;

               if List_Dependencies then
                  if First_Compiled_File /= No_File then
                     Inform
                       (First_Compiled_File,
                        "must be recompiled. Can't generate dependence list.");
                  else
                     List_Depend;
                  end if;

               elsif First_Compiled_File = No_File
                 and then not Do_Bind_Step
                 and then not Quiet_Output
                 and then not Library_Rebuilt
                 and then Osint.Number_Of_Files = 1
               then
                  Inform (Msg => "objects up to date.");

               elsif Do_Not_Execute
                 and then First_Compiled_File /= No_File
               then
                  Write_Name (First_Compiled_File);
                  Write_Eol;
               end if;

               --  Stop after compile step if any of:

               --    1) -n (Do_Not_Execute) specified

               --    2) -M (List_Dependencies) specified (also sets
               --       Do_Not_Execute above, so this is probably superfluous).

               --    3) -c (Compile_Only) specified, but not -b (Bind_Only)

               --    4) Made unit cannot be a main unit

               if (Do_Not_Execute
                   or List_Dependencies
                   or not Do_Bind_Step
                   or not Is_Main_Unit)
                 and then not No_Main_Subprogram
                 and then not Build_Bind_And_Link_Full_Project
               then
                  if Osint.Number_Of_Files = 1 then
                     exit Multiple_Main_Loop;

                  else
                     goto Next_Main;
                  end if;
               end if;

               --  If the objects were up-to-date check if the executable file
               --  is also up-to-date. For now always bind and link on the JVM
               --  since there is currently no simple way to check the
               --  up-to-date status of objects

               if not Hostparm.Java_VM
                 and then First_Compiled_File = No_File
               then
                  Executable_Stamp    := File_Stamp (Executable);

                  if not Executable_Obsolete then
                     Executable_Obsolete :=
                       Youngest_Obj_Stamp > Executable_Stamp;
                  end if;

                  if not Executable_Obsolete then
                     for Index in reverse 1 .. Dependencies.Last loop
                        if Is_In_Obsoleted
                             (Dependencies.Table (Index).Depends_On)
                        then
                           Enter_Into_Obsoleted
                             (Dependencies.Table (Index).This);
                        end if;
                     end loop;

                     Executable_Obsolete := Is_In_Obsoleted (Main_Source_File);
                     Dependencies.Init;
                  end if;

                  if not Executable_Obsolete then

                     --  If no Ada object files obsolete the executable, check
                     --  for younger or missing linker files.

                     Check_Linker_Options
                       (Executable_Stamp,
                        Youngest_Obj_File,
                        Youngest_Obj_Stamp);

                     Executable_Obsolete := Youngest_Obj_File /= No_File;
                  end if;

                  --  Return if the executable is up to date
                  --  and otherwise motivate the relink/rebind.

                  if not Executable_Obsolete then
                     if not Quiet_Output then
                        Inform (Executable, "up to date.");
                     end if;

                     if Osint.Number_Of_Files = 1 then
                        exit Multiple_Main_Loop;

                     else
                        goto Next_Main;
                     end if;
                  end if;

                  if Executable_Stamp (1) = ' ' then
                     Verbose_Msg (Executable, "missing.", Prefix => "  ");

                  elsif Youngest_Obj_Stamp (1) = ' ' then
                     Verbose_Msg
                       (Youngest_Obj_File,
                        "missing.",
                        Prefix => "  ");

                  elsif Youngest_Obj_Stamp > Executable_Stamp then
                     Verbose_Msg
                       (Youngest_Obj_File,
                        "(" & String (Youngest_Obj_Stamp) & ") newer than",
                        Executable,
                        "(" & String (Executable_Stamp) & ")");

                  else
                     Verbose_Msg
                       (Executable, "needs to be rebuild.",
                        Prefix => "  ");

                  end if;
               end if;
            end Recursive_Compilation_Step;
         end if;

         --  For binding and linking, we need to be in the object directory of
         --  the main project.

         if Main_Project /= No_Project then
            Change_To_Object_Directory (Main_Project);
         end if;

         --  If we are here, it means that we need to rebuilt the current
         --  main. So we set Executable_Obsolete to True to make sure that
         --  the subsequent mains will be rebuilt.

         Main_ALI_In_Place_Mode_Step : declare
            ALI_File : File_Name_Type;
            Src_File : File_Name_Type;

         begin
            Src_File      := Strip_Directory (Main_Source_File);
            ALI_File      := Lib_File_Name (Src_File, Current_Main_Index);
            Main_ALI_File := Full_Lib_File_Name (ALI_File);

            --  When In_Place_Mode, the library file can be located in the
            --  Main_Source_File directory which may not be present in the
            --  library path. In this case, use the corresponding library file
            --  name.

            if Main_ALI_File = No_File and then In_Place_Mode then
               Get_Name_String (Get_Directory (Full_Source_Name (Src_File)));
               Get_Name_String_And_Append (ALI_File);
               Main_ALI_File := Name_Find;
               Main_ALI_File := Full_Lib_File_Name (Main_ALI_File);
            end if;

            if Main_ALI_File = No_File then
               Make_Failed ("could not find the main ALI file");
            end if;
         end Main_ALI_In_Place_Mode_Step;

         if Do_Bind_Step then
            Bind_Step : declare
               Args : Argument_List
                        (Binder_Switches.First .. Binder_Switches.Last + 2);
               --  The arguments for the invocation of gnatbind

               Last_Arg : Natural := Binder_Switches.Last;
               --  Index of the last argument in Args

               Shared_Libs : Boolean := False;
               --  Set to True when there are shared library project files or
               --  when gnatbind is invoked with -shared.

            begin
               --  Check if there are shared libraries, so that gnatbind is
               --  called with -shared. Check also if gnatbind is called with
               --  -shared, so that gnatlink is called with -shared-libgcc
               --  for GCC version 3 and above, ensuring that the shared
               --  version of libgcc will be used.

               if Main_Project /= No_Project
                 and then MLib.Tgt.Support_For_Libraries /= MLib.Tgt.None
               then
                  for Proj in Project_Table.First ..
                        Project_Table.Last (Project_Tree.Projects)
                  loop
                     if Project_Tree.Projects.Table (Proj).Library
                       and then Project_Tree.Projects.Table
                                  (Proj).Library_Kind /= Static
                     then
                        Shared_Libs := True;
                        Bind_Shared := Shared_Switch'Access;
                        exit;
                     end if;
                  end loop;
               end if;

               --  Check now for switch -shared

               if not Shared_Libs then
                  for J in Binder_Switches.First .. Last_Arg loop
                     if Binder_Switches.Table (J).all = "-shared" then
                        Shared_Libs := True;
                        exit;
                     end if;
                  end loop;
               end if;

               --  If there are shared libraries, invoke gnatlink with
               --  -shared-libgcc if GCC version is 3 or more.

               if Shared_Libs and then GCC_Version >= 3 then
                  Link_With_Shared_Libgcc := Shared_Libgcc_Switch'Access;
               end if;

               --  Get all the binder switches

               for J in Binder_Switches.First .. Last_Arg loop
                  Args (J) := Binder_Switches.Table (J);
               end loop;

               if There_Are_Stand_Alone_Libraries then
                  Last_Arg := Last_Arg + 1;
                  Args (Last_Arg) := Force_Elab_Flags_String'Access;
               end if;

               if Main_Project /= No_Project then

                  --  Put all the source directories in ADA_INCLUDE_PATH,
                  --  and all the object directories in ADA_OBJECTS_PATH

                  Prj.Env.Set_Ada_Paths (Main_Project, Project_Tree, False);

                  --  If switch -C was specified, create a binder mapping file

                  if Create_Mapping_File then
                     Create_Binder_Mapping_File (Args, Last_Arg);
                  end if;

               end if;

               begin
                  Bind (Main_ALI_File,
                        Bind_Shared.all & Args (Args'First .. Last_Arg));

               exception
                  when others =>

                     --  If -dn was not specified, delete the temporary mapping
                     --  file, if one was created.

                     if not Debug.Debug_Flag_N
                       and then Mapping_Path /= No_Name
                     then
                        Delete_File (Get_Name_String (Mapping_Path), Discard);
                     end if;

                     --  And reraise the exception

                     raise;
               end;

               --  If -dn was not specified, delete the temporary mapping file,
               --  if one was created.

               if not Debug.Debug_Flag_N and then Mapping_Path /= No_Name then
                  Delete_File (Get_Name_String (Mapping_Path), Discard);
               end if;
            end Bind_Step;
         end if;

         if Do_Link_Step then
            Link_Step : declare
               There_Are_Libraries  : Boolean := False;
               Linker_Switches_Last : constant Integer := Linker_Switches.Last;
               Path_Option : constant String_Access :=
                               MLib.Linker_Library_Path_Option;
               Current : Natural;
               Proj2   : Project_Id;
               Depth   : Natural;

            begin
               if not Run_Path_Option then
                  Linker_Switches.Increment_Last;
                  Linker_Switches.Table (Linker_Switches.Last) :=
                    new String'("-R");
               end if;

               if Main_Project /= No_Project then
                  Library_Paths.Set_Last (0);
                  Library_Projs.Init;

                  if MLib.Tgt.Support_For_Libraries /= MLib.Tgt.None then
                     --  Check for library projects

                     for Proj1 in Project_Table.First ..
                           Project_Table.Last (Project_Tree.Projects)
                     loop
                        if Proj1 /= Main_Project
                          and then
                            Project_Tree.Projects.Table (Proj1).Library
                        then
                           --  Add this project to table Library_Projs

                           There_Are_Libraries := True;
                           Depth :=
                             Project_Tree.Projects.Table (Proj1).Depth;
                           Library_Projs.Increment_Last;
                           Current := Library_Projs.Last;

                           --  Any project with a greater depth should be
                           --  after this project in the list.

                           while Current > 1 loop
                              Proj2 := Library_Projs.Table (Current - 1);
                              exit when Project_Tree.Projects.Table
                                          (Proj2).Depth <= Depth;
                              Library_Projs.Table (Current) := Proj2;
                              Current := Current - 1;
                           end loop;

                           Library_Projs.Table (Current) := Proj1;

                           --  If it is not a static library and path option
                           --  is set, add it to the Library_Paths table.

                           if Project_Tree.Projects.Table
                                (Proj1).Library_Kind /= Static
                             and then Path_Option /= null
                           then
                              Library_Paths.Increment_Last;
                              Library_Paths.Table (Library_Paths.Last) :=
                                new String'
                                  (Get_Name_String
                                       (Project_Tree.Projects.Table
                                            (Proj1).Library_Dir));
                           end if;
                        end if;
                     end loop;

                     for Index in 1 .. Library_Projs.Last loop
                        --  Add the -L switch

                        Linker_Switches.Increment_Last;
                        Linker_Switches.Table (Linker_Switches.Last) :=
                          new String'("-L" &
                                      Get_Name_String
                                        (Project_Tree.Projects.Table
                                           (Library_Projs.Table (Index)).
                                              Library_Dir));

                        --  Add the -l switch

                        Linker_Switches.Increment_Last;
                        Linker_Switches.Table (Linker_Switches.Last) :=
                          new String'("-l" &
                                      Get_Name_String
                                        (Project_Tree.Projects.Table
                                           (Library_Projs.Table (Index)).
                                              Library_Name));
                     end loop;
                  end if;

                  if There_Are_Libraries then

                     --  If Path_Option is not null, create the switch
                     --  ("-Wl,-rpath," or equivalent) with all the non static
                     --  library dirs plus the standard GNAT library dir.
                     --  We do that only if Run_Path_Option is True
                     --  (not disabled by -R switch).

                     if Run_Path_Option and Path_Option /= null then
                        declare
                           Option  : String_Access;
                           Length  : Natural := Path_Option'Length;
                           Current : Natural;

                        begin
                           for Index in
                             Library_Paths.First .. Library_Paths.Last
                           loop
                              --  Add the length of the library dir plus one
                              --  for the directory separator.

                              Length :=
                                Length +
                                Library_Paths.Table (Index)'Length + 1;
                           end loop;

                           --  Finally, add the length of the standard GNAT
                           --  library dir.

                           Length := Length + MLib.Utl.Lib_Directory'Length;
                           Option := new String (1 .. Length);
                           Option (1 .. Path_Option'Length) := Path_Option.all;
                           Current := Path_Option'Length;

                           --  Put each library dir followed by a dir separator

                           for Index in
                             Library_Paths.First .. Library_Paths.Last
                           loop
                              Option
                                (Current + 1 ..
                                   Current +
                                   Library_Paths.Table (Index)'Length) :=
                                Library_Paths.Table (Index).all;
                              Current :=
                                Current +
                                Library_Paths.Table (Index)'Length + 1;
                              Option (Current) := Path_Separator;
                           end loop;

                           --  Finally put the standard GNAT library dir

                           Option
                             (Current + 1 ..
                                Current + MLib.Utl.Lib_Directory'Length) :=
                             MLib.Utl.Lib_Directory;

                           --  And add the switch to the linker switches

                           Linker_Switches.Increment_Last;
                           Linker_Switches.Table (Linker_Switches.Last) :=
                             Option;
                        end;
                     end if;

                  end if;

                  --  Put the object directories in ADA_OBJECTS_PATH

                  Prj.Env.Set_Ada_Paths (Main_Project, Project_Tree, False);

                  --  Check for attributes Linker'Linker_Options in projects
                  --  other than the main project

                  declare
                     Linker_Options : constant String_List :=
                                        Linker_Options_Switches
                                          (Main_Project, Project_Tree);
                  begin
                     for Option in Linker_Options'Range loop
                        Linker_Switches.Increment_Last;
                        Linker_Switches.Table (Linker_Switches.Last) :=
                          Linker_Options (Option);
                     end loop;
                  end;
               end if;

               declare
                  Args : Argument_List
                           (Linker_Switches.First .. Linker_Switches.Last + 2);

                  Last_Arg : Integer := Linker_Switches.First - 1;
                  Skip     : Boolean := False;

               begin
                  --  Get all the linker switches

                  for J in Linker_Switches.First .. Linker_Switches.Last loop
                     if Skip then
                        Skip := False;

                     elsif Non_Std_Executable
                       and then Linker_Switches.Table (J).all = "-o"
                     then
                        Skip := True;

                     else
                        Last_Arg := Last_Arg + 1;
                        Args (Last_Arg) := Linker_Switches.Table (J);
                     end if;
                  end loop;

                  --  If need be, add the -o switch

                  if Non_Std_Executable then
                     Last_Arg := Last_Arg + 1;
                     Args (Last_Arg) := new String'("-o");
                     Last_Arg := Last_Arg + 1;
                     Args (Last_Arg) :=
                       new String'(Get_Name_String (Executable));
                  end if;

                  --  And invoke the linker

                  begin
                     Link (Main_ALI_File,
                           Link_With_Shared_Libgcc.all &
                           Args (Args'First .. Last_Arg));
                     Successful_Links.Increment_Last;
                     Successful_Links.Table (Successful_Links.Last) :=
                       Main_ALI_File;

                  exception
                     when Link_Failed =>
                        if Osint.Number_Of_Files = 1 or not Keep_Going then
                           raise;

                        else
                           Write_Line ("*** link failed");
                           Failed_Links.Increment_Last;
                           Failed_Links.Table (Failed_Links.Last) :=
                             Main_ALI_File;
                        end if;
                  end;
               end;

               Linker_Switches.Set_Last (Linker_Switches_Last);
            end Link_Step;
         end if;

         --  We go to here when we skip the bind and link steps

         <<Next_Main>>

         --  We go to the next main, if we did not process the last one

         if N_File < Osint.Number_Of_Files then
            Main_Source_File := Next_Main_Source;

            if Current_File_Index /= No_Index then
               Main_Index := Current_File_Index;
            end if;

            if Main_Project /= No_Project then

               --  Find the file name of the main unit

               declare
                  Main_Source_File_Name : constant String :=
                                            Get_Name_String (Main_Source_File);

                  Main_Unit_File_Name : constant String :=
                                          Prj.Env.
                                            File_Name_Of_Library_Unit_Body
                                              (Name => Main_Source_File_Name,
                                               Project => Main_Project,
                                               In_Tree => Project_Tree,
                                               Main_Project_Only =>
                                                 not Unique_Compile);

                  The_Packages : constant Package_Id :=
                    Project_Tree.Projects.Table
                      (Main_Project).Decl.Packages;

                  Binder_Package : constant Prj.Package_Id :=
                               Prj.Util.Value_Of
                                 (Name        => Name_Binder,
                                  In_Packages => The_Packages,
                                  In_Tree     => Project_Tree);

                  Linker_Package : constant Prj.Package_Id :=
                               Prj.Util.Value_Of
                                 (Name        => Name_Linker,
                                  In_Packages => The_Packages,
                                  In_Tree     => Project_Tree);

               begin
                  --  We fail if we cannot find the main source file
                  --  as an immediate source of the main project file.

                  if Main_Unit_File_Name = "" then
                     Make_Failed ('"' & Main_Source_File_Name,
                                  """ is not a unit of project ",
                                  Project_File_Name.all & ".");

                  else
                     --  Remove any directory information from the main
                     --  source file name.

                     declare
                        Pos : Natural := Main_Unit_File_Name'Last;

                     begin
                        loop
                           exit when Pos < Main_Unit_File_Name'First
                             or else
                             Main_Unit_File_Name (Pos) = Directory_Separator;
                           Pos := Pos - 1;
                        end loop;

                        Name_Len := Main_Unit_File_Name'Last - Pos;

                        Name_Buffer (1 .. Name_Len) :=
                          Main_Unit_File_Name
                          (Pos + 1 .. Main_Unit_File_Name'Last);

                        Main_Source_File := Name_Find;
                     end;
                  end if;

                  --  We now deal with the binder and linker switches.
                  --  If no project file is used, there is nothing to do
                  --  because the binder and linker switches are the same
                  --  for all mains.

                  --  Reset the tables Binder_Switches and Linker_Switches

                  Binder_Switches.Set_Last (Last_Binder_Switch);
                  Linker_Switches.Set_Last (Last_Linker_Switch);

                  --  Add binder switches from the project file for this main,
                  --  if any.

                  if Do_Bind_Step and Binder_Package /= No_Package then
                     if Verbose_Mode then
                        Write_Str ("Adding binder switches for """);
                        Write_Str (Main_Unit_File_Name);
                        Write_Line (""".");
                     end if;

                     Add_Switches
                       (File_Name   => Main_Unit_File_Name,
                        Index       => Main_Index,
                        The_Package => Binder_Package,
                        Program     => Binder);
                  end if;

                  --  Add linker switches from the project file for this main,
                  --  if any.

                  if Do_Link_Step and Linker_Package /= No_Package then
                     if Verbose_Mode then
                        Write_Str ("Adding linker switches for""");
                        Write_Str (Main_Unit_File_Name);
                        Write_Line (""".");
                     end if;

                     Add_Switches
                       (File_Name   => Main_Unit_File_Name,
                        Index       => Main_Index,
                        The_Package => Linker_Package,
                        Program     => Linker);
                  end if;

                  --  As we are using a project file, for relative paths we add
                  --  the current working directory for any relative path on
                  --  the command line and the project directory, for any
                  --  relative path in the project file.

                  declare
                     Dir_Path : constant String_Access :=
                       new String'(Get_Name_String
                                     (Project_Tree.Projects.Table
                                        (Main_Project).Directory));
                  begin
                     for
                       J in Last_Binder_Switch + 1 .. Binder_Switches.Last
                     loop
                        Test_If_Relative_Path
                          (Binder_Switches.Table (J),
                           Parent => Dir_Path, Including_L_Switch => False);
                     end loop;

                     for
                       J in Last_Linker_Switch + 1 .. Linker_Switches.Last
                     loop
                        Test_If_Relative_Path
                          (Linker_Switches.Table (J), Parent => Dir_Path);
                     end loop;
                  end;

                  --  We now put in the Binder_Switches and Linker_Switches
                  --  tables, the binder and linker switches of the command
                  --  line that have been put in the Saved_ tables.
                  --  These switches will follow the project file switches.

                  for J in 1 .. Saved_Binder_Switches.Last loop
                     Add_Switch
                       (Saved_Binder_Switches.Table (J),
                        Binder,
                        And_Save => False);
                  end loop;

                  for J in 1 .. Saved_Linker_Switches.Last loop
                     Add_Switch
                       (Saved_Linker_Switches.Table (J),
                        Linker,
                        And_Save => False);
                  end loop;
               end;
            end if;
         end if;

         --  Remove all marks to be sure to check sources for all executables,
         --  as the switches may be different and -s may be in use.

         Delete_All_Marks;
      end loop Multiple_Main_Loop;

      if Failed_Links.Last > 0 then
         for Index in 1 .. Successful_Links.Last loop
            Write_Str ("Linking of """);
            Write_Str (Get_Name_String (Successful_Links.Table (Index)));
            Write_Line (""" succeeded.");
         end loop;

         for Index in 1 .. Failed_Links.Last loop
            Write_Str ("Linking of """);
            Write_Str (Get_Name_String (Failed_Links.Table (Index)));
            Write_Line (""" failed.");
         end loop;

         if Total_Compilation_Failures = 0 then
            raise Compilation_Failed;
         end if;
      end if;

      if Total_Compilation_Failures /= 0 then
         List_Bad_Compilations;
         raise Compilation_Failed;
      end if;

      --  Delete the temporary mapping file that was created if we are
      --  using project files.

      if not Debug.Debug_Flag_N then
         Delete_Mapping_Files;
         Prj.Env.Delete_All_Path_Files (Project_Tree);
      end if;

      Exit_Program (E_Success);

   exception
      when Bind_Failed =>
         Make_Failed ("*** bind failed.");

      when Compilation_Failed =>
         if not Debug.Debug_Flag_N then
            Delete_Mapping_Files;
            Prj.Env.Delete_All_Path_Files (Project_Tree);
         end if;

         Exit_Program (E_Fatal);

      when Link_Failed =>
         Make_Failed ("*** link failed.");

      when X : others =>
         Write_Line (Exception_Information (X));
         Make_Failed ("INTERNAL ERROR. Please report.");
   end Gnatmake;

   ----------
   -- Hash --
   ----------

   function Hash (F : Name_Id) return Header_Num is
   begin
      return Header_Num (1 + F mod Max_Header);
   end Hash;

   --------------------
   -- In_Ada_Lib_Dir --
   --------------------

   function In_Ada_Lib_Dir (File : File_Name_Type) return Boolean is
      D : constant Name_Id := Get_Directory (File);
      B : constant Byte    := Get_Name_Table_Byte (D);
   begin
      return (B and Ada_Lib_Dir) /= 0;
   end In_Ada_Lib_Dir;

   ------------
   -- Inform --
   ------------

   procedure Inform (N : Name_Id := No_Name; Msg : String) is
   begin
      Osint.Write_Program_Name;

      Write_Str (": ");

      if N /= No_Name then
         Write_Str ("""");
         Write_Name (N);
         Write_Str (""" ");
      end if;

      Write_Str (Msg);
      Write_Eol;
   end Inform;

   -----------------------
   -- Init_Mapping_File --
   -----------------------

   procedure Init_Mapping_File
     (Project    : Project_Id;
      File_Index : in out Natural)
   is
      FD : File_Descriptor;

      Status : Boolean;
      --  For call to Close

   begin
      --  Increase the index of the last mapping file for this project

      Last_Mapping_File_Names (Project) :=
        Last_Mapping_File_Names (Project) + 1;

      --  If there is a project file, call Create_Mapping_File with
      --  the project id.

      if Project /= No_Project then
         Prj.Env.Create_Mapping_File
           (Project, Project_Tree,
            The_Mapping_File_Names
              (Project, Last_Mapping_File_Names (Project)));

      --  Otherwise, just create an empty file

      else
         Tempdir.Create_Temp_File
           (FD,
            The_Mapping_File_Names
              (No_Project, Last_Mapping_File_Names (No_Project)));
         if FD = Invalid_FD then
            Make_Failed ("disk full");
         end if;

         Close (FD, Status);

         if not Status then
            Make_Failed ("disk full");
         end if;
      end if;

      --  And return the index of the newly created file

      File_Index := Last_Mapping_File_Names (Project);
   end Init_Mapping_File;

   ------------
   -- Init_Q --
   ------------

   procedure Init_Q is
   begin
      First_Q_Initialization := False;
      Q_Front := Q.First;
      Q.Set_Last (Q.First);
   end Init_Q;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Override default initialization of Check_Object_Consistency
      --  since this is normally False for GNATBIND, but is True for
      --  GNATMAKE since we do not need to check source consistency
      --  again once GNATMAKE has looked at the sources to check.

      Check_Object_Consistency := True;

      --  Package initializations. The order of calls is important here

      Output.Set_Standard_Error;

      Gcc_Switches.Init;
      Binder_Switches.Init;
      Linker_Switches.Init;

      Csets.Initialize;
      Namet.Initialize;

      Snames.Initialize;

      Prj.Initialize (Project_Tree);

      Dependencies.Init;

      RTS_Specified := null;

      Mains.Delete;

      --  Add the directory where gnatmake is invoked in front of the
      --  path, if gnatmake is invoked with directory information.
      --  Only do this if the platform is not VMS, where the notion of path
      --  does not really exist.

      if not OpenVMS then
         declare
            Command : constant String := Command_Name;

         begin
            for Index in reverse Command'Range loop
               if Command (Index) = Directory_Separator then
                  declare
                     Absolute_Dir : constant String :=
                                      Normalize_Pathname
                                        (Command (Command'First .. Index));

                     PATH : constant String :=
                                      Absolute_Dir &
                                      Path_Separator &
                                      Getenv ("PATH").all;

                  begin
                     Setenv ("PATH", PATH);
                  end;

                  exit;
               end if;
            end loop;
         end;
      end if;

      --  Scan the switches and arguments

      Scan_Args : for Next_Arg in 1 .. Argument_Count loop
         Scan_Make_Arg (Argument (Next_Arg), And_Save => True);
      end loop Scan_Args;

      if Usage_Requested then
         Usage;
      end if;

      --  Test for trailing -P switch

      if Project_File_Name_Present and then Project_File_Name = null then
         Make_Failed ("project file name missing after -P");

      --  Test for trailing -o switch

      elsif Output_File_Name_Present
        and then not Output_File_Name_Seen
      then
         Make_Failed ("output file name missing after -o");

      --  Test for trailing -D switch

      elsif Object_Directory_Present
        and then not Object_Directory_Seen then
         Make_Failed ("object directory missing after -D");
      end if;

      --  Test for simultaneity of -i and -D

      if Object_Directory_Path /= null and then In_Place_Mode then
         Make_Failed ("-i and -D cannot be used simutaneously");
      end if;

      --  Deal with -C= switch

      if Gnatmake_Mapping_File /= null then
         --  First, check compatibility with other switches

         if Project_File_Name /= null then
            Make_Failed ("-C= switch is not compatible with -P switch");

         elsif Saved_Maximum_Processes > 1 then
            Make_Failed ("-C= switch is not compatible with -jnnn switch");
         end if;

         Fmap.Initialize (Gnatmake_Mapping_File.all);
         Add_Switch
           ("-gnatem=" & Gnatmake_Mapping_File.all,
            Compiler,
            And_Save => True);
      end if;

      if Project_File_Name /= null then

         --  A project file was specified by a -P switch

         if Verbose_Mode then
            Write_Eol;
            Write_Str ("Parsing Project File """);
            Write_Str (Project_File_Name.all);
            Write_Str (""".");
            Write_Eol;
         end if;

         --  Avoid looking in the current directory for ALI files

         --  Look_In_Primary_Dir := False;

         --  Set the project parsing verbosity to whatever was specified
         --  by a possible -vP switch.

         Prj.Pars.Set_Verbosity (To => Current_Verbosity);

         --  Parse the project file.
         --  If there is an error, Main_Project will still be No_Project.

         Prj.Pars.Parse
           (Project           => Main_Project,
            In_Tree           => Project_Tree,
            Project_File_Name => Project_File_Name.all,
            Packages_To_Check => Packages_To_Check_By_Gnatmake);

         if Main_Project = No_Project then
            Make_Failed ("""", Project_File_Name.all, """ processing failed");
         end if;

         if Verbose_Mode then
            Write_Eol;
            Write_Str ("Parsing of Project File """);
            Write_Str (Project_File_Name.all);
            Write_Str (""" is finished.");
            Write_Eol;
         end if;

         --  We add the source directories and the object directories
         --  to the search paths.

         Add_Source_Directories (Main_Project, Project_Tree);
         Add_Object_Directories (Main_Project, Project_Tree);

         --  Compute depth of each project

         for Proj in Project_Table.First ..
                     Project_Table.Last (Project_Tree.Projects)
         loop
            Project_Tree.Projects.Table (Proj).Seen := False;
            Project_Tree.Projects.Table (Proj).Depth := 0;
         end loop;

         Recursive_Compute_Depth
           (Main_Project, Depth => 1);

         --  For each project compute the list of the projects it imports
         --  directly or indirectly.

         for Proj in Project_Table.First ..
                     Project_Table.Last (Project_Tree.Projects)
         loop
            Compute_All_Imported_Projects (Proj);
         end loop;

      else

         Osint.Add_Default_Search_Dirs;

         --  Source file lookups should be cached for efficiency.
         --  Source files are not supposed to change. However, we do that now
         --  only if no project file is used; if a project file is used, we
         --  do it just after changing the directory to the object directory.

         Osint.Source_File_Data (Cache => True);

         --  Read gnat.adc file to initialize Fname.UF

         Fname.UF.Initialize;

         begin
            Fname.SF.Read_Source_File_Name_Pragmas;

         exception
            when Err : SFN_Scan.Syntax_Error_In_GNAT_ADC =>
               Make_Failed (Exception_Message (Err));
         end;
      end if;

      --  Make sure no project object directory is recorded

      Project_Object_Directory := No_Project;

   end Initialize;

   ----------------------------
   -- Insert_Project_Sources --
   ----------------------------

   procedure Insert_Project_Sources
     (The_Project  : Project_Id;
      All_Projects : Boolean;
      Into_Q       : Boolean)
   is
      Put_In_Q : Boolean := Into_Q;
      Unit     : Unit_Data;
      Sfile    : Name_Id;

      Extending : constant Boolean :=
                    Project_Tree.Projects.Table
                      (The_Project).Extends /= No_Project;

      function Check_Project (P : Project_Id) return Boolean;
      --  Returns True if P is The_Project or a project extended by
      --  The_Project.

      -------------------
      -- Check_Project --
      -------------------

      function Check_Project (P : Project_Id) return Boolean is
      begin
         if All_Projects or P = The_Project then
            return True;
         elsif Extending then
            declare
               Data : Project_Data :=
                        Project_Tree.Projects.Table (The_Project);

            begin
               loop
                  if P = Data.Extends then
                     return True;
                  end if;

                  Data := Project_Tree.Projects.Table (Data.Extends);
                  exit when Data.Extends = No_Project;
               end loop;
            end;
         end if;

         return False;
      end Check_Project;

   --  Start of processing for Insert_Project_Sources

   begin
      --  For all the sources in the project files,

      for Id in Unit_Table.First ..
                Unit_Table.Last (Project_Tree.Units)
      loop
         Unit  := Project_Tree.Units.Table (Id);
         Sfile := No_Name;

         --  If there is a source for the body, and the body has not been
         --  locally removed,

         if Unit.File_Names (Body_Part).Name /= No_Name
           and then Unit.File_Names (Body_Part).Path /= Slash
         then
            --  And it is a source for the specified project

            if Check_Project (Unit.File_Names (Body_Part).Project) then

               --  If we don't have a spec, we cannot consider the source
               --  if it is a subunit

               if Unit.File_Names (Specification).Name = No_Name then
                  declare
                     Src_Ind : Source_File_Index;

                     --  Here we are cheating a little bit: we don't want to
                     --  use Sinput.L, because it depends on the GNAT tree
                     --  (Atree, Sinfo, ...). So, we pretend that it is
                     --  a project file, and we use Sinput.P.
                     --  Source_File_Is_Subunit is just scanning through
                     --  the file until it finds one of the reserved words
                     --  separate, procedure, function, generic or package.
                     --  Fortunately, these Ada reserved words are also
                     --  reserved for project files.

                  begin
                     Src_Ind := Sinput.P.Load_Project_File
                                  (Get_Name_String
                                     (Unit.File_Names (Body_Part).Path));

                     --  If it is a subunit, discard it

                     if Sinput.P.Source_File_Is_Subunit (Src_Ind) then
                        Sfile := No_Name;

                     else
                        Sfile := Unit.File_Names (Body_Part).Name;
                     end if;
                  end;

               else
                  Sfile := Unit.File_Names (Body_Part).Name;
               end if;
            end if;

         elsif Unit.File_Names (Specification).Name /= No_Name
           and then Unit.File_Names (Specification).Path /= Slash
           and then Check_Project (Unit.File_Names (Specification).Project)
         then
            --  If there is no source for the body, but there is a source
            --  for the spec which has not been locally removed, then we take
            --  this one.

            Sfile := Unit.File_Names (Specification).Name;
         end if;

         --  If Put_In_Q is True, we insert into the Q

         if Put_In_Q then

            --  For the first source inserted into the Q, we need
            --  to initialize the Q, but not for the subsequent sources.

            if First_Q_Initialization then
               Init_Q;
            end if;

            --  And of course, we only insert in the Q if the source
            --  is not marked.

            if Sfile /= No_Name and then not Is_Marked (Sfile) then
               if Verbose_Mode then
                  Write_Str ("Adding """);
                  Write_Str (Get_Name_String (Sfile));
                  Write_Line (""" to the queue");
               end if;

               Insert_Q (Sfile);
               Mark (Sfile);
            end if;

         elsif Sfile /= No_Name then

            --  If Put_In_Q is False, we add the source as it it were
            --  specified on the command line, and we set Put_In_Q to True,
            --  so that the following sources will be put directly in the
            --  queue. This will allow parallel compilation processes if -jx
            --  switch is used.

            if Verbose_Mode then
               Write_Str ("Adding """);
               Write_Str (Get_Name_String (Sfile));
               Write_Line (""" as if on the command line");
            end if;

            Osint.Add_File (Get_Name_String (Sfile));
            Put_In_Q := True;

            --  As we may look into the Q later, ensure the Q has been
            --  initialized to avoid errors.

            if First_Q_Initialization then
               Init_Q;
            end if;
         end if;
      end loop;
   end Insert_Project_Sources;

   --------------
   -- Insert_Q --
   --------------

   procedure Insert_Q
     (Source_File : File_Name_Type;
      Source_Unit : Unit_Name_Type := No_Name;
      Index       : Int            := 0)
   is
   begin
      if Debug.Debug_Flag_Q then
         Write_Str ("   Q := Q + [ ");
         Write_Name (Source_File);

         if Index /= 0 then
            Write_Str (", ");
            Write_Int (Index);
         end if;

         Write_Str (" ] ");
         Write_Eol;
      end if;

      Q.Table (Q.Last) :=
        (File  => Source_File,
         Unit  => Source_Unit,
         Index => Index);
      Q.Increment_Last;
   end Insert_Q;

   ---------------------
   -- Is_In_Obsoleted --
   ---------------------

   function Is_In_Obsoleted (F : Name_Id) return Boolean is
   begin
      if F = No_File then
         return False;

      else
         declare
            Name  : constant String := Get_Name_String (F);
            First : Natural := Name'Last;
            F2    : Name_Id := F;

         begin
            while First > Name'First
              and then Name (First - 1) /= Directory_Separator
              and then Name (First - 1) /= '/'
            loop
               First := First - 1;
            end loop;

            if First /= Name'First then
               Name_Len := 0;
               Add_Str_To_Name_Buffer (Name (First .. Name'Last));
               F2 := Name_Find;
            end if;

            return Obsoleted.Get (F2);
         end;
      end if;
   end Is_In_Obsoleted;

   ----------------------------
   -- Is_In_Object_Directory --
   ----------------------------

   function Is_In_Object_Directory
     (Source_File   : File_Name_Type;
      Full_Lib_File : File_Name_Type) return Boolean
   is
   begin
      --  There is something to check only when using project files.
      --  Otherwise, this function returns True (last line of the function).

      if Main_Project /= No_Project then
         declare
            Source_File_Name : constant String :=
                                 Get_Name_String (Source_File);
            Saved_Verbosity  : constant Verbosity := Current_Verbosity;
            Project          : Project_Id := No_Project;
            Path_Name        : Name_Id := No_Name;
            Data             : Project_Data;

         begin
            --  Call Get_Reference to know the ultimate extending project of
            --  the source. Call it with verbosity default to avoid verbose
            --  messages.

            Current_Verbosity := Default;
            Prj.Env.
              Get_Reference
              (Source_File_Name => Source_File_Name,
               Project          => Project,
               In_Tree          => Project_Tree,
               Path             => Path_Name);
            Current_Verbosity := Saved_Verbosity;

            --  If this source is in a project, check that the ALI file is
            --  in its object directory. If it is not, return False, so that
            --  the ALI file will not be skipped.

            --  If the source is not in an extending project, we fall back to
            --  the general case and return True at the end of the function.

            if Project /= No_Project
              and then Project_Tree.Projects.Table
                         (Project).Extends /= No_Project
            then
               Data := Project_Tree.Projects.Table (Project);

               declare
                  Object_Directory : constant String :=
                                       Normalize_Pathname
                                         (Get_Name_String
                                           (Data.Object_Directory));

                  Olast : Natural := Object_Directory'Last;

                  Lib_File_Directory : constant String :=
                                         Normalize_Pathname (Dir_Name
                                           (Get_Name_String (Full_Lib_File)));

                  Llast : Natural := Lib_File_Directory'Last;

               begin
                  --  For directories, Normalize_Pathname may or may not put
                  --  a directory separator at the end, depending on its input.
                  --  Remove any last directory separator before comparaison.
                  --  Returns True only if the two directories are the same.

                  if Object_Directory (Olast) = Directory_Separator then
                     Olast := Olast - 1;
                  end if;

                  if Lib_File_Directory (Llast) = Directory_Separator then
                     Llast := Llast - 1;
                  end if;

                  return Object_Directory (Object_Directory'First .. Olast) =
                        Lib_File_Directory (Lib_File_Directory'First .. Llast);
               end;
            end if;
         end;
      end if;

      --  When the source is not in a project file, always return True

      return True;
   end Is_In_Object_Directory;

   ----------
   -- Link --
   ----------

   procedure Link (ALI_File : File_Name_Type; Args : Argument_List) is
      Link_Args : Argument_List (1 .. Args'Length + 1);
      Success   : Boolean;

   begin
      Get_Name_String (ALI_File);
      Link_Args (1) := new String'(Name_Buffer (1 .. Name_Len));

      Link_Args (2 .. Args'Length + 1) :=  Args;

      GNAT.OS_Lib.Normalize_Arguments (Link_Args);

      Display (Gnatlink.all, Link_Args);

      if Gnatlink_Path = null then
         Make_Failed ("error, unable to locate ", Gnatlink.all);
      end if;

      GNAT.OS_Lib.Spawn (Gnatlink_Path.all, Link_Args, Success);

      if not Success then
         raise Link_Failed;
      end if;
   end Link;

   ---------------------------
   -- List_Bad_Compilations --
   ---------------------------

   procedure List_Bad_Compilations is
   begin
      for J in Bad_Compilation.First .. Bad_Compilation.Last loop
         if Bad_Compilation.Table (J).File = No_File then
            null;
         elsif not Bad_Compilation.Table (J).Found then
            Inform (Bad_Compilation.Table (J).File, "not found");
         else
            Inform (Bad_Compilation.Table (J).File, "compilation error");
         end if;
      end loop;
   end List_Bad_Compilations;

   -----------------
   -- List_Depend --
   -----------------

   procedure List_Depend is
      Lib_Name  : Name_Id;
      Obj_Name  : Name_Id;
      Src_Name  : Name_Id;

      Len       : Natural;
      Line_Pos  : Natural;
      Line_Size : constant := 77;

   begin
      Set_Standard_Output;

      for A in ALIs.First .. ALIs.Last loop
         Lib_Name := ALIs.Table (A).Afile;

         --  We have to provide the full library file name in In_Place_Mode

         if In_Place_Mode then
            Lib_Name := Full_Lib_File_Name (Lib_Name);
         end if;

         Obj_Name := Object_File_Name (Lib_Name);
         Write_Name (Obj_Name);
         Write_Str (" :");

         Get_Name_String (Obj_Name);
         Len := Name_Len;
         Line_Pos := Len + 2;

         for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
            Src_Name := Sdep.Table (D).Sfile;

            if Is_Internal_File_Name (Src_Name)
              and then not Check_Readonly_Files
            then
               null;
            else
               if not Quiet_Output then
                  Src_Name := Full_Source_Name (Src_Name);
               end if;

               Get_Name_String (Src_Name);
               Len := Name_Len;

               if Line_Pos + Len + 1 > Line_Size then
                  Write_Str (" \");
                  Write_Eol;
                  Line_Pos := 0;
               end if;

               Line_Pos := Line_Pos + Len + 1;

               Write_Str (" ");
               Write_Name (Src_Name);
            end if;
         end loop;

         Write_Eol;
      end loop;

      Set_Standard_Error;
   end List_Depend;

   -----------------
   -- Make_Failed --
   -----------------

   procedure Make_Failed (S1 : String; S2 : String := ""; S3 : String := "") is
   begin
      Delete_All_Temp_Files;
      Osint.Fail (S1, S2, S3);
   end Make_Failed;

   --------------------
   -- Mark_Directory --
   --------------------

   procedure Mark_Directory
     (Dir             : String;
      Mark            : Lib_Mark_Type;
      On_Command_Line : Boolean)
   is
      N : Name_Id;
      B : Byte;

   begin
      if On_Command_Line then
         declare
            Real_Path : constant String :=
              Normalize_Pathname (Dir);

         begin
            if Real_Path'Length = 0 then
               Name_Len := Dir'Length;
               Name_Buffer (1 .. Name_Len) := Dir;

            else
               Name_Len := Real_Path'Length;
               Name_Buffer (1 .. Name_Len) := Real_Path;
            end if;
         end;

      else
         declare
            Real_Path : constant String :=
              Normalize_Pathname
                (Dir,
                 Get_Name_String
                   (Project_Tree.Projects.Table (Main_Project).Directory));

         begin
            if Real_Path'Length = 0 then
               Name_Len := Dir'Length;
               Name_Buffer (1 .. Name_Len) := Dir;

            else
               Name_Len := Real_Path'Length;
               Name_Buffer (1 .. Name_Len) := Real_Path;
            end if;
         end;
      end if;

      --  Last character is supposed to be a directory separator

      if not Is_Directory_Separator (Name_Buffer (Name_Len)) then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Directory_Separator;
      end if;

      --  Add flags to the already existing flags

      N := Name_Find;
      B := Get_Name_Table_Byte (N);
      Set_Name_Table_Byte (N, B or Mark);
   end Mark_Directory;

   -----------------------------
   -- Recursive_Compute_Depth --
   -----------------------------

   procedure Recursive_Compute_Depth
     (Project : Project_Id;
      Depth   : Natural)
   is
      List : Project_List;
      Proj : Project_Id;

   begin
      --  Nothing to do if there is no project or if the project has already
      --  been seen or if the depth is large enough.

      if Project = No_Project
        or else Project_Tree.Projects.Table (Project).Seen
        or else Project_Tree.Projects.Table (Project).Depth >= Depth
      then
         return;
      end if;

      Project_Tree.Projects.Table (Project).Depth := Depth;

      --  Mark the project as Seen to avoid endless loop caused by limited
      --  withs.

      Project_Tree.Projects.Table (Project).Seen := True;

      List := Project_Tree.Projects.Table (Project).Imported_Projects;

      --  Visit each imported project

      while List /= Empty_Project_List loop
         Proj := Project_Tree.Project_Lists.Table (List).Project;
         List := Project_Tree.Project_Lists.Table (List).Next;
         Recursive_Compute_Depth
           (Project => Proj,
            Depth => Depth + 1);
      end loop;

      --  Visit a project being extended, if any

      Recursive_Compute_Depth
        (Project => Project_Tree.Projects.Table (Project).Extends,
         Depth   => Depth + 1);

      --  Reset the Seen flag, as we leave this project

      Project_Tree.Projects.Table (Project).Seen := False;
   end Recursive_Compute_Depth;

   -----------------------
   -- Sigint_Intercpted --
   -----------------------

   procedure Sigint_Intercepted is
   begin
      Write_Line ("*** Interrupted ***");
      Delete_All_Temp_Files;
      OS_Exit (1);
   end Sigint_Intercepted;

   -------------------
   -- Scan_Make_Arg --
   -------------------

   procedure Scan_Make_Arg (Argv : String; And_Save : Boolean) is
   begin
      pragma Assert (Argv'First = 1);

      if Argv'Length = 0 then
         return;
      end if;

      --  If the previous switch has set the Project_File_Name_Present
      --  flag (that is we have seen a -P alone), then the next argument is
      --  the name of the project file.

      if Project_File_Name_Present and then Project_File_Name = null then
         if Argv (1) = '-' then
            Make_Failed ("project file name missing after -P");

         else
            Project_File_Name_Present := False;
            Project_File_Name := new String'(Argv);
         end if;

      --  If the previous switch has set the Output_File_Name_Present
      --  flag (that is we have seen a -o), then the next argument is
      --  the name of the output executable.

      elsif Output_File_Name_Present
        and then not Output_File_Name_Seen
      then
         Output_File_Name_Seen := True;

         if Argv (1) = '-' then
            Make_Failed ("output file name missing after -o");

         else
            Add_Switch ("-o", Linker, And_Save => And_Save);

            --  Automatically add the executable suffix if it has not been
            --  specified explicitly.

            declare
               Canonical_Argv : String := Argv;
            begin
               --  Get the file name in canonical case to accept as is
               --  names ending with ".EXE" on VMS and Windows.

               Canonical_Case_File_Name (Canonical_Argv);

               if Executable_Suffix'Length /= 0
                 and then (Canonical_Argv'Length <= Executable_Suffix'Length
                        or else Canonical_Argv
                                  (Canonical_Argv'Last -
                                   Executable_Suffix'Length + 1
                                   .. Canonical_Argv'Last)
                                /= Executable_Suffix)
               then
                  Add_Switch
                    (Argv & Executable_Suffix,
                     Linker,
                     And_Save => And_Save);
               else
                  Add_Switch (Argv, Linker, And_Save => And_Save);
               end if;
            end;
         end if;

      --  If the previous switch has set the Object_Directory_Present flag
      --  (that is we have seen a -D), then the next argument is
      --  the path name of the object directory..

      elsif Object_Directory_Present
        and then not Object_Directory_Seen
      then
         Object_Directory_Seen := True;

         if Argv (1) = '-' then
            Make_Failed ("object directory path name missing after -D");

         elsif not Is_Directory (Argv) then
            Make_Failed ("cannot find object directory """, Argv, """");

         else
            Add_Lib_Search_Dir (Argv);

            --  Specify the object directory to the binder

            Add_Switch ("-aO" & Argv, Binder, And_Save => And_Save);

            --  Record the object directory. Make sure it ends with a directory
            --  separator.

            if Argv (Argv'Last) = Directory_Separator then
               Object_Directory_Path := new String'(Argv);

            else
               Object_Directory_Path :=
                 new String'(Argv & Directory_Separator);
            end if;
         end if;

      --  Then check if we are dealing with -cargs/-bargs/-largs/-margs

      elsif Argv = "-bargs"
              or else
            Argv = "-cargs"
              or else
            Argv = "-largs"
              or else
            Argv = "-margs"
      then
         case Argv (2) is
            when 'c' => Program_Args := Compiler;
            when 'b' => Program_Args := Binder;
            when 'l' => Program_Args := Linker;
            when 'm' => Program_Args := None;

            when others =>
               raise Program_Error;
         end case;

      --  A special test is needed for the -o switch within a -largs
      --  since that is another way to specify the name of the final
      --  executable.

      elsif Program_Args = Linker
        and then Argv = "-o"
      then
         Make_Failed ("switch -o not allowed within a -largs. " &
                      "Use -o directly.");

      --  Check to see if we are reading switches after a -cargs,
      --  -bargs or -largs switch. If yes save it.

      elsif Program_Args /= None then

         --  Check to see if we are reading -I switches in order
         --  to take into account in the src & lib search directories.

         if Argv'Length > 2 and then Argv (1 .. 2) = "-I" then
            if Argv (3 .. Argv'Last) = "-" then
               Look_In_Primary_Dir := False;

            elsif Program_Args = Compiler then
               if Argv (3 .. Argv'Last) /= "-" then
                  Add_Source_Search_Dir (Argv (3 .. Argv'Last), And_Save);
               end if;

            elsif Program_Args = Binder then
               Add_Library_Search_Dir (Argv (3 .. Argv'Last), And_Save);
            end if;
         end if;

         Add_Switch (Argv, Program_Args, And_Save => And_Save);

      --  Handle non-default compiler, binder, linker, and handle --RTS switch

      elsif Argv'Length > 2 and then Argv (1 .. 2) = "--" then
         if Argv'Length > 6
           and then Argv (1 .. 6) = "--GCC="
         then
            declare
               Program_Args : constant Argument_List_Access :=
                                Argument_String_To_List
                                  (Argv (7 .. Argv'Last));

            begin
               if And_Save then
                  Saved_Gcc := new String'(Program_Args.all (1).all);
               else
                  Gcc := new String'(Program_Args.all (1).all);
               end if;

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch
                    (Program_Args.all (J).all,
                     Compiler,
                     And_Save => And_Save);
               end loop;
            end;

         elsif Argv'Length > 11
           and then Argv (1 .. 11) = "--GNATBIND="
         then
            declare
               Program_Args : constant Argument_List_Access :=
                                Argument_String_To_List
                                  (Argv (12 .. Argv'Last));

            begin
               if And_Save then
                  Saved_Gnatbind := new String'(Program_Args.all (1).all);
               else
                  Gnatbind := new String'(Program_Args.all (1).all);
               end if;

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch
                    (Program_Args.all (J).all, Binder, And_Save => And_Save);
               end loop;
            end;

         elsif Argv'Length > 11
           and then Argv (1 .. 11) = "--GNATLINK="
         then
            declare
               Program_Args : constant Argument_List_Access :=
                                Argument_String_To_List
                                  (Argv (12 .. Argv'Last));
            begin
               if And_Save then
                  Saved_Gnatlink := new String'(Program_Args.all (1).all);
               else
                  Gnatlink := new String'(Program_Args.all (1).all);
               end if;

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch (Program_Args.all (J).all, Linker);
               end loop;
            end;

         elsif Argv'Length >= 5 and then
           Argv (1 .. 5) = "--RTS"
         then
            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Add_Switch (Argv, Binder, And_Save => And_Save);

            if Argv'Length <= 6 or else Argv (6) /= '=' then
               Make_Failed ("missing path for --RTS");

            else
               --  Check that this is the first time we see this switch or
               --  if it is not the first time, the same path is specified.

               if RTS_Specified = null then
                  RTS_Specified := new String'(Argv (7 .. Argv'Last));

               elsif RTS_Specified.all /= Argv (7 .. Argv'Last) then
                  Make_Failed ("--RTS cannot be specified multiple times");
               end if;

               --  Valid --RTS switch

               No_Stdinc := True;
               No_Stdlib := True;
               RTS_Switch := True;

               declare
                  Src_Path_Name : constant String_Ptr :=
                                    Get_RTS_Search_Dir
                                      (Argv (7 .. Argv'Last), Include);

                  Lib_Path_Name : constant String_Ptr :=
                                    Get_RTS_Search_Dir
                                      (Argv (7 .. Argv'Last), Objects);

               begin
                  if Src_Path_Name /= null and then
                    Lib_Path_Name /= null
                  then
                     --  Set the RTS_*_Path_Name variables, so that the correct
                     --  directories will be set when
                     --  Osint.Add_Default_Search_Dirs will be called later.

                     RTS_Src_Path_Name := Src_Path_Name;
                     RTS_Lib_Path_Name := Lib_Path_Name;

                  elsif  Src_Path_Name = null
                    and Lib_Path_Name = null then
                     Make_Failed ("RTS path not valid: missing " &
                                  "adainclude and adalib directories");

                  elsif Src_Path_Name = null then
                     Make_Failed ("RTS path not valid: missing adainclude " &
                                  "directory");

                  elsif  Lib_Path_Name = null then
                     Make_Failed ("RTS path not valid: missing adalib " &
                                  "directory");
                  end if;
               end;
            end if;

         else
            Make_Failed ("unknown switch: ", Argv);
         end if;

      --  If we have seen a regular switch process it

      elsif Argv (1) = '-' then

         if Argv'Length = 1 then
            Make_Failed ("switch character cannot be followed by a blank");

         --  -I-

         elsif Argv (2 .. Argv'Last) = "I-" then
            Look_In_Primary_Dir := False;

         --  Forbid  -?-  or  -??-  where ? is any character

         elsif (Argv'Length = 3 and then Argv (3) = '-')
           or else (Argv'Length = 4 and then Argv (4) = '-')
         then
            Make_Failed ("trailing ""-"" at the end of ", Argv, " forbidden.");

         --  -Idir

         elsif Argv (2) = 'I' then
            Add_Source_Search_Dir (Argv (3 .. Argv'Last), And_Save);
            Add_Library_Search_Dir (Argv (3 .. Argv'Last), And_Save);
            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Add_Switch (Argv, Binder, And_Save => And_Save);

         --  -aIdir (to gcc this is like a -I switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aI" then
            Add_Source_Search_Dir (Argv (4 .. Argv'Last), And_Save);
            Add_Switch ("-I" & Argv (4 .. Argv'Last),
                        Compiler,
                        And_Save => And_Save);
            Add_Switch (Argv, Binder, And_Save => And_Save);

         --  -aOdir

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aO" then
            Add_Library_Search_Dir (Argv (4 .. Argv'Last), And_Save);
            Add_Switch (Argv, Binder, And_Save => And_Save);

         --  -aLdir (to gnatbind this is like a -aO switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aL" then
            Mark_Directory (Argv (4 .. Argv'Last), Ada_Lib_Dir, And_Save);
            Add_Library_Search_Dir (Argv (4 .. Argv'Last), And_Save);
            Add_Switch ("-aO" & Argv (4 .. Argv'Last),
                        Binder,
                        And_Save => And_Save);

         --  -Adir (to gnatbind this is like a -aO switch, to gcc like a -I)

         elsif Argv (2) = 'A' then
            Mark_Directory (Argv (3 .. Argv'Last), Ada_Lib_Dir, And_Save);
            Add_Source_Search_Dir (Argv (3 .. Argv'Last), And_Save);
            Add_Library_Search_Dir (Argv (3 .. Argv'Last), And_Save);
            Add_Switch ("-I"  & Argv (3 .. Argv'Last),
                        Compiler,
                        And_Save => And_Save);
            Add_Switch ("-aO" & Argv (3 .. Argv'Last),
                        Binder,
                        And_Save => And_Save);

         --  -Ldir

         elsif Argv (2) = 'L' then
            Add_Switch (Argv, Linker, And_Save => And_Save);

         --  For -gxxxxx, -pg, -mxxx, -fxxx: give the switch to both the
         --  compiler and the linker (except for -gnatxxx which is only for
         --  the compiler). Some of the -mxxx (for example -m64) and -fxxx
         --  (for example -ftest-coverage for gcov) need to be used when
         --  compiling the binder generated files, and using all these gcc
         --  switches for the binder generated files should not be a problem.

         elsif
           (Argv (2) = 'g' and then (Argv'Last < 5
                                       or else Argv (2 .. 5) /= "gnat"))
             or else Argv (2 .. Argv'Last) = "pg"
             or else (Argv (2) = 'm' and then Argv'Last > 2)
             or else (Argv (2) = 'f' and then Argv'Last > 2)
         then
            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Add_Switch (Argv, Linker, And_Save => And_Save);

         --  -C=<mapping file>

         elsif Argv'Last > 2 and then Argv (2) = 'C' then
            if And_Save then
               if Argv (3) /= '=' or else Argv'Last <= 3 then
                  Make_Failed ("illegal switch ", Argv);
               end if;

               Gnatmake_Mapping_File := new String'(Argv (4 .. Argv'Last));
            end if;

         --  -D

         elsif Argv'Last = 2 and then Argv (2) = 'D' then
            if Project_File_Name /= null then
               Make_Failed ("-D cannot be used in conjunction with a " &
                            "project file");

            else
               Scan_Make_Switches (Argv);
            end if;

         --  -d

         elsif Argv (2) = 'd'
           and then Argv'Last = 2
         then
            Display_Compilation_Progress := True;

         --  -i

         elsif Argv'Last = 2 and then Argv (2) = 'i' then
            if Project_File_Name /= null then
               Make_Failed ("-i cannot be used in conjunction with a " &
                            "project file");

            else
               Scan_Make_Switches (Argv);
            end if;

         --  -j (need to save the result)

         elsif Argv (2) = 'j' then
            Scan_Make_Switches (Argv);

            if And_Save then
               Saved_Maximum_Processes := Maximum_Processes;
            end if;

         --  -m

         elsif Argv (2) = 'm'
           and then Argv'Last = 2
         then
            Minimal_Recompilation := True;

         --  -u

         elsif Argv (2) = 'u'
           and then Argv'Last = 2
         then
            Unique_Compile   := True;
            Compile_Only := True;
            Do_Bind_Step     := False;
            Do_Link_Step     := False;

         --  -U

         elsif Argv (2) = 'U'
           and then Argv'Last = 2
         then
            Unique_Compile_All_Projects := True;
            Unique_Compile   := True;
            Compile_Only := True;
            Do_Bind_Step     := False;
            Do_Link_Step     := False;

         --  -Pprj or -P prj (only once, and only on the command line)

         elsif Argv (2) = 'P' then
            if Project_File_Name /= null then
               Make_Failed ("cannot have several project files specified");

            elsif Object_Directory_Path /= null then
               Make_Failed ("-D cannot be used in conjunction with a " &
                            "project file");

            elsif In_Place_Mode then
               Make_Failed ("-i cannot be used in conjunction with a " &
                            "project file");

            elsif not And_Save then

               --  It could be a tool other than gnatmake (i.e, gnatdist)
               --  or a -P switch inside a project file.

               Fail
                 ("either the tool is not ""project-aware"" or " &
                  "a project file is specified inside a project file");

            elsif Argv'Last = 2 then

               --  -P is used alone: the project file name is the next option

               Project_File_Name_Present := True;

            else
               Project_File_Name := new String'(Argv (3 .. Argv'Last));
            end if;

         --  -vPx  (verbosity of the parsing of the project files)

         elsif Argv'Last = 4
           and then Argv (2 .. 3) = "vP"
           and then Argv (4) in '0' .. '2'
         then
            if And_Save then
               case Argv (4) is
                  when '0' =>
                     Current_Verbosity := Prj.Default;
                  when '1' =>
                     Current_Verbosity := Prj.Medium;
                  when '2' =>
                     Current_Verbosity := Prj.High;
                  when others =>
                     null;
               end case;
            end if;

         --  -Xext=val  (External assignment)

         elsif Argv (2) = 'X'
           and then Is_External_Assignment (Argv)
         then
            --  Is_External_Assignment has side effects
            --  when it returns True;

            null;

         --  If -gnath is present, then generate the usage information
         --  right now and do not pass this option on to the compiler calls.

         elsif Argv = "-gnath" then
            Usage;

         --  If -gnatc is specified, make sure the bind step and the link
         --  step are not executed.

         elsif Argv'Length >= 6 and then Argv (2 .. 6) = "gnatc" then

            --  If -gnatc is specified, make sure the bind step and the link
            --  step are not executed.

            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Operating_Mode := Check_Semantics;
            Check_Object_Consistency := False;
            Compile_Only             := True;
            Do_Bind_Step                 := False;
            Do_Link_Step                 := False;

         elsif Argv (2 .. Argv'Last) = "nostdlib" then

            --  Don't pass -nostdlib to gnatlink, it will disable
            --  linking with all standard library files.

            No_Stdlib := True;

            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Add_Switch (Argv, Binder, And_Save => And_Save);

         elsif Argv (2 .. Argv'Last) = "nostdinc" then

            --  Pass -nostdinc to the Compiler and to gnatbind

            No_Stdinc := True;
            Add_Switch (Argv, Compiler, And_Save => And_Save);
            Add_Switch (Argv, Binder, And_Save => And_Save);

            --  By default all switches with more than one character
            --  or one character switches which are not in 'a' .. 'z'
            --  (except 'C', 'F', 'M' and 'B') are passed to the compiler,
            --  unless we are dealing with a debug switch (starts with 'd')
            --  or an extended gnatmake switch (starts with 'e').

         elsif Argv (2) /= 'd'
           and then Argv (2) /= 'e'
           and then Argv (2 .. Argv'Last) /= "C"
           and then Argv (2 .. Argv'Last) /= "F"
           and then Argv (2 .. Argv'Last) /= "M"
           and then Argv (2 .. Argv'Last) /= "B"
           and then Argv (2 .. Argv'Last) /= "vl"
           and then Argv (2 .. Argv'Last) /= "vm"
           and then Argv (2 .. Argv'Last) /= "vh"
           and then (Argv'Length > 2 or else Argv (2) not in 'a' .. 'z')
         then
            Add_Switch (Argv, Compiler, And_Save => And_Save);

         --  All other options are handled by Scan_Make_Switches

         else
            Scan_Make_Switches (Argv);
         end if;

      --  If not a switch it must be a file name

      else
         Add_File (Argv);
         Mains.Add_Main (Argv);
      end if;
   end Scan_Make_Arg;

   -----------------
   -- Switches_Of --
   -----------------

   function Switches_Of
     (Source_File      : Name_Id;
      Source_File_Name : String;
      Source_Index     : Int;
      Naming           : Naming_Data;
      In_Package       : Package_Id;
      Allow_ALI        : Boolean) return Variable_Value
   is
      Switches : Variable_Value;

      Defaults : constant Array_Element_Id :=
                   Prj.Util.Value_Of
                     (Name      => Name_Default_Switches,
                      In_Arrays =>
                        Project_Tree.Packages.Table
                          (In_Package).Decl.Arrays,
                      In_Tree   => Project_Tree);

      Switches_Array : constant Array_Element_Id :=
                         Prj.Util.Value_Of
                           (Name      => Name_Switches,
                            In_Arrays =>
                              Project_Tree.Packages.Table
                                (In_Package).Decl.Arrays,
                            In_Tree   => Project_Tree);

   begin
      Switches :=
        Prj.Util.Value_Of
          (Index     => Source_File,
           Src_Index => Source_Index,
           In_Array  => Switches_Array,
           In_Tree   => Project_Tree);

      if Switches = Nil_Variable_Value then
         declare
            Name        : String (1 .. Source_File_Name'Length + 3);
            Last        : Positive := Source_File_Name'Length;
            Spec_Suffix : constant String :=
                            Get_Name_String (Naming.Ada_Spec_Suffix);
            Body_Suffix : constant String :=
                            Get_Name_String (Naming.Ada_Body_Suffix);
            Truncated   : Boolean := False;

         begin
            Name (1 .. Last) := Source_File_Name;

            if Last > Body_Suffix'Length
               and then Name (Last - Body_Suffix'Length + 1 .. Last) =
                                                                  Body_Suffix
            then
               Truncated := True;
               Last := Last - Body_Suffix'Length;
            end if;

            if not Truncated
              and then Last > Spec_Suffix'Length
              and then Name (Last - Spec_Suffix'Length + 1 .. Last) =
                                                                 Spec_Suffix
            then
               Truncated := True;
               Last := Last - Spec_Suffix'Length;
            end if;

            if Truncated then
               Name_Len := Last;
               Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
               Switches :=
                 Prj.Util.Value_Of
                   (Index     => Name_Find,
                    Src_Index => 0,
                    In_Array  => Switches_Array,
                    In_Tree   => Project_Tree);

               if Switches = Nil_Variable_Value
                 and then Allow_ALI
               then
                  Last := Source_File_Name'Length;

                  while Name (Last) /= '.' loop
                     Last := Last - 1;
                  end loop;

                  Name (Last + 1 .. Last + 3) := "ali";
                  Name_Len := Last + 3;
                  Name_Buffer (1 .. Name_Len) := Name (1 .. Name_Len);
                  Switches :=
                    Prj.Util.Value_Of
                      (Index     => Name_Find,
                       Src_Index => 0,
                       In_Array  => Switches_Array,
                       In_Tree   => Project_Tree);
               end if;
            end if;
         end;
      end if;

      if Switches = Nil_Variable_Value then
         Switches :=
           Prj.Util.Value_Of
             (Index     => Name_Ada,
              Src_Index => 0,
              In_Array  => Defaults,
              In_Tree   => Project_Tree);
      end if;

      return Switches;
   end Switches_Of;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if Usage_Needed then
         Usage_Needed := False;
         Makeusg;
      end if;
   end Usage;

   -----------------
   -- Verbose_Msg --
   -----------------

   procedure Verbose_Msg
     (N1                : Name_Id;
      S1                : String;
      N2                : Name_Id := No_Name;
      S2                : String  := "";
      Prefix            : String := "  -> ";
      Minimum_Verbosity : Verbosity_Level_Type := Opt.Low)
   is
   begin
      if (not Verbose_Mode) or else (Minimum_Verbosity > Verbosity_Level) then
         return;
      end if;

      Write_Str (Prefix);
      Write_Str ("""");
      Write_Name (N1);
      Write_Str (""" ");
      Write_Str (S1);

      if N2 /= No_Name then
         Write_Str (" """);
         Write_Name (N2);
         Write_Str (""" ");
      end if;

      Write_Str (S2);
      Write_Eol;
   end Verbose_Msg;

begin
   --  Make sure that in case of failure, the temp files will be deleted

   Prj.Com.Fail    := Make_Failed'Access;
   MLib.Fail       := Make_Failed'Access;
   Makeutl.Do_Fail := Make_Failed'Access;
end Make;
