------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M A K E                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with ALI;       use ALI;
with ALI.Util;  use ALI.Util;
with Csets;
with Debug;
with Fmap;
with Fname;     use Fname;
with Fname.SF;
with Fname.UF;  use Fname.UF;
with Gnatvsn;   use Gnatvsn;
with Hostparm;  use Hostparm;
with Makeusg;
with Make_Util; use Make_Util;
with Namet;     use Namet;
with Opt;       use Opt;
with Osint.M;   use Osint.M;
with Osint;     use Osint;
with Output;    use Output;
with SFN_Scan;
with Sinput;
with Snames;
with Stringt;

pragma Warnings (Off);
with System.HTable;
pragma Warnings (On);

with Switch;   use Switch;
with Switch.M; use Switch.M;
with Table;
with Targparm;
with Tempdir;
with Types;    use Types;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;   use Ada.Exceptions;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

package body Make is

   use ASCII;
   --  Make control characters visible

   Standard_Library_Package_Body_Name : constant String := "s-stalib.adb";
   System_Package_Spec_Name : constant String := "system.ads";
   --  Every program depends on one of these packages: usually the first one,
   --  or if Supress_Standard_Library is true on the second one. The dependency
   --  is not always explicit and considering it is important when -f and -a
   --  are used.

   type Sigint_Handler is access procedure;
   pragma Convention (C, Sigint_Handler);

   procedure Install_Int_Handler (Handler : Sigint_Handler);
   pragma Import (C, Install_Int_Handler, "__gnat_install_int_handler");
   --  Called by Gnatmake to install the SIGINT handler below

   procedure Sigint_Intercepted;
   pragma Convention (C, Sigint_Intercepted);
   pragma No_Return (Sigint_Intercepted);
   --  Called when the program is interrupted by Ctrl-C to delete the
   --  temporary mapping files and configuration pragmas files.

   No_Mapping_File : constant Natural := 0;

   type Compilation_Data is record
      Pid              : Process_Id;
      Full_Source_File : File_Name_Type;
      Lib_File         : File_Name_Type;
      Source_Unit      : Unit_Name_Type;
      Full_Lib_File    : File_Name_Type;
      Lib_File_Attr    : aliased File_Attributes;
      Mapping_File     : Natural := No_Mapping_File;
   end record;
   --  Data recorded for each compilation process spawned

   No_Compilation_Data : constant Compilation_Data :=
     (Invalid_Pid, No_File, No_File, No_Unit_Name, No_File, Unknown_Attributes,
      No_Mapping_File);

   type Comp_Data_Arr is array (Positive range <>) of Compilation_Data;
   type Comp_Data_Ptr is access Comp_Data_Arr;
   Running_Compile : Comp_Data_Ptr;
   --  Used to save information about outstanding compilations

   Outstanding_Compiles : Natural := 0;
   --  Current number of outstanding compiles

   procedure Initialize;

   Project_File_Name_Present : Boolean := False;

   Project_File_Name : String_Access := null;

   -------------------------
   -- Note on terminology --
   -------------------------

   --  In this program, we use the phrase "termination" of a file name to refer
   --  to the suffix that appears after the unit name portion. Very often this
   --  is simply the extension, but in some cases, the sequence may be more
   --  complex, for example in main.1.ada, the termination in this name is
   --  ".1.ada" and in main_.ada the termination is "_.ada".

   Unique_Compile : Boolean := False;
   --  Set to True if -u or -U is used

   Must_Compile : Boolean := False;
   --  True if gnatmake is invoked with -f -u and one or several mains on the
   --  command line.

   Main_On_Command_Line : Boolean := False;
   --  True if gnatmake is invoked with one or several mains on the command
   --  line.

   RTS_Specified : String_Access := null;
   --  Used to detect multiple --RTS= switches

   N_M_Switch : Natural := 0;
   --  Used to count -mxxx switches that can affect multilib

   --  The 3 following packages are used to store gcc, gnatbind and gnatlink
   --  switches found in the project files.

   package Gcc_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Gcc_Switches");

   package Binder_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Binder_Switches");

   package Linker_Switches is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Linker_Switches");

   package Switches_To_Check is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Make.Switches_To_Check");

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

   Normalized_Switches : Argument_List_Access := new Argument_List (1 .. 10);
   Last_Norm_Switch    : Natural := 0;

   Map_File : String_Access := null;
   --  Value of switch --create-map-file

   procedure Add_Library_Search_Dir (Path : String);
   --  Call Add_Lib_Search_Dir with an absolute directory path. If Path is
   --  relative path,, it is relative to the current working directory.

   procedure Add_Source_Search_Dir (Path : String);
   --  Call Add_Src_Search_Dir with an absolute directory path. If Path is a
   --  relative path, it is relative to the current working directory.

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
   --  Flags to indicate what step should be executed. Can be set to False
   --  with the switches -c, -b and -l. These flags are reset to True for
   --  each invocation of procedure Gnatmake.

   CodePeer_Mode_String    : aliased String := "-P";

   No_Shared_Switch : aliased Argument_List := (1 .. 0 => null);
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

   procedure Make_Failed (S : String);
   pragma No_Return (Make_Failed);
   --  Delete all temp files created by Gnatmake and call Osint.Fail, with the
   --  parameter S (see osint.ads).

   --------------------------
   -- Obsolete Executables --
   --------------------------

   Executable_Obsolete : Boolean := False;
   --  Executable_Obsolete is initially set to False for each executable,
   --  and is set to True whenever one of the source of the executable is
   --  compiled, or has already been compiled for another executable.

   Max_Header : constant := 200;
   --  This needs a proper comment, it used to say "arbitrary" that's not an
   --  adequate comment ???

   type Header_Num is range 1 .. Max_Header;
   --  Header_Num for the hash table Obsoleted below

   function Hash (F : File_Name_Type) return Header_Num;
   --  Hash function for the hash table Obsoleted below

   package Obsoleted is new System.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to keep all files that have been compiled, to detect
   --  if an executable is up to date or not.

   procedure Enter_Into_Obsoleted (F : File_Name_Type);
   --  Enter a file name, without directory information, into the hash table
   --  Obsoleted.

   function Is_In_Obsoleted (F : File_Name_Type) return Boolean;
   --  Check if a file name, without directory information, has already been
   --  entered into the hash table Obsoleted.

   type Dependency is record
      This       : File_Name_Type;
      Depends_On : File_Name_Type;
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
   --  is obsolete. More explanation needed ???

   ----------------------------
   -- Arguments and Switches --
   ----------------------------

   Arguments : Argument_List_Access;
   --  Used to gather the arguments for invocation of the compiler

   Last_Argument : Natural := 0;
   --  Last index of arguments in Arguments above

   Dummy_Switch : constant String_Access := new String'("- ");
   --  Used to initialized Prev_Switch in procedure Check

   procedure Add_Arguments (Args : Argument_List);
   --  Add arguments to global variable Arguments, increasing its size
   --  if necessary and adjusting Last_Argument.

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

   procedure List_Bad_Compilations;
   --  Prints out the list of all files for which the compilation failed

   Usage_Needed : Boolean := True;
   --  Flag used to make sure Makeusg is call at most once

   procedure Usage;
   --  Call Makeusg, if Usage_Needed is True.
   --  Set Usage_Needed to False.

   procedure Debug_Msg (S : String; N : Name_Id);
   procedure Debug_Msg (S : String; N : File_Name_Type);
   procedure Debug_Msg (S : String; N : Unit_Name_Type);
   --  If Debug.Debug_Flag_W is set outputs string S followed by name N

   -----------------------
   -- Gnatmake Routines --
   -----------------------

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

   function In_Ada_Lib_Dir (File : File_Name_Type) return Boolean;
   --  Get directory prefix of this file and get lib mark stored in name
   --  table for this directory. Then check if an Ada lib mark has been set.

   procedure Mark_Directory (Dir : String; Mark : Lib_Mark_Type);
   --  Store the absolute path from Dir in name table and set lib mark as name
   --  info to identify Ada libraries.
   --
   --  If Dir is a relative path, it is relative to the current working
   --  directory.

   Output_Is_Object : Boolean := True;
   --  Set to False when using a switch -S for the compiler

   procedure Check_For_S_Switch;
   --  Set Output_Is_Object to False when the -S switch is used for the
   --  compiler.

   procedure Process_Multilib;
   --  Add appropriate --RTS argument to handle multilib

   procedure Compute_Executable
     (Main_Source_File   : File_Name_Type;
      Executable         : out File_Name_Type;
      Non_Std_Executable : out Boolean);
   --  Parse the linker switches and project file to compute the name of the
   --  executable to generate.
   --  ??? What is the meaning of Non_Std_Executable

   procedure Compilation_Phase
     (Main_Source_File           : File_Name_Type;
      Current_Main_Index         : Int := 0;
      Total_Compilation_Failures : in out Natural;
      Executable                 : File_Name_Type := No_File;
      Stop_Compile               : out Boolean);
   --  Build all source files for a given main file
   --
   --  Current_Main_Index, if not zero, is the index of the current main unit
   --  in its source file.
   --
   --  Stand_Alone_Libraries is set to True when there are Stand-Alone
   --  Libraries, so that gnatbind is invoked with the -F switch to force
   --  checking of elaboration flags.
   --
   --  Stop_Compile is set to true if we should not try to compile any more
   --  of the main units

   procedure Binding_Phase
     (Main_ALI_File : File_Name_Type);
   --  Stand_Alone_Libraries should be set to True when there are Stand-Alone
   --  Libraries, so that gnatbind is invoked with the -F switch to force
   --  checking of elaboration flags.

   procedure Linking_Phase
     (Non_Std_Executable : Boolean := False;
      Executable         : File_Name_Type := No_File;
      Main_ALI_File      : File_Name_Type);
   --  Perform the link of a single executable. The ali file corresponds
   --  to Main_ALI_File. Executable is the file name of an executable.
   --  Non_Std_Executable is set to True when there is a possibility that
   --  the linker will not choose the correct executable file name.

   ----------------------------------------------------
   -- Compiler, Binder & Linker Data and Subprograms --
   ----------------------------------------------------

   Gcc      : String_Access := Program_Name ("gcc", "gnatmake");
   Gnatbind : String_Access := Program_Name ("gnatbind", "gnatmake");
   Gnatlink : String_Access := Program_Name ("gnatlink", "gnatmake");
   --  Default compiler, binder, linker programs

   Gcc_Path      : String_Access :=
                     GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);
   Gnatbind_Path : String_Access :=
                     GNAT.OS_Lib.Locate_Exec_On_Path (Gnatbind.all);
   Gnatlink_Path : String_Access :=
                     GNAT.OS_Lib.Locate_Exec_On_Path (Gnatlink.all);
   --  Path for compiler, binder, linker programs, defaulted now for gnatdist.
   --  Changed later if overridden on command line.

   Comp_Flag         : constant String_Access := new String'("-c");
   Output_Flag       : constant String_Access := new String'("-o");
   Ada_Flag_1        : constant String_Access := new String'("-x");
   Ada_Flag_2        : constant String_Access := new String'("ada");
   AdaSCIL_Flag      : constant String_Access := new String'("adascil");
   GNAT_Flag         : constant String_Access := new String'("-gnatpg");
   Do_Not_Check_Flag : constant String_Access := new String'("-x");

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;

   Syntax_Only : Boolean := False;
   --  Set to True when compiling with -gnats

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

   procedure Add_Switch
     (S             : String_Access;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True);
   procedure Add_Switch
     (S             : String;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True);
   --  Make invokes one of three programs (the compiler, the binder or the
   --  linker). For the sake of convenience, some program specific switches
   --  can be passed directly on the gnatmake command line. This procedure
   --  records these switches so that gnatmake can pass them to the right
   --  program. S is the switch to be added at the end of the command line
   --  for Program if Append_Switch is True. If Append_Switch is False S is
   --  added at the beginning of the command line.

   procedure Check
     (Source_File    : File_Name_Type;
      The_Args       : Argument_List;
      Lib_File       : File_Name_Type;
      Full_Lib_File  : File_Name_Type;
      Lib_File_Attr  : access File_Attributes;
      Read_Only      : Boolean;
      ALI            : out ALI_Id;
      O_File         : out File_Name_Type;
      O_Stamp        : out Time_Stamp_Type);
   --  Determines whether the library file Lib_File is up-to-date or not. The
   --  full name (with path information) of the object file corresponding to
   --  Lib_File is returned in O_File. Its time stamp is saved in O_Stamp.
   --  ALI is the ALI_Id corresponding to Lib_File. If Lib_File in not
   --  up-to-date, then the corresponding source file needs to be recompiled.
   --  In this case ALI = No_ALI_Id.
   --  Full_Lib_File must be the result of calling Osint.Full_Lib_File_Name on
   --  Lib_File. Precomputing it saves system calls. Lib_File_Attr is the
   --  initialized attributes of that file, which is also used to save on
   --  system calls (it can safely be initialized to Unknown_Attributes).

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

   procedure Collect_Arguments (Args : Argument_List);
   --  Collect all arguments for a source to be compiled.

   procedure Display (Program : String; Args : Argument_List);
   --  Displays Program followed by the arguments in Args if variable
   --  Display_Executed_Programs is set. The lower bound of Args must be 1.

   procedure Report_Compilation_Failed;
   pragma No_Return (Report_Compilation_Failed);
   --  Delete all temporary files and fail graciously

   -----------------
   --  Mapping files
   -----------------

   type Temp_Path_Names is array (Positive range <>) of Path_Name_Type;
   type Temp_Path_Ptr is access Temp_Path_Names;

   type Free_File_Indexes is array (Positive range <>) of Positive;
   type Free_Indexes_Ptr is access Free_File_Indexes;

   type Mapping_File_Data is record
      Mapping_File_Names : Temp_Path_Ptr;
      --  The name ids of the temporary mapping files used. This is indexed
      --  on the maximum number of compilation processes we will be spawning
      --  (-j parameter)

      Last_Mapping_File_Names : Natural;
      --  Index of the last mapping file created for this project

      Free_Mapping_File_Indexes : Free_Indexes_Ptr;
      --  Indexes in Mapping_File_Names of the mapping file names that can be
      --  reused for subsequent compilations.

      Last_Free_Indexes : Natural;
      --  Number of mapping files that can be reused
   end record;
   --  Information necessary when compiling a project

   The_Mapping_Files : Mapping_File_Data;

   Gnatmake_Mapping_File : String_Access := null;
   --  The path name of a mapping file specified by switch -C=

   procedure Init_Mapping_File (File_Index : out Natural);
   --  Create a new mapping file or reuse one already created.

   package Temp_File_Paths is new Table.Table
     (Table_Component_Type => Path_Name_Type,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 4,
      Table_Increment      => 100,
      Table_Name           => "Make.Temp_File_Paths",
      Release_Threshold    => 0);

   procedure Record_Temp_File (Path : Path_Name_Type);
   --  Record the path of a temporary file, so that it can be deleted at the
   --  end of execution of gnatmake.

   procedure Record_Temp_File (Path : Path_Name_Type) is
   begin
      for J in 1 .. Temp_File_Paths.Last loop
         if Temp_File_Paths.Table (J) = Path then
            return;
         end if;
      end loop;

      Temp_File_Paths.Append (Path);
   end Record_Temp_File;

   -------------------------------------------------
   -- Subprogram declarations moved from the spec --
   -------------------------------------------------

   procedure Bind (ALI_File : File_Name_Type; Args : Argument_List);
   --  Binds ALI_File. Args are the arguments to pass to the binder.
   --  Args must have a lower bound of 1.

   --  If a compilation, bind or link failed one of the following 3 exceptions
   --  is raised. These need to be handled by the calling routines.

   procedure Compile_Sources
     (Main_Source           : File_Name_Type;
      Args                  : Argument_List;
      First_Compiled_File   : out File_Name_Type;
      Most_Recent_Obj_File  : out File_Name_Type;
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
      Max_Process           : Positive := 1);
   --  Compile_Sources will recursively compile all the sources needed by
   --  Main_Source. Before calling this routine make sure Namet has been
   --  initialized. This routine can be called repeatedly with different
   --  Main_Source file as long as all the source (-I flags), library
   --  (-B flags) and ada library (-A flags) search paths between calls are
   --  *exactly* the same. The default directory must also be the same.
   --
   --    Args contains the arguments to use during the compilations.
   --    The lower bound of Args must be 1.
   --
   --    First_Compiled_File is set to the name of the first file that is
   --    compiled or that needs to be compiled. This is set to No_Name if no
   --    compilations were needed.
   --
   --    Most_Recent_Obj_File is set to the full name of the most recent
   --    object file found when no compilations are needed, that is when
   --    First_Compiled_File is set to No_Name. When First_Compiled_File
   --    is set then Most_Recent_Obj_File is set to No_Name.
   --
   --    Most_Recent_Obj_Stamp is the time stamp of Most_Recent_Obj_File.
   --
   --    Main_Unit is set to True if Main_Source can be a main unit.
   --    If Do_Not_Execute is False and First_Compiled_File /= No_Name
   --    the value of Main_Unit is always False.
   --    Is this used any more??? It is certainly not used by gnatmake???
   --
   --    Compilation_Failures is a count of compilation failures. This count
   --    is used to extract compilation failure reports with Extract_Failure.
   --
   --    Main_Index, when not zero, is the index of the main unit in source
   --    file Main_Source which is a multi-unit source.
   --    Zero indicates that Main_Source is a single unit source file.
   --
   --    Check_Readonly_Files set it to True to compile source files
   --    which library files are read-only. When compiling GNAT predefined
   --    files the "-gnatg" flag is used.
   --
   --    Do_Not_Execute set it to True to find out the first source that
   --    needs to be recompiled, but without recompiling it. This file is
   --    saved in First_Compiled_File.
   --
   --    Force_Compilations forces all compilations no matter what but
   --    recompiles read-only files only if Check_Readonly_Files
   --    is set.
   --
   --    Keep_Going when True keep compiling even in the presence of
   --    compilation errors.
   --
   --    In_Place_Mode when True save library/object files in their object
   --    directory if they already exist; otherwise, in the source directory.
   --
   --    Initialize_ALI_Data set it to True when you want to initialize ALI
   --    data-structures. This is what you should do most of the time.
   --    (especially the first time around when you call this routine).
   --    This parameter is set to False to preserve previously recorded
   --    ALI file data.
   --
   --    Max_Process is the maximum number of processes that should be spawned
   --    to carry out compilations.
   --
   --  Flags in Package Opt Affecting Compile_Sources
   --  -----------------------------------------------
   --
   --    Check_Object_Consistency set it to False to omit all consistency
   --      checks between an .ali file and its corresponding object file.
   --      When this flag is set to true, every time an .ali is read,
   --      package Osint checks that the corresponding object file
   --      exists and is more recent than the .ali.
   --
   --  Use of Name Table Info
   --  ----------------------
   --
   --  All file names manipulated by Compile_Sources are entered into the
   --  Names table. The Byte field of a source file is used to mark it.
   --
   --  Calling Compile_Sources Several Times
   --  -------------------------------------
   --
   --  Upon return from Compile_Sources all the ALI data structures are left
   --  intact for further browsing. HOWEVER upon entry to this routine ALI
   --  data structures are re-initialized if parameter Initialize_ALI_Data
   --  above is set to true. Typically this is what you want the first time
   --  you call Compile_Sources. You should not load an ali file, call this
   --  routine with flag Initialize_ALI_Data set to True and then expect
   --  that ALI information to be around after the call. Note that the first
   --  time you call Compile_Sources you better set Initialize_ALI_Data to
   --  True unless you have called Initialize_ALI yourself.
   --
   --  Compile_Sources ALGORITHM : Compile_Sources (Main_Source)
   --  -------------------------
   --
   --  1. Insert Main_Source in a Queue (Q) and mark it.
   --
   --  2. Let unit.adb be the file at the head of the Q. If unit.adb is
   --     missing but its corresponding ali file is in an Ada library directory
   --     (see below) then, remove unit.adb from the Q and goto step 4.
   --     Otherwise, look at the files under the D (dependency) section of
   --     unit.ali. If unit.ali does not exist or some of the time stamps do
   --     not match, (re)compile unit.adb.
   --
   --     An Ada library directory is a directory containing Ada specs, ali
   --     and object files but no source files for the bodies. An Ada library
   --     directory is communicated to gnatmake by means of some switch so that
   --     gnatmake can skip the sources whole ali are in that directory.
   --     There are two reasons for skipping the sources in this case. Firstly,
   --     Ada libraries typically come without full sources but binding and
   --     linking against those libraries is still possible. Secondly, it would
   --     be very wasteful for gnatmake to systematically check the consistency
   --     of every external Ada library used in a program. The binder is
   --     already in charge of catching any potential inconsistencies.
   --
   --  3. Look into the W section of unit.ali and insert into the Q all
   --     unmarked source files. Mark all files newly inserted in the Q.
   --     Specifically, assuming that the W section looks like
   --
   --     W types%s               types.adb               types.ali
   --     W unchecked_deallocation%s
   --     W xref_tab%s            xref_tab.adb            xref_tab.ali
   --
   --     Then xref_tab.adb and types.adb are inserted in the Q if they are not
   --     already marked.
   --     Note that there is no file listed under W unchecked_deallocation%s
   --     so no generic body should ever be explicitly compiled (unless the
   --     Main_Source at the start was a generic body).
   --
   --  4. Repeat steps 2 and 3 above until the Q is empty
   --
   --  Note that the above algorithm works because the units withed in
   --  subunits are transitively included in the W section (with section) of
   --  the main unit. Likewise the withed units in a generic body needed
   --  during a compilation are also transitively included in the W section
   --  of the originally compiled file.

   procedure Link
     (ALI_File : File_Name_Type;
      Args     : Argument_List;
      Success  : out Boolean);
   --  Links ALI_File. Args are the arguments to pass to the linker.
   --  Args must have a lower bound of 1. Success indicates if the link
   --  succeeded or not.

   Gnatmake_Switch_Found : Boolean := False;

   procedure Scan_Make_Arg (Argv : String);
   --  Scan make arguments. Argv is a single argument to be processed.

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

   ----------------------------
   -- Add_Library_Search_Dir --
   ----------------------------

   procedure Add_Library_Search_Dir (Path : String) is
   begin
      Add_Lib_Search_Dir (Normalize_Pathname (Path));
   end Add_Library_Search_Dir;

   ---------------------------
   -- Add_Source_Search_Dir --
   ---------------------------

   procedure Add_Source_Search_Dir (Path : String) is
   begin
      Add_Src_Search_Dir (Normalize_Pathname (Path));
   end Add_Source_Search_Dir;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (S             : String_Access;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True)
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

      New_Position : Integer;

   --  Start of processing for Add_Switch

   begin
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
   end Add_Switch;

   procedure Add_Switch
     (S             : String;
      Program       : Make_Program_Type;
      Append_Switch : Boolean := True)
   is
   begin
      Add_Switch (S             => new String'(S),
                  Program       => Program,
                  Append_Switch => Append_Switch);
   end Add_Switch;

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
      --     gnatbind -aO. -I- file.ali
      --  into
      --     gnatbind file.adb

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
         Make_Failed ("error, unable to locate " & Gnatbind.all);
      end if;

      GNAT.OS_Lib.Spawn
        (Gnatbind_Path.all, Bind_Args (Args'First .. Bind_Last), Success);

      if not Success then
         Make_Failed ("*** bind failed.");
      end if;
   end Bind;

   -----------
   -- Check --
   -----------

   procedure Check
     (Source_File    : File_Name_Type;
      The_Args       : Argument_List;
      Lib_File       : File_Name_Type;
      Full_Lib_File  : File_Name_Type;
      Lib_File_Attr  : access File_Attributes;
      Read_Only      : Boolean;
      ALI            : out ALI_Id;
      O_File         : out File_Name_Type;
      O_Stamp        : out Time_Stamp_Type)
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
         --  a spec not mentioned in ALI file A. If True is returned
         --  Spec_File_Name above is set to the name of this spec file.

         --------------
         -- New_Spec --
         --------------

         function New_Spec (Uname : Unit_Name_Type) return Boolean is
            Spec_Name : Unit_Name_Type;
            File_Name : File_Name_Type;

         begin
            --  Test whether Uname is the name of a body unit (i.e. ends
            --  with %b).

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

      Full_Obj_File : File_Name_Type;
      --  Full name of the object file corresponding to Lib_File

      Lib_Stamp : Time_Stamp_Type;
      --  Time stamp of the current ada library file

      Obj_Stamp : Time_Stamp_Type;
      --  Time stamp of the current object file

      Modified_Source : File_Name_Type;
      --  The first source in Lib_File whose current time stamp differs from
      --  that stored in Lib_File.

      New_Spec : File_Name_Type;
      --  If Lib_File contains in its W (with) section a body (for a
      --  subprogram) for which there exists a spec, and the spec did not
      --  appear in the Sdep section of Lib_File, New_Spec contains the file
      --  name of this new spec.

      Source_Name : File_Name_Type;
      Text        : Text_Buffer_Ptr;

      First_Arg : Arg_Id;
      --  Index of the first argument in Args.Table for a given unit

      Last_Arg  : Arg_Id;
      --  Index of the last argument in Args.Table for a given unit

      Arg : Arg_Id := Arg_Id'First;
      --  Current index in Args.Table for a given unit (init to stop warning)

      Number_Of_Switches : Natural;
      --  Number of switches recorded for a given unit

      Prev_Switch : String_Access;
      --  Previous switch processed

      Switch_Found : Boolean;
      --  True if a given switch has been found

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
            Text := Read_Library_Info_From_Full (Full_Lib_File, Lib_File_Attr);
            Check_Object_Consistency := Saved_Check_Object_Consistency;
         end;

      else
         Text := Read_Library_Info_From_Full (Full_Lib_File, Lib_File_Attr);
      end if;

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

         --  Don't take ALI file into account if it was generated with errors

         if ALIs.Table (ALI).Compile_Errors then
            Verbose_Msg (Full_Lib_File, "had errors, must be recompiled");
            ALI := No_ALI_Id;
            return;
         end if;

         --  Don't take ALI file into account if no object was generated

         if Operating_Mode /= Check_Semantics
           and then ALIs.Table (ALI).No_Object
         then
            Verbose_Msg (Full_Lib_File, "has no corresponding object");
            ALI := No_ALI_Id;
            return;
         end if;

         --  When compiling with -gnatc, don't take ALI file into account if
         --  it has not been generated for the current source, for example if
         --  it has been generated for the spec, but we are compiling the body.

         if Operating_Mode = Check_Semantics then
            declare
               File_Name : String  := Get_Name_String (Source_File);
               OK        : Boolean := False;

            begin
               --  In the ALI file, the source file names are in canonical case

               Canonical_Case_File_Name (File_Name);

               for U in ALIs.Table (ALI).First_Unit ..
                 ALIs.Table (ALI).Last_Unit
               loop
                  OK := Get_Name_String (Units.Table (U).Sfile) = File_Name;
                  exit when OK;
               end loop;

               if not OK then
                  Verbose_Msg
                    (Full_Lib_File, "not generated for the same source");
                  ALI := No_ALI_Id;
                  return;
               end if;
            end;
         end if;

         --  Check for matching compiler switches if needed

         if Check_Switches then

            --  First, collect all the switches

            Collect_Arguments (The_Args);
            Prev_Switch := Dummy_Switch;
            Get_Name_String (ALIs.Table (ALI).Sfile);
            Switches_To_Check.Set_Last (0);

            for J in 1 .. Last_Argument loop

               --  Skip -c, -I and -o switches

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

            First_Arg := Units.Table (ALIs.Table (ALI).First_Unit).First_Arg;
            Last_Arg  := Units.Table (ALIs.Table (ALI).First_Unit).Last_Arg;

            for J in 1 .. Switches_To_Check.Last loop

               --  Comparing switches is delicate because gcc reorders a number
               --  of switches, according to lang-specs.h, but gnatmake doesn't
               --  have sufficient knowledge to perform the same reordering.
               --  Instead, we ignore orders between different "first letter"
               --  switches, but keep orders between same switches, e.g -O -O2
               --  is different than -O2 -O, but -g -O is equivalent to -O -g.

               if Switches_To_Check.Table (J) (2) /= Prev_Switch (2) or else
                   (Prev_Switch'Length >= 6 and then
                    Prev_Switch (2 .. 5) = "gnat" and then
                    Switches_To_Check.Table (J)'Length >= 6 and then
                    Switches_To_Check.Table (J) (2 .. 5) = "gnat" and then
                    Prev_Switch (6) /= Switches_To_Check.Table (J) (6))
               then
                  Prev_Switch := Switches_To_Check.Table (J);
                  Arg := First_Arg;
               end if;

               Switch_Found := False;

               for K in Arg .. Last_Arg loop
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

            Number_Of_Switches := Natural (Last_Arg - First_Arg + 1);

            --  Do not count the multilib switches reinstated by the compiler
            --  according to the lang-specs.h.settings.

            for K in First_Arg .. Last_Arg loop
               if Args.Table (K).all = "-mrtp" then
                  Number_Of_Switches := Number_Of_Switches - 1;
               end if;
            end loop;

            if Switches_To_Check.Last /= Number_Of_Switches then
               if Verbose_Mode then
                  Verbose_Msg (ALIs.Table (ALI).Sfile,
                               "different number of switches");

                  for K in First_Arg .. Last_Arg loop
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

         --  To avoid using too much memory when switch -m is used, free the
         --  memory allocated for the source file when computing the checksum.

         if Minimal_Recompilation then
            Sinput.Clear_Source_File_Table;
         end if;

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
     (E_Stamp : Time_Stamp_Type;
      O_File  : out File_Name_Type;
      O_Stamp : out Time_Stamp_Type)
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
               when '*' =>
                  Add_Str_To_Name_Buffer (Name);

               when ';' =>
                  File := Full_Lib_File_Name (Name_Find);
                  exit when File /= No_File;
                  Name_Len := 0;

               when NUL =>
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
         Check_File (File_Name_Type (Linker_Options.Table (Opt).Name));
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

   -----------------------
   -- Collect_Arguments --
   -----------------------

   procedure Collect_Arguments (Args : Argument_List) is
   begin
      Last_Argument := 0;
      Add_Arguments (Args);

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
      First_Compiled_File   : out File_Name_Type;
      Most_Recent_Obj_File  : out File_Name_Type;
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
      Mfile            : Natural := No_Mapping_File;
      Mapping_File_Arg : String_Access;
      --  Info on the mapping file

      Need_To_Check_Standard_Library : Boolean :=
                                         (Check_Readonly_Files or Must_Compile)
                                           and not Unique_Compile;

      procedure Add_Process
        (Pid           : Process_Id;
         Sfile         : File_Name_Type;
         Afile         : File_Name_Type;
         Uname         : Unit_Name_Type;
         Full_Lib_File : File_Name_Type;
         Lib_File_Attr : File_Attributes;
         Mfile         : Natural := No_Mapping_File);
      --  Adds process Pid to the current list of outstanding compilation
      --  processes and record the full name of the source file Sfile that
      --  we are compiling, the name of its library file Afile and the
      --  name of its unit Uname. If Mfile is not equal to No_Mapping_File,
      --  it is the index of the mapping file used during compilation in the
      --  array The_Mapping_File_Names.

      procedure Await_Compile
        (Data  : out Compilation_Data;
         OK    : out Boolean);
      --  Awaits that an outstanding compilation process terminates. When it
      --  does set Data to the information registered for the corresponding
      --  call to Add_Process. Note that this time stamp can be used to check
      --  whether the compilation did generate an object file. OK is set to
      --  True if the compilation succeeded. Data could be No_Compilation_Data
      --  if there was no compilation to wait for.

      function Bad_Compilation_Count return Natural;
      --  Returns the number of compilation failures

      procedure Check_Standard_Library;
      --  Check if s-stalib.adb needs to be compiled

      procedure Collect_Arguments_And_Compile
        (Full_Source_File : File_Name_Type;
         Lib_File         : File_Name_Type;
         Source_Index     : Int;
         Pid              : out Process_Id;
         Process_Created  : out Boolean);
      --  Collect arguments from project file (if any) and compile. If no
      --  compilation was attempted, Processed_Created is set to False, and the
      --  value of Pid is unknown.

      function Compile
        (S            : File_Name_Type;
         L            : File_Name_Type;
         Source_Index : Int;
         Args         : Argument_List) return Process_Id;
      --  Compiles S using Args. If S is a GNAT predefined source "-gnatpg" is
      --  added to Args. Non blocking call. L corresponds to the expected
      --  library file name. Process_Id of the process spawned to execute the
      --  compilation.

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

      procedure Get_Mapping_File;
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

      function Must_Exit_Because_Of_Error return Boolean;
      --  Return True if there were errors and the user decided to exit in such
      --  a case. This waits for any outstanding compilation.

      function Start_Compile_If_Possible (Args : Argument_List) return Boolean;
      --  Check if there is more work that we can do (i.e. the Queue is non
      --  empty). If there is, do it only if we have not yet used up all the
      --  available processes.
      --  Returns True if we should exit the main loop

      procedure Wait_For_Available_Slot;
      --  Check if we should wait for a compilation to finish. This is the case
      --  if all the available processes are busy compiling sources or there is
      --  nothing else to do (that is the Q is empty and there are no good ALIs
      --  to process).

      procedure Fill_Queue_From_ALI_Files;
      --  Check if we recorded good ALI files. If yes process them now in the
      --  order in which they have been recorded. There are two occasions in
      --  which we record good ali files. The first is in phase 1 when, after
      --  scanning an existing ALI file we realize it is up-to-date, the second
      --  instance is after a successful compilation.

      -----------------
      -- Add_Process --
      -----------------

      procedure Add_Process
        (Pid           : Process_Id;
         Sfile         : File_Name_Type;
         Afile         : File_Name_Type;
         Uname         : Unit_Name_Type;
         Full_Lib_File : File_Name_Type;
         Lib_File_Attr : File_Attributes;
         Mfile         : Natural := No_Mapping_File)
      is
         OC1 : constant Positive := Outstanding_Compiles + 1;

      begin
         pragma Assert (OC1 <= Max_Process);
         pragma Assert (Pid /= Invalid_Pid);

         Running_Compile (OC1) :=
           (Pid              => Pid,
            Full_Source_File => Sfile,
            Lib_File         => Afile,
            Full_Lib_File    => Full_Lib_File,
            Lib_File_Attr    => Lib_File_Attr,
            Source_Unit      => Uname,
            Mapping_File     => Mfile);

         Outstanding_Compiles := OC1;

      end Add_Process;

      --------------------
      -- Await_Compile --
      -------------------

      procedure Await_Compile
        (Data : out Compilation_Data;
         OK   : out Boolean)
      is
         Pid : Process_Id;

      begin
         pragma Assert (Outstanding_Compiles > 0);

         Data := No_Compilation_Data;
         OK   := False;

         Wait_Process (Pid, OK);

         if Pid = Invalid_Pid then
            return;
         end if;

         --  Look into the running compilation processes for this PID

         for J in Running_Compile'First .. Outstanding_Compiles loop
            if Pid = Running_Compile (J).Pid then
               Data := Running_Compile (J);
               --  If a mapping file was used by this compilation, get its file
               --  name for reuse by a subsequent compilation.

               if Running_Compile (J).Mapping_File /= No_Mapping_File then
                  The_Mapping_Files.Last_Free_Indexes :=
                    The_Mapping_Files.Last_Free_Indexes + 1;
                  The_Mapping_Files.Free_Mapping_File_Indexes
                    (The_Mapping_Files.Last_Free_Indexes) :=
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
               exit;
            end if;
         end loop;

         --  If the PID was not found, return with OK set to False

         if Data = No_Compilation_Data then
            OK := False;
         end if;
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
         Name_Len := 0;

         if not Targparm.Suppress_Standard_Library_On_Target then
            Add_Str_To_Name_Buffer (Standard_Library_Package_Body_Name);
         else
            Add_Str_To_Name_Buffer (System_Package_Spec_Name);
         end if;

         declare
            Add_It : Boolean := True;
            Sfile  : File_Name_Type;

         begin
            Sfile := Name_Enter;

            --  If we have a special runtime, we add the standard library only
            --  if we can find it.

            if RTS_Switch then
               Add_It := Full_Source_Name (Sfile) /= No_File;
            end if;

            if Add_It then
               if not Queue.Insert
                        ((File  => Sfile,
                          Unit  => No_Unit_Name,
                          Index => 0))
               then
                  if Is_In_Obsoleted (Sfile) then
                     Executable_Obsolete := True;
                  end if;
               end if;
            end if;
         end;
      end Check_Standard_Library;

      -----------------------------------
      -- Collect_Arguments_And_Compile --
      -----------------------------------

      procedure Collect_Arguments_And_Compile
        (Full_Source_File : File_Name_Type;
         Lib_File         : File_Name_Type;
         Source_Index     : Int;
         Pid              : out Process_Id;
         Process_Created  : out Boolean) is
      begin
         Process_Created := False;

         --  If we use mapping file (-P or -C switches), then get one

         if Create_Mapping_File then
            Get_Mapping_File;
         end if;

         Pid :=
           Compile
             (S              => Full_Source_File,
              L              => Lib_File,
              Source_Index   => Source_Index,
              Args           => Arguments (1 .. Last_Argument));
         Process_Created := True;
      end Collect_Arguments_And_Compile;

      -------------
      -- Compile --
      -------------

      function Compile
        (S            : File_Name_Type;
         L            : File_Name_Type;
         Source_Index : Int;
         Args         : Argument_List) return Process_Id
      is
         Comp_Args : Argument_List (Args'First .. Args'Last + 10);
         Comp_Next : Integer := Args'First;
         Comp_Last : Integer;
         Arg_Index : Integer;

         function Ada_File_Name (Name : File_Name_Type) return Boolean;
         --  Returns True if Name is the name of an ada source file
         --  (i.e. suffix is .ads or .adb)

         -------------------
         -- Ada_File_Name --
         -------------------

         function Ada_File_Name (Name : File_Name_Type) return Boolean is
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

         Comp_Args (Comp_Next) := new String'("-gnatea");
         Comp_Next := Comp_Next + 1;

         Comp_Args (Comp_Next) := Comp_Flag;
         Comp_Next := Comp_Next + 1;

         --  Optimize the simple case where the gcc command line looks like
         --     gcc -c -I. ... -I- file.adb
         --  into
         --     gcc -c ... file.adb

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
         --  may deallocate some arguments. Also strip target specific -mxxx
         --  switches in CodePeer mode.

         declare
            Index : Natural;
            Last  : constant Natural := Comp_Last;

         begin
            Index := Comp_Next;
            for J in Comp_Next .. Last loop
               declare
                  Str : String renames Args (Arg_Index).all;
               begin
                  if CodePeer_Mode
                    and then Str'Length > 2
                    and then Str (Str'First .. Str'First + 1) = "-m"
                  then
                     Comp_Last := Comp_Last - 1;
                  else
                     Comp_Args (Index) := new String'(Str);
                     Index := Index + 1;
                  end if;
               end;

               Arg_Index := Arg_Index + 1;
            end loop;
         end;

         --  Set -gnatpg for predefined files (for this purpose the renamings
         --  such as Text_IO do not count as predefined). Note that we strip
         --  the directory name from the source file name because the call to
         --  Fname.Is_Predefined_File_Name cannot deal with directory prefixes.

         declare
            Fname : constant File_Name_Type := Strip_Directory (S);

         begin
            if Is_Predefined_File_Name
                 (Fname, Renamings_Included => False)
            then
               if Check_Readonly_Files or else Must_Compile then
                  Comp_Args (Comp_Args'First + 2 .. Comp_Last + 1) :=
                    Comp_Args (Comp_Args'First + 1 .. Comp_Last);
                  Comp_Last := Comp_Last + 1;
                  Comp_Args (Comp_Args'First + 1) := GNAT_Flag;

               else
                  Make_Failed
                    ("not allowed to compile """ &
                     Get_Name_String (Fname) &
                     """; use -a switch, or use the compiler directly with "
                     & "the ""-gnatg"" switch");
               end if;
            end if;
         end;

         --  Now check if the file name has one of the suffixes familiar to
         --  the gcc driver. If this is not the case then add the ada flag
         --  "-x ada".
         --  Append systematically "-x adascil" in CodePeer mode instead, to
         --  force the use of gnat1scil instead of gnat1.

         if CodePeer_Mode then
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := Ada_Flag_1;
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := AdaSCIL_Flag;

         elsif not Ada_File_Name (S) then
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

         if Source_Index /= 0
           or else L /= Strip_Directory (L)
           or else Object_Directory_Path /= null
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

         if Create_Mapping_File and then Mapping_File_Arg /= null then
            Comp_Last := Comp_Last + 1;
            Comp_Args (Comp_Last) := new String'(Mapping_File_Arg.all);
         end if;

         Get_Name_String (S);

         Comp_Last := Comp_Last + 1;
         Comp_Args (Comp_Last) := new String'(Name_Buffer (1 .. Name_Len));

         --  Change to object directory of the project file, if necessary

         GNAT.OS_Lib.Normalize_Arguments (Comp_Args (Args'First .. Comp_Last));

         Comp_Last := Comp_Last + 1;
         Comp_Args (Comp_Last) := new String'("-gnatez");

         Display (Gcc.all, Comp_Args (Args'First .. Comp_Last));

         if Gcc_Path = null then
            Make_Failed ("error, unable to locate " & Gcc.all);
         end if;

         return
           GNAT.OS_Lib.Non_Blocking_Spawn
             (Gcc_Path.all, Comp_Args (Args'First .. Comp_Last));
      end Compile;

      -------------------------------
      -- Fill_Queue_From_ALI_Files --
      -------------------------------

      procedure Fill_Queue_From_ALI_Files is
         ALI          : ALI_Id;
         Source_Index : Int;
         Sfile        : File_Name_Type;

      begin
         while Good_ALI_Present loop
            ALI        := Get_Next_Good_ALI;
            Source_Index := Unit_Index_Of (ALIs.Table (ALI).Afile);

            --  If we are processing the library file corresponding to the
            --  main source file check if this source can be a main unit.

            if ALIs.Table (ALI).Sfile = Main_Source
              and then Source_Index = Main_Index
            then
               Main_Unit := ALIs.Table (ALI).Main_Program /= None;
            end if;

            --  The following adds the standard library (s-stalib) to the list
            --  of files to be handled by gnatmake: this file and any files it
            --  depends on are always included in every bind, even if they are
            --  not in the explicit dependency list. Of course, it is not added
            --  if Suppress_Standard_Library is True.

            --  However, to avoid annoying output about s-stalib.ali being read
            --  only, when "-v" is used, we add the standard library only when
            --  "-a" is used.

            if Need_To_Check_Standard_Library then
               Check_Standard_Library;
            end if;

            --  Now insert in the Q the unmarked source files (i.e. those which
            --  have never been inserted in the Q and hence never considered).
            --  Only do that if Unique_Compile is False.

            if not Unique_Compile then
               for J in
                 ALIs.Table (ALI).First_Unit .. ALIs.Table (ALI).Last_Unit
               loop
                  for K in
                    Units.Table (J).First_With .. Units.Table (J).Last_With
                  loop
                     Sfile := Withs.Table (K).Sfile;

                     Dependencies.Append ((ALIs.Table (ALI).Sfile, Sfile));

                     if Is_In_Obsoleted (Sfile) then
                        Executable_Obsolete := True;
                     end if;

                     if Sfile = No_File then
                        Debug_Msg ("Skipping generic:", Withs.Table (K).Uname);

                     else
                        Source_Index := Unit_Index_Of (Withs.Table (K).Afile);

                        if not (Check_Readonly_Files or Must_Compile)
                          and then Is_Internal_File_Name (Sfile, False)
                        then
                           Debug_Msg ("Skipping internal file:", Sfile);

                        else
                           Queue.Insert
                             ((File    => Sfile,
                               Unit    => Withs.Table (K).Uname,
                               Index   => Source_Index));
                        end if;
                     end if;
                  end loop;
               end loop;
            end if;
         end loop;
      end Fill_Queue_From_ALI_Files;

      ----------------------
      -- Get_Mapping_File --
      ----------------------

      procedure Get_Mapping_File is
      begin
         --  If there is a mapping file ready to be reused, reuse it

         if The_Mapping_Files.Last_Free_Indexes > 0 then
            Mfile :=
              The_Mapping_Files.Free_Mapping_File_Indexes
                                  (The_Mapping_Files.Last_Free_Indexes);
            The_Mapping_Files.Last_Free_Indexes :=
              The_Mapping_Files.Last_Free_Indexes - 1;

         --  Otherwise, create and initialize a new one

         else
            Init_Mapping_File (File_Index => Mfile);
         end if;

         --  Put the name in the mapping file argument for the invocation
         --  of the compiler.

         Free (Mapping_File_Arg);
         Mapping_File_Arg :=
           new String'
             ("-gnatem=" &
                Get_Name_String
                  (The_Mapping_Files.Mapping_File_Names (Mfile)));
      end Get_Mapping_File;

      -----------------------
      -- Get_Next_Good_ALI --
      -----------------------

      function Get_Next_Good_ALI return ALI_Id is
         ALIP : ALI_Id;

      begin
         pragma Assert (Good_ALI_Present);
         ALIP := Good_ALI.Table (Good_ALI.Last);
         Good_ALI.Decrement_Last;
         return ALIP;
      end Get_Next_Good_ALI;

      ----------------------
      -- Good_ALI_Present --
      ----------------------

      function Good_ALI_Present return Boolean is
      begin
         return Good_ALI.First <= Good_ALI.Last;
      end Good_ALI_Present;

      --------------------------------
      -- Must_Exit_Because_Of_Error --
      --------------------------------

      function Must_Exit_Because_Of_Error return Boolean is
         Data    : Compilation_Data;
         Success : Boolean;

      begin
         if Bad_Compilation_Count > 0 and then not Keep_Going then
            while Outstanding_Compiles > 0 loop
               Await_Compile (Data, Success);

               if not Success then
                  Record_Failure (Data.Full_Source_File, Data.Source_Unit);
               end if;
            end loop;

            return True;
         end if;

         return False;
      end Must_Exit_Because_Of_Error;

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

      -------------------------------
      -- Start_Compile_If_Possible --
      -------------------------------

      function Start_Compile_If_Possible
        (Args : Argument_List) return Boolean
      is
         In_Lib_Dir      : Boolean;
         Need_To_Compile : Boolean;
         Pid             : Process_Id := Invalid_Pid;
         Process_Created : Boolean;

         Source           : Queue.Source_Info;
         Full_Source_File : File_Name_Type := No_File;
         Source_File_Attr : aliased File_Attributes;
         --  The full name of the source file and its attributes (size, ...)

         Lib_File      : File_Name_Type;
         Full_Lib_File : File_Name_Type := No_File;
         Lib_File_Attr : aliased File_Attributes;
         Read_Only     : Boolean := False;
         ALI           : ALI_Id := No_ALI_Id;
         --  The ALI file and its attributes (size, stamp, ...)

         Obj_File  : File_Name_Type := No_File;
         Obj_Stamp : Time_Stamp_Type;
         --  The object file

         Found : Boolean;

      begin
         if not Queue.Is_Empty and then
            Outstanding_Compiles < Max_Process
         then
            Queue.Extract (Found, Source);

            Osint.Full_Source_Name
              (Source.File,
               Full_File => Full_Source_File,
               Attr      => Source_File_Attr'Access);

            Lib_File := Osint.Lib_File_Name (Source.File, Source.Index);

            Osint.Full_Lib_File_Name
              (Lib_File,
               Lib_File => Full_Lib_File,
               Attr     => Lib_File_Attr);

            --  If source has already been compiled, executable is obsolete

            if Is_In_Obsoleted (Source.File) then
               Executable_Obsolete := True;
            end if;

            In_Lib_Dir := Full_Lib_File /= No_File
                          and then In_Ada_Lib_Dir (Full_Lib_File);

            --  Since the following requires a system call, we precompute it
            --  when needed.

            if not In_Lib_Dir then
               if Full_Lib_File /= No_File
                 and then not (Check_Readonly_Files or else Must_Compile)
               then
                  Get_Name_String (Full_Lib_File);
                  Name_Buffer (Name_Len + 1) := ASCII.NUL;
                  Read_Only := not Is_Writable_File
                    (Name_Buffer'Address, Lib_File_Attr'Access);
               else
                  Read_Only := False;
               end if;
            end if;

            --  If the library file is an Ada library skip it

            if In_Lib_Dir then
               Verbose_Msg
                 (Lib_File,
                  "is in an Ada library",
                  Prefix => "  ",
                  Minimum_Verbosity => Opt.High);

               --  If the library file is a read-only library skip it, but only
               --  if, when using project files, this library file is in the
               --  right object directory (a read-only ALI file in the object
               --  directory of a project being extended must not be skipped).

            elsif Read_Only then
               Verbose_Msg
                 (Lib_File,
                  "is a read-only library",
                  Prefix => "  ",
                  Minimum_Verbosity => Opt.High);

               --  The source file that we are checking cannot be located

            elsif Full_Source_File = No_File then
               Record_Failure (Source.File, Source.Unit, False);

               --  Source and library files can be located but are internal
               --  files.

            elsif not (Check_Readonly_Files or else Must_Compile)
              and then Full_Lib_File /= No_File
              and then Is_Internal_File_Name (Source.File, False)
            then
               if Force_Compilations then
                  Fail
                    ("not allowed to compile """ &
                     Get_Name_String (Source.File) &
                     """; use -a switch, or use the compiler directly with "
                     & "the ""-gnatg"" switch");
               end if;

               Verbose_Msg
                 (Lib_File,
                  "is an internal library",
                  Prefix => "  ",
                  Minimum_Verbosity => Opt.High);

               --  The source file that we are checking can be located

            else
               Collect_Arguments (Args);

               --  Don't waste any time if we have to recompile anyway

               Obj_Stamp       := Empty_Time_Stamp;
               Need_To_Compile := Force_Compilations;

               if not Force_Compilations then
                  Check (Source_File    => Source.File,
                         The_Args       => Args,
                         Lib_File       => Lib_File,
                         Full_Lib_File  => Full_Lib_File,
                         Lib_File_Attr  => Lib_File_Attr'Access,
                         Read_Only      => Read_Only,
                         ALI            => ALI,
                         O_File         => Obj_File,
                         O_Stamp        => Obj_Stamp);
                  Need_To_Compile := (ALI = No_ALI_Id);
               end if;

               if not Need_To_Compile then

                  --  The ALI file is up-to-date; record its Id

                  Record_Good_ALI (ALI);

                  --  Record the time stamp of the most recent object
                  --  file as long as no (re)compilations are needed.

                  if First_Compiled_File = No_File
                    and then (Most_Recent_Obj_File = No_File
                              or else Obj_Stamp > Most_Recent_Obj_Stamp)
                  then
                     Most_Recent_Obj_File  := Obj_File;
                     Most_Recent_Obj_Stamp := Obj_Stamp;
                  end if;

               else
                  --  Is this the first file we have to compile?

                  if First_Compiled_File = No_File then
                     First_Compiled_File  := Full_Source_File;
                     Most_Recent_Obj_File := No_File;

                     if Do_Not_Execute then

                        --  Exit the main loop

                        return True;
                     end if;
                  end if;

                  --  Compute where the ALI file must be generated in
                  --  In_Place_Mode (this does not require to know the
                  --  location of the object directory).

                  if In_Place_Mode then
                     if Full_Lib_File = No_File then

                        --  If the library file was not found, then save
                        --  the library file near the source file.

                        Lib_File :=
                          Osint.Lib_File_Name
                            (Full_Source_File, Source.Index);
                        Full_Lib_File := Lib_File;

                     else
                        --  If the library file was found, then save the
                        --  library file in the same place.

                        Lib_File := Full_Lib_File;
                     end if;
                  end if;

                  --  Start the compilation and record it. We can do this
                  --  because there is at least one free process. This might
                  --  change the current directory.

                  Collect_Arguments_And_Compile
                    (Full_Source_File => Full_Source_File,
                     Lib_File         => Lib_File,
                     Source_Index     => Source.Index,
                     Pid              => Pid,
                     Process_Created  => Process_Created);

                  --  Compute where the ALI file will be generated (for
                  --  cases that might require to know the current
                  --  directory). The current directory might be changed
                  --  when compiling other files so we cannot rely on it
                  --  being the same to find the resulting ALI file.

                  if not In_Place_Mode then

                     --  Compute the expected location of the ALI file. This
                     --  can be from several places:
                     --    -i => in place mode. In such a case,
                     --          Full_Lib_File has already been set above
                     --    -D => if specified
                     --    or defaults in current dir
                     --  We could simply use a call similar to
                     --     Osint.Full_Lib_File_Name (Lib_File)
                     --  but that involves system calls and is thus slower

                     if Object_Directory_Path /= null then
                        Name_Len := 0;
                        Add_Str_To_Name_Buffer (Object_Directory_Path.all);
                        Add_Str_To_Name_Buffer (Get_Name_String (Lib_File));
                        Full_Lib_File := Name_Find;

                     else
                        Full_Lib_File := Lib_File;
                     end if;

                  end if;

                  Lib_File_Attr := Unknown_Attributes;

                  --  Make sure we could successfully start the compilation

                  if Process_Created then
                     if Pid = Invalid_Pid then
                        Record_Failure (Full_Source_File, Source.Unit);
                     else
                        Add_Process
                          (Pid           => Pid,
                           Sfile         => Full_Source_File,
                           Afile         => Lib_File,
                           Uname         => Source.Unit,
                           Mfile         => Mfile,
                           Full_Lib_File => Full_Lib_File,
                           Lib_File_Attr => Lib_File_Attr);
                     end if;
                  end if;
               end if;
            end if;
         end if;
         return False;
      end Start_Compile_If_Possible;

      -----------------------------
      -- Wait_For_Available_Slot --
      -----------------------------

      procedure Wait_For_Available_Slot is
         Compilation_OK : Boolean;
         Text           : Text_Buffer_Ptr;
         ALI            : ALI_Id;
         Data           : Compilation_Data;

      begin
         if Outstanding_Compiles = Max_Process
           or else (Queue.Is_Empty
                     and then not Good_ALI_Present
                     and then Outstanding_Compiles > 0)
         then
            Await_Compile (Data, Compilation_OK);

            if not Compilation_OK then
               Record_Failure (Data.Full_Source_File, Data.Source_Unit);
            end if;

            if Compilation_OK or else Keep_Going then

               --  Re-read the updated library file

               declare
                  Saved_Object_Consistency : constant Boolean :=
                                               Check_Object_Consistency;

               begin
                  --  If compilation was not OK, or if output is not an object
                  --  file and we don't do the bind step, don't check for
                  --  object consistency.

                  Check_Object_Consistency :=
                    Check_Object_Consistency
                      and Compilation_OK
                      and (Output_Is_Object or Do_Bind_Step);

                  Text :=
                    Read_Library_Info_From_Full
                      (Data.Full_Lib_File, Data.Lib_File_Attr'Access);

                  --  Restore Check_Object_Consistency to its initial value

                  Check_Object_Consistency := Saved_Object_Consistency;
               end;

               --  If an ALI file was generated by this compilation, scan the
               --  ALI file and record it.

               --  If the scan fails, a previous ali file is inconsistent with
               --  the unit just compiled.

               if Text /= null then
                  ALI :=
                    Scan_ALI
                      (Data.Lib_File, Text, Ignore_ED => False, Err => True);

                  if ALI = No_ALI_Id then

                     --  Record a failure only if not already done

                     if Compilation_OK then
                        Inform
                          (Data.Lib_File,
                           "incompatible ALI file, please recompile");
                        Record_Failure
                          (Data.Full_Source_File, Data.Source_Unit);
                     end if;

                  else
                     Record_Good_ALI (ALI);
                  end if;

                  Free (Text);

               --  If we could not read the ALI file that was just generated
               --  then there could be a problem reading either the ALI or the
               --  corresponding object file (if Check_Object_Consistency is
               --  set Read_Library_Info checks that the time stamp of the
               --  object file is more recent than that of the ALI). However,
               --  we record a failure only if not already done.

               else
                  if Compilation_OK and not Syntax_Only then
                     Inform
                       (Data.Lib_File,
                        "WARNING: ALI or object file not found after compile");

                     if not Is_Regular_File
                              (Get_Name_String (Name_Id (Data.Full_Lib_File)))
                     then
                        Inform (Data.Full_Lib_File, "not found");
                     end if;

                     Record_Failure (Data.Full_Source_File, Data.Source_Unit);
                  end if;
               end if;
            end if;
         end if;
      end Wait_For_Available_Slot;

   --  Start of processing for Compile_Sources

   begin
      pragma Assert (Args'First = 1);

      Outstanding_Compiles := 0;
      Running_Compile := new Comp_Data_Arr (1 .. Max_Process);

      --  Package and Queue initializations

      Good_ALI.Init;

      if Initialize_ALI_Data then
         Initialize_ALI;
         Initialize_ALI_Source;
      end if;

      --  The following two flags affect the behavior of ALI.Set_Source_Table.
      --  We set Check_Source_Files to True to ensure that source file time
      --  stamps are checked, and we set All_Sources to False to avoid checking
      --  the presence of the source files listed in the source dependency
      --  section of an ali file (which would be a mistake since the ali file
      --  may be obsolete).

      Check_Source_Files := True;
      All_Sources        := False;

      Queue.Insert
        ((File    => Main_Source,
          Unit    => No_Unit_Name,
          Index   => Main_Index));

      First_Compiled_File   := No_File;
      Most_Recent_Obj_File  := No_File;
      Most_Recent_Obj_Stamp := Empty_Time_Stamp;
      Main_Unit             := False;

      --  Keep looping until there is no more work to do (the Q is empty)
      --  and all the outstanding compilations have terminated.

      Make_Loop :
      while not Queue.Is_Empty or else Outstanding_Compiles > 0 loop
         exit Make_Loop when Must_Exit_Because_Of_Error;
         exit Make_Loop when Start_Compile_If_Possible (Args);

         Wait_For_Available_Slot;

         --  ??? Should be done as soon as we add a Good_ALI, wouldn't it avoid
         --  the need for a list of good ALI?

         Fill_Queue_From_ALI_Files;

         if Display_Compilation_Progress then
            Write_Str ("completed ");
            Write_Int (Int (Queue.Processed));
            Write_Str (" out of ");
            Write_Int (Int (Queue.Size));
            Write_Str (" (");
            Write_Int (Int ((Queue.Processed * 100) / Queue.Size));
            Write_Str ("%)...");
            Write_Eol;
         end if;
      end loop Make_Loop;

      Compilation_Failures := Bad_Compilation_Count;

      --  Compilation is finished

   end Compile_Sources;

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

   procedure Debug_Msg (S : String; N : File_Name_Type) is
   begin
      Debug_Msg (S, Name_Id (N));
   end Debug_Msg;

   procedure Debug_Msg (S : String; N : Unit_Name_Type) is
   begin
      Debug_Msg (S, Name_Id (N));
   end Debug_Msg;

   -------------
   -- Display --
   -------------

   procedure Display (Program : String; Args : Argument_List) is
   begin
      pragma Assert (Args'First = 1);

      if not Quiet_Output then
         Write_Str (Program);

         for J in Args'Range loop

            --  Never display -gnatea nor -gnatez

            if Args (J).all /= "-gnatea"
              and then
                Args (J).all /= "-gnatez"
            then
               --  Do not display the -F=mapping_file switch for gnatbind if
               --  -dn is not specified.

               if Opt.Keep_Temporary_Files
                 or else Args (J)'Length < 4
                 or else
                   Args (J) (Args (J)'First .. Args (J)'First + 2) /= "-F="
               then
                  Write_Str (" ");

                  --  If -df is used, only display file names, not path
                  --  names.

                  if Debug.Debug_Flag_F then
                     declare
                        Equal_Pos : Natural;

                     begin
                        Equal_Pos := Args (J)'First - 1;
                        for K in Args (J)'Range loop
                           if Args (J) (K) = '=' then
                              Equal_Pos := K;
                              exit;
                           end if;
                        end loop;

                        if Is_Absolute_Path
                          (Args (J) (Equal_Pos + 1 .. Args (J)'Last))
                        then
                           Write_Str
                             (Args (J) (Args (J)'First .. Equal_Pos));
                           Write_Str
                             (File_Name
                                (Args (J)
                                 (Equal_Pos + 1 .. Args (J)'Last)));

                        else
                           Write_Str (Args (J).all);
                        end if;
                     end;

                  else
                     Write_Str (Args (J).all);
                  end if;
               end if;
            end if;
         end loop;

         Write_Eol;
      end if;
   end Display;

   --------------------------
   -- Enter_Into_Obsoleted --
   --------------------------

   procedure Enter_Into_Obsoleted (F : File_Name_Type) is
      Name  : constant String := Get_Name_String (F);
      First : Natural;
      F2    : File_Name_Type;

   begin
      First := Name'Last;
      while First > Name'First
        and then not Is_Directory_Separator (Name (First - 1))
      loop
         First := First - 1;
      end loop;

      if First /= Name'First then
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Name (First .. Name'Last));
         F2 := Name_Find;

      else
         F2 := F;
      end if;

      Debug_Msg ("New entry in Obsoleted table:", F2);
      Obsoleted.Set (F2, True);
   end Enter_Into_Obsoleted;

   -------------------
   -- Linking_Phase --
   -------------------

   procedure Linking_Phase
     (Non_Std_Executable : Boolean := False;
      Executable         : File_Name_Type := No_File;
      Main_ALI_File      : File_Name_Type)
   is
      Linker_Switches_Last : constant Integer := Linker_Switches.Last;

   begin
      if not Run_Path_Option then
         Linker_Switches.Increment_Last;
         Linker_Switches.Table (Linker_Switches.Last) :=
           new String'("-R");
      end if;

      if CodePeer_Mode then
         Linker_Switches.Increment_Last;
         Linker_Switches.Table (Linker_Switches.Last) :=
           new String'(CodePeer_Mode_String);
      end if;

      --  Add switch -M to gnatlink if builder switch --create-map-file
      --  has been specified.

      if Map_File /= null then
         Linker_Switches.Increment_Last;
         Linker_Switches.Table (Linker_Switches.Last) :=
           new String'("-M" & Map_File.all);
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

               --  Here we capture and duplicate the linker argument. We
               --  need to do the duplication since the arguments will get
               --  normalized. Not doing so will result in calling normalized
               --  two times for the same set of arguments if gnatmake is
               --  passed multiple mains. This can result in the wrong
               --  argument being passed to the linker.

            else
               Last_Arg := Last_Arg + 1;
               Args (Last_Arg) := new String'(Linker_Switches.Table (J).all);
            end if;
         end loop;

         --  If need be, add the -o switch

         if Non_Std_Executable then
            Last_Arg := Last_Arg + 1;
            Args (Last_Arg) := new String'("-o");
            Last_Arg := Last_Arg + 1;
            Args (Last_Arg) := new String'(Get_Name_String (Executable));
         end if;

         --  And invoke the linker

         declare
            Success : Boolean := False;

         begin
            --  If gnatmake was invoked with --subdirs, put the executable in
            --  the subdirectory specified.

            if Subdirs /= null then
               Change_Dir (Object_Directory_Path.all);
            end if;

            Link (Main_ALI_File,
                  Link_With_Shared_Libgcc.all &
                  Args (Args'First .. Last_Arg),
                  Success);

            if Success then
               Successful_Links.Increment_Last;
               Successful_Links.Table (Successful_Links.Last) := Main_ALI_File;

            elsif Osint.Number_Of_Files = 1 or else not Keep_Going then
               Make_Failed ("*** link failed.");

            else
               Set_Standard_Error;
               Write_Line ("*** link failed");

               if Commands_To_Stdout then
                  Set_Standard_Output;
               end if;

               Failed_Links.Increment_Last;
               Failed_Links.Table (Failed_Links.Last) := Main_ALI_File;
            end if;
         end;
      end;

      Linker_Switches.Set_Last (Linker_Switches_Last);
   end Linking_Phase;

   -------------------
   -- Binding_Phase --
   -------------------

   procedure Binding_Phase (Main_ALI_File : File_Name_Type) is
      Args : Argument_List (Binder_Switches.First .. Binder_Switches.Last + 2);
      --  The arguments for the invocation of gnatbind

      Last_Arg : Natural := Binder_Switches.Last;
      --  Index of the last argument in Args

      Shared_Libs : Boolean := False;
      --  Set to True when there are shared library project files or
      --  when gnatbind is invoked with -shared.

   begin

      --  Check now for switch -shared

      for J in Binder_Switches.First .. Last_Arg loop
         if Binder_Switches.Table (J).all = "-shared" then
            Shared_Libs := True;
            exit;
         end if;
      end loop;

      --  If shared libraries present, invoke gnatlink with
      --  -shared-libgcc.

      if Shared_Libs then
         Link_With_Shared_Libgcc := Shared_Libgcc_Switch'Access;
      end if;

      --  Get all the binder switches

      for J in Binder_Switches.First .. Last_Arg loop
         Args (J) := Binder_Switches.Table (J);
      end loop;

      if CodePeer_Mode then
         Last_Arg := Last_Arg + 1;
         Args (Last_Arg) := CodePeer_Mode_String'Access;
      end if;

      --  If gnatmake was invoked with --subdirs, put the
      --  binder generated files in the subdirectory specified.

      if Subdirs /= null then
         Change_Dir (Object_Directory_Path.all);
      end if;

      Bind (Main_ALI_File, Bind_Shared.all & Args (Args'First .. Last_Arg));

   end Binding_Phase;

   -----------------------
   -- Compilation_Phase --
   -----------------------

   procedure Compilation_Phase
     (Main_Source_File           : File_Name_Type;
      Current_Main_Index         : Int := 0;
      Total_Compilation_Failures : in out Natural;
      Executable                 : File_Name_Type := No_File;
      Stop_Compile               : out Boolean)
   is
      Args                : Argument_List (1 .. Gcc_Switches.Last);
      First_Compiled_File : File_Name_Type;
      Youngest_Obj_File   : File_Name_Type;
      Youngest_Obj_Stamp  : Time_Stamp_Type;

      Is_Main_Unit : Boolean;
      --  Set True by Compile_Sources if Main_Source_File can be a main unit

      Compilation_Failures : Natural;

      Executable_Stamp : Time_Stamp_Type;

   begin
      Stop_Compile := False;

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
         Max_Process           => Maximum_Processes);

      if Verbose_Mode then
         Write_Str ("End of compilation");
         Write_Eol;
      end if;

      Total_Compilation_Failures :=
        Total_Compilation_Failures + Compilation_Failures;

      if Total_Compilation_Failures /= 0 then
         Stop_Compile := True;
         return;
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
        and then Osint.Number_Of_Files = 1
      then
         Inform (Msg => "objects up to date.");
         Stop_Compile := True;
         return;

      elsif Do_Not_Execute and then First_Compiled_File /= No_File then
         Write_Name (First_Compiled_File);
         Write_Eol;
      end if;

      --  Stop after compile step if any of:

      --    1) -n (Do_Not_Execute) specified

      --    2) -M (List_Dependencies) specified (also sets
      --       Do_Not_Execute above, so this is probably superfluous).

      --    3) -c (Compile_Only) specified, but not -b (Bind_Only)

      --    4) Made unit cannot be a main unit

      if ((Do_Not_Execute
            or List_Dependencies
            or not Do_Bind_Step
            or not Is_Main_Unit)
          and not No_Main_Subprogram
          and not Build_Bind_And_Link_Full_Project)
        or Unique_Compile
      then
         Stop_Compile := True;
         return;
      end if;

      --  If the objects were up-to-date check if the executable file is also
      --  up-to-date. For now always bind and link in CodePeer mode where there
      --  is no executable.

      if not CodePeer_Mode
        and then Do_Link_Step
        and then First_Compiled_File = No_File
      then
         Executable_Stamp := File_Stamp (Executable);

         if not Executable_Obsolete then
            Executable_Obsolete := Youngest_Obj_Stamp > Executable_Stamp;
         end if;

         if not Executable_Obsolete then
            for Index in reverse 1 .. Dependencies.Last loop
               if Is_In_Obsoleted (Dependencies.Table (Index).Depends_On) then
                  Enter_Into_Obsoleted (Dependencies.Table (Index).This);
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

         --  Return if the executable is up to date and otherwise
         --  motivate the relink/rebind.

         if not Executable_Obsolete then
            if not Quiet_Output then
               Inform (Executable, "up to date.");
            end if;

            Stop_Compile := True;
            return;
         end if;

         if Executable_Stamp (1) = ' ' then
            if not No_Main_Subprogram then
               Verbose_Msg (Executable, "missing.", Prefix => "  ");
            end if;

         elsif Youngest_Obj_Stamp (1) = ' ' then
            Verbose_Msg
              (Youngest_Obj_File, "missing.",  Prefix => "  ");

         elsif Youngest_Obj_Stamp > Executable_Stamp then
            Verbose_Msg
              (Youngest_Obj_File,
               "(" & String (Youngest_Obj_Stamp) & ") newer than",
               Executable,
               "(" & String (Executable_Stamp) & ")");

         else
            Verbose_Msg
              (Executable, "needs to be rebuilt", Prefix => "  ");

         end if;
      end if;
   end Compilation_Phase;

   ------------------------
   -- Compute_Executable --
   ------------------------

   procedure Compute_Executable
     (Main_Source_File   : File_Name_Type;
      Executable         : out File_Name_Type;
      Non_Std_Executable : out Boolean)
   is
   begin
      Executable          := No_File;
      Non_Std_Executable  :=
        Targparm.Executable_Extension_On_Target /= No_Name;

      --  Look inside the linker switches to see if the name of the final
      --  executable program was specified.

      for J in reverse Linker_Switches.First .. Linker_Switches.Last loop
         if Linker_Switches.Table (J).all = Output_Flag.all then
            pragma Assert (J < Linker_Switches.Last);

            --  We cannot specify a single executable for several main
            --  subprograms

            if Osint.Number_Of_Files > 1 then
               Fail ("cannot specify a single executable for several mains");
            end if;

            Name_Len := 0;
            Add_Str_To_Name_Buffer (Linker_Switches.Table (J + 1).all);
            Executable := Name_Enter;

            Verbose_Msg (Executable, "final executable");
         end if;
      end loop;

      --  If the name of the final executable program was not specified then
      --  construct it from the main input file.

      if Executable = No_File then
         Executable := Executable_Name (Strip_Suffix (Main_Source_File));
      end if;

   end Compute_Executable;

   --------------
   -- Gnatmake --
   --------------

   procedure Gnatmake is
      Main_Source_File : File_Name_Type;
      --  The source file containing the main compilation unit

      Total_Compilation_Failures : Natural := 0;

      Main_ALI_File : File_Name_Type;
      --  The ali file corresponding to Main_Source_File

      Executable : File_Name_Type := No_File;
      --  The file name of an executable

      Non_Std_Executable : Boolean := False;
      --  Non_Std_Executable is set to True when there is a possibility that
      --  the linker will not choose the correct executable file name.

      Current_Main_Index : Int := 0;
      --  If not zero, the index of the current main unit in its source file

      Is_First_Main : Boolean;
      --  Whether we are processing the first main

      Stop_Compile : Boolean;

      Discard : Boolean;
      pragma Warnings (Off, Discard);

   begin
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

      --  Special case when switch -B was specified

      if Main_Index /= 0 and then Osint.Number_Of_Files > 1 then
         Make_Failed ("cannot specify several mains with a multi-unit index");
      end if;

      if Verbose_Mode then
         Write_Eol;
         Display_Version ("GNATMAKE", "1992");
      end if;

      if Osint.Number_Of_Files = 0 then
         --  Call Get_Target_Parameters to ensure that flags are properly
         --  set before calling Usage.

         Targparm.Get_Target_Parameters;

         --  Output usage information if no argument on the command line

         if Argument_Count = 0 then
            Usage;
         else
            Try_Help;
         end if;

         Finish_Program (E_Success);
      end if;

      --  Get the first executable.
      --  ??? This needs to be done early, because Osint.Next_Main_File also
      --  initializes the primary search directory, used below to initialize
      --  the "-I" parameter

      Main_Source_File := Next_Main_Source;  --  No directory information

      --  If -M was specified, behave as if -n was specified

      if List_Dependencies then
         Do_Not_Execute := True;
      end if;

      Add_Switch ("-I-", Compiler);

      if Look_In_Primary_Dir then
         Add_Switch
           ("-I" &
              Normalize_Directory_Name
              (Get_Primary_Src_Search_Directory.all).all,
            Compiler,
            Append_Switch => False);

      end if;

      --  If the user wants a program without a main subprogram, add the
      --  appropriate switch to the binder.

      if No_Main_Subprogram then
         Add_Switch ("-z", Binder);
      end if;

      --  The combination of -f -u and one or several mains on the command line
      --  implies -a.

      if Force_Compilations
        and then Unique_Compile
        and then Main_On_Command_Line
      then
         Must_Compile := True;
      end if;

      Bad_Compilation.Init;

      --  Here is where the make process is started

      Queue.Initialize;

      Is_First_Main := True;

      Multiple_Main_Loop : for N_File in 1 .. Osint.Number_Of_Files loop
         if Current_File_Index /= No_Index then
            Main_Index := Current_File_Index;
         end if;

         Current_Main_Index := Main_Index;

         if Is_First_Main then

            --  Put the default source dirs in the source path only now, so
            --  that we take the correct ones in the case where --RTS= is
            --  specified in the Builder switches.

            Osint.Add_Default_Search_Dirs;

            --  Get the target parameters, which are only needed for a couple
            --  of cases in gnatmake. Protect against an exception, such as the
            --  case of system.ads missing from the library, and fail
            --  gracefully.

            begin
               Targparm.Get_Target_Parameters;
            exception
               when Unrecoverable_Error =>
                  Make_Failed ("*** make failed.");
            end;

            Gcc_Path       := GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);
            Gnatbind_Path  := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatbind.all);
            Gnatlink_Path  := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatlink.all);

            --  If we have specified -j switch both from the project file
            --  and on the command line, the one from the command line takes
            --  precedence.

            if Debug.Debug_Flag_M then
               Write_Line ("Maximum number of simultaneous compilations =" &
                           Maximum_Processes'Img);
            end if;

            --  Allocate as many temporary mapping file names as the maximum
            --  number of compilations processed.

            The_Mapping_Files :=
                (Mapping_File_Names        =>
                    new Temp_Path_Names (1 .. Maximum_Processes),
                 Last_Mapping_File_Names   => 0,
                 Free_Mapping_File_Indexes =>
                    new Free_File_Indexes (1 .. Maximum_Processes),
                 Last_Free_Indexes         => 0);

            Is_First_Main := False;
         end if;

         Executable_Obsolete := False;

         Compute_Executable
           (Main_Source_File   => Main_Source_File,
            Executable         => Executable,
            Non_Std_Executable => Non_Std_Executable);

         if Do_Compile_Step then
            Compilation_Phase
              (Main_Source_File           => Main_Source_File,
               Current_Main_Index         => Current_Main_Index,
               Total_Compilation_Failures => Total_Compilation_Failures,
               Executable                 => Executable,
               Stop_Compile               => Stop_Compile);

            if Stop_Compile then
               if Total_Compilation_Failures /= 0 then
                  if Keep_Going then
                     goto Next_Main;

                  else
                     List_Bad_Compilations;
                     Report_Compilation_Failed;
                  end if;

               elsif Osint.Number_Of_Files = 1 then
                  exit Multiple_Main_Loop;
               else
                  goto Next_Main;
               end if;
            end if;
         end if;

         --  If we are here, it means that we need to rebuilt the current main,
         --  so we set Executable_Obsolete to True to make sure that subsequent
         --  mains will be rebuilt.

         Main_ALI_In_Place_Mode_Step : declare
            ALI_File : File_Name_Type;
            Src_File : File_Name_Type;

         begin
            Src_File      := Strip_Directory (Main_Source_File);
            ALI_File      := Lib_File_Name (Src_File, Current_Main_Index);
            Main_ALI_File := Full_Lib_File_Name (ALI_File);

            --  When In_Place_Mode, the library file can be located in the
            --  Main_Source_File directory which may not be present in the
            --  library path. If it is not present then use the corresponding
            --  library file name.

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
            Binding_Phase (Main_ALI_File);
         end if;

         if Do_Link_Step then
            Linking_Phase
              (Non_Std_Executable => Non_Std_Executable,
               Executable         => Executable,
               Main_ALI_File      => Main_ALI_File);
         end if;

         --  We go to here when we skip the bind and link steps

         <<Next_Main>>

         Queue.Remove_Marks;

         if N_File < Osint.Number_Of_Files then
            Main_Source_File := Next_Main_Source;  --  No directory information
         end if;
      end loop Multiple_Main_Loop;

      if Failed_Links.Last > 0 then
         for Index in 1 .. Successful_Links.Last loop
            Write_Str ("Linking of """);
            Write_Str (Get_Name_String (Successful_Links.Table (Index)));
            Write_Line (""" succeeded.");
         end loop;

         Set_Standard_Error;

         for Index in 1 .. Failed_Links.Last loop
            Write_Str ("Linking of """);
            Write_Str (Get_Name_String (Failed_Links.Table (Index)));
            Write_Line (""" failed.");
         end loop;

         if Commands_To_Stdout then
            Set_Standard_Output;
         end if;

         if Total_Compilation_Failures = 0 then
            Report_Compilation_Failed;
         end if;
      end if;

      if Total_Compilation_Failures /= 0 then
         List_Bad_Compilations;
         Report_Compilation_Failed;
      end if;

      Finish_Program (E_Success);

   exception
      when X : others =>
         Set_Standard_Error;
         Write_Line (Exception_Information (X));
         Make_Failed ("INTERNAL ERROR. Please report.");
   end Gnatmake;

   ----------
   -- Hash --
   ----------

   function Hash (F : File_Name_Type) return Header_Num is
   begin
      return Header_Num (1 + F mod Max_Header);
   end Hash;

   --------------------
   -- In_Ada_Lib_Dir --
   --------------------

   function In_Ada_Lib_Dir (File : File_Name_Type) return Boolean is
      D : constant File_Name_Type := Get_Directory (File);
      B : constant Byte           := Get_Name_Table_Byte (D);
   begin
      return (B and Ada_Lib_Dir) /= 0;
   end In_Ada_Lib_Dir;

   -----------------------
   -- Init_Mapping_File --
   -----------------------

   procedure Init_Mapping_File (File_Index : out Natural) is
      FD     : File_Descriptor;
      Status : Boolean;
      --  For call to Close

   begin
      --  Increase the index of the last mapping file for this project

      The_Mapping_Files.Last_Mapping_File_Names :=
        The_Mapping_Files.Last_Mapping_File_Names + 1;

      --  Just create an empty file

      Tempdir.Create_Temp_File
        (FD,
         The_Mapping_Files.Mapping_File_Names
           (The_Mapping_Files.Last_Mapping_File_Names));

      if FD = Invalid_FD then
         Make_Failed ("disk full");
      else
         Record_Temp_File
           (The_Mapping_Files.Mapping_File_Names
                          (The_Mapping_Files.Last_Mapping_File_Names));
      end if;

      Close (FD, Status);

      if not Status then
         Make_Failed ("disk full");
      end if;

      --  And return the index of the newly created file

      File_Index := The_Mapping_Files.Last_Mapping_File_Names;
   end Init_Mapping_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      procedure Check_Version_And_Help is
        new Check_Version_And_Help_G (Makeusg);

   begin
      --  Override default initialization of Check_Object_Consistency since
      --  this is normally False for GNATBIND, but is True for GNATMAKE since
      --  we do not need to check source consistency again once GNATMAKE has
      --  looked at the sources to check.

      Check_Object_Consistency := True;

      --  Package initializations (the order of calls is important here)

      Output.Set_Standard_Error;

      Gcc_Switches.Init;
      Binder_Switches.Init;
      Linker_Switches.Init;

      Csets.Initialize;
      Snames.Initialize;
      Stringt.Initialize;

      Dependencies.Init;

      RTS_Specified := null;
      N_M_Switch := 0;

      Mains.Delete;

      --  Add the directory where gnatmake is invoked in front of the path,
      --  if gnatmake is invoked from a bin directory or with directory
      --  information.

      declare
         Prefix  : constant String := Executable_Prefix_Path;
         Command : constant String := Command_Name;

      begin
         if Prefix'Length > 0 then
            declare
               PATH : constant String :=
                        Prefix & Directory_Separator & "bin" & Path_Separator
                        & Getenv ("PATH").all;
            begin
               Setenv ("PATH", PATH);
            end;

         else
            for Index in reverse Command'Range loop
               if Command (Index) = Directory_Separator then
                  declare
                     Absolute_Dir : constant String :=
                                      Normalize_Pathname
                                        (Command (Command'First .. Index));
                     PATH         : constant String :=
                                      Absolute_Dir &
                                      Path_Separator &
                                      Getenv ("PATH").all;
                  begin
                     Setenv ("PATH", PATH);
                  end;

                  exit;
               end if;
            end loop;
         end if;
      end;

      --  Scan the switches and arguments

      --  First, scan to detect --version and/or --help

      Check_Version_And_Help ("GNATMAKE", "1995");

      --  Scan again the switch and arguments, now that we are sure that they
      --  do not include --version or --help.

      --  First, check for switch -P and, if found and gprbuild is available,
      --  silently invoke gprbuild, with switch --target if not on a native
      --  platform.

      declare
         Arg_Len       : Natural       := Argument_Count;
         Call_Gprbuild : Boolean       := False;
         Gprbuild      : String_Access := null;
         Pos           : Natural       := 0;
         Success       : Boolean;
         Target        : String_Access := null;

         In_Gnatmake_Switches : Boolean := True;
         --  Set to False after -cargs, -bargs, or -largs, to avoid detecting
         --  -P switches that are not for gnatmake.

      begin
         Find_Program_Name;

         if Name_Len >= 8
           and then Name_Buffer (Name_Len - 7 .. Name_Len) = "gnatmake"
         then
            if Name_Len > 9 then
               Target  := new String'(Name_Buffer (1 .. Name_Len - 9));
               Arg_Len := Arg_Len + 1;
            end if;

            for J in 1 .. Argument_Count loop
               declare
                  Arg : constant String := Argument (J);
               begin
                  if Arg = "-cargs" or Arg = "-bargs" or Arg = "-largs" then
                     In_Gnatmake_Switches := False;

                  elsif Arg = "-margs" then
                     In_Gnatmake_Switches := True;

                  elsif In_Gnatmake_Switches
                    and then Arg'Length >= 2
                    and then Arg (Arg'First .. Arg'First + 1) = "-P"
                  then
                     Call_Gprbuild := True;
                     exit;
                  end if;
               end;
            end loop;

            if Call_Gprbuild then
               Gprbuild := Locate_Exec_On_Path (Exec_Name => "gprbuild");

               if Gprbuild = null then
                  Fail_Program
                    ("project files are no longer supported by gnatmake;" &
                     " use gprbuild instead");
               end if;

               declare
                  Args : Argument_List (1 .. Arg_Len);
               begin
                  if Target /= null then
                     Args (1) := new String'("--target=" & Target.all);
                     Pos := 1;
                  end if;

                  for J in 1 .. Argument_Count loop
                     Pos := Pos + 1;
                     Args (Pos) := new String'(Argument (J));
                  end loop;

                  Spawn (Gprbuild.all, Args, Success);

                  Free (Gprbuild);

                  if Success then
                     Exit_Program (E_Success);
                  else
                     Exit_Program (E_Errors);
                  end if;
               end;
            end if;
         end if;
      end;

      Scan_Args : for Next_Arg in 1 .. Argument_Count loop
         Scan_Make_Arg (Argument (Next_Arg));
      end loop Scan_Args;

      if Make_Steps then
         Do_Compile_Step := Compile_Only;
         Do_Bind_Step    := Bind_Only;
         Do_Link_Step    := Link_Only;

         if Do_Compile_Step and then Do_Link_Step then
            Do_Bind_Step := True;
         end if;
      end if;

      if N_M_Switch > 0 and RTS_Specified = null then
         Process_Multilib;
      end if;

      if Commands_To_Stdout then
         Set_Standard_Output;
      end if;

      if Usage_Requested then
         Usage;
      end if;

      --  Test for trailing -P switch

      if Project_File_Name_Present and then Project_File_Name = null then
         Make_Failed ("project file name missing after -P");

      --  Test for trailing -o switch

      elsif Output_File_Name_Present and then not Output_File_Name_Seen then
         Make_Failed ("output file name missing after -o");

      --  Test for trailing -D switch

      elsif Object_Directory_Present and then not Object_Directory_Seen then
         Make_Failed ("object directory missing after -D");
      end if;

      --  Test for simultaneity of -i and -D

      if Object_Directory_Path /= null and then In_Place_Mode then
         Make_Failed ("-i and -D cannot be used simultaneously");
      end if;

      --  If --subdirs= is specified, but not -P, this is equivalent to -D,
      --  except that the directory is created if it does not exist.

      if Subdirs /= null then
         if Object_Directory_Path /= null then
            Make_Failed ("--subdirs and -D cannot be used simultaneously");

         elsif In_Place_Mode then
            Make_Failed ("--subdirs and -i cannot be used simultaneously");

         else
            if not Is_Directory (Subdirs.all) then
               begin
                  Ada.Directories.Create_Path (Subdirs.all);
               exception
                  when others =>
                     Make_Failed ("unable to create object directory " &
                                  Subdirs.all);
               end;
            end if;

            Object_Directory_Present := True;

            declare
               Argv : constant String (1 .. Subdirs'Length) :=
                        Subdirs.all;
            begin
               Scan_Make_Arg (Argv);
            end;
         end if;
      end if;

      --  Deal with -C= switch

      if Gnatmake_Mapping_File /= null then

         --  First, check compatibility with other switches

         if Maximum_Processes > 1 then
            Make_Failed ("-C= switch is not compatible with -jnnn switch");
         end if;

         Fmap.Initialize (Gnatmake_Mapping_File.all);
         Add_Switch
           ("-gnatem=" & Gnatmake_Mapping_File.all,
            Compiler);
      end if;

      Osint.Add_Default_Search_Dirs;

      --  Source file lookups should be cached for efficiency. Source files
      --  are not supposed to change. However, we do that now only if no
      --  project file is used; if a project file is used, we do it just
      --  after changing the directory to the object directory.

      Osint.Source_File_Data (Cache => True);

      --  Read gnat.adc file to initialize Fname.UF

      Fname.UF.Initialize;

      if Config_File then
         begin
            Fname.SF.Read_Source_File_Name_Pragmas;

         exception
            when Err : SFN_Scan.Syntax_Error_In_GNAT_ADC =>
               Make_Failed (Exception_Message (Err));
         end;
      end if;

      if Debug.Debug_Flag_N then
         Opt.Keep_Temporary_Files := True;
      end if;
   end Initialize;

   ---------------------
   -- Is_In_Obsoleted --
   ---------------------

   function Is_In_Obsoleted (F : File_Name_Type) return Boolean is
   begin
      if F = No_File then
         return False;

      else
         declare
            Name  : constant String := Get_Name_String (F);
            First : Natural;
            F2    : File_Name_Type;

         begin
            First := Name'Last;
            while First > Name'First
              and then not Is_Directory_Separator (Name (First - 1))
            loop
               First := First - 1;
            end loop;

            if First /= Name'First then
               Name_Len := 0;
               Add_Str_To_Name_Buffer (Name (First .. Name'Last));
               F2 := Name_Find;

            else
               F2 := F;
            end if;

            return Obsoleted.Get (F2);
         end;
      end if;
   end Is_In_Obsoleted;

   ----------
   -- Link --
   ----------

   procedure Link
     (ALI_File : File_Name_Type;
      Args     : Argument_List;
      Success  : out Boolean)
   is
      Link_Args : Argument_List (1 .. Args'Length + 1);

   begin
      Get_Name_String (ALI_File);
      Link_Args (1) := new String'(Name_Buffer (1 .. Name_Len));

      Link_Args (2 .. Args'Length + 1) := Args;

      GNAT.OS_Lib.Normalize_Arguments (Link_Args);

      Display (Gnatlink.all, Link_Args);

      if Gnatlink_Path = null then
         Make_Failed ("error, unable to locate " & Gnatlink.all);
      end if;

      GNAT.OS_Lib.Spawn (Gnatlink_Path.all, Link_Args, Success);
   end Link;

   ---------------------------
   -- List_Bad_Compilations --
   ---------------------------

   procedure List_Bad_Compilations is
   begin
      if not No_Exit_Message then
         for J in Bad_Compilation.First .. Bad_Compilation.Last loop
            if Bad_Compilation.Table (J).File = No_File then
               null;
            elsif not Bad_Compilation.Table (J).Found then
               Inform (Bad_Compilation.Table (J).File, "not found");
            else
               Inform (Bad_Compilation.Table (J).File, "compilation error");
            end if;
         end loop;
      end if;
   end List_Bad_Compilations;

   -----------------
   -- List_Depend --
   -----------------

   procedure List_Depend is
      Lib_Name  : File_Name_Type;
      Obj_Name  : File_Name_Type;
      Src_Name  : File_Name_Type;

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

      if not Commands_To_Stdout then
         Set_Standard_Error;
      end if;
   end List_Depend;

   -----------------
   -- Make_Failed --
   -----------------

   procedure Make_Failed (S : String) is
   begin
      Fail_Program (S);
   end Make_Failed;

   --------------------
   -- Mark_Directory --
   --------------------

   procedure Mark_Directory (Dir : String; Mark : Lib_Mark_Type) is
      N : Name_Id;
      B : Byte;

      Real_Path : constant String := Normalize_Pathname (Dir, "");

   begin
      Name_Len := 0;

      if Real_Path'Length = 0 then
         Add_Str_To_Name_Buffer (Dir);

      else
         Add_Str_To_Name_Buffer (Real_Path);
      end if;

      --  Last character is supposed to be a directory separator

      if not Is_Directory_Separator (Name_Buffer (Name_Len)) then
         Add_Char_To_Name_Buffer (Directory_Separator);
      end if;

      --  Add flags to the already existing flags

      N := Name_Find;
      B := Get_Name_Table_Byte (N);
      Set_Name_Table_Byte (N, B or Mark);
   end Mark_Directory;

   ----------------------
   -- Process_Multilib --
   ----------------------

   procedure Process_Multilib  is
      Output_FD         : File_Descriptor;
      Output_Name       : String_Access;
      Arg_Index         : Natural := 0;
      Success           : Boolean := False;
      Return_Code       : Integer := 0;
      Multilib_Gcc_Path : String_Access;
      Multilib_Gcc      : String_Access;
      N_Read            : Integer := 0;
      Line              : String (1 .. 1000);
      Args              : Argument_List (1 .. N_M_Switch + 1);

   begin
      pragma Assert (N_M_Switch > 0 and RTS_Specified = null);

      --  In case we detected a multilib switch and the user has not
      --  manually specified a specific RTS we emulate the following command:
      --  gnatmake $FLAGS --RTS=$(gcc -print-multi-directory $FLAGS)

      --  First select the flags which might have an impact on multilib
      --  processing. Note that this is an heuristic selection and it
      --  will need to be maintained over time. The condition has to
      --  be kept synchronized with N_M_Switch counting in Scan_Make_Arg.

      for Next_Arg in 1 .. Argument_Count loop
         declare
            Argv : constant String := Argument (Next_Arg);

         begin
            if Argv'Length > 2
              and then Argv (1) = '-'
              and then Argv (2) = 'm'
              and then Argv /= "-margs"

              --  Ignore -mieee to avoid spawning an extra gcc in this case

              and then Argv /= "-mieee"
            then
               Arg_Index := Arg_Index + 1;
               Args (Arg_Index) := new String'(Argv);
            end if;
         end;
      end loop;

      pragma Assert (Arg_Index = N_M_Switch);

      Args (Args'Last) := new String'("-print-multi-directory");

      --  Call the GCC driver with the collected flags and save its
      --  output. Alternate design would be to link in gnatmake the
      --  relevant part of the GCC driver.

      Multilib_Gcc := Gcc;

      Multilib_Gcc_Path := GNAT.OS_Lib.Locate_Exec_On_Path (Multilib_Gcc.all);

      Create_Temp_Output_File (Output_FD, Output_Name);

      if Output_FD = Invalid_FD then
         return;
      end if;

      GNAT.OS_Lib.Spawn
        (Multilib_Gcc_Path.all, Args, Output_FD, Return_Code, False);
      Close (Output_FD);

      if Return_Code /= 0 then
         return;
      end if;

      --  Parse the GCC driver output which is a single line, removing CR/LF

      Output_FD := Open_Read (Output_Name.all, Binary);

      if Output_FD = Invalid_FD then
         return;
      end if;

      N_Read := Read (Output_FD, Line (1)'Address, Line'Length);
      Close (Output_FD);
      Delete_File (Output_Name.all, Success);

      for J in reverse 1 .. N_Read loop
         if Line (J) = ASCII.CR or else Line (J) = ASCII.LF then
            N_Read := N_Read - 1;
         else
            exit;
         end if;
      end loop;

      --  In case the standard RTS is selected do nothing

      if N_Read = 0 or else Line (1 .. N_Read) = "." then
         return;
      end if;

      --  Otherwise add -margs --RTS=output

      Scan_Make_Arg ("-margs");
      Scan_Make_Arg ("--RTS=" & Line (1 .. N_Read));
   end Process_Multilib;

   -------------------------------
   -- Report_Compilation_Failed --
   -------------------------------

   procedure Report_Compilation_Failed is
   begin
      Fail_Program ("");
   end Report_Compilation_Failed;

   ------------------------
   -- Sigint_Intercepted --
   ------------------------

   procedure Sigint_Intercepted is
   begin
      Set_Standard_Error;
      Write_Line ("*** Interrupted ***");

      --  Send SIGINT to all outstanding compilation processes spawned

      for J in 1 .. Outstanding_Compiles loop
         Kill (Running_Compile (J).Pid, Hard_Kill => False);
      end loop;

      Finish_Program (E_No_Compile);
   end Sigint_Intercepted;

   -------------------
   -- Scan_Make_Arg --
   -------------------

   procedure Scan_Make_Arg (Argv : String) is
      Success : Boolean;

   begin
      Gnatmake_Switch_Found := True;

      pragma Assert (Argv'First = 1);

      if Argv'Length = 0 then
         return;
      end if;

      --  If the previous switch has set the Output_File_Name_Present flag
      --  (that is we have seen a -o), then the next argument is the name of
      --  the output executable.

      if Output_File_Name_Present and then not Output_File_Name_Seen then
         Output_File_Name_Seen := True;

         if Argv (1) = '-' then
            Make_Failed ("output file name missing after -o");

         else
            Add_Switch ("-o", Linker);
            Add_Switch (Executable_Name (Argv), Linker);
         end if;

      --  If the previous switch has set the Object_Directory_Present flag
      --  (that is we have seen a -D), then the next argument is the path name
      --  of the object directory.

      elsif Object_Directory_Present
        and then not Object_Directory_Seen
      then
         Object_Directory_Seen := True;

         if Argv (1) = '-' then
            Make_Failed ("object directory path name missing after -D");

         elsif not Is_Directory (Argv) then
            Make_Failed ("cannot find object directory """ & Argv & """");

         else
            --  Record the object directory. Make sure it ends with a directory
            --  separator.

            declare
               Norm : constant String := Normalize_Pathname (Argv);

            begin
               if Norm (Norm'Last) = Directory_Separator then
                  Object_Directory_Path := new String'(Norm);
               else
                  Object_Directory_Path :=
                    new String'(Norm & Directory_Separator);
               end if;

               Add_Lib_Search_Dir (Norm);

               --  Specify the object directory to the binder

               Add_Switch ("-aO" & Norm, Binder);
            end;

         end if;

      --  Then check if we are dealing with -cargs/-bargs/-largs/-margs. These
      --  options are taken as is when found in package Compiler, Binder or
      --  Linker of the main project file.

      elsif Argv = "-bargs" or else
            Argv = "-cargs" or else
            Argv = "-largs" or else
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

      --  A special test is needed for the -o switch within a -largs since that
      --  is another way to specify the name of the final executable.

      elsif Program_Args = Linker and then Argv = "-o" then
         Make_Failed
           ("switch -o not allowed within a -largs. Use -o directly.");

      --  Check to see if we are reading switches after a -cargs, -bargs or
      --  -largs switch. If so, save it.

      elsif Program_Args /= None then

         --  Check to see if we are reading -I switches in order to take into
         --  account in the src & lib search directories.

         if Argv'Length > 2 and then Argv (1 .. 2) = "-I" then
            if Argv (3 .. Argv'Last) = "-" then
               Look_In_Primary_Dir := False;

            elsif Program_Args = Compiler then
               Add_Source_Search_Dir (Argv (3 .. Argv'Last));

            elsif Program_Args = Binder then
               Add_Library_Search_Dir (Argv (3 .. Argv'Last));
            end if;
         end if;

         Add_Switch (Argv, Program_Args);

         --  Make sure that all significant switches -m on the command line
         --  are counted.

         if Argv'Length > 2
           and then Argv (1 .. 2) = "-m"
           and then Argv /= "-mieee"
         then
            N_M_Switch := N_M_Switch + 1;
         end if;

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
               Gcc := new String'(Program_Args.all (1).all);

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch
                    (Program_Args.all (J).all, Compiler);
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
               Gnatbind := new String'(Program_Args.all (1).all);

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch
                    (Program_Args.all (J).all, Binder);
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
               Gnatlink := new String'(Program_Args.all (1).all);

               for J in 2 .. Program_Args.all'Last loop
                  Add_Switch (Program_Args.all (J).all, Linker);
               end loop;
            end;

         elsif Argv'Length >= 5 and then
           Argv (1 .. 5) = "--RTS"
         then
            Add_Switch (Argv, Compiler);
            Add_Switch (Argv, Binder);

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
                  if Src_Path_Name /= null
                    and then Lib_Path_Name /= null
                  then
                     --  Set RTS_*_Path_Name variables, so that correct direct-
                     --  ories will be set when Osint.Add_Default_Search_Dirs
                     --  is called later.

                     RTS_Src_Path_Name := Src_Path_Name;
                     RTS_Lib_Path_Name := Lib_Path_Name;

                  elsif Src_Path_Name = null
                    and then Lib_Path_Name = null
                  then
                     Make_Failed
                       ("RTS path not valid: missing adainclude and adalib "
                        & "directories");

                  elsif Src_Path_Name = null then
                     Make_Failed
                       ("RTS path not valid: missing adainclude directory");

                  elsif Lib_Path_Name = null then
                     Make_Failed
                       ("RTS path not valid: missing adalib directory");
                  end if;
               end;
            end if;

         elsif (Argv'Length >= 8 and then Argv (1 .. 8) = "--param=")
           or else (Argv'Length >= 10 and then Argv (1 .. 10) = "--sysroot=")
         then
            Add_Switch (Argv, Compiler);
            Add_Switch (Argv, Linker);

         elsif Argv = Create_Map_File_Switch then
            Map_File := new String'("");

         elsif Argv'Length > Create_Map_File_Switch'Length + 1
           and then
             Argv (1 .. Create_Map_File_Switch'Length) = Create_Map_File_Switch
           and then
             Argv (Create_Map_File_Switch'Length + 1) = '='
         then
            Map_File :=
              new String'
                (Argv (Create_Map_File_Switch'Length + 2 .. Argv'Last));

         else
            Scan_Make_Switches (Argv, Success);
         end if;

      --  If we have seen a regular switch process it

      elsif Argv (1) = '-' then
         if Argv'Length = 1 then
            Make_Failed ("switch character cannot be followed by a blank");

         --  Incorrect switches that should start with "--"

         elsif     (Argv'Length > 5  and then Argv (1 .. 5) = "-RTS=")
           or else (Argv'Length > 5  and then Argv (1 .. 5) = "-GCC=")
           or else (Argv'Length > 8  and then Argv (1 .. 7) = "-param=")
           or else (Argv'Length > 10 and then Argv (1 .. 10) = "-GNATLINK=")
           or else (Argv'Length > 10 and then Argv (1 .. 10) = "-GNATBIND=")
         then
            Make_Failed ("option " & Argv & " should start with '--'");

         --  -I-

         elsif Argv (2 .. Argv'Last) = "I-" then
            Look_In_Primary_Dir := False;

         --  Forbid  -?-  or  -??-  where ? is any character

         elsif (Argv'Length = 3 and then Argv (3) = '-')
           or else (Argv'Length = 4 and then Argv (4) = '-')
         then
            Make_Failed
              ("trailing ""-"" at the end of " & Argv & " forbidden.");

         --  -Idir

         elsif Argv (2) = 'I' then
            Add_Source_Search_Dir  (Argv (3 .. Argv'Last));
            Add_Library_Search_Dir (Argv (3 .. Argv'Last));
            Add_Switch (Argv, Compiler);
            Add_Switch (Argv, Binder);

         --  -aIdir (to gcc this is like a -I switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aI" then
            Add_Source_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch
              ("-I" & Argv (4 .. Argv'Last), Compiler);
            Add_Switch (Argv, Binder);

         --  -aOdir

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aO" then
            Add_Library_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch (Argv, Binder);

         --  -aLdir (to gnatbind this is like a -aO switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aL" then
            Mark_Directory (Argv (4 .. Argv'Last), Ada_Lib_Dir);
            Add_Library_Search_Dir (Argv (4 .. Argv'Last));
            Add_Switch
              ("-aO" & Argv (4 .. Argv'Last), Binder);

         --  -aamp_target=...

         elsif Argv'Length >= 13 and then Argv (2 .. 13) = "aamp_target=" then
            Add_Switch (Argv, Compiler);

            --  Set the aamp_target environment variable so that the binder and
            --  linker will use the proper target library. This is consistent
            --  with how things work when -aamp_target is passed on the command
            --  line to gnaampmake.

            Setenv ("aamp_target", Argv (14 .. Argv'Last));

         --  -Adir (to gnatbind this is like a -aO switch, to gcc like a -I)

         elsif Argv (2) = 'A' then
            Mark_Directory (Argv (3 .. Argv'Last), Ada_Lib_Dir);
            Add_Source_Search_Dir  (Argv (3 .. Argv'Last));
            Add_Library_Search_Dir (Argv (3 .. Argv'Last));
            Add_Switch ("-I"  & Argv (3 .. Argv'Last), Compiler);
            Add_Switch ("-aO" & Argv (3 .. Argv'Last), Binder);

         --  -Ldir

         elsif Argv (2) = 'L' then
            Add_Switch (Argv, Linker);

         --  For -gxxx, -pg, -mxxx, -fxxx, -Oxxx, pass the switch to both the
         --  compiler and the linker (except for -gnatxxx which is only for the
         --  compiler). Some of the -mxxx (for example -m64) and -fxxx (for
         --  example -ftest-coverage for gcov) need to be used when compiling
         --  the binder generated files, and using all these gcc switches for
         --  them should not be a problem. Pass -Oxxx to the linker for LTO.

         elsif
           (Argv (2) = 'g' and then (Argv'Last < 5
                                       or else Argv (2 .. 5) /= "gnat"))
             or else Argv (2 .. Argv'Last) = "pg"
             or else (Argv (2) = 'm' and then Argv'Last > 2)
             or else (Argv (2) = 'f' and then Argv'Last > 2)
             or else Argv (2) = 'O'
         then
            Add_Switch (Argv, Compiler);
            Add_Switch (Argv, Linker);

            --  The following condition has to be kept synchronized with
            --  the Process_Multilib one.

            if Argv (2) = 'm'
              and then Argv /= "-mieee"
            then
               N_M_Switch := N_M_Switch + 1;
            end if;

         --  -C=<mapping file>

         elsif Argv'Last > 2 and then Argv (2) = 'C' then
            if Argv (3) /= '=' or else Argv'Last <= 3 then
               Make_Failed ("illegal switch " & Argv);
            end if;

            Gnatmake_Mapping_File := new String'(Argv (4 .. Argv'Last));

         --  -D

         elsif Argv'Last = 2 and then Argv (2) = 'D' then
            if Project_File_Name /= null then
               Make_Failed
                 ("-D cannot be used in conjunction with a project file");

            else
               Scan_Make_Switches (Argv, Success);
            end if;

         --  -d

         elsif Argv (2) = 'd' and then Argv'Last = 2 then
            Display_Compilation_Progress := True;

         --  -i

         elsif Argv'Last = 2 and then Argv (2) = 'i' then
            if Project_File_Name /= null then
               Make_Failed
                 ("-i cannot be used in conjunction with a project file");
            else
               Scan_Make_Switches (Argv, Success);
            end if;

         --  -j (need to save the result)

         elsif Argv (2) = 'j' then
            Scan_Make_Switches (Argv, Success);

         --  -m

         elsif Argv (2) = 'm' then
            pragma Assert (Argv'Last = 2);
            Minimal_Recompilation := True;

         --  -u

         elsif Argv (2) = 'u' and then Argv'Last = 2 then
            Unique_Compile := True;
            Compile_Only   := True;
            Do_Bind_Step   := False;
            Do_Link_Step   := False;

         --  -U

         elsif Argv (2) = 'U'
           and then Argv'Last = 2
         then
            Unique_Compile := True;
            Compile_Only   := True;
            Do_Bind_Step   := False;
            Do_Link_Step   := False;

         --  -Pprj or -P prj (only once, and only on the command line)

         elsif Argv (2) = 'P' then
            if Project_File_Name /= null then
               Make_Failed ("cannot have several project files specified");

            elsif Object_Directory_Path /= null then
               Make_Failed
                 ("-D cannot be used in conjunction with a project file");

            elsif In_Place_Mode then
               Make_Failed
                 ("-i cannot be used in conjunction with a project file");

            elsif Argv'Last = 2 then

               --  -P is used alone: the project file name is the next option

               Project_File_Name_Present := True;

            else
               Project_File_Name := new String'(Argv (3 .. Argv'Last));
            end if;

         --  -vPx  (verbosity of the parsing of the project files)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "vP" then
            if Argv'Last /= 4 or else Argv (4) not in '0' .. '2' then
               Make_Failed
                 ("invalid verbosity level " & Argv (4 .. Argv'Last));
            end if;

         --  -Xext=val  (External assignment)

         elsif Argv (2) = 'X' then
            null;

         --  If -gnath is present, then generate the usage information right
         --  now and do not pass this option on to the compiler calls.

         elsif Argv = "-gnath" then
            Usage;

         --  If -gnatc is specified, make sure the bind and link steps are not
         --  executed.

         elsif Argv'Length >= 6 and then Argv (2 .. 6) = "gnatc" then

            --  If -gnatc is specified, make sure the bind and link steps are
            --  not executed.

            Add_Switch (Argv, Compiler);
            Operating_Mode           := Check_Semantics;
            Check_Object_Consistency := False;

            --  Except in CodePeer mode (set by -gnatcC), where we do want to
            --  call bind/link in CodePeer mode (-P switch).

            if Argv'Last >= 7 and then Argv (7) = 'C' then
               CodePeer_Mode := True;
            else
               Compile_Only := True;
               Do_Bind_Step := False;
               Do_Link_Step := False;
            end if;

         --  If -gnatA is specified, make sure that gnat.adc is never read

         elsif Argv'Length >= 6 and then Argv (2 .. 6) = "gnatA" then
            Add_Switch (Argv, Compiler);
            Opt.Config_File := False;

         elsif Argv (2 .. Argv'Last) = "nostdlib" then

            --  Pass -nstdlib to gnatbind and gnatlink

            No_Stdlib := True;
            Add_Switch (Argv, Binder);
            Add_Switch (Argv, Linker);

         elsif Argv (2 .. Argv'Last) = "nostdinc" then

            --  Pass -nostdinc to the Compiler and to gnatbind

            No_Stdinc := True;
            Add_Switch (Argv, Compiler);
            Add_Switch (Argv, Binder);

         --  All other switches are processed by Scan_Make_Switches. If the
         --  call returns with Gnatmake_Switch_Found = False, then the switch
         --  is passed to the compiler.

         else
            Scan_Make_Switches (Argv, Gnatmake_Switch_Found);

            if not Gnatmake_Switch_Found then
               Add_Switch (Argv, Compiler);
            end if;
         end if;

      --  If not a switch it must be a file name

      else
         Main_On_Command_Line := True;

         Add_File (Argv);
         Mains.Add_Main (Argv);
      end if;
   end Scan_Make_Arg;

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

end Make;
