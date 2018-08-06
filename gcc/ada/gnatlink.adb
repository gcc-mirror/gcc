------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T L I N K                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2018, Free Software Foundation, Inc.         --
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

--  Gnatlink usage: please consult the gnat documentation

with ALI;      use ALI;
with Csets;
with Gnatvsn;  use Gnatvsn;
with Indepsw;  use Indepsw;
with Namet;    use Namet;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Snames;
with Switch;   use Switch;
with System;   use System;
with Table;
with Targparm;
with Types;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;

with System.OS_Lib; use System.OS_Lib;
with System.CRTL;

with Interfaces.C_Streams; use Interfaces.C_Streams;
with Interfaces.C.Strings; use Interfaces.C.Strings;

procedure Gnatlink is
   pragma Ident (Gnatvsn.Gnat_Static_Version_String);

   Shared_Libgcc_String : constant String := "-shared-libgcc";
   Shared_Libgcc        : constant String_Access :=
                            new String'(Shared_Libgcc_String);
   --  Used to invoke gcc when the binder is invoked with -shared

   Static_Libgcc_String : constant String := "-static-libgcc";
   Static_Libgcc        : constant String_Access :=
                            new String'(Static_Libgcc_String);
   --  Used to invoke gcc when shared libs are not used

   package Gcc_Linker_Options is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Gcc_Linker_Options");
   --  Comments needed ???

   package Libpath is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 4096,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Libpath");
   --  Comments needed ???

   package Linker_Options is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Linker_Options");
   --  Comments needed ???

   package Linker_Objects is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Linker_Objects");
   --  This table collects the objects file to be passed to the linker. In the
   --  case where the linker command line is too long then programs objects
   --  are put on the Response_File_Objects table. Note that the binder object
   --  file and the user's objects remain in this table. This is very
   --  important because on the GNU linker command line the -L switch is not
   --  used to look for objects files but -L switch is used to look for
   --  objects listed in the response file. This is not a problem with the
   --  applications objects as they are specified with a full name.

   package Response_File_Objects is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Response_File_Objects");
   --  This table collects the objects file that are to be put in the response
   --  file. Only application objects are collected there (see details in
   --  Linker_Objects table comments)

   package Binder_Options_From_ALI is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1, -- equals low bound of Argument_List for Spawn
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Binder_Options_From_ALI");
   --  This table collects the switches from the ALI file of the main
   --  subprogram.

   package Binder_Options is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1, -- equals low bound of Argument_List for Spawn
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Binder_Options");
   --  This table collects the arguments to be passed to compile the binder
   --  generated file.

   Gcc : String_Access := Program_Name ("gcc", "gnatlink");

   Read_Mode : constant String := "r" & ASCII.NUL;

   Begin_Info : constant String := "--  BEGIN Object file/option list";
   End_Info   : constant String := "--  END Object file/option list   ";

   Gcc_Path             : String_Access;
   Linker_Path          : String_Access;
   Output_File_Name     : String_Access;
   Ali_File_Name        : String_Access;
   Binder_Spec_Src_File : String_Access;
   Binder_Body_Src_File : String_Access;
   Binder_Ali_File      : String_Access;
   Binder_Obj_File      : String_Access;

   Base_Command_Name    : String_Access;

   Target_Debuggable_Suffix : String_Access;

   Tname    : Temp_File_Name;
   Tname_FD : File_Descriptor := Invalid_FD;
   --  Temporary file used by linker to pass list of object files on
   --  certain systems with limitations on size of arguments.

   Debug_Flag_Present : Boolean := False;
   Verbose_Mode       : Boolean := False;
   Very_Verbose_Mode  : Boolean := False;

   Standard_Gcc : Boolean := True;

   Compile_Bind_File : Boolean := True;
   --  Set to False if bind file is not to be compiled

   Create_Map_File : Boolean := False;
   --  Set to True by switch -M. The map file name is derived from
   --  the ALI file name (mainprog.ali => mainprog.map).

   Object_List_File_Supported : Boolean;
   for Object_List_File_Supported'Size use Character'Size;
   pragma Import
     (C, Object_List_File_Supported, "__gnat_objlist_file_supported");
   --  Predicate indicating whether the linker has an option whereby the
   --  names of object files can be passed to the linker in a file.

   Object_File_Option_Ptr : Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Object_File_Option_Ptr, "__gnat_object_file_option");
   --  Pointer to a string representing the linker option which specifies
   --  the response file.

   Object_File_Option : constant String := Value (Object_File_Option_Ptr);
   --  The linker option which specifies the response file as a string

   Using_GNU_response_file : constant Boolean :=
     Object_File_Option'Length > 0
       and then Object_File_Option (Object_File_Option'Last) = '@';
   --  Whether a GNU response file is used

   Object_List_File_Required : Boolean := False;
   --  Set to True to force generation of a response file

   Shared_Libgcc_Default : Character;
   for Shared_Libgcc_Default'Size use Character'Size;
   pragma Import
     (C, Shared_Libgcc_Default, "__gnat_shared_libgcc_default");
   --  Indicates wether libgcc should be statically linked (use 'T') or
   --  dynamically linked (use 'H') by default.

   function Base_Name (File_Name : String) return String;
   --  Return just the file name part without the extension (if present)

   procedure Check_Existing_Executable (File_Name : String);
   --  Delete any existing executable to avoid accidentally updating the target
   --  of a symbolic link, but produce a Fatail_Error if File_Name matches any
   --  of the source file names. This avoids overwriting of extensionless
   --  source files by accident on systems where executables do not have
   --  extensions.

   procedure Delete (Name : String);
   --  Wrapper to unlink as status is ignored by this application

   procedure Error_Msg (Message : String);
   --  Output the error or warning Message

   procedure Exit_With_Error (Error : String);
   --  Output Error and exit program with a fatal condition

   procedure Process_Args;
   --  Go through all the arguments and build option tables

   procedure Process_Binder_File (Name : String);
   --  Reads the binder file and extracts linker arguments

   procedure Usage;
   --  Display usage

   procedure Write_Header;
   --  Show user the program name, version and copyright

   procedure Write_Usage;
   --  Show user the program options

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (File_Name : String) return String is
      Findex1 : Natural;
      Findex2 : Natural;

   begin
      Findex1 := File_Name'First;

      --  The file might be specified by a full path name. However,
      --  we want the path to be stripped away.

      for J in reverse File_Name'Range loop
         if Is_Directory_Separator (File_Name (J)) then
            Findex1 := J + 1;
            exit;
         end if;
      end loop;

      Findex2 := File_Name'Last;
      while Findex2 > Findex1 and then File_Name (Findex2) /=  '.' loop
         Findex2 := Findex2 - 1;
      end loop;

      if Findex2 = Findex1 then
         Findex2 := File_Name'Last + 1;
      end if;

      return File_Name (Findex1 .. Findex2 - 1);
   end Base_Name;

   -------------------------------
   -- Check_Existing_Executable --
   -------------------------------

   procedure Check_Existing_Executable (File_Name : String) is
      Ename : String := File_Name;
      Efile : File_Name_Type;
      Sfile : File_Name_Type;

   begin
      Canonical_Case_File_Name (Ename);
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Ename);
      Efile := Name_Find;

      for J in Units.Table'First .. Units.Last loop
         Sfile := Units.Table (J).Sfile;
         if Sfile = Efile then
            Exit_With_Error
              ("executable name """ & File_Name & """ matches "
               & "source file name """ & Get_Name_String (Sfile) & """");
         end if;
      end loop;

      Delete (File_Name);
   end Check_Existing_Executable;

   ------------
   -- Delete --
   ------------

   procedure Delete (Name : String) is
      Status : int;
      pragma Unreferenced (Status);
   begin
      Status := unlink (Name'Address);
      --  Is it really right to ignore an error here ???
   end Delete;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Message : String) is
   begin
      Write_Str (Base_Command_Name.all);
      Write_Str (": ");
      Write_Str (Message);
      Write_Eol;
   end Error_Msg;

   ---------------------
   -- Exit_With_Error --
   ---------------------

   procedure Exit_With_Error (Error : String) is
   begin
      Error_Msg (Error);
      Exit_Program (E_Fatal);
   end Exit_With_Error;

   ------------------
   -- Process_Args --
   ------------------

   procedure Process_Args is
      Next_Arg : Integer;

      Skip_Next : Boolean := False;
      --  Set to true if the next argument is to be added into the list of
      --  linker's argument without parsing it.

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

      --  Start of processing for Process_Args

   begin
      --  First, check for --version and --help

      Check_Version_And_Help ("GNATLINK", "1996");

      --  Loop through arguments of gnatlink command

      Next_Arg := 1;
      loop
         exit when Next_Arg > Argument_Count;

         Process_One_Arg : declare
            Arg : constant String := Argument (Next_Arg);

         begin
            --  Case of argument which is a switch

            --  We definitely need section by section comments here ???

            if Skip_Next then

               --  This argument must not be parsed, just add it to the
               --  list of linker's options.

               Skip_Next := False;

               Linker_Options.Increment_Last;
               Linker_Options.Table (Linker_Options.Last) :=
                 new String'(Arg);

            elsif Arg'Length /= 0 and then Arg (1) = '-' then
               if Arg'Length > 4 and then Arg (2 .. 5) = "gnat" then
                  Exit_With_Error
                    ("invalid switch: """ & Arg & """ (gnat not needed here)");
               end if;

               if Arg = "-Xlinker" then

                  --  Next argument should be sent directly to the linker.
                  --  We do not want to parse it here.

                  Skip_Next := True;

                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                    new String'(Arg);

               elsif Arg (2) = 'g'
                 and then (Arg'Length < 5 or else Arg (2 .. 5) /= "gnat")
               then
                  Debug_Flag_Present := True;

                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                   new String'(Arg);

                  Binder_Options.Increment_Last;
                  Binder_Options.Table (Binder_Options.Last) :=
                    Linker_Options.Table (Linker_Options.Last);

               elsif Arg'Length >= 3 and then Arg (2) = 'M' then
                  declare
                     Switches : String_List_Access;

                  begin
                     Convert (Map_File, Arg (3 .. Arg'Last), Switches);

                     if Switches /= null then
                        for J in Switches'Range loop
                           Linker_Options.Increment_Last;
                           Linker_Options.Table (Linker_Options.Last) :=
                             Switches (J);
                        end loop;
                     end if;
                  end;

               elsif Arg'Length = 2 then
                  case Arg (2) is
                     when 'f' =>
                        if Object_List_File_Supported then
                           Object_List_File_Required := True;
                        else
                           Exit_With_Error
                             ("Object list file not supported on this target");
                        end if;

                     when 'M' =>
                        Create_Map_File := True;

                     when 'n' =>
                        Compile_Bind_File := False;

                     when 'o' =>
                        Next_Arg := Next_Arg + 1;

                        if Next_Arg > Argument_Count then
                           Exit_With_Error ("Missing argument for -o");
                        end if;

                        Output_File_Name :=
                          new String'(Executable_Name
                                        (Argument (Next_Arg),
                                         Only_If_No_Suffix => True));

                     when 'P' =>
                        Opt.CodePeer_Mode := True;

                     when 'R' =>
                        Opt.Run_Path_Option := False;

                     when 'v' =>

                        --  Support "double" verbose mode.  Second -v
                        --  gets sent to the linker and binder phases.

                        if Verbose_Mode then
                           Very_Verbose_Mode := True;

                           Linker_Options.Increment_Last;
                           Linker_Options.Table (Linker_Options.Last) :=
                            new String'(Arg);

                           Binder_Options.Increment_Last;
                           Binder_Options.Table (Binder_Options.Last) :=
                             Linker_Options.Table (Linker_Options.Last);

                        else
                           Verbose_Mode := True;

                        end if;

                     when others =>
                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                         new String'(Arg);

                  end case;

               elsif Arg (2) = 'B' then
                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                    new String'(Arg);

                  Binder_Options.Increment_Last;
                  Binder_Options.Table (Binder_Options.Last) :=
                    Linker_Options.Table (Linker_Options.Last);

               elsif Arg'Length >= 7 and then Arg (1 .. 7) = "--LINK=" then
                  if Arg'Length = 7 then
                     Exit_With_Error ("Missing argument for --LINK=");
                  end if;

                  declare
                     L_Args : constant Argument_List_Access :=
                               Argument_String_To_List (Arg (8 .. Arg'Last));
                  begin
                     --  The linker program is the first argument

                     Linker_Path :=
                      System.OS_Lib.Locate_Exec_On_Path (L_Args.all (1).all);

                     if Linker_Path = null then
                        Exit_With_Error
                          ("Could not locate linker: " & L_Args.all (1).all);
                     end if;

                     --  The other arguments are passed as-is to the linker and
                     --  override those coming from --GCC= if any.

                     if L_Args.all'Last >= 2 then
                        Gcc_Linker_Options.Set_Last (0);
                     end if;

                     for J in 2 .. L_Args.all'Last loop
                        Gcc_Linker_Options.Increment_Last;
                        Gcc_Linker_Options.Table
                          (Gcc_Linker_Options.Last) :=
                                             new String'(L_Args.all (J).all);
                     end loop;
                  end;

               elsif Arg'Length >= 6 and then Arg (1 .. 6) = "--GCC=" then
                  if Arg'Length = 6 then
                     Exit_With_Error ("Missing argument for --GCC=");
                  end if;

                  declare
                     Program_Args : constant Argument_List_Access :=
                                      Argument_String_To_List
                                                 (Arg (7 .. Arg'Last));

                  begin
                     if Program_Args.all (1).all /= Gcc.all then
                        Gcc := new String'(Program_Args.all (1).all);
                        Standard_Gcc := False;
                     end if;

                     --  Set appropriate flags for switches passed

                     for J in 2 .. Program_Args.all'Last loop
                        declare
                           Arg : constant String := Program_Args.all (J).all;
                           AF  : constant Integer := Arg'First;

                        begin
                           if Arg'Length /= 0 and then Arg (AF) = '-' then
                              if Arg (AF + 1) = 'g'
                                and then (Arg'Length = 2
                                  or else Arg (AF + 2) in '0' .. '3'
                                  or else Arg (AF + 2 .. Arg'Last) = "coff")
                              then
                                 Debug_Flag_Present := True;
                              end if;
                           end if;

                           --  Add directory to source search dirs so that
                           --  Get_Target_Parameters can find system.ads

                           if Arg (AF .. AF + 1) = "-I"
                             and then Arg'Length > 2
                           then
                              Add_Src_Search_Dir (Arg (AF + 2 .. Arg'Last));
                           end if;

                           --  Pass to gcc for compiling binder generated file
                           --  No use passing libraries, it will just generate
                           --  a warning

                           if not (Arg (AF .. AF + 1) = "-l"
                             or else Arg (AF .. AF + 1) = "-L")
                           then
                              Binder_Options.Increment_Last;
                              Binder_Options.Table (Binder_Options.Last) :=
                                new String'(Arg);
                           end if;

                           --  Pass to gcc for linking program

                           Gcc_Linker_Options.Increment_Last;
                           Gcc_Linker_Options.Table
                             (Gcc_Linker_Options.Last) := new String'(Arg);
                        end;
                     end loop;
                  end;

               --  Send all multi-character switches not recognized as
               --  a special case by gnatlink to the linker/loader stage.

               else
                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                    new String'(Arg);
               end if;

            --  Here if argument is a file name rather than a switch

            else
               --  If explicit ali file, capture it

               if Arg'Length > 4
                 and then Arg (Arg'Last - 3 .. Arg'Last) = ".ali"
               then
                  if Ali_File_Name = null then
                     Ali_File_Name := new String'(Arg);
                  else
                     Exit_With_Error ("cannot handle more than one ALI file");
                  end if;

               --  If target object file, record object file

               elsif Arg'Length > Get_Target_Object_Suffix.all'Length
                 and then Arg
                   (Arg'Last -
                    Get_Target_Object_Suffix.all'Length + 1 .. Arg'Last)
                   = Get_Target_Object_Suffix.all
               then
                  Linker_Objects.Increment_Last;
                  Linker_Objects.Table (Linker_Objects.Last) :=
                    new String'(Arg);

               --  If host object file, record object file

               elsif Arg'Length > Get_Object_Suffix.all'Length
                 and then Arg
                   (Arg'Last - Get_Object_Suffix.all'Length + 1 .. Arg'Last)
                                                = Get_Object_Suffix.all
               then
                  Linker_Objects.Increment_Last;
                  Linker_Objects.Table (Linker_Objects.Last) :=
                    new String'(Arg);

               --  If corresponding ali file exists, capture it

               elsif Ali_File_Name = null
                 and then Is_Regular_File (Arg & ".ali")
               then
                  Ali_File_Name := new String'(Arg & ".ali");

               --  Otherwise assume this is a linker options entry, but
               --  see below for interesting adjustment to this assumption.

               else
                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                    new String'(Arg);
               end if;
            end if;
         end Process_One_Arg;

         Next_Arg := Next_Arg + 1;
      end loop;

      --  Compile the bind file with warnings suppressed, because
      --  otherwise the with of the main program may cause junk warnings.

      Binder_Options.Increment_Last;
      Binder_Options.Table (Binder_Options.Last) := new String'("-gnatws");

      --  If we did not get an ali file at all, and we had at least one
      --  linker option, then assume that was the intended ali file after
      --  all, so that we get a nicer message later on.

      if Ali_File_Name = null
        and then Linker_Options.Last >= Linker_Options.First
      then
         Ali_File_Name :=
           new String'(Linker_Options.Table (Linker_Options.First).all
                       & ".ali");
      end if;
   end Process_Args;

   -------------------------
   -- Process_Binder_File --
   -------------------------

   procedure Process_Binder_File (Name : String) is
      Fd : FILEs;
      --  Binder file's descriptor

      Link_Bytes : Integer := 0;
      --  Projected number of bytes for the linker command line

      Link_Max : Integer;
      pragma Import (C, Link_Max, "__gnat_link_max");
      --  Maximum number of bytes on the command line supported by the OS
      --  linker. Passed this limit the response file mechanism must be used
      --  if supported.

      Next_Line : String (1 .. 1000);
      --  Current line value

      Nlast  : Integer;
      Nfirst : Integer;
      --  Current line slice (the slice does not contain line terminator)

      Last : Integer;
      --  Current line last character for shared libraries (without version)

      Objs_Begin : Integer := 0;
      --  First object file index in Linker_Objects table

      Objs_End : Integer := 0;
      --  Last object file index in Linker_Objects table

      Status : int;
      pragma Warnings (Off, Status);
      --  Used for various Interfaces.C_Streams calls

      Closing_Status : Boolean;
      pragma Warnings (Off, Closing_Status);
      --  For call to Close

      GNAT_Static : Boolean := False;
      --  Save state of -static option

      GNAT_Shared : Boolean := False;
      --  Save state of -shared option

      Xlinker_Was_Previous : Boolean := False;
      --  Indicate that "-Xlinker" was the option preceding the current option.
      --  If True, then the current option is never suppressed.

      --  Rollback data

      --  These data items are used to store current binder file context. The
      --  context is composed of the file descriptor position and the current
      --  line together with the slice indexes (first and last position) for
      --  this line. The rollback data are used by the Store_File_Context and
      --  Rollback_File_Context routines below. The file context mechanism
      --  interact only with the Get_Next_Line call. For example:

      --     Store_File_Context;
      --     Get_Next_Line;
      --     Rollback_File_Context;
      --     Get_Next_Line;

      --  Both Get_Next_Line calls above will read the exact same data from
      --  the file. In other words, Next_Line, Nfirst and Nlast variables
      --  will be set with the exact same values.

      RB_File_Pos  : long;                -- File position
      RB_Next_Line : String (1 .. 1000);  -- Current line content
      RB_Nlast     : Integer;             -- Slice last index
      RB_Nfirst    : Integer;             -- Slice first index

      Run_Path_Option_Ptr : Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Run_Path_Option_Ptr, "__gnat_run_path_option");
      --  Pointer to string representing the native linker option which
      --  specifies the path where the dynamic loader should find shared
      --  libraries. Equal to null string if this system doesn't support it.

      Libgcc_Subdir_Ptr : Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Libgcc_Subdir_Ptr, "__gnat_default_libgcc_subdir");
      --  Pointer to string indicating the installation subdirectory where
      --  a default shared libgcc might be found.

      Object_Library_Ext_Ptr : Interfaces.C.Strings.chars_ptr;
      pragma Import
        (C, Object_Library_Ext_Ptr, "__gnat_object_library_extension");
      --  Pointer to string specifying the default extension for
      --  object libraries, e.g. Unix uses ".a".

      Separate_Run_Path_Options : Boolean;
      for Separate_Run_Path_Options'Size use Character'Size;
      pragma Import
        (C, Separate_Run_Path_Options, "__gnat_separate_run_path_options");
      --  Whether separate rpath options should be emitted for each directory

      procedure Get_Next_Line;
      --  Read the next line from the binder file without the line
      --  terminator.

      function Index (S, Pattern : String) return Natural;
      --  Return the last occurrence of Pattern in S, or 0 if none

      procedure Store_File_Context;
      --  Store current file context, Fd position and current line data.
      --  The file context is stored into the rollback data above (RB_*).
      --  Store_File_Context can be called at any time, only the last call
      --  will be used (i.e. this routine overwrites the file context).

      procedure Rollback_File_Context;
      --  Restore file context from rollback data. This routine must be called
      --  after Store_File_Context. The binder file context will be restored
      --  with the data stored by the last Store_File_Context call.

      procedure Write_RF (S : String);
      --  Write a string to the response file and check if it was successful.
      --  Fail the program if it was not successful (disk full).

      -------------------
      -- Get_Next_Line --
      -------------------

      procedure Get_Next_Line is
         Fchars : chars;

      begin
         Fchars := fgets (Next_Line'Address, Next_Line'Length, Fd);

         if Fchars = System.Null_Address then
            Exit_With_Error ("Error reading binder output");
         end if;

         Nfirst := Next_Line'First;
         Nlast := Nfirst;
         while Nlast <= Next_Line'Last
           and then Next_Line (Nlast) /= ASCII.LF
           and then Next_Line (Nlast) /= ASCII.CR
         loop
            Nlast := Nlast + 1;
         end loop;

         Nlast := Nlast - 1;
      end Get_Next_Line;

      -----------
      -- Index --
      -----------

      function Index (S, Pattern : String) return Natural is
         Len : constant Natural := Pattern'Length;

      begin
         for J in reverse S'First .. S'Last - Len + 1 loop
            if Pattern = S (J .. J + Len - 1) then
               return J;
            end if;
         end loop;

         return 0;
      end Index;

      ---------------------------
      -- Rollback_File_Context --
      ---------------------------

      procedure Rollback_File_Context is
      begin
         Next_Line := RB_Next_Line;
         Nfirst    := RB_Nfirst;
         Nlast     := RB_Nlast;
         Status    := fseek (Fd, RB_File_Pos, Interfaces.C_Streams.SEEK_SET);

         if Status = -1 then
            Exit_With_Error ("Error setting file position");
         end if;
      end Rollback_File_Context;

      ------------------------
      -- Store_File_Context --
      ------------------------

      procedure Store_File_Context is
         use type System.CRTL.long;

      begin
         RB_Next_Line := Next_Line;
         RB_Nfirst    := Nfirst;
         RB_Nlast     := Nlast;
         RB_File_Pos  := ftell (Fd);

         if RB_File_Pos = -1 then
            Exit_With_Error ("Error getting file position");
         end if;
      end Store_File_Context;

      --------------
      -- Write_RF --
      --------------

      procedure Write_RF (S : String) is
         Success    : Boolean            := True;
         Back_Slash : constant Character := '\';

      begin
         --  If a GNU response file is used, space and backslash need to be
         --  escaped because they are interpreted as a string separator and
         --  an escape character respectively by the underlying mechanism.
         --  On the other hand, quote and double-quote are not escaped since
         --  they are interpreted as string delimiters on both sides.

         if Using_GNU_response_file then
            for J in S'Range loop
               if S (J) = ' ' or else S (J) = '\' then
                  if Write (Tname_FD, Back_Slash'Address, 1) /= 1 then
                     Success := False;
                  end if;
               end if;

               if Write (Tname_FD, S (J)'Address, 1) /= 1 then
                  Success := False;
               end if;
            end loop;

         else
            if Write (Tname_FD, S'Address, S'Length) /= S'Length then
               Success := False;
            end if;
         end if;

         if Write (Tname_FD, ASCII.LF'Address, 1) /= 1 then
            Success := False;
         end if;

         if not Success then
            Exit_With_Error ("Error generating response file: disk full");
         end if;
      end Write_RF;

   --  Start of processing for Process_Binder_File

   begin
      Fd := fopen (Name'Address, Read_Mode'Address);

      if Fd = NULL_Stream then
         Exit_With_Error ("Failed to open binder output");
      end if;

      --  Skip up to the Begin Info line

      loop
         Get_Next_Line;
         exit when Next_Line (Nfirst .. Nlast) = Begin_Info;
      end loop;

      loop
         Get_Next_Line;

         --  Go to end when end line is reached (this will happen in
         --  High_Integrity_Mode where no -L switches are generated)

         exit when Next_Line (Nfirst .. Nlast) = End_Info;

         Next_Line (Nfirst .. Nlast - 8) := Next_Line (Nfirst + 8 .. Nlast);
         Nlast := Nlast - 8;

         --  Go to next section when switches are reached

         exit when Next_Line (1) = '-';

         --  Otherwise we have another object file to collect

         Linker_Objects.Increment_Last;

         --  Mark the positions of first and last object files in case they
         --  need to be placed with a named file on systems having linker
         --  line limitations.

         if Objs_Begin = 0 then
            Objs_Begin := Linker_Objects.Last;
         end if;

         Linker_Objects.Table (Linker_Objects.Last) :=
           new String'(Next_Line (Nfirst .. Nlast));

         --  Nlast - Nfirst + 1, for the size, plus one for the space between
         --  each arguments.

         Link_Bytes := Link_Bytes + Nlast - Nfirst + 2;
      end loop;

      Objs_End := Linker_Objects.Last;

      --  Continue to compute the Link_Bytes, the linker options are part of
      --  command line length.

      Store_File_Context;

      while Next_Line (Nfirst .. Nlast) /= End_Info loop
         Link_Bytes := Link_Bytes + Nlast - Nfirst + 2;
         Get_Next_Line;
      end loop;

      Rollback_File_Context;

      --  On systems that have limitations on handling very long linker lines
      --  we make use of the system linker option which takes a list of object
      --  file names from a file instead of the command line itself. What we do
      --  is to replace the list of object files by the special linker option
      --  which then reads the object file list from a file instead. The option
      --  to read from a file instead of the command line is only triggered if
      --  a conservative threshold is passed.

      if Object_List_File_Required
        or else (Object_List_File_Supported
                   and then Link_Bytes > Link_Max)
      then
         --  Create a temporary file containing the Ada user object files
         --  needed by the link. This list is taken from the bind file and is
         --  output one object per line for maximal compatibility with linkers
         --  supporting this option.

         Create_Temp_File (Tname_FD, Tname);

         --  ??? File descriptor should be checked to not be Invalid_FD.
         --  ??? Status of Write and Close operations should be checked, and
         --  failure should occur if a status is wrong.

         for J in Objs_Begin .. Objs_End loop
            Write_RF (Linker_Objects.Table (J).all);

            Response_File_Objects.Increment_Last;
            Response_File_Objects.Table (Response_File_Objects.Last) :=
              Linker_Objects.Table (J);
         end loop;

         Close (Tname_FD, Closing_Status);

         --  Add the special objects list file option together with the name
         --  of the temporary file (removing the null character) to the objects
         --  file table.

         Linker_Objects.Table (Objs_Begin) :=
           new String'(Object_File_Option &
                       Tname (Tname'First .. Tname'Last - 1));

         --  The slots containing these object file names are then removed
         --  from the objects table so they do not appear in the link. They are
         --  removed by moving up the linker options and non-Ada object files
         --  appearing after the Ada object list in the table.

         declare
            N : Integer;

         begin
            N := Objs_End - Objs_Begin + 1;

            for J in Objs_End + 1 .. Linker_Objects.Last loop
               Linker_Objects.Table (J - N + 1) := Linker_Objects.Table (J);
            end loop;

            Linker_Objects.Set_Last (Linker_Objects.Last - N + 1);
         end;
      end if;

      --  Process switches and options

      if Next_Line (Nfirst .. Nlast) /= End_Info then
         Xlinker_Was_Previous := False;

         loop
            if Xlinker_Was_Previous
              or else Next_Line (Nfirst .. Nlast) = "-Xlinker"
            then
               Linker_Options.Increment_Last;
               Linker_Options.Table (Linker_Options.Last) :=
                 new String'(Next_Line (Nfirst .. Nlast));

            elsif Next_Line (Nfirst .. Nlast) = "-static" then
               GNAT_Static := True;

            elsif Next_Line (Nfirst .. Nlast) = "-shared" then
               GNAT_Shared := True;

            --  Add binder options only if not already set on the command line.
            --  This rule is a way to control the linker options order.

            else
               if Nlast > Nfirst + 2 and then
                 Next_Line (Nfirst .. Nfirst + 1) = "-L"
               then
                  --  Construct a library search path for use later to locate
                  --  static gnatlib libraries.

                  if Libpath.Last > 1 then
                     Libpath.Increment_Last;
                     Libpath.Table (Libpath.Last) := Path_Separator;
                  end if;

                  for I in Nfirst + 2 .. Nlast loop
                     Libpath.Increment_Last;
                     Libpath.Table (Libpath.Last) := Next_Line (I);
                  end loop;

                  Linker_Options.Increment_Last;

                  Linker_Options.Table (Linker_Options.Last) :=
                    new String'(Next_Line (Nfirst .. Nlast));

               elsif Next_Line (Nfirst .. Nlast) = "-lgnarl"
                 or else Next_Line (Nfirst .. Nlast) = "-lgnat"
                 or else
                   Next_Line
                     (1 .. Natural'Min (Nlast, 8 + Library_Version'Length)) =
                       Shared_Lib ("gnarl")
                 or else
                   Next_Line
                     (1 .. Natural'Min (Nlast, 7 + Library_Version'Length)) =
                       Shared_Lib ("gnat")
               then
                  --  If it is a shared library, remove the library version.
                  --  We will be looking for the static version of the library
                  --  as it is in the same directory as the shared version.

                  if Nlast >= Library_Version'Length
                    and then Next_Line
                      (Nlast - Library_Version'Length + 1 .. Nlast)
                        = Library_Version
                  then
                     --  Set Last to point to last character before the
                     --  library version.

                     Last := Nlast - Library_Version'Length - 1;
                  else
                     Last := Nlast;
                  end if;

                  --  Given a Gnat standard library, search the library path to
                  --  find the library location.

                  --  Shouldn't we abstract a proc here, we are getting awfully
                  --  heavily nested ???

                  declare
                     File_Path : String_Access;

                     Object_Lib_Extension : constant String :=
                       Value (Object_Library_Ext_Ptr);

                     File_Name : constant String := "lib" &
                       Next_Line (Nfirst + 2 .. Last) & Object_Lib_Extension;

                     Run_Path_Opt : constant String :=
                       Value (Run_Path_Option_Ptr);

                     GCC_Index          : Natural;
                     Run_Path_Opt_Index : Natural := 0;

                  begin
                     File_Path :=
                       Locate_Regular_File (File_Name,
                         String (Libpath.Table (1 .. Libpath.Last)));

                     if File_Path /= null then
                        if GNAT_Static then

                           --  If static gnatlib found, explicitly specify to
                           --  overcome possible linker default usage of shared
                           --  version.

                           Linker_Options.Increment_Last;

                           Linker_Options.Table (Linker_Options.Last) :=
                             new String'(File_Path.all);

                        elsif GNAT_Shared then
                           if Opt.Run_Path_Option then

                              --  If shared gnatlib desired, add appropriate
                              --  system specific switch so that it can be
                              --  located at runtime.

                              if Run_Path_Opt'Length /= 0 then

                                 --  Output the system specific linker command
                                 --  that allows the image activator to find
                                 --  the shared library at runtime. Also add
                                 --  path to find libgcc_s.so, if relevant.

                                 declare
                                    Path : String (1 .. File_Path'Length + 15);

                                    Path_Last : constant Natural :=
                                                  File_Path'Length;

                                 begin
                                    Path (1 .. File_Path'Length) :=
                                      File_Path.all;

                                 --  To find the location of the shared version
                                 --  of libgcc, we look for "gcc-lib" in the
                                 --  path of the library. However, this
                                 --  subdirectory is no longer present in
                                 --  recent versions of GCC. So, we look for
                                 --  the last subdirectory "lib" in the path.

                                    GCC_Index :=
                                      Index (Path (1 .. Path_Last), "gcc-lib");

                                    if GCC_Index /= 0 then

                                       --  The shared version of libgcc is
                                       --  located in the parent directory.

                                       GCC_Index := GCC_Index - 1;

                                    else
                                       GCC_Index :=
                                         Index
                                           (Path (1 .. Path_Last),
                                            "/lib/");

                                       if GCC_Index = 0 then
                                          GCC_Index :=
                                            Index (Path (1 .. Path_Last),
                                                   Directory_Separator & "lib"
                                                   & Directory_Separator);
                                       end if;

                                       --  If we have found a "lib" subdir in
                                       --  the path to libgnat, the possible
                                       --  shared libgcc of interest by default
                                       --  is in libgcc_subdir at the same
                                       --  level.

                                       if GCC_Index /= 0 then
                                          declare
                                             Subdir : constant String :=
                                               Value (Libgcc_Subdir_Ptr);
                                          begin
                                             Path
                                               (GCC_Index + 1 ..
                                                GCC_Index + Subdir'Length) :=
                                               Subdir;
                                             GCC_Index :=
                                               GCC_Index + Subdir'Length;
                                          end;
                                       end if;
                                    end if;

                                 --  Look for an eventual run_path_option in
                                 --  the linker switches.

                                    if Separate_Run_Path_Options then
                                       Linker_Options.Increment_Last;
                                       Linker_Options.Table
                                         (Linker_Options.Last) :=
                                           new String'
                                             (Run_Path_Opt
                                              & File_Path
                                                (1 .. File_Path'Length
                                                 - File_Name'Length));

                                       if GCC_Index /= 0 then
                                          Linker_Options.Increment_Last;
                                          Linker_Options.Table
                                            (Linker_Options.Last) :=
                                            new String'
                                              (Run_Path_Opt
                                               & Path (1 .. GCC_Index));
                                       end if;

                                    else
                                       for J in reverse
                                         1 .. Linker_Options.Last
                                       loop
                                          if Linker_Options.Table (J) /= null
                                            and then
                                              Linker_Options.Table (J)'Length
                                                        > Run_Path_Opt'Length
                                            and then
                                              Linker_Options.Table (J)
                                                (1 .. Run_Path_Opt'Length) =
                                                                 Run_Path_Opt
                                          then
                                             --  We have found an already
                                             --  specified run_path_option:
                                             --  we will add to this
                                             --  switch, because only one
                                             --  run_path_option should be
                                             --  specified.

                                             Run_Path_Opt_Index := J;
                                             exit;
                                          end if;
                                       end loop;

                                       --  If there is no run_path_option, we
                                       --  need to add one.

                                       if Run_Path_Opt_Index = 0 then
                                          Linker_Options.Increment_Last;
                                       end if;

                                       if GCC_Index = 0 then
                                          if Run_Path_Opt_Index = 0 then
                                             Linker_Options.Table
                                               (Linker_Options.Last) :=
                                                 new String'
                                                   (Run_Path_Opt
                                                    & File_Path
                                                      (1 .. File_Path'Length
                                                       - File_Name'Length));

                                          else
                                             Linker_Options.Table
                                               (Run_Path_Opt_Index) :=
                                                 new String'
                                                   (Linker_Options.Table
                                                     (Run_Path_Opt_Index).all
                                                    & Path_Separator
                                                    & File_Path
                                                      (1 .. File_Path'Length
                                                       - File_Name'Length));
                                          end if;

                                       else
                                          if Run_Path_Opt_Index = 0 then
                                             Linker_Options.Table
                                               (Linker_Options.Last) :=
                                                 new String'
                                                   (Run_Path_Opt
                                                    & File_Path
                                                      (1 .. File_Path'Length
                                                       - File_Name'Length)
                                                    & Path_Separator
                                                    & Path (1 .. GCC_Index));

                                          else
                                             Linker_Options.Table
                                               (Run_Path_Opt_Index) :=
                                                 new String'
                                                   (Linker_Options.Table
                                                     (Run_Path_Opt_Index).all
                                                    & Path_Separator
                                                    & File_Path
                                                      (1 .. File_Path'Length
                                                       - File_Name'Length)
                                                    & Path_Separator
                                                    & Path (1 .. GCC_Index));
                                          end if;
                                       end if;
                                    end if;
                                 end;
                              end if;
                           end if;

                           --  Then we add the appropriate -l switch

                           Linker_Options.Increment_Last;
                           Linker_Options.Table (Linker_Options.Last) :=
                             new String'(Next_Line (Nfirst .. Nlast));
                        end if;

                     else
                        --  If gnatlib library not found, then add it anyway in
                        --  case some other mechanism may find it.

                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                          new String'(Next_Line (Nfirst .. Nlast));
                     end if;
                  end;
               else
                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                    new String'(Next_Line (Nfirst .. Nlast));
               end if;
            end if;

            Xlinker_Was_Previous := Next_Line (Nfirst .. Nlast) = "-Xlinker";

            Get_Next_Line;
            exit when Next_Line (Nfirst .. Nlast) = End_Info;

            Next_Line (Nfirst .. Nlast - 8) := Next_Line (Nfirst + 8 .. Nlast);
            Nlast := Nlast - 8;
         end loop;
      end if;

      --  If -shared was specified, invoke gcc with -shared-libgcc

      if GNAT_Shared then
         Linker_Options.Increment_Last;
         Linker_Options.Table (Linker_Options.Last) := Shared_Libgcc;
      end if;

      Status := fclose (Fd);
   end Process_Binder_File;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Write_Str ("Usage: ");
      Write_Str (Base_Command_Name.all);
      Write_Str (" switches mainprog.ali [non-Ada-objects] [linker-options]");
      Write_Eol;
      Write_Eol;
      Write_Line ("  mainprog.ali   the ALI file of the main program");
      Write_Eol;
      Write_Eol;
      Display_Usage_Version_And_Help;
      Write_Line ("  -f    Force object file list to be generated");
      Write_Line ("  -g    Compile binder source file with debug information");
      Write_Line ("  -n    Do not compile the binder source file");
      Write_Line ("  -P    Process files for use by CodePeer");
      Write_Line ("  -R    Do not use a run_path_option");
      Write_Line ("  -v    Verbose mode");
      Write_Line ("  -v -v Very verbose mode");
      Write_Eol;
      Write_Line ("  -o nam     Use 'nam' as the name of the executable");
      Write_Line ("  -Bdir      Load compiler executables from dir");

      if Is_Supported (Map_File) then
         Write_Line ("  -Mmap      Create map file map");
         Write_Line ("  -M         Create map file mainprog.map");
      end if;

      Write_Line ("  --GCC=comp Use 'comp' as the compiler rather than 'gcc'");
      Write_Line ("  --LINK=lnk Use 'lnk' as the linker rather than 'gcc'");
      Write_Eol;
      Write_Line ("  [non-Ada-objects]  list of non Ada object files");
      Write_Line ("  [linker-options]   other options for the linker");
   end Usage;

   ------------------
   -- Write_Header --
   ------------------

   procedure Write_Header is
   begin
      if Verbose_Mode then
         Write_Eol;
         Display_Version ("GNATLINK", "1995");
      end if;
   end Write_Header;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage is
   begin
      Write_Header;
      Usage;
   end Write_Usage;

--  Start of processing for Gnatlink

begin
   --  Add the directory where gnatlink is invoked in front of the path, if
   --  gnatlink is invoked with directory information.

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

   Base_Command_Name := new String'(Base_Name (Command_Name));
   Process_Args;

   if Argument_Count = 0
     or else (Verbose_Mode and then Argument_Count = 1)
   then
      Write_Usage;
      Exit_Program (E_Fatal);
   end if;

   --  Initialize packages to be used

   Csets.Initialize;
   Snames.Initialize;

   --  We always compile with -c

   Binder_Options_From_ALI.Increment_Last;
   Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
     new String'("-c");

   if Ali_File_Name = null then
      Exit_With_Error ("no ali file given for link");
   end if;

   if not Is_Regular_File (Ali_File_Name.all) then
      Exit_With_Error (Ali_File_Name.all & " not found");
   end if;

   --  Read the ALI file of the main subprogram if the binder generated file
   --  needs to be compiled and no --GCC= switch has been specified. Fetch the
   --  back end switches from this ALI file and use these switches to compile
   --  the binder generated file

   if Compile_Bind_File and then Standard_Gcc then
      Initialize_ALI;
      Name_Len := Ali_File_Name'Length;
      Name_Buffer (1 .. Name_Len) := Ali_File_Name.all;

      declare
         use Types;
         F : constant File_Name_Type := Name_Find;
         T : Text_Buffer_Ptr;
         A : ALI_Id;

      begin
         --  Load the ALI file

         T := Read_Library_Info (F, True);

         --  Read it. Note that we ignore errors, since we only want very
         --  limited information from the ali file, and likely a slightly
         --  wrong version will be just fine, though in normal operation
         --  we don't expect this to happen.

         A := Scan_ALI
               (F,
                T,
                Ignore_ED     => False,
                Err           => False,
                Ignore_Errors => True);

         if A /= No_ALI_Id then
            for
              Index in Units.Table (ALIs.Table (A).First_Unit).First_Arg ..
                       Units.Table (ALIs.Table (A).First_Unit).Last_Arg
            loop
               --  Do not compile with the front end switches. However, --RTS
               --  is to be dealt with specially because it needs to be passed
               --  to compile the file generated by the binder.

               declare
                  Arg : String_Ptr renames Args.Table (Index);
               begin
                  if not Is_Front_End_Switch (Arg.all) then
                     Binder_Options_From_ALI.Increment_Last;
                     Binder_Options_From_ALI.Table
                       (Binder_Options_From_ALI.Last) := String_Access (Arg);

                     --  GNAT doesn't support GCC's multilib mechanism when it
                     --  is configured with --disable-libada. This means that,
                     --  when a multilib switch is used to request a particular
                     --  compilation mode, the corresponding --RTS switch must
                     --  also be specified. It is convenient to eliminate the
                     --  redundancy by keying the compilation mode on a single
                     --  switch, namely --RTS, and have the compiler reinstate
                     --  the multilib switch (see gcc-interface/lang-specs.h).
                     --  This switch must be passed to the driver at link time.

                     if Arg'Length = 5
                       and then Arg (Arg'First + 1 .. Arg'First + 4) = "mrtp"
                     then
                        Linker_Options.Increment_Last;
                        Linker_Options.Table
                          (Linker_Options.Last) := String_Access (Arg);
                     end if;

                  elsif Arg'Length > 5
                    and then Arg (Arg'First + 2 .. Arg'First + 5) = "RTS="
                  then
                     Binder_Options_From_ALI.Increment_Last;
                     Binder_Options_From_ALI.Table
                       (Binder_Options_From_ALI.Last) := String_Access (Arg);

                     --  Set the RTS_*_Path_Name variables, so that
                     --  the correct directories will be set when
                     --  Osint.Add_Default_Search_Dirs will be called later.

                     Opt.RTS_Src_Path_Name :=
                       Get_RTS_Search_Dir
                         (Arg (Arg'First + 6 .. Arg'Last), Include);

                     Opt.RTS_Lib_Path_Name :=
                       Get_RTS_Search_Dir
                         (Arg (Arg'First + 6 .. Arg'Last), Objects);
                  end if;
               end;
            end loop;
         end if;
      end;
   end if;

   --  Get target parameters

   Osint.Add_Default_Search_Dirs;
   Targparm.Get_Target_Parameters;

   --  Compile the bind file with the following switches:

   --    -gnatA   stops reading gnat.adc, since we don't know what
   --             pragmas would work, and we do not need it anyway.

   --    -gnatWb  allows brackets coding for wide characters

   --    -gnatiw  allows wide characters in identifiers. This is needed
   --             because bindgen uses brackets encoding for all upper
   --             half and wide characters in identifier names.

   --  In addition, in CodePeer mode compile with -x adascil -gnatcC

   Binder_Options_From_ALI.Increment_Last;
   Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("-gnatA");
   Binder_Options_From_ALI.Increment_Last;
   Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("-gnatWb");
   Binder_Options_From_ALI.Increment_Last;
   Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("-gnatiw");

   if Opt.CodePeer_Mode then
      Binder_Options_From_ALI.Increment_Last;
      Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("-x");
      Binder_Options_From_ALI.Increment_Last;
      Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("adascil");
      Binder_Options_From_ALI.Increment_Last;
      Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("-gnatcC");
   end if;

   --  Locate all the necessary programs and verify required files are present

   Gcc_Path := System.OS_Lib.Locate_Exec_On_Path (Gcc.all);

   if Gcc_Path = null then
      Exit_With_Error ("Couldn't locate " & Gcc.all);
   end if;

   if Linker_Path = null then
      Linker_Path := Gcc_Path;
   end if;

   Write_Header;

   Target_Debuggable_Suffix := Get_Target_Debuggable_Suffix;

   --  If no output name specified, then use the base name of .ali file name

   if Output_File_Name = null then
      Output_File_Name :=
        new String'(Base_Name (Ali_File_Name.all)
                      & Target_Debuggable_Suffix.all);
   end if;

   Linker_Options.Increment_Last;
   Linker_Options.Table (Linker_Options.Last) := new String'("-o");

   Linker_Options.Increment_Last;
   Linker_Options.Table (Linker_Options.Last) :=
     new String'(Output_File_Name.all);

   Check_Existing_Executable (Output_File_Name.all);

   --  Warn if main program is called "test", as that may be a built-in command
   --  on Unix. On non-Unix systems executables have a suffix, so the warning
   --  will not appear. However, do not warn in the case of a cross compiler.

   --  Assume this is a cross tool if the executable name is not gnatlink.
   --  Note that the executable name is also gnatlink on windows, but in that
   --  case the output file name will be test.exe rather than test.

   if Base_Command_Name.all = "gnatlink"
     and then Output_File_Name.all = "test"
   then
      Error_Msg ("warning: executable name """ & Output_File_Name.all
                 & """ may conflict with shell command");
   end if;

   --  Special warnings for worrisome file names on windows

   --  Recent versions of Windows by default cause privilege escalation if an
   --  executable file name contains substrings "install", "setup", "update"
   --  or "patch". A console application will typically fail to load as a
   --  result, so we should warn the user.

   Bad_File_Names_On_Windows : declare
      FN : String := Output_File_Name.all;

      procedure Check_File_Name (S : String);
      --  Warn if file name has the substring S

      procedure Check_File_Name (S : String) is
      begin
         for J in 1 .. FN'Length - (S'Length - 1) loop
            if FN (J .. J + (S'Length - 1)) = S then
               Error_Msg
                 ("warning: executable file name """ & Output_File_Name.all
                  & """ contains substring """ & S & '"');
               Error_Msg
                 ("admin privileges may be required to run this file");
            end if;
         end loop;
      end Check_File_Name;

   --  Start of processing for Bad_File_Names_On_Windows

   begin
      for J in FN'Range loop
            FN (J) := Csets.Fold_Lower (FN (J));
      end loop;

      --  For now we detect Windows by its executable suffix of .exe

      if Target_Debuggable_Suffix.all = ".exe" then
         Check_File_Name ("install");
         Check_File_Name ("setup");
         Check_File_Name ("update");
         Check_File_Name ("patch");
      end if;
   end Bad_File_Names_On_Windows;

   --  If -M switch was specified, add the switches to create the map file

   if Create_Map_File then
      declare
         Map_Name : constant String := Base_Name (Ali_File_Name.all) & ".map";
         Switches : String_List_Access;

      begin
         Convert (Map_File, Map_Name, Switches);

         if Switches /= null then
            for J in Switches'Range loop
               Linker_Options.Increment_Last;
               Linker_Options.Table (Linker_Options.Last) := Switches (J);
            end loop;
         end if;
      end;
   end if;

   --  Perform consistency checks

   --  Transform the .ali file name into the binder output file name

   Make_Binder_File_Names : declare
      Fname     : constant String  := Base_Name (Ali_File_Name.all);
      Fname_Len : Integer := Fname'Length;

      function Get_Maximum_File_Name_Length return Integer;
      pragma Import (C, Get_Maximum_File_Name_Length,
                        "__gnat_get_maximum_file_name_length");

      Maximum_File_Name_Length : constant Integer :=
                                   Get_Maximum_File_Name_Length;

      Bind_File_Prefix : Types.String_Ptr;
      --  Contains prefix used for bind files

   begin
      --  Set prefix

      Bind_File_Prefix := new String'("b~");

      --  If the length of the binder file becomes too long due to
      --  the addition of the "b?" prefix, then truncate it.

      if Maximum_File_Name_Length > 0 then
         while Fname_Len >
                 Maximum_File_Name_Length - Bind_File_Prefix.all'Length
         loop
            Fname_Len := Fname_Len - 1;
         end loop;
      end if;

      declare
         Fnam : constant String :=
                  Bind_File_Prefix.all &
                    Fname (Fname'First .. Fname'First + Fname_Len - 1);

      begin
         Binder_Spec_Src_File := new String'(Fnam & ".ads");
         Binder_Body_Src_File := new String'(Fnam & ".adb");
         Binder_Ali_File      := new String'(Fnam & ".ali");

         Binder_Obj_File := new String'(Fnam & Get_Target_Object_Suffix.all);
      end;

      if Fname_Len /= Fname'Length then
         Binder_Options.Increment_Last;
         Binder_Options.Table (Binder_Options.Last) := new String'("-o");
         Binder_Options.Increment_Last;
         Binder_Options.Table (Binder_Options.Last) := Binder_Obj_File;
      end if;
   end Make_Binder_File_Names;

   Process_Binder_File (Binder_Body_Src_File.all & ASCII.NUL);

   --  Compile the binder file. This is fast, so we always do it, unless
   --  specifically told not to by the -n switch

   if Compile_Bind_File then
      Bind_Step : declare
         Success : Boolean;

         Args : Argument_List
                 (1 .. Binder_Options_From_ALI.Last + Binder_Options.Last + 1);

      begin
         for J in 1 .. Binder_Options_From_ALI.Last loop
            Args (J) := Binder_Options_From_ALI.Table (J);
         end loop;

         for J in 1 .. Binder_Options.Last loop
            Args (Binder_Options_From_ALI.Last + J) :=
              Binder_Options.Table (J);
         end loop;

         --  Use the full path of the binder generated source, so that it is
         --  guaranteed that the debugger will find this source, even with
         --  STABS.

         Args (Args'Last) :=
           new String'(Normalize_Pathname (Binder_Body_Src_File.all));

         if Verbose_Mode then
            Write_Str (Base_Name (Gcc_Path.all));

            for J in Args'Range loop
               Write_Str (" ");
               Write_Str (Args (J).all);
            end loop;

            Write_Eol;
         end if;

         System.OS_Lib.Spawn (Gcc_Path.all, Args, Success);

         if not Success then
            Exit_Program (E_Fatal);
         end if;
      end Bind_Step;
   end if;

   --  In CodePeer mode, there's nothing left to do after the binder file has
   --  been compiled.

   if Opt.CodePeer_Mode then
      if Tname_FD /= Invalid_FD then
         Delete (Tname);
      end if;

      return;
   end if;

   --  Now, actually link the program

   Link_Step : declare
      Num_Args : Natural :=
        (Linker_Options.Last - Linker_Options.First + 1) +
        (Gcc_Linker_Options.Last - Gcc_Linker_Options.First + 1) +
        (Linker_Objects.Last - Linker_Objects.First + 1);
      Stack_Op : Boolean := False;

   begin
      --  Remove duplicate stack size setting from the Linker_Options table.
      --  The stack setting option "-Xlinker --stack=R,C" can be found
      --  in one line when set by a pragma Linker_Options or in two lines
      --  ("-Xlinker" then "--stack=R,C") when set on the command line. We
      --  also check for the "-Wl,--stack=R" style option.

      --  We must remove the second stack setting option instance because
      --  the one on the command line will always be the first one. And any
      --  subsequent stack setting option will overwrite the previous one.
      --  This is done especially for GNAT/NT where we set the stack size
      --  for tasking programs by a pragma in the NT specific tasking
      --  package System.Task_Primitives.Operations.

      --  Note: This is not a FOR loop that runs from Linker_Options.First
      --  to Linker_Options.Last, since operations within the loop can
      --  modify the length of the table.

      Clean_Link_Option_Set : declare
         J                  : Natural;
         Shared_Libgcc_Seen : Boolean := False;

      begin
         J := Linker_Options.First;
         while J <= Linker_Options.Last loop
            if Linker_Options.Table (J).all = "-Xlinker"
              and then J < Linker_Options.Last
              and then Linker_Options.Table (J + 1)'Length > 8
              and then Linker_Options.Table (J + 1) (1 .. 8) = "--stack="
            then
               if Stack_Op then
                  Linker_Options.Table (J .. Linker_Options.Last - 2) :=
                    Linker_Options.Table (J + 2 .. Linker_Options.Last);
                  Linker_Options.Decrement_Last;
                  Linker_Options.Decrement_Last;
                  Num_Args := Num_Args - 2;

               else
                  Stack_Op := True;
               end if;
            end if;

            --  Remove duplicate -shared-libgcc switch

            if Linker_Options.Table (J).all = Shared_Libgcc_String then
               if Shared_Libgcc_Seen then
                  Linker_Options.Table (J .. Linker_Options.Last - 1) :=
                    Linker_Options.Table (J + 1 .. Linker_Options.Last);
                  Linker_Options.Decrement_Last;
                  Num_Args := Num_Args - 1;

               else
                  Shared_Libgcc_Seen := True;
               end if;
            end if;

            --  Here we just check for a canonical form that matches the
            --  pragma Linker_Options set in the NT runtime.

            if (Linker_Options.Table (J)'Length > 17
                and then Linker_Options.Table (J) (1 .. 17) =
                  "-Xlinker --stack=")
              or else
                (Linker_Options.Table (J)'Length > 12
                 and then Linker_Options.Table (J) (1 .. 12) =
                       "-Wl,--stack=")
            then
               if Stack_Op then
                  Linker_Options.Table (J .. Linker_Options.Last - 1) :=
                    Linker_Options.Table (J + 1 .. Linker_Options.Last);
                  Linker_Options.Decrement_Last;
                  Num_Args := Num_Args - 1;

               else
                  Stack_Op := True;
               end if;
            end if;

            J := J + 1;
         end loop;

         if Linker_Path = Gcc_Path then

            --  For systems where the default is to link statically with
            --  libgcc, if gcc is not called with -shared-libgcc, call it
            --  with -static-libgcc, as there are some platforms where one
            --  of these two switches is compulsory to link.

            if Shared_Libgcc_Default = 'T'
              and then not Shared_Libgcc_Seen
            then
               Linker_Options.Increment_Last;
               Linker_Options.Table (Linker_Options.Last) := Static_Libgcc;
               Num_Args := Num_Args + 1;
            end if;
         end if;
      end Clean_Link_Option_Set;

      --  Prepare arguments for call to linker

      Call_Linker : declare
         Success  : Boolean;
         Args     : Argument_List (1 .. Num_Args + 1);
         Index    : Integer := Args'First;

      begin
         Args (Index) := Binder_Obj_File;

         --  Add the object files and any -largs libraries

         for J in Linker_Objects.First .. Linker_Objects.Last loop
            Index := Index + 1;
            Args (Index) := Linker_Objects.Table (J);
         end loop;

         --  Add the linker options from the binder file

         for J in Linker_Options.First .. Linker_Options.Last loop
            Index := Index + 1;
            Args (Index) := Linker_Options.Table (J);
         end loop;

         --  Finally add the libraries from the --GCC= switch

         for J in Gcc_Linker_Options.First .. Gcc_Linker_Options.Last loop
            Index := Index + 1;
            Args (Index) := Gcc_Linker_Options.Table (J);
         end loop;

         if Verbose_Mode then
            Write_Str (Linker_Path.all);

            for J in Args'Range loop
               Write_Str (" ");
               Write_Str (Args (J).all);
            end loop;

            Write_Eol;

            --  If we are on very verbose mode (-v -v) and a response file
            --  is used we display its content.

            if Very_Verbose_Mode and then Tname_FD /= Invalid_FD then
               Write_Eol;
               Write_Str ("Response file (" &
                            Tname (Tname'First .. Tname'Last - 1) &
                            ") content : ");
               Write_Eol;

               for J in
                 Response_File_Objects.First .. Response_File_Objects.Last
               loop
                  Write_Str (Response_File_Objects.Table (J).all);
                  Write_Eol;
               end loop;

               Write_Eol;
            end if;
         end if;

         System.OS_Lib.Spawn (Linker_Path.all, Args, Success);

         --  Delete the temporary file used in conjunction with linking if one
         --  was created. See Process_Bind_File for details.

         if Tname_FD /= Invalid_FD then
            Delete (Tname);
         end if;

         if not Success then
            Error_Msg ("error when calling " & Linker_Path.all);
            Exit_Program (E_Fatal);
         end if;
      end Call_Linker;
   end Link_Step;

   --  Only keep the binder output file and it's associated object
   --  file if compiling with the -g option.  These files are only
   --  useful if debugging.

   if not Debug_Flag_Present then
      Delete (Binder_Ali_File.all & ASCII.NUL);
      Delete (Binder_Spec_Src_File.all & ASCII.NUL);
      Delete (Binder_Body_Src_File.all & ASCII.NUL);
      Delete (Binder_Obj_File.all & ASCII.NUL);
   end if;

   Exit_Program (E_Success);

exception
   when X : others =>
      Write_Line (Exception_Information (X));
      Exit_With_Error ("INTERNAL ERROR. Please report");
end Gnatlink;
