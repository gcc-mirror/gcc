------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T L I N K                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2009, Free Software Foundation, Inc.         --
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
with Hostparm;
with Indepsw;  use Indepsw;
with Namet;    use Namet;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Snames;
with Switch;   use Switch;
with System;   use System;
with Table;
with Targparm; use Targparm;
with Types;

with Ada.Command_Line;     use Ada.Command_Line;
with Ada.Exceptions;       use Ada.Exceptions;

with System.OS_Lib;        use System.OS_Lib;
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

   Begin_Info : String := "--  BEGIN Object file/option list";
   End_Info   : String := "--  END Object file/option list   ";
   --  Note: above lines are modified in C mode, see option processing

   Gcc_Path             : String_Access;
   Linker_Path          : String_Access;
   Output_File_Name     : String_Access;
   Ali_File_Name        : String_Access;
   Binder_Spec_Src_File : String_Access;
   Binder_Body_Src_File : String_Access;
   Binder_Ali_File      : String_Access;
   Binder_Obj_File      : String_Access;

   Tname    : Temp_File_Name;
   Tname_FD : File_Descriptor := Invalid_FD;
   --  Temporary file used by linker to pass list of object files on
   --  certain systems with limitations on size of arguments.

   Lname : String_Access := null;
   --  File used by linker for CLI target, used to concatenate all .il files
   --  when the command line passed to ilasm is too long

   Debug_Flag_Present : Boolean := False;
   Verbose_Mode       : Boolean := False;
   Very_Verbose_Mode  : Boolean := False;

   Ada_Bind_File : Boolean := True;
   --  Set to True if bind file is generated in Ada

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
      while Findex2 > Findex1
        and then File_Name (Findex2) /=  '.'
      loop
         Findex2 := Findex2 - 1;
      end loop;

      if Findex2 = Findex1 then
         Findex2 := File_Name'Last + 1;
      end if;

      return File_Name (Findex1 .. Findex2 - 1);
   end Base_Name;

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
      Write_Str (Base_Name (Command_Name));
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
      Next_Arg  : Integer;
      Skip_Next : Boolean := False;
      --  Set to true if the next argument is to be added into the list of
      --  linker's argument without parsing it.

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

      --  Start of processing for Process_Args

   begin
      --  First, check for --version and --help

      Check_Version_And_Help ("GNATLINK", "1995");

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
               if Arg'Length > 4 and then Arg (2 .. 5) =  "gnat" then
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
                     when 'A' =>
                        Ada_Bind_File := True;
                        Begin_Info := "--  BEGIN Object file/option list";
                        End_Info   := "--  END Object file/option list   ";

                     when 'b' =>
                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                          new String'(Arg);

                        Binder_Options.Increment_Last;
                        Binder_Options.Table (Binder_Options.Last) :=
                          Linker_Options.Table (Linker_Options.Last);

                        Next_Arg := Next_Arg + 1;

                        if Next_Arg > Argument_Count then
                           Exit_With_Error ("Missing argument for -b");
                        end if;

                        Get_Machine_Name : declare
                           Name_Arg : constant String_Access :=
                                        new String'(Argument (Next_Arg));

                        begin
                           Linker_Options.Increment_Last;
                           Linker_Options.Table (Linker_Options.Last) :=
                             Name_Arg;

                           Binder_Options.Increment_Last;
                           Binder_Options.Table (Binder_Options.Last) :=
                             Name_Arg;

                        end Get_Machine_Name;

                     when 'C' =>
                        Ada_Bind_File := False;
                        Begin_Info := "/*  BEGIN Object file/option list";
                        End_Info   := "    END Object file/option list */";

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

                        Output_File_Name := new String'(Argument (Next_Arg));

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

                  Linker_Path :=
                    System.OS_Lib.Locate_Exec_On_Path (Arg (8 .. Arg'Last));

                  if Linker_Path = null then
                     Exit_With_Error
                       ("Could not locate linker: " & Arg (8 .. Arg'Last));
                  end if;

               elsif Arg'Length > 6 and then Arg (1 .. 6) = "--GCC=" then
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
               --  e.g. accept foo.o as well as foo.obj on VMS target

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

      --  If Ada bind file, then compile it with warnings suppressed, because
      --  otherwise the with of the main program may cause junk warnings.

      if Ada_Bind_File then
         Binder_Options.Increment_Last;
         Binder_Options.Table (Binder_Options.Last) := new String'("-gnatws");
      end if;

      --  If we did not get an ali file at all, and we had at least one
      --  linker option, then assume that was the intended ali file after
      --  all, so that we get a nicer message later on.

      if Ali_File_Name = null
        and then Linker_Options.Last >= Linker_Options.First
      then
         Ali_File_Name :=
           new String'(Linker_Options.Table (Linker_Options.First).all &
                                                                   ".ali");
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
      --  Indicate that "-Xlinker" was the option preceding the current
      --  option. If True, then the current option is never suppressed.

      --  Rollback data

      --  These data items are used to store current binder file context.
      --  The context is composed of the file descriptor position and the
      --  current line together with the slice indexes (first and last
      --  position) for this line. The rollback data are used by the
      --  Store_File_Context and Rollback_File_Context routines below.
      --  The file context mechanism interact only with the Get_Next_Line
      --  call. For example:

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

      Object_Library_Ext_Ptr : Interfaces.C.Strings.chars_ptr;
      pragma Import
        (C, Object_Library_Ext_Ptr, "__gnat_object_library_extension");
      --  Pointer to string specifying the default extension for
      --  object libraries, e.g. Unix uses ".a", VMS uses ".olb".

      Object_File_Option_Ptr : Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Object_File_Option_Ptr, "__gnat_object_file_option");
      --  Pointer to a string representing the linker option which specifies
      --  the response file.

      Using_GNU_Linker : Boolean;
      for Using_GNU_Linker'Size use Character'Size;
      pragma Import (C, Using_GNU_Linker, "__gnat_using_gnu_linker");
      --  Predicate indicating whether this target uses the GNU linker. In
      --  this case we must output a GNU linker compatible response file.

      Separate_Run_Path_Options : Boolean;
      for Separate_Run_Path_Options'Size use Character'Size;
      pragma Import
        (C, Separate_Run_Path_Options, "__gnat_separate_run_path_options");
      --  Whether separate rpath options should be emitted for each directory

      Opening : aliased constant String := """";
      Closing : aliased constant String := '"' & ASCII.LF;
      --  Needed to quote object paths in object list files when GNU linker
      --  is used.

      procedure Get_Next_Line;
      --  Read the next line from the binder file without the line
      --  terminator.

      function Index (S, Pattern : String) return Natural;
      --  Return the last occurrence of Pattern in S, or 0 if none

      function Is_Option_Present (Opt : String) return Boolean;
      --  Return true if the option Opt is already present in
      --  Linker_Options table.

      procedure Store_File_Context;
      --  Store current file context, Fd position and current line data.
      --  The file context is stored into the rollback data above (RB_*).
      --  Store_File_Context can be called at any time, only the last call
      --  will be used (i.e. this routine overwrites the file context).

      procedure Rollback_File_Context;
      --  Restore file context from rollback data. This routine must be called
      --  after Store_File_Context. The binder file context will be restored
      --  with the data stored by the last Store_File_Context call.

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

      -----------------------
      -- Is_Option_Present --
      -----------------------

      function Is_Option_Present (Opt : String) return Boolean is
      begin
         for I in 1 .. Linker_Options.Last loop

            if Linker_Options.Table (I).all = Opt then
               return True;
            end if;

         end loop;

         return False;
      end Is_Option_Present;

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

         if Ada_Bind_File then
            Next_Line (Nfirst .. Nlast - 8) :=
              Next_Line (Nfirst + 8 .. Nlast);
            Nlast := Nlast - 8;
         end if;

         --  Go to next section when switches are reached

         exit when Next_Line (1) = '-';

         --  Otherwise we have another object file to collect

         Linker_Objects.Increment_Last;

         --  Mark the positions of first and last object files in case
         --  they need to be placed with a named file on systems having
         --  linker line limitations.

         if Objs_Begin = 0 then
            Objs_Begin := Linker_Objects.Last;
         end if;

         Linker_Objects.Table (Linker_Objects.Last) :=
           new String'(Next_Line (Nfirst .. Nlast));

         Link_Bytes := Link_Bytes + Nlast - Nfirst + 2;
         --  Nlast - Nfirst + 1, for the size, plus one for the space between
         --  each arguments.
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

      if VM_Target = CLI_Target
        and then Link_Bytes > Link_Max
      then
         Lname := new String'("l~" & Base_Name (Ali_File_Name.all) & ".il");

         for J in Objs_Begin .. Objs_End loop
            Copy_File (Linker_Objects.Table (J).all, Lname.all,
                       Success => Closing_Status,
                       Mode    => Append);
         end loop;

         --  Add the special objects list file option together with the name
         --  of the temporary file to the objects file table.

         Linker_Objects.Table (Objs_Begin) :=
           new String'(Value (Object_File_Option_Ptr) & Lname.all);

         --  The slots containing these object file names are then removed
         --  from the objects table so they do not appear in the link. They
         --  are removed by moving up the linker options and non-Ada object
         --  files appearing after the Ada object list in the table.

         declare
            N : Integer;

         begin
            N := Objs_End - Objs_Begin + 1;

            for J in Objs_End + 1 .. Linker_Objects.Last loop
               Linker_Objects.Table (J - N + 1) := Linker_Objects.Table (J);
            end loop;

            Linker_Objects.Set_Last (Linker_Objects.Last - N + 1);
         end;

      elsif Object_List_File_Required
        or else (Object_List_File_Supported
                   and then Link_Bytes > Link_Max)
      then
         --  Create a temporary file containing the Ada user object files
         --  needed by the link. This list is taken from the bind file
         --  and is output one object per line for maximal compatibility with
         --  linkers supporting this option.

         Create_Temp_File (Tname_FD, Tname);

         --  ??? File descriptor should be checked to not be Invalid_FD.
         --  ??? Status of Write and Close operations should be checked, and
         --  failure should occur if a status is wrong.

         --  If target is using the GNU linker we must add a special header
         --  and footer in the response file.

         --  The syntax is : INPUT (object1.o object2.o ... )

         --  Because the GNU linker does not like name with characters such
         --  as '!', we must put the object paths between double quotes.

         if Using_GNU_Linker then
            declare
               GNU_Header : aliased constant String := "INPUT (";

            begin
               Status := Write (Tname_FD, GNU_Header'Address,
                 GNU_Header'Length);
            end;
         end if;

         for J in Objs_Begin .. Objs_End loop

            --  Opening quote for GNU linker

            if Using_GNU_Linker then
               Status := Write (Tname_FD, Opening'Address, 1);
            end if;

            Status := Write (Tname_FD, Linker_Objects.Table (J).all'Address,
                             Linker_Objects.Table (J).all'Length);

            --  Closing quote for GNU linker

            if Using_GNU_Linker then
               Status := Write (Tname_FD, Closing'Address, 2);

            else
               Status := Write (Tname_FD, ASCII.LF'Address, 1);
            end if;

            Response_File_Objects.Increment_Last;
            Response_File_Objects.Table (Response_File_Objects.Last) :=
              Linker_Objects.Table (J);
         end loop;

         --  Handle GNU linker response file footer

         if Using_GNU_Linker then
            declare
               GNU_Footer : aliased constant String := ")";

            begin
               Status := Write (Tname_FD, GNU_Footer'Address,
                 GNU_Footer'Length);
            end;
         end if;

         Close (Tname_FD, Closing_Status);

         --  Add the special objects list file option together with the name
         --  of the temporary file (removing the null character) to the objects
         --  file table.

         Linker_Objects.Table (Objs_Begin) :=
           new String'(Value (Object_File_Option_Ptr) &
                       Tname (Tname'First .. Tname'Last - 1));

         --  The slots containing these object file names are then removed
         --  from the objects table so they do not appear in the link. They
         --  are removed by moving up the linker options and non-Ada object
         --  files appearing after the Ada object list in the table.

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

            --  Add binder options only if not already set on the command
            --  line. This rule is a way to control the linker options order.

            --  The following test needs comments, why is it VMS specific.
            --  The above comment looks out of date ???

            elsif not (OpenVMS_On_Target
                         and then
                       Is_Option_Present (Next_Line (Nfirst .. Nlast)))
            then
               if Nlast > Nfirst + 2 and then
                 Next_Line (Nfirst .. Nfirst + 1) = "-L"
               then
                  --  Construct a library search path for use later
                  --  to locate static gnatlib libraries.

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

               elsif Next_Line (Nfirst .. Nlast) = "-ldecgnat"
                 or else Next_Line (Nfirst .. Nlast) = "-lgnarl"
                 or else Next_Line (Nfirst .. Nlast) = "-lgnat"
                 or else Next_Line
                     (1 .. Natural'Min (Nlast, 8 + Library_Version'Length)) =
                       Shared_Lib ("gnarl")
                 or else Next_Line
                     (1 .. Natural'Min (Nlast, 7 + Library_Version'Length)) =
                       Shared_Lib ("gnat")
               then
                  --  If it is a shared library, remove the library version.
                  --  We will be looking for the static version of the library
                  --  as it is in the same directory as the shared version.

                  if Next_Line (Nlast - Library_Version'Length + 1 .. Nlast)
                       = Library_Version
                  then
                     --  Set Last to point to last character before the
                     --  library version.

                     Last := Nlast - Library_Version'Length - 1;
                  else
                     Last := Nlast;
                  end if;

                  --  Given a Gnat standard library, search the
                  --  library path to find the library location

                  declare
                     File_Path : String_Access;

                     Object_Lib_Extension : constant String :=
                                              Value (Object_Library_Ext_Ptr);

                     File_Name : constant String := "lib" &
                                   Next_Line (Nfirst + 2 .. Last) &
                                   Object_Lib_Extension;

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

                           --  If static gnatlib found, explicitly
                           --  specify to overcome possible linker
                           --  default usage of shared version.

                           Linker_Options.Increment_Last;

                           Linker_Options.Table (Linker_Options.Last) :=
                             new String'(File_Path.all);

                        elsif GNAT_Shared then
                           if Opt.Run_Path_Option then
                              --  If shared gnatlib desired, add the
                              --  appropriate system specific switch
                              --  so that it can be located at runtime.

                              if Run_Path_Opt'Length /= 0 then
                                 --  Output the system specific linker command
                                 --  that allows the image activator to find
                                 --  the shared library at runtime.
                                 --  Also add path to find libgcc_s.so, if
                                 --  relevant.

                                 --  To find the location of the shared version
                                 --  of libgcc, we look for "gcc-lib" in the
                                 --  path of the library. However, this
                                 --  subdirectory is no longer present in
                                 --  in recent version of GCC. So, we look for
                                 --  the last subdirectory "lib" in the path.

                                 GCC_Index :=
                                   Index (File_Path.all, "gcc-lib");

                                 if GCC_Index /= 0 then
                                    --  The shared version of libgcc is
                                    --  located in the parent directory.

                                    GCC_Index := GCC_Index - 1;

                                 else
                                    GCC_Index :=
                                      Index (File_Path.all, "/lib/");

                                    if GCC_Index = 0 then
                                       GCC_Index :=
                                         Index (File_Path.all,
                                                Directory_Separator &
                                                "lib" &
                                                Directory_Separator);
                                    end if;

                                    --  We have found a subdirectory "lib",
                                    --  this is where the shared version of
                                    --  libgcc should be located.

                                    if GCC_Index /= 0 then
                                       GCC_Index := GCC_Index + 3;
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
                                            & File_Path (1 .. GCC_Index));
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
                                          --  We have found a already specified
                                          --  run_path_option: we will add to
                                          --  this switch, because only one
                                          --  run_path_option should be
                                          --  specified.

                                          Run_Path_Opt_Index := J;
                                          exit;
                                       end if;
                                    end loop;

                                    --  If there is no run_path_option, we need
                                    --  to add one.

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
                                            new String'(Run_Path_Opt
                                              & File_Path
                                                (1 .. File_Path'Length
                                                 - File_Name'Length)
                                              & Path_Separator
                                              & File_Path (1 .. GCC_Index));

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
                                               & File_Path (1 .. GCC_Index));
                                       end if;
                                    end if;
                                 end if;
                              end if;
                           end if;

                           --  Then we add the appropriate -l switch

                           Linker_Options.Increment_Last;
                           Linker_Options.Table (Linker_Options.Last) :=
                             new String'(Next_Line (Nfirst .. Nlast));
                        end if;

                     else
                        --  If gnatlib library not found, then
                        --  add it anyway in case some other
                        --  mechanism may find it.

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

            if Ada_Bind_File then
               Next_Line (Nfirst .. Nlast - 8) :=
                 Next_Line (Nfirst + 8 .. Nlast);
               Nlast := Nlast - 8;
            end if;
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
      Write_Str (Base_Name (Command_Name));
      Write_Str (" switches mainprog.ali [non-Ada-objects] [linker-options]");
      Write_Eol;
      Write_Eol;
      Write_Line ("  mainprog.ali   the ALI file of the main program");
      Write_Eol;
      Write_Line ("  -A    Binder generated source file is in Ada (default)");
      Write_Line ("  -C    Binder generated source file is in C");
      Write_Line ("  -f    force object file list to be generated");
      Write_Line ("  -g    Compile binder source file with debug information");
      Write_Line ("  -n    Do not compile the binder source file");
      Write_Line ("  -R    Do not use a run_path_option");
      Write_Line ("  -v    verbose mode");
      Write_Line ("  -v -v very verbose mode");
      Write_Eol;
      Write_Line ("  -o nam     Use 'nam' as the name of the executable");
      Write_Line ("  -b target  Compile the binder source to run on target");
      Write_Line ("  -Bdir      Load compiler executables from dir");

      if Is_Supported (Map_File) then
         Write_Line ("  -Mmap      Create map file map");
         Write_Line ("  -M         Create map file mainprog.map");
      end if;

      Write_Line ("  --GCC=comp Use comp as the compiler");
      Write_Line ("  --LINK=nam Use 'nam' for the linking rather than 'gcc'");
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
   --  Add the directory where gnatlink is invoked in front of the
   --  path, if gnatlink is invoked with directory information.
   --  Only do this if the platform is not VMS, where the notion of path
   --  does not really exist.

   if not Hostparm.OpenVMS then
      declare
         Command : constant String := Command_Name;

      begin
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
      end;
   end if;

   Process_Args;

   if Argument_Count = 0
     or else
     (Verbose_Mode and then Argument_Count = 1)
   then
      Write_Usage;
      Exit_Program (E_Fatal);
   end if;

   --  Initialize packages to be used

   Namet.Initialize;
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

   --  Read the ALI file of the main subprogram if the binder generated
   --  file needs to be compiled and no --GCC= switch has been specified.
   --  Fetch the back end switches from this ALI file and use these switches
   --  to compile the binder generated file

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
         --  we don't expect this to happen!

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
               --  if the binder-generated file is in Ada and may also be used
               --  to drive the linker.

               declare
                  Arg : String_Ptr renames Args.Table (Index);
               begin
                  if not Is_Front_End_Switch (Arg.all) then
                     Binder_Options_From_ALI.Increment_Last;
                     Binder_Options_From_ALI.Table
                       (Binder_Options_From_ALI.Last) := String_Access (Arg);

                  elsif Arg'Length > 5
                    and then Arg (Arg'First + 2 .. Arg'First + 5) = "RTS="
                  then
                     if Ada_Bind_File then
                        Binder_Options_From_ALI.Increment_Last;
                        Binder_Options_From_ALI.Table
                          (Binder_Options_From_ALI.Last)
                            := String_Access (Arg);
                     end if;

                     --  Set the RTS_*_Path_Name variables, so that the
                     --  correct directories will be set when
                     --  Osint.Add_Default_Search_Dirs will be called later.

                     Opt.RTS_Src_Path_Name :=
                       Get_RTS_Search_Dir
                         (Arg (Arg'First + 6 .. Arg'Last), Include);

                     Opt.RTS_Lib_Path_Name :=
                       Get_RTS_Search_Dir
                         (Arg (Arg'First + 6 .. Arg'Last), Objects);

                     --  GNAT doesn't support the GCC multilib mechanism.
                     --  This means that, when a multilib switch is used
                     --  to request a particular compilation mode, the
                     --  corresponding runtime switch (--RTS) must also be
                     --  specified. The long-term goal is to fully support the
                     --  multilib mechanism; however, in the meantime, it is
                     --  convenient to eliminate the redundancy by keying the
                     --  compilation mode on a single switch, namely --RTS.

                     --  Pass -mrtp to the linker if --RTS=rtp was passed

                     if Arg'Length > 8
                       and then Arg (Arg'First + 6 .. Arg'First + 8) = "rtp"
                     then
                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                          new String'("-mrtp");

                     --  Pass -fsjlj to the linker if --RTS=sjlj was passed

                     elsif Arg'Length > 9
                       and then Arg (Arg'First + 6 .. Arg'First + 9) = "sjlj"
                     then
                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                          new String'("-fsjlj");
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end;
   end if;

   --  Get target parameters

   Osint.Add_Default_Search_Dirs;
   Targparm.Get_Target_Parameters;

   if VM_Target /= No_VM then
      case VM_Target is
         when JVM_Target => Gcc := new String'("jvm-gnatcompile");
         when CLI_Target => Gcc := new String'("dotnet-gnatcompile");
         when No_VM      => raise Program_Error;
      end case;

      Ada_Bind_File := True;
      Begin_Info := "--  BEGIN Object file/option list";
      End_Info   := "--  END Object file/option list   ";
   end if;

   --  If the main program is in Ada it is compiled with the following
   --  switches:

   --    -gnatA   stops reading gnat.adc, since we don't know what
   --             pragmas would work, and we do not need it anyway.

   --    -gnatWb  allows brackets coding for wide characters

   --    -gnatiw  allows wide characters in identifiers. This is needed
   --             because bindgen uses brackets encoding for all upper
   --             half and wide characters in identifier names.

   if Ada_Bind_File then
      Binder_Options_From_ALI.Increment_Last;
      Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("-gnatA");
      Binder_Options_From_ALI.Increment_Last;
      Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("-gnatWb");
      Binder_Options_From_ALI.Increment_Last;
      Binder_Options_From_ALI.Table (Binder_Options_From_ALI.Last) :=
        new String'("-gnatiw");
   end if;

   --  Locate all the necessary programs and verify required files are present

   Gcc_Path := System.OS_Lib.Locate_Exec_On_Path (Gcc.all);

   if Gcc_Path = null then
      Exit_With_Error ("Couldn't locate " & Gcc.all);
   end if;

   if Linker_Path = null then
      if VM_Target = CLI_Target then
         Linker_Path := System.OS_Lib.Locate_Exec_On_Path ("ilasm");

         if Linker_Path = null then
            Exit_With_Error ("Couldn't locate ilasm");
         end if;

      elsif RTX_RTSS_Kernel_Module_On_Target then

         --  Use Microsoft linker for RTSS modules

         Linker_Path := System.OS_Lib.Locate_Exec_On_Path ("link");

         if Linker_Path = null then
            Exit_With_Error ("Couldn't locate link");
         end if;

      else
         Linker_Path := Gcc_Path;
      end if;
   end if;

   Write_Header;

   --  If no output name specified, then use the base name of .ali file name

   if Output_File_Name = null then
      Output_File_Name :=
        new String'(Base_Name (Ali_File_Name.all)
                      & Get_Target_Debuggable_Suffix.all);
   end if;

   if VM_Target = CLI_Target then
      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) := new String'("/QUIET");

      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) := new String'("/DEBUG");

      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) :=
        new String'("/OUTPUT=" & Output_File_Name.all);

   elsif RTX_RTSS_Kernel_Module_On_Target then
      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) :=
        new String'("/OUT:" & Output_File_Name.all);

   else
      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) := new String'("-o");

      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) :=
        new String'(Output_File_Name.all);
   end if;

   --  Delete existing executable, in case it is a symbolic link, to avoid
   --  modifying the target of the symbolic link.

   declare
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Delete_File (Output_File_Name.all, Dummy);
   end;

   --  Warn if main program is called "test", as that may be a built-in command
   --  on Unix. On non-Unix systems executables have a suffix, so the warning
   --  will not appear. However, do not warn in the case of a cross compiler.

   --  Assume this is a cross tool if the executable name is not gnatlink

   if Base_Name (Command_Name) = "gnatlink"
     and then Output_File_Name.all = "test"
   then
      Error_Msg ("warning: executable name """ & Output_File_Name.all
                   & """ may conflict with shell command");
   end if;

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

      if not Ada_Bind_File then
         Bind_File_Prefix := new String'("b_");
      elsif OpenVMS_On_Target then
         Bind_File_Prefix := new String'("b__");
      else
         Bind_File_Prefix := new String'("b~");
      end if;

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
         if Ada_Bind_File then
            Binder_Spec_Src_File := new String'(Fnam & ".ads");
            Binder_Body_Src_File := new String'(Fnam & ".adb");
            Binder_Ali_File      := new String'(Fnam & ".ali");
         else
            Binder_Body_Src_File := new String'(Fnam & ".c");
         end if;

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
         Args    : Argument_List
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

   --  Now, actually link the program

   --  Skip this step for now on JVM since the Java interpreter will do
   --  the actual link at run time. We might consider packing all class files
   --  in a .zip file during this step.

   if VM_Target /= JVM_Target then
      Link_Step : declare
         Num_Args : Natural :=
                     (Linker_Options.Last - Linker_Options.First + 1) +
                     (Gcc_Linker_Options.Last - Gcc_Linker_Options.First + 1) +
                     (Linker_Objects.Last - Linker_Objects.First + 1);
         Stack_Op : Boolean := False;
         IDENT_Op : Boolean := False;

      begin
         if VM_Target = CLI_Target then

            --  Remove extraneous flags not relevant for CIL. Also remove empty
            --  arguments, since ilasm chokes on them.

            for J in reverse Linker_Options.First .. Linker_Options.Last loop
               if Linker_Options.Table (J)'Length = 0
                 or else Linker_Options.Table (J) (1 .. 2) = "-L"
                 or else Linker_Options.Table (J) (1 .. 2) = "-l"
                 or else Linker_Options.Table (J) (1 .. 3) = "-Wl"
                 or else Linker_Options.Table (J) (1 .. 3) = "-sh"
                 or else Linker_Options.Table (J) (1 .. 2) = "-g"
               then
                  Linker_Options.Table (J .. Linker_Options.Last - 1) :=
                    Linker_Options.Table (J + 1 .. Linker_Options.Last);
                  Linker_Options.Decrement_Last;
                  Num_Args := Num_Args - 1;
               end if;
            end loop;

         elsif RTX_RTSS_Kernel_Module_On_Target then

            --  Remove flags not relevant for Microsoft linker and adapt some
            --  others.

            for J in reverse Linker_Options.First .. Linker_Options.Last loop

               --  Remove flags that are not accepted
               if Linker_Options.Table (J)'Length = 0
                 or else Linker_Options.Table (J) (1 .. 2) = "-l"
                 or else Linker_Options.Table (J) (1 .. 3) = "-Wl"
                 or else Linker_Options.Table (J) (1 .. 3) = "-sh"
                 or else Linker_Options.Table (J) (1 .. 8) = "-Xlinker"
                 or else Linker_Options.Table (J) (1 .. 9) = "-mthreads"
               then
                  Linker_Options.Table (J .. Linker_Options.Last - 1) :=
                    Linker_Options.Table (J + 1 .. Linker_Options.Last);
                  Linker_Options.Decrement_Last;
                  Num_Args := Num_Args - 1;

               --  Replace "-L" by its counterpart "/LIBPATH:" and UNIX "/" by
               --  Windows "\".
               elsif Linker_Options.Table (J) (1 .. 2) = "-L" then
                  declare
                     Libpath_Option : constant String_Access := new String'
                       ("/LIBPATH:" &
                        Linker_Options.Table (J)
                          (3 .. Linker_Options.Table (J).all'Last));
                  begin
                     for Index in 10 .. Libpath_Option'Last loop
                        if Libpath_Option (Index) = '/' then
                           Libpath_Option (Index) := '\';
                        end if;
                     end loop;

                     Linker_Options.Table (J) := Libpath_Option;
                  end;

               --  Replace "-g" by "/DEBUG"
               elsif Linker_Options.Table (J) (1 .. 2) = "-g" then
                  Linker_Options.Table (J) := new String'("/DEBUG");

               --  Replace "-o" by "/OUT:"
               elsif Linker_Options.Table (J) (1 .. 2) = "-o" then
                  Linker_Options.Table (J + 1) := new String'
                    ("/OUT:" & Linker_Options.Table (J + 1).all);

                  Linker_Options.Table (J .. Linker_Options.Last - 1) :=
                    Linker_Options.Table (J + 1 .. Linker_Options.Last);
                  Linker_Options.Decrement_Last;
                  Num_Args := Num_Args - 1;

               --  Replace "--stack=" by "/STACK:"
               elsif Linker_Options.Table (J) (1 .. 8) = "--stack=" then
                  Linker_Options.Table (J) := new String'
                    ("/STACK:" &
                     Linker_Options.Table (J)
                       (9 .. Linker_Options.Table (J).all'Last));

               --  Replace "-v" by its counterpart "/VERBOSE"
               elsif Linker_Options.Table (J) (1 .. 2) = "-v" then
                  Linker_Options.Table (J) := new String'("/VERBOSE");
               end if;
            end loop;

            --  Add some required flags to create RTSS modules

            declare
               Flags_For_Linker : constant array (1 .. 17) of String_Access :=
                 (new String'("/NODEFAULTLIB"),
                  new String'("/INCREMENTAL:NO"),
                  new String'("/NOLOGO"),
                  new String'("/DRIVER"),
                  new String'("/ALIGN:0x20"),
                  new String'("/SUBSYSTEM:NATIVE"),
                  new String'("/ENTRY:_RtapiProcessEntryCRT@8"),
                  new String'("/RELEASE"),
                  new String'("startupCRT.obj"),
                  new String'("rtxlibcmt.lib"),
                  new String'("oldnames.lib"),
                  new String'("rtapi_rtss.lib"),
                  new String'("Rtx_Rtss.lib"),
                  new String'("libkernel32.a"),
                  new String'("libws2_32.a"),
                  new String'("libmswsock.a"),
                  new String'("libadvapi32.a"));
               --  These flags need to be passed to Microsoft linker. They
               --  come from the RTX documentation.

               Gcc_Lib_Path : constant String_Access := new String'
                 ("/LIBPATH:" & Include_Dir_Default_Prefix & "\..\");
               --  Place to look for gcc related libraries, such as libgcc

            begin
               --  Replace UNIX "/" by Windows "\" in the path

               for Index in 10 .. Gcc_Lib_Path.all'Last loop
                  if Gcc_Lib_Path (Index) = '/' then
                     Gcc_Lib_Path (Index) := '\';
                  end if;
               end loop;

               Linker_Options.Increment_Last;
               Linker_Options.Table (Linker_Options.Last) := Gcc_Lib_Path;
               Num_Args := Num_Args + 1;

               for Index in Flags_For_Linker'Range loop
                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                    Flags_For_Linker (Index);
                  Num_Args := Num_Args + 1;
               end loop;
            end;
         end if;

         --  Remove duplicate stack size setting from the Linker_Options
         --  table. The stack setting option "-Xlinker --stack=R,C" can be
         --  found in one line when set by a pragma Linker_Options or in two
         --  lines ("-Xlinker" then "--stack=R,C") when set on the command
         --  line. We also check for the "-Wl,--stack=R" style option.

         --  We must remove the second stack setting option instance
         --  because the one on the command line will always be the first
         --  one. And any subsequent stack setting option will overwrite the
         --  previous one. This is done especially for GNAT/NT where we set
         --  the stack size for tasking programs by a pragma in the NT
         --  specific tasking package System.Task_Primitives.Operations.

         --  Note: This is not a FOR loop that runs from Linker_Options.First
         --  to Linker_Options.Last, since operations within the loop can
         --  modify the length of the table.

         Clean_Link_Option_Set : declare
            J : Natural := Linker_Options.First;
            Shared_Libgcc_Seen : Boolean := False;

         begin
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
                   and then Linker_Options.Table (J) (1 .. 17)
                           = "-Xlinker --stack=")
                 or else
                  (Linker_Options.Table (J)'Length > 12
                   and then Linker_Options.Table (J) (1 .. 12)
                            = "-Wl,--stack=")
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

               --  Remove duplicate IDENTIFICATION directives (VMS)

               if Linker_Options.Table (J)'Length > 27
                 and then Linker_Options.Table (J) (1 .. 28)
                          = "--for-linker=IDENTIFICATION="
               then
                  if IDENT_Op then
                     Linker_Options.Table (J .. Linker_Options.Last - 1) :=
                       Linker_Options.Table (J + 1 .. Linker_Options.Last);
                     Linker_Options.Decrement_Last;
                     Num_Args := Num_Args - 1;
                  else
                     IDENT_Op := True;
                  end if;
               end if;

               J := J + 1;
            end loop;

            if Linker_Path = Gcc_Path and then VM_Target = No_VM then

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

            elsif RTX_RTSS_Kernel_Module_On_Target then

               --  Force the use of the static libgcc for RTSS modules

               Linker_Options.Increment_Last;
               Linker_Options.Table (Linker_Options.Last) :=
                 new String'("libgcc.a");
               Num_Args := Num_Args + 1;
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
                    Response_File_Objects.First ..
                    Response_File_Objects.Last
                  loop
                     Write_Str (Response_File_Objects.Table (J).all);
                     Write_Eol;
                  end loop;

                  Write_Eol;
               end if;
            end if;

            System.OS_Lib.Spawn (Linker_Path.all, Args, Success);

            --  Delete the temporary file used in conjunction with linking if
            --  one was created. See Process_Bind_File for details.

            if Tname_FD /= Invalid_FD then
               Delete (Tname);
            end if;

            if Lname /= null then
               Delete (Lname.all & ASCII.NUL);
            end if;

            if not Success then
               Error_Msg ("error when calling " & Linker_Path.all);
               Exit_Program (E_Fatal);
            end if;
         end Call_Linker;
      end Link_Step;
   end if;

   --  Only keep the binder output file and it's associated object
   --  file if compiling with the -g option.  These files are only
   --  useful if debugging.

   if not Debug_Flag_Present then
      if Binder_Ali_File /= null then
         Delete (Binder_Ali_File.all & ASCII.NUL);
      end if;

      if Binder_Spec_Src_File /= null then
         Delete (Binder_Spec_Src_File.all & ASCII.NUL);
      end if;

      Delete (Binder_Body_Src_File.all & ASCII.NUL);

      if VM_Target = No_VM then
         Delete (Binder_Obj_File.all & ASCII.NUL);
      end if;
   end if;

   Exit_Program (E_Success);

exception
   when X : others =>
      Write_Line (Exception_Information (X));
      Exit_With_Error ("INTERNAL ERROR. Please report");
end Gnatlink;
