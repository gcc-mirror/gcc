------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T L I N K                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1996-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Gnatlink usage: please consult the gnat documentation

with Gnatvsn;  use Gnatvsn;
with Hostparm;
with Osint;    use Osint;
with Output;   use Output;
with System;   use System;
with Table;

with Ada.Command_Line;     use Ada.Command_Line;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Interfaces.C_Streams; use Interfaces.C_Streams;

procedure Gnatlink is

   pragma Ident (Gnat_Version_String);

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
     Table_Increment      => 2,
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
   --  applications objects as they are specified with a fullname.

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

   package Binder_Options is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1, -- equals low bound of Argument_List for Spawn
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Binder_Options");
   --  This table collects the arguments to be passed to compile the binder
   --  generated file.

   subtype chars_ptr is System.Address;

   Gcc : String_Access := Program_Name ("gcc");

   Read_Mode  : constant String := "r" & ASCII.Nul;

   Begin_Info : String := "-- BEGIN Object file/option list";
   End_Info   : String := "-- END Object file/option list   ";
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

   Debug_Flag_Present : Boolean := False;
   Verbose_Mode       : Boolean := False;
   Very_Verbose_Mode  : Boolean := False;

   Ada_Bind_File : Boolean := True;
   --  Set to True if bind file is generated in Ada

   Compile_Bind_File : Boolean := True;
   --  Set to False if bind file is not to be compiled

   Object_List_File_Supported : Boolean;
   pragma Import (C, Object_List_File_Supported, "objlist_file_supported");
   --  Predicate indicating whether the linker has an option whereby the
   --  names of object files can be passed to the linker in a file.

   Object_List_File_Required : Boolean := False;
   --  Set to True to force generation of a response file

   function Base_Name (File_Name : in String) return String;
   --  Return just the file name part without the extension (if present).

   procedure Delete (Name : in String);
   --  Wrapper to unlink as status is ignored by this application.

   procedure Error_Msg (Message : in String);
   --  Output the error or warning Message

   procedure Exit_With_Error (Error : in String);
   --  Output Error and exit program with a fatal condition.

   procedure Process_Args;
   --  Go through all the arguments and build option tables.

   procedure Process_Binder_File (Name : in String);
   --  Reads the binder file and extracts linker arguments.

   function Value (chars : chars_ptr) return String;
   --  Return NUL-terminated string chars as an Ada string.

   procedure Write_Usage;
   --  Show user the program options.

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (File_Name : in String) return String is
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

   procedure Delete (Name : in String) is
      Status : int;

   begin
      Status := unlink (Name'Address);
   end Delete;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Message : in String) is
   begin
      Write_Str (Base_Name (Command_Name));
      Write_Str (": ");
      Write_Str (Message);
      Write_Eol;
   end Error_Msg;

   ---------------------
   -- Exit_With_Error --
   ---------------------

   procedure Exit_With_Error (Error : in String) is
   begin
      Error_Msg (Error);
      Exit_Program (E_Fatal);
   end Exit_With_Error;

   ------------------
   -- Process_Args --
   ------------------

   procedure Process_Args is
      Next_Arg : Integer;

   begin
      Binder_Options.Increment_Last;
      Binder_Options.Table (Binder_Options.Last) := new String'("-c");

      --  If the main program is in Ada it is compiled with the following
      --  switches:

      --    -gnatA   stops reading gnat.adc, since we don't know what
      --             pagmas would work, and we do not need it anyway.

      --    -gnatWb  allows brackets coding for wide characters

      --    -gnatiw  allows wide characters in identifiers. This is needed
      --             because bindgen uses brackets encoding for all upper
      --             half and wide characters in identifier names.

      if Ada_Bind_File then
         Binder_Options.Increment_Last;
         Binder_Options.Table (Binder_Options.Last) := new String'("-gnatA");
         Binder_Options.Increment_Last;
         Binder_Options.Table (Binder_Options.Last) := new String'("-gnatWb");
         Binder_Options.Increment_Last;
         Binder_Options.Table (Binder_Options.Last) := new String'("-gnatiw");
      end if;

      --  Loop through arguments of gnatlink command

      Next_Arg := 1;
      loop
         exit when Next_Arg > Argument_Count;

         Process_One_Arg : declare
            Arg : String := Argument (Next_Arg);

         begin
            --  Case of argument which is a switch

            --  We definitely need section by section comments here ???

            if Arg'Length /= 0
              and then (Arg (1) = Switch_Character or else Arg (1) = '-')
            then
               if Arg'Length > 4
                 and then Arg (2 .. 5) =  "gnat"
               then
                  Exit_With_Error
                    ("invalid switch: """ & Arg & """ (gnat not needed here)");
               end if;

               if Arg (2) = 'g'
                 and then (Arg'Length < 5 or else Arg (2 .. 5) /= "gnat")
               then
                  Debug_Flag_Present := True;

                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                   new String'(Arg);

                  Binder_Options.Increment_Last;
                  Binder_Options.Table (Binder_Options.Last) :=
                    Linker_Options.Table (Linker_Options.Last);

               elsif Arg'Length = 2 then
                  case Arg (2) is
                     when 'A' =>
                        Ada_Bind_File := True;
                        Begin_Info := "-- BEGIN Object file/option list";
                        End_Info   := "-- END Object file/option list   ";

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
                           Name_Arg : String_Access :=
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
                        Begin_Info := "/* BEGIN Object file/option list";
                        End_Info   := "   END Object file/option list */";

                     when 'f' =>
                        if Object_List_File_Supported then
                           Object_List_File_Required := True;
                        else
                           Exit_With_Error
                             ("Object list file not supported on this target");
                        end if;

                     when 'n' =>
                        Compile_Bind_File := False;

                     when 'o' =>
                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                         new String'(Arg);

                        Next_Arg := Next_Arg + 1;

                        if Next_Arg > Argument_Count then
                           Exit_With_Error ("Missing argument for -o");
                        end if;

                        Output_File_Name := new String'(Argument (Next_Arg));

                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                          Output_File_Name;

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
                    GNAT.OS_Lib.Locate_Exec_On_Path (Arg (8 .. Arg'Last));

                  if Linker_Path = null then
                     Exit_With_Error
                       ("Could not locate linker: " & Arg (8 .. Arg'Last));
                  end if;

               elsif Arg'Length > 6 and then Arg (1 .. 6) = "--GCC=" then
                  declare
                     Program_Args : Argument_List_Access :=
                                      Argument_String_To_List
                                                 (Arg (7 .. Arg'Last));

                  begin
                     Gcc := new String'(Program_Args.all (1).all);

                     --  Set appropriate flags for switches passed

                     for J in 2 .. Program_Args.all'Last loop
                        declare
                           Arg : String := Program_Args.all (J).all;
                           AF  : Integer := Arg'First;

                        begin
                           if Arg'Length /= 0
                             and then (Arg (AF) = Switch_Character
                                        or else Arg (AF) = '-')
                           then
                              if Arg (AF + 1) = 'g'
                                and then (Arg'Length = 2
                                  or else Arg (AF + 2) in '0' .. '3'
                                  or else Arg (AF + 2 .. Arg'Last) = "coff")
                              then
                                 Debug_Flag_Present := True;
                              end if;
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

                           --  Pass to gcc for linking program.

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
               if Arg'Length > 4
                 and then Arg (Arg'Last - 3 .. Arg'Last) = ".ali"
               then
                  if Ali_File_Name = null then
                     Ali_File_Name := new String'(Arg);
                  else
                     Exit_With_Error ("cannot handle more than one ALI file");
                  end if;

               elsif Is_Regular_File (Arg & ".ali")
                 and then Ali_File_Name = null
               then
                  Ali_File_Name := new String'(Arg & ".ali");

               elsif Arg'Length > Get_Object_Suffix.all'Length
                 and then Arg
                   (Arg'Last - Get_Object_Suffix.all'Length + 1 .. Arg'Last)
                                                = Get_Object_Suffix.all
               then
                  Linker_Objects.Increment_Last;
                  Linker_Objects.Table (Linker_Objects.Last) :=
                    new String'(Arg);

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
   end Process_Args;

   -------------------------
   -- Process_Binder_File --
   -------------------------

   procedure Process_Binder_File (Name : in String) is
      Fd           : FILEs;
      Link_Bytes   : Integer := 0;
      Link_Max     : Integer;
      pragma Import (C, Link_Max, "link_max");

      Next_Line    : String (1 .. 1000);
      Nlast        : Integer;
      Nfirst       : Integer;
      Objs_Begin   : Integer := 0;
      Objs_End     : Integer := 0;

      Status       : int;
      N            : Integer;

      GNAT_Static  : Boolean := False;
      --  Save state of -static option.

      GNAT_Shared  : Boolean := False;
      --  Save state of -shared option.

      Run_Path_Option_Ptr : Address;
      pragma Import (C, Run_Path_Option_Ptr, "run_path_option");
      --  Pointer to string representing the native linker option which
      --  specifies the path where the dynamic loader should find shared
      --  libraries. Equal to null string if this system doesn't support it.

      Object_Library_Ext_Ptr : Address;
      pragma Import (C, Object_Library_Ext_Ptr, "object_library_extension");
      --  Pointer to string specifying the default extension for
      --  object libraries, e.g. Unix uses ".a", VMS uses ".olb".

      Object_File_Option_Ptr : Address;
      pragma Import (C, Object_File_Option_Ptr, "object_file_option");
      --  Pointer to a string representing the linker option which specifies
      --  the response file.

      Using_GNU_Linker : Boolean;
      pragma Import (C, Using_GNU_Linker, "using_gnu_linker");
      --  Predicate indicating whether this target uses the GNU linker. In
      --  this case we must output a GNU linker compatible response file.

      procedure Get_Next_Line;
      --  Read the next line from the binder file without the line
      --  terminator.

      function Is_Option_Present (Opt : in String) return Boolean;
      --  Return true if the option Opt is already present in
      --  Linker_Options table.

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

      function Is_Option_Present (Opt : in String) return Boolean is
      begin
         for I in 1 .. Linker_Options.Last loop

            if Linker_Options.Table (I).all = Opt then
               return True;
            end if;

         end loop;

         return False;
      end Is_Option_Present;

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
         --  No_Run_Time mode where no -L switches are generated)

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

         Link_Bytes := Link_Bytes + Nlast - Nfirst;
      end loop;

      Objs_End := Linker_Objects.Last;

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
         --  needed by the link. This list is taken from the bind file
         --  and is output one object per line for maximal compatibility with
         --  linkers supporting this option.

         Create_Temp_File (Tname_FD, Tname);

         --  If target is using the GNU linker we must add a special header
         --  and footer in the response file.
         --  The syntax is : INPUT (object1.o object2.o ... )

         if Using_GNU_Linker then
            declare
               GNU_Header : aliased constant String := "INPUT (";

            begin
               Status := Write (Tname_FD, GNU_Header'Address,
                 GNU_Header'Length);
            end;
         end if;

         for J in Objs_Begin .. Objs_End loop
            Status := Write (Tname_FD, Linker_Objects.Table (J).all'Address,
              Linker_Objects.Table (J).all'Length);
            Status := Write (Tname_FD, ASCII.LF'Address, 1);

            Response_File_Objects.Increment_Last;
            Response_File_Objects.Table (Response_File_Objects.Last) :=
              Linker_Objects.Table (J);
         end loop;

         --  handle GNU linker response file footer.

         if Using_GNU_Linker then
            declare
               GNU_Footer : aliased constant String := ")";

            begin
               Status := Write (Tname_FD, GNU_Footer'Address,
                 GNU_Footer'Length);
            end;
         end if;

         Close (Tname_FD);

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

         N := Objs_End - Objs_Begin + 1;
         for J in Objs_End + 1 .. Linker_Objects.Last loop
            Linker_Objects.Table (J - N + 1) := Linker_Objects.Table (J);
         end loop;

         Linker_Objects.Set_Last (Linker_Objects.Last - N + 1);
      end if;

      --  Process switches and options

      if Next_Line (Nfirst .. Nlast) /= End_Info then
         loop
            --  Add binder options only if not already set on the command
            --  line. This rule is a way to control the linker options order.

            if not Is_Option_Present
              (Next_Line (Nfirst .. Nlast))
            then
               if Next_Line (Nfirst .. Nlast) = "-static" then
                  GNAT_Static := True;

               elsif Next_Line (Nfirst .. Nlast) = "-shared" then
                  GNAT_Shared := True;

               else
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
                  then
                     --  Given a Gnat standard library, search the
                     --  library path to find the library location
                     declare
                        File_Path : String_Access;

                        Object_Lib_Extension : constant String :=
                                                 Value
                                                   (Object_Library_Ext_Ptr);

                        File_Name : String :=
                                      "lib" &
                                        Next_Line (Nfirst + 2 .. Nlast) &
                                        Object_Lib_Extension;

                     begin
                        File_Path :=
                          Locate_Regular_File
                           (File_Name,
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

                              --  If shared gnatlib desired, add the
                              --  appropriate system specific switch
                              --  so that it can be located at runtime.

                              declare
                                 Run_Path_Opt : constant String :=
                                                  Value
                                                    (Run_Path_Option_Ptr);

                              begin
                                 if Run_Path_Opt'Length /= 0 then

                                    --  Output the system specific linker
                                    --  command that allows the image
                                    --  activator to find the shared library
                                    --  at runtime.

                                    Linker_Options.Increment_Last;

                                    Linker_Options.Table
                                     (Linker_Options.Last) :=
                                       new String'(Run_Path_Opt
                                          & File_Path
                                            (1 .. File_Path'Length
                                                   - File_Name'Length));
                                 end if;

                                 Linker_Options.Increment_Last;

                                 Linker_Options.Table
                                  (Linker_Options.Last) :=
                                   new String'(Next_Line
                                                (Nfirst .. Nlast));

                              end;
                           end if;

                        else
                           --  If gnatlib library not found, then
                           --  add it anyway in case some other
                           --  mechanimsm may find it.

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
            end if;

            Get_Next_Line;
            exit when Next_Line (Nfirst .. Nlast) = End_Info;

            if Ada_Bind_File then
               Next_Line (Nfirst .. Nlast - 8) :=
                 Next_Line (Nfirst + 8 .. Nlast);
               Nlast := Nlast - 8;
            end if;
         end loop;
      end if;

      Status := fclose (Fd);
   end Process_Binder_File;

   -----------
   -- Value --
   -----------

   function Value (chars : chars_ptr) return String is
      function Strlen (chars : chars_ptr) return Natural;
      pragma Import (C, Strlen);

   begin
      if chars = Null_Address then
         return "";

      else
         declare
            subtype Result_Type is String (1 .. Strlen (chars));

            Result : Result_Type;
            for Result'Address use chars;

         begin
            return Result;
         end;
      end if;
   end Value;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage is
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
      Write_Line ("  -v    verbose mode");
      Write_Line ("  -v -v very verbose mode");
      Write_Eol;
      Write_Line ("  -o nam     Use 'nam' as the name of the executable");
      Write_Line ("  -b target  Compile the binder source to run on target");
      Write_Line ("  -Bdir      Load compiler executables from dir");
      Write_Line ("  --GCC=comp Use comp as the compiler");
      Write_Line ("  --LINK=nam Use 'nam' for the linking rather than 'gcc'");
      Write_Eol;
      Write_Line ("  [non-Ada-objects]  list of non Ada object files");
      Write_Line ("  [linker-options]   other options for the linker");
   end Write_Usage;

--  Start of processing for Gnatlink

begin

   if Argument_Count = 0 then
      Write_Usage;
      Exit_Program (E_Fatal);
   end if;

   if Hostparm.Java_VM then
      Gcc := new String'("jgnat");
      Ada_Bind_File := True;
      Begin_Info := "-- BEGIN Object file/option list";
      End_Info   := "-- END Object file/option list   ";
   end if;

   Process_Args;

   --  Locate all the necessary programs and verify required files are present

   Gcc_Path := GNAT.OS_Lib.Locate_Exec_On_Path (Gcc.all);

   if Gcc_Path = null then
      Exit_With_Error ("Couldn't locate " & Gcc.all);
   end if;

   if Linker_Path = null then
      Linker_Path := Gcc_Path;
   end if;

   if Ali_File_Name = null then
      Exit_With_Error ("Required 'name'.ali not present.");
   end if;

   if not Is_Regular_File (Ali_File_Name.all) then
      Exit_With_Error (Ali_File_Name.all & " not found.");
   end if;

   if Verbose_Mode then
      Write_Eol;
      Write_Str ("GNATLINK ");
      Write_Str (Gnat_Version_String);
      Write_Str (" Copyright 1996-2001 Free Software Foundation, Inc.");
      Write_Eol;
   end if;

   --  If there wasn't an output specified, then use the base name of
   --  the .ali file name.

   if Output_File_Name = null then

      Output_File_Name :=
        new String'(Base_Name (Ali_File_Name.all)
                       & Get_Debuggable_Suffix.all);

      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) :=
        new String'("-o");

      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) :=
        new String'(Output_File_Name.all);

   end if;

   --  Warn if main program is called "test", as that may be a built-in command
   --  on Unix. On non-Unix systems executables have a suffix, so the warning
   --  will not appear. However, do not warn in the case of a cross compiler.

   --  Assume that if the executable name is not gnatlink, this is a cross
   --  tool.

   if Base_Name (Command_Name) = "gnatlink"
     and then Output_File_Name.all = "test"
   then
      Error_Msg ("warning: executable name """ & Output_File_Name.all
                   & """ may conflict with shell command");
   end if;

   --  Perform consistency checks

   --  Transform the .ali file name into the binder output file name.

   Make_Binder_File_Names : declare
      Fname     : String  := Base_Name (Ali_File_Name.all);
      Fname_Len : Integer := Fname'Length;

      function Get_Maximum_File_Name_Length return Integer;
      pragma Import (C, Get_Maximum_File_Name_Length,
                        "__gnat_get_maximum_file_name_length");

      Maximum_File_Name_Length : Integer := Get_Maximum_File_Name_Length;

      Second_Char : Character;
      --  Second character of name of files

   begin
      --  Set proper second character of file name

      if not Ada_Bind_File then
         Second_Char := '_';

      elsif Hostparm.OpenVMS then
         Second_Char := '$';

      else
         Second_Char := '~';
      end if;

      --  If the length of the binder file becomes too long due to
      --  the addition of the "b?" prefix, then truncate it.

      if Maximum_File_Name_Length > 0 then
         while Fname_Len > Maximum_File_Name_Length - 2 loop
            Fname_Len := Fname_Len - 1;
         end loop;
      end if;

      if Ada_Bind_File then
         Binder_Spec_Src_File :=
           new String'('b'
                       & Second_Char
                       & Fname (Fname'First .. Fname'First + Fname_Len - 1)
                       & ".ads");
         Binder_Body_Src_File :=
           new String'('b'
                       & Second_Char
                       & Fname (Fname'First .. Fname'First + Fname_Len - 1)
                       & ".adb");
         Binder_Ali_File :=
           new String'('b'
                       & Second_Char
                       & Fname (Fname'First .. Fname'First + Fname_Len - 1)
                       & ".ali");

      else
         Binder_Body_Src_File :=
           new String'('b'
                       & Second_Char
                       & Fname (Fname'First .. Fname'First + Fname_Len - 1)
                       & ".c");
      end if;

      Binder_Obj_File :=
        new String'('b'
                    & Second_Char
                    & Fname (Fname'First .. Fname'First + Fname_Len - 1)
                    & Get_Object_Suffix.all);

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
         Args    : Argument_List (1 .. Binder_Options.Last + 1);

      begin
         for J in Binder_Options.First .. Binder_Options.Last loop
            Args (J) := Binder_Options.Table (J);
         end loop;

         Args (Args'Last) := Binder_Body_Src_File;

         if Verbose_Mode then
            Write_Str (Base_Name (Gcc_Path.all));

            for J in Args'Range loop
               Write_Str (" ");
               Write_Str (Args (J).all);
            end loop;

            Write_Eol;
         end if;

         GNAT.OS_Lib.Spawn (Gcc_Path.all, Args, Success);

         if not Success then
            Exit_Program (E_Fatal);
         end if;
      end Bind_Step;
   end if;

   --  Now, actually link the program.

   --  Skip this step for now on the JVM since the Java interpreter will do
   --  the actual link at run time. We might consider packing all class files
   --  in a .zip file during this step.

   if not Hostparm.Java_VM then
      Link_Step : declare
         Num_Args : Natural :=
                     (Linker_Options.Last - Linker_Options.First + 1) +
                     (Gcc_Linker_Options.Last - Gcc_Linker_Options.First + 1) +
                     (Linker_Objects.Last - Linker_Objects.First + 1);
         Stack_Op : Boolean := False;
         IDENT_Op : Boolean := False;

      begin
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
         --  specific tasking package System.Task_Primitives.Oparations.

         for J in Linker_Options.First .. Linker_Options.Last loop
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
              and then Linker_Options.Table (J) (1 .. 27)
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
         end loop;

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

            GNAT.OS_Lib.Spawn (Linker_Path.all, Args, Success);

            --  Delete the temporary file used in conjuction with linking if
            --  one was created. See Process_Bind_File for details.

            if Tname_FD /= Invalid_FD then
               Delete (Tname);
            end if;

            if not Success then
               Error_Msg ("cannot call " & Linker_Path.all);
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

      if not Hostparm.Java_VM then
         Delete (Binder_Obj_File.all & ASCII.NUL);
      end if;
   end if;

   Exit_Program (E_Success);

exception
   when others =>
      Exit_With_Error ("INTERNAL ERROR. Please report.");
end Gnatlink;
