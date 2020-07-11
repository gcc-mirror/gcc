------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T D L L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2020, Free Software Foundation, Inc.         --
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

--  GNATDLL is a Windows specific tool for building a DLL.
--  Both relocatable and non-relocatable DLL's are supported

with Gnatvsn;
with MDLL.Fil; use MDLL.Fil;
with MDLL.Utl;
with Switch;   use Switch;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Command_Line;      use Ada.Command_Line;

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GNAT.Command_Line; use GNAT.Command_Line;

procedure Gnatdll is

   procedure Syntax;
   --  Print out usage

   procedure Check (Filename : String);
   --  Check that the file whose name is Filename exists

   procedure Parse_Command_Line;
   --  Parse the command line arguments passed to gnatdll

   procedure Check_Context;
   --  Check the context before running any commands to build the library

   Syntax_Error : exception;
   --  Raised when a syntax error is detected, in this case a usage info will
   --  be displayed.

   Context_Error : exception;
   --  Raised when some files (specified on the command line) are missing to
   --  build the DLL.

   Help : Boolean := False;
   --  Help will be set to True the usage information is to be displayed

   Version : constant String := Gnatvsn.Gnat_Version_String;
   --  Why should it be necessary to make a copy of this

   Default_DLL_Address : constant String := "0x11000000";
   --  Default address for non relocatable DLL (Win32)

   Lib_Filename : Unbounded_String := Null_Unbounded_String;
   --  The DLL filename that will be created (.dll)

   Def_Filename : Unbounded_String := Null_Unbounded_String;
   --  The definition filename (.def)

   List_Filename : Unbounded_String := Null_Unbounded_String;
   --  The name of the file containing the objects file to put into the DLL

   DLL_Address : Unbounded_String := To_Unbounded_String (Default_DLL_Address);
   --  The DLL's base address

   Gen_Map_File : Boolean := False;
   --  Set to True if a map file is to be generated

   Objects_Files : Argument_List_Access := MDLL.Null_Argument_List_Access;
   --  List of objects to put inside the library

   Ali_Files : Argument_List_Access := MDLL.Null_Argument_List_Access;
   --  For each Ada file specified, we keep a record of the corresponding
   --  ALI file. This list of SLI files is used to build the binder program.

   Options : Argument_List_Access := MDLL.Null_Argument_List_Access;
   --  A list of options set in the command line

   Largs_Options : Argument_List_Access := MDLL.Null_Argument_List_Access;
   Bargs_Options : Argument_List_Access := MDLL.Null_Argument_List_Access;
   --  GNAT linker and binder args options

   type Build_Mode_State is (Import_Lib, Dynamic_Lib, Dynamic_Lib_Only, Nil);
   --  Import_Lib means only the .a file will be created, Dynamic_Lib means
   --  that both the DLL and the import library will be created.
   --  Dynamic_Lib_Only means that only the DLL will be created (no import
   --  library).

   Build_Mode : Build_Mode_State := Nil;
   --  Will be set when parsing the command line

   Must_Build_Relocatable : Boolean := True;
   --  True means build a relocatable DLL, will be set to False if a
   --  non-relocatable DLL must be built.

   ------------
   -- Syntax --
   ------------

   procedure Syntax is
      procedure P (Str : String) renames Put_Line;
   begin
      P ("Usage : gnatdll [options] [list-of-files]");
      New_Line;
      P ("[list-of-files] a list of Ada libraries (.ali) and/or " &
         "foreign object files");
      New_Line;
      P ("[options] can be");
      P ("   -h            Help - display this message");
      P ("   -v            Verbose");
      P ("   -q            Quiet");
      P ("   -k            Remove @nn suffix from exported names");
      P ("   -g            Generate debugging information");
      P ("   -Idir         Specify source and object files search path");
      P ("   -l file       File contains a list-of-files to be added to "
         & "the library");
      P ("   -e file       Definition file containing exports");
      P ("   -d file       Put objects in the relocatable dynamic "
         & "library <file>");
      P ("   -b addr       Set base address for the relocatable DLL");
      P ("                 default address is " & Default_DLL_Address);
      P ("   -a[addr]      Build non-relocatable DLL at address <addr>");
      P ("                 if <addr> is not specified use "
         & Default_DLL_Address);
      P ("   -m            Generate map file");
      P ("   -n            No-import - do not create the import library");
      P ("   -bargs opts   opts are passed to the binder");
      P ("   -largs opts   opts are passed to the linker");
   end Syntax;

   -----------
   -- Check --
   -----------

   procedure Check (Filename : String) is
   begin
      if not Is_Regular_File (Filename) then
         Raise_Exception
           (Context_Error'Identity, "Error: " & Filename & " not found.");
      end if;
   end Check;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is

      procedure Add_File (Filename : String);
      --  Add one file to the list of file to handle

      procedure Add_Files_From_List (List_Filename : String);
      --  Add the files listed in List_Filename (one by line) to the list
      --  of file to handle

      Max_Files   : constant := 5_000;
      Max_Options : constant :=   100;
      --  These are arbitrary limits, a better way will be to use linked list.
      --  No, a better choice would be to use tables ???
      --  Limits on what???

      Ofiles : Argument_List (1 .. Max_Files);
      O      : Positive := Ofiles'First;
      --  List of object files to put in the library. O is the next entry
      --  to be used.

      Afiles : Argument_List (1 .. Max_Files);
      A      : Positive := Afiles'First;
      --  List of ALI files. A is the next entry to be used

      Gopts  : Argument_List (1 .. Max_Options);
      G      : Positive := Gopts'First;
      --  List of gcc options. G is the next entry to be used

      Lopts  : Argument_List (1 .. Max_Options);
      L      : Positive := Lopts'First;
      --  A list of -largs options (L is next entry to be used)

      Bopts  : Argument_List (1 .. Max_Options);
      B      : Positive := Bopts'First;
      --  A list of -bargs options (B is next entry to be used)

      Build_Import : Boolean := True;
      --  Set to False if option -n if specified (no-import)

      --------------
      -- Add_File --
      --------------

      procedure Add_File (Filename : String) is
      begin
         if Is_Ali (Filename) then
            Check (Filename);

            --  Record it to generate the binder program when
            --  building dynamic library

            Afiles (A) := new String'(Filename);
            A := A + 1;

         elsif Is_Obj (Filename) then
            Check (Filename);

            --  Just record this object file

            Ofiles (O) := new String'(Filename);
            O := O + 1;

         else
            --  Unknown file type

            Raise_Exception
              (Syntax_Error'Identity,
               "don't know what to do with " & Filename & " !");
         end if;
      end Add_File;

      -------------------------
      -- Add_Files_From_List --
      -------------------------

      procedure Add_Files_From_List (List_Filename : String) is
         File   : File_Type;
         Buffer : String (1 .. 500);
         Last   : Natural;

      begin
         Open (File, In_File, List_Filename);

         while not End_Of_File (File) loop
            Get_Line (File, Buffer, Last);
            Add_File (Buffer (1 .. Last));
         end loop;

         Close (File);

      exception
         when Name_Error =>
            Raise_Exception
              (Syntax_Error'Identity,
               "list-of-files file " & List_Filename & " not found.");
      end Add_Files_From_List;

   --  Start of processing for Parse_Command_Line

   begin
      Initialize_Option_Scan ('-', False, "bargs largs");

      --  scan gnatdll switches

      loop
         case Getopt ("g h v q k a? b: d: e: l: n m I:") is
            when ASCII.NUL =>
               exit;

            when 'h' =>
               Help := True;

            when 'g' =>
               Gopts (G) := new String'("-g");
               G := G + 1;

            when 'v' =>

               --  Turn verbose mode on

               MDLL.Verbose := True;
               if MDLL.Quiet then
                  Raise_Exception
                    (Syntax_Error'Identity,
                     "impossible to use -q and -v together.");
               end if;

            when 'q' =>

               --  Turn quiet mode on

               MDLL.Quiet := True;
               if MDLL.Verbose then
                  Raise_Exception
                    (Syntax_Error'Identity,
                     "impossible to use -v and -q together.");
               end if;

            when 'k' =>
               MDLL.Kill_Suffix := True;

            when 'a' =>
               if Parameter = "" then

                  --  Default address for a relocatable dynamic library.
                  --  address for a non relocatable dynamic library.

                  DLL_Address := To_Unbounded_String (Default_DLL_Address);

               else
                  DLL_Address := To_Unbounded_String (Parameter);
               end if;

               Must_Build_Relocatable := False;

            when 'b' =>
               DLL_Address := To_Unbounded_String (Parameter);
               Must_Build_Relocatable := True;

            when 'e' =>
               Def_Filename := To_Unbounded_String (Parameter);

            when 'd' =>

               --  Build a non relocatable DLL

               Lib_Filename := To_Unbounded_String (Parameter);

               if Def_Filename = Null_Unbounded_String then
                  Def_Filename := To_Unbounded_String
                    (Ext_To (Parameter, "def"));
               end if;

               Build_Mode := Dynamic_Lib;

            when 'm' =>
               Gen_Map_File := True;

            when 'n' =>
               Build_Import := False;

            when 'l' =>
               List_Filename := To_Unbounded_String (Parameter);

            when 'I' =>
               Gopts (G) := new String'("-I" & Parameter);
               G := G + 1;

            when others =>
               raise Invalid_Switch;
         end case;
      end loop;

      --  Get parameters

      loop
         declare
            File : constant String := Get_Argument (Do_Expansion => True);
         begin
            exit when File'Length = 0;
            Add_File (File);
         end;
      end loop;

      --  Get largs parameters

      Goto_Section ("largs");

      loop
         case Getopt ("*") is
            when ASCII.NUL =>
               exit;

            when others =>
               Lopts (L) := new String'(Full_Switch);
               L := L + 1;
         end case;
      end loop;

      --  Get bargs parameters

      Goto_Section ("bargs");

      loop
         case Getopt ("*") is
            when ASCII.NUL =>
               exit;

            when others =>
               Bopts (B) := new String'(Full_Switch);
               B := B + 1;
         end case;
      end loop;

      --  if list filename has been specified, parse it

      if List_Filename /= Null_Unbounded_String then
         Add_Files_From_List (To_String (List_Filename));
      end if;

      --  Check if the set of parameters are compatible

      if Build_Mode = Nil and then not Help and then not MDLL.Verbose then
         Raise_Exception (Syntax_Error'Identity, "nothing to do.");
      end if;

      --  -n option but no file specified

      if not Build_Import
        and then A = Afiles'First
        and then O = Ofiles'First
      then
         Raise_Exception
           (Syntax_Error'Identity,
            "-n specified but there are no objects to build the library.");
      end if;

      --  Check if we want to build an import library (option -e and
      --  no file specified)

      if Build_Mode = Dynamic_Lib
        and then A = Afiles'First
        and then O = Ofiles'First
      then
         Build_Mode := Import_Lib;
      end if;

      --  If map file is to be generated, add linker option here

      if Gen_Map_File and then Build_Mode = Import_Lib then
         Raise_Exception
           (Syntax_Error'Identity,
            "Can't generate a map file for an import library.");
      end if;

      --  Check if only a dynamic library must be built

      if Build_Mode = Dynamic_Lib and then not Build_Import then
         Build_Mode := Dynamic_Lib_Only;
      end if;

      if O /= Ofiles'First then
         Objects_Files := new Argument_List'(Ofiles (1 .. O - 1));
      end if;

      if A /= Afiles'First then
         Ali_Files     := new Argument_List'(Afiles (1 .. A - 1));
      end if;

      if G /= Gopts'First then
         Options       := new Argument_List'(Gopts (1 .. G - 1));
      end if;

      if L /= Lopts'First then
         Largs_Options := new Argument_List'(Lopts (1 .. L - 1));
      end if;

      if B /= Bopts'First then
         Bargs_Options := new Argument_List'(Bopts (1 .. B - 1));
      end if;

   exception
      when Invalid_Switch    =>
         Raise_Exception
           (Syntax_Error'Identity,
            Message => "Invalid Switch " & Full_Switch);

      when Invalid_Parameter =>
         Raise_Exception
           (Syntax_Error'Identity,
            Message => "No parameter for " & Full_Switch);
   end Parse_Command_Line;

   -------------------
   -- Check_Context --
   -------------------

   procedure Check_Context is
   begin
      Check (To_String (Def_Filename));

      --  Check that each object file specified exists and raise exception
      --  Context_Error if it does not.

      for F in Objects_Files'Range loop
         Check (Objects_Files (F).all);
      end loop;
   end Check_Context;

   procedure Check_Version_And_Help is new Check_Version_And_Help_G (Syntax);

--  Start of processing for Gnatdll

begin
   Check_Version_And_Help ("GNATDLL", "1997");

   if Ada.Command_Line.Argument_Count = 0 then
      Help := True;
   else
      Parse_Command_Line;
   end if;

   if MDLL.Verbose or else Help then
      New_Line;
      Put_Line ("GNATDLL " & Version & " - Dynamic Libraries Builder");
      New_Line;
   end if;

   MDLL.Utl.Locate;

   if Help
     or else (MDLL.Verbose and then Ada.Command_Line.Argument_Count = 1)
   then
      Syntax;
   else
      Check_Context;

      case Build_Mode is
         when Import_Lib =>
            MDLL.Build_Import_Library
              (To_String (Lib_Filename),
               To_String (Def_Filename));

         when Dynamic_Lib =>
            MDLL.Build_Dynamic_Library
              (Objects_Files.all,
               Ali_Files.all,
               Options.all,
               Bargs_Options.all,
               Largs_Options.all,
               To_String (Lib_Filename),
               To_String (Def_Filename),
               To_String (DLL_Address),
               Build_Import => True,
               Relocatable  => Must_Build_Relocatable,
               Map_File     => Gen_Map_File);

         when Dynamic_Lib_Only =>
            MDLL.Build_Dynamic_Library
              (Objects_Files.all,
               Ali_Files.all,
               Options.all,
               Bargs_Options.all,
               Largs_Options.all,
               To_String (Lib_Filename),
               To_String (Def_Filename),
               To_String (DLL_Address),
               Build_Import => False,
               Relocatable  => Must_Build_Relocatable,
               Map_File     => Gen_Map_File);

         when Nil =>
            null;
      end case;
   end if;

   Set_Exit_Status (Success);

exception
   when SE : Syntax_Error =>
      Put_Line ("Syntax error : " & Exception_Message (SE));
      New_Line;
      Syntax;
      Set_Exit_Status (Failure);

   when E : MDLL.Tools_Error | Context_Error =>
      Put_Line (Exception_Message (E));
      Set_Exit_Status (Failure);

   when others =>
      Put_Line ("gnatdll: INTERNAL ERROR. Please report");
      Set_Exit_Status (Failure);
end Gnatdll;
