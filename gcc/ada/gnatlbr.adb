------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T L B R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2004 Free Software Foundation, Inc.          --
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

--  Program to create, set, or delete an alternate runtime library.

--  Works by calling an appropriate target specific Makefile residing
--  in the default library object (e.g. adalib) directory from the context
--  of the new library objects directory.

--  Command line arguments are:
--  1st:  --[create | set | delete]=<directory_spec>
--    --create : Build a library
--    --set    : Set environment variables to point to a library
--    --delete : Delete a library

--  2nd:  --config=<file_spec>
--  A -gnatg valid file containing desired configuration pragmas

--  This program is currently used only on Alpha/VMS

with Ada.Command_Line;     use Ada.Command_Line;
with Ada.Text_IO;          use Ada.Text_IO;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Gnatvsn;              use Gnatvsn;
with Interfaces.C_Streams; use Interfaces.C_Streams;
with Osint;                use Osint;
with System;

procedure GnatLbr is
   pragma Ident (Gnat_Static_Version_String);

   type Lib_Mode is (None, Create, Set, Delete);
   Next_Arg  : Integer;
   Mode      : Lib_Mode := None;
   ADC_File  : String_Access := null;
   Lib_Dir   : String_Access := null;
   Make      : constant String := "make";
   Make_Path : String_Access;

   procedure Create_Directory (Name : System.Address; Mode : Integer);
   pragma Import (C, Create_Directory, "mkdir");

begin
   if Argument_Count = 0 then
      Put ("Usage: ");
      Put_Line
        ("gnatlbr --[create|set|delete]=<directory> [--config=<file>]");
      Exit_Program (E_Fatal);
   end if;

   Next_Arg := 1;

   loop
      exit when Next_Arg > Argument_Count;

      Process_One_Arg : declare
         Arg : constant String := Argument (Next_Arg);

      begin
         if Arg'Length > 9 and then Arg (1 .. 9) = "--create=" then
            if Mode = None then
               Mode := Create;
               Lib_Dir := new String'(Arg (10 .. Arg'Last));
            else
               Put_Line (Standard_Error, "Error: Multiple modes specified");
               Exit_Program (E_Fatal);
            end if;

         elsif Arg'Length > 6 and then Arg (1 .. 6) = "--set=" then
            if Mode = None then
               Mode := Set;
               Lib_Dir := new String'(Arg (7 .. Arg'Last));
            else
               Put_Line (Standard_Error, "Error: Multiple modes specified");
               Exit_Program (E_Fatal);
            end if;

         elsif Arg'Length > 9 and then Arg (1 .. 9) = "--delete=" then
            if Mode = None then
               Mode := Delete;
               Lib_Dir := new String'(Arg (10 .. Arg'Last));
            else
               Put_Line (Standard_Error, "Error: Multiple modes specified");
               Exit_Program (E_Fatal);
            end if;

         elsif Arg'Length > 9 and then Arg (1 .. 9) = "--config=" then
            if ADC_File /= null then
               Put_Line (Standard_Error,
                         "Error: Multiple gnat.adc files specified");
               Exit_Program (E_Fatal);
            end if;

            ADC_File := new String'(Arg (10 .. Arg'Last));

         else
            Put_Line (Standard_Error, "Error: Unrecognized option: " & Arg);
            Exit_Program (E_Fatal);

         end if;
      end Process_One_Arg;

      Next_Arg := Next_Arg + 1;
   end loop;

   case Mode is
      when Create =>

         --  Validate arguments

         if Lib_Dir = null then
            Put_Line (Standard_Error, "Error: No library directory specified");
            Exit_Program (E_Fatal);
         end if;

         if Is_Directory (Lib_Dir.all) then
            Put_Line (Standard_Error,
                      "Error:" & Lib_Dir.all & " already exists");
            Exit_Program (E_Fatal);
         end if;

         if ADC_File = null then
            Put_Line (Standard_Error,
                      "Error: No configuration file specified");
            Exit_Program (E_Fatal);
         end if;

         if not Is_Regular_File (ADC_File.all) then
            Put_Line (Standard_Error,
                      "Error: " & ADC_File.all & " doesn't exist");
            Exit_Program (E_Fatal);
         end if;

         Create_Block : declare
            Success        : Boolean;
            Make_Args      : Argument_List (1 .. 9);
            C_Lib_Dir      : String := Lib_Dir.all & ASCII.Nul;
            C_ADC_File     : String := ADC_File.all & ASCII.Nul;
            F_ADC_File     : String (1 .. max_path_len);
            F_ADC_File_Len : Integer := max_path_len;
            Include_Dirs   : Integer;
            Object_Dirs    : Integer;
            Include_Dir    : array (Integer range 1 .. 256) of String_Access;
            Object_Dir     : array (Integer range 1 .. 256) of String_Access;
            Include_Dir_Name : String_Access;
            Object_Dir_Name  : String_Access;

         begin
            --  Create the new top level library directory

            if not Is_Directory (Lib_Dir.all) then
               Create_Directory (C_Lib_Dir'Address, 8#755#);
            end if;

            full_name (C_ADC_File'Address, F_ADC_File'Address);

            for I in 1 .. max_path_len loop
               if F_ADC_File (I) = ASCII.Nul then
                  F_ADC_File_Len := I - 1;
                  exit;
               end if;
            end loop;

            --
            --  Make a list of the default library source and object
            --  directories.  Usually only one, except on VMS where
            --  there are two.
            --
            Include_Dirs := 0;
            Include_Dir_Name := new String'(Include_Dir_Default_Prefix);
            Get_Next_Dir_In_Path_Init (Include_Dir_Name);

            loop
               declare
                  Dir : constant String_Access := String_Access
                    (Get_Next_Dir_In_Path (Include_Dir_Name));
               begin
                  exit when Dir = null;
                  Include_Dirs := Include_Dirs + 1;
                  Include_Dir (Include_Dirs) :=
                    String_Access (Normalize_Directory_Name (Dir.all));
               end;
            end loop;

            Object_Dirs := 0;
            Object_Dir_Name := new String'(Object_Dir_Default_Prefix);
            Get_Next_Dir_In_Path_Init (Object_Dir_Name);

            loop
               declare
                  Dir : constant String_Access :=
                          String_Access
                            (Get_Next_Dir_In_Path (Object_Dir_Name));
               begin
                  exit when Dir = null;
                  Object_Dirs := Object_Dirs + 1;
                  Object_Dir (Object_Dirs)
                    := String_Access (Normalize_Directory_Name (Dir.all));
               end;
            end loop;

            --  "Make" an alternate sublibrary for each default sublibrary.

            for Dirs in 1 .. Object_Dirs loop
               Make_Args (1) :=
                 new String'("-C");

               Make_Args (2) :=
                 new String'(Lib_Dir.all);

               --  Resolve /gnu on VMS by converting to host format and then
               --  convert resolved path back to canonical format for the
               --  make program. This fixes the problem that can occur when
               --  GNU: is a search path pointing to multiple versions of GNAT.

               Make_Args (3) :=
                 new String'("ADA_INCLUDE_PATH=" &
                   To_Canonical_Dir_Spec
                     (To_Host_Dir_Spec
                       (Include_Dir (Dirs).all, True).all, True).all);

               Make_Args (4) :=
                 new String'("ADA_OBJECTS_PATH=" &
                   To_Canonical_Dir_Spec
                     (To_Host_Dir_Spec
                       (Object_Dir (Dirs).all, True).all, True).all);

               Make_Args (5) :=
                 new String'("GNAT_ADC_FILE="
                             & F_ADC_File (1 .. F_ADC_File_Len));

               Make_Args (6) :=
                 new String'("LIBRARY_VERSION=" & '"' &
                             Verbose_Library_Version & '"');

               Make_Args (7) :=
                 new String'("-f");

               Make_Args (8) :=
                 new String'(Object_Dir (Dirs).all & "Makefile.lib");

               Make_Args (9) :=
                 new String'("create");

               Make_Path := Locate_Exec_On_Path (Make);
               Put (Make);

               for J in 1 .. Make_Args'Last loop
                  Put (" ");
                  Put (Make_Args (J).all);
               end loop;

               New_Line;
               Spawn (Make_Path.all, Make_Args, Success);

               if not Success then
                  Put_Line (Standard_Error, "Error: Make failed");
                  Exit_Program (E_Fatal);
               end if;
            end loop;
         end Create_Block;

      when Set =>

         --  Validate arguments

         if Lib_Dir = null then
            Put_Line (Standard_Error,
                      "Error: No library directory specified");
            Exit_Program (E_Fatal);
         end if;

         if not Is_Directory (Lib_Dir.all) then
            Put_Line (Standard_Error,
                      "Error: " & Lib_Dir.all & " doesn't exist");
            Exit_Program (E_Fatal);
         end if;

         if ADC_File = null then
            Put_Line (Standard_Error,
                      "Error: No configuration file specified");
            Exit_Program (E_Fatal);
         end if;

         if not Is_Regular_File (ADC_File.all) then
            Put_Line (Standard_Error,
                      "Error: " & ADC_File.all & " doesn't exist");
            Exit_Program (E_Fatal);
         end if;

         --  Give instructions

         Put_Line ("Copy the contents of "
           & ADC_File.all & " into your GNAT.ADC file");
         Put_Line ("and use GNAT Make qualifier /OBJECT_SEARCH=("
           & To_Host_Dir_Spec
               (Lib_Dir (Lib_Dir'First .. Lib_Dir'Last) & "/declib", False).all
           & ","
           & To_Host_Dir_Spec
               (Lib_Dir (Lib_Dir'First .. Lib_Dir'Last) & "/adalib", False).all
           & ")");
         Put_Line ("or else define ADA_OBJECTS_PATH as " & '"'
           & To_Host_Dir_Spec
               (Lib_Dir (Lib_Dir'First .. Lib_Dir'Last) & "/declib", False).all
           & ','
           & To_Host_Dir_Spec
               (Lib_Dir (Lib_Dir'First .. Lib_Dir'Last) & "/adalib", False).all
           & '"');

      when Delete =>

         --  Give instructions

         Put_Line ("GNAT Librarian DELETE not yet implemented.");
         Put_Line ("Use appropriate system tools to remove library");

      when None =>
         Put_Line (Standard_Error,
                   "Error: No mode (create|set|delete) specified");
         Exit_Program (E_Fatal);

   end case;

end GnatLbr;
